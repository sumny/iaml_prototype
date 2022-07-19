# Accurate Intelligible Models with Pairwise Interactions - Lou et al. 2013
# we assume integer or numeric features due to later using xgboost
# logicals must be converted to integers
# FIXME: check this and maybe use implementation from EBM
InteractionDetector = R6Class("InteractionDetector",
  public = list(
    initialize = function(task, grid_size = 10L) {
      assert_task(task, feature_types = c("integer", "numeric"))
      assert_int(grid_size, lower = 1L)
      xs = task$data(cols = task$feature_names)
      feature_names = task$feature_names
      feature_types = task$feature_types
      y = as.numeric(task$data(cols = task$target_names)[[1L]])  # regardless of regr or classif
      self$task = task
      self$xs = xs
      self$n_features = length(feature_names)
      self$feature_names = feature_names
      self$feature_types = feature_types
      self$y = y

      self$grids = map(feature_names, function(feature_name) {
        feature_type = feature_types[id == feature_name][["type"]]
        if (feature_type == "numeric") {
          seq(min(xs[[feature_name]]), max(xs[[feature_name]]), length.out = grid_size)
        } else if (feature_type == "integer") {
          min_x = min(xs[[feature_name]])
          max_x = max(xs[[feature_name]])
          by_x = ceiling((max_x - min_x) / grid_size)
          seq(min(xs[[feature_name]]), max(xs[[feature_name]]), by = by_x)
        }
      })
    },

    task = NULL,
    xs = NULL,
    n_features = NULL,
    feature_names = NULL,
    feature_types = NULL,
    y = NULL,
    grids = NULL,
    rss = NULL,

    compute_best_rss = function() {
      pb = progress_bar$new(format = "Interactions [:bar] :percent eta: :eta", total = (self$n_features * self$n_features - 1L) / 2L)
      rss = matrix(0, nrow = self$n_features, ncol = self$n_features)
        for (i in seq_len(self$n_features)) {
          for (j in seq_len(self$n_features)) {
            if (i < j) {
              pb$tick()
              rss[i, j] = compute_best_rss_pairwise(self$xs[[i]], self$xs[[j]], self$y, self$grids[[i]], self$grids[[j]])
            }
          }
        }
      self$rss = rss
    },

    get_eqcs_from_top_k = function(k = 1L, features = NULL) {
      assert_subset(features, choice = self$feature_names)
      if (is.null(features)) features = self$feature_names
      n_features = length(features)
      assert_int(k, lower = 1L, upper = (n_features * (n_features - 1L)) / 2L)
      rss = self$rss[match(features, self$feature_names), match(features, self$feature_names)]
      top_rss = sort(rss[upper.tri(rss)])[seq_len(k)]
      top_interactions = unlist(map(top_rss, function(x) asplit(which(rss == x, arr.ind = TRUE), MARGIN = 1L)), recursive = FALSE)
      relation_matrix = matrix(0, nrow = n_features, ncol = n_features)
      colnames(relation_matrix) = rownames(relation_matrix) = features
      for (interaction in top_interactions) {
        relation_matrix[interaction[1L], interaction[2L]] = 1
        relation_matrix[interaction[2L], interaction[1L]] = 1  # make symmetric
      }
      diag(relation_matrix) = 1  # make reflexive
      relation = relation(list(features, features), incidence = relation_matrix)
      relation = transitive_closure(relation)  # make transitive
      belonging = get_class_ids_from_incidence(relation_incidence(relation)[features, features, drop = FALSE])
      belonging
    }
  )
)

# CH_i^t(v) if lower.tail else \bar{CH}_i^t 
compute_cumsum_targets = function(x, v, y, lower.tail = TRUE) {
  if (lower.tail) {
    sum(y[x <= v])
  } else {
    sum(y[x > v])
  }
}

# CH_i^w(v) if lower.tail else \bar{CH}_i^w 
compute_cumsum_counts = function(x, v, lower.tail = TRUE) {
  if (lower.tail) {
    sum(x <= v)
  } else {
    sum(x > v)
  }
}

# H_ij^t(u, v)
compute_bivar_sum_targets = function(xi, xj, y, v, u) {
  sum(y[xi == v & xj == u])
}

# H_ij^t(u, v)
compute_bivar_sum_counts = function(xi, xj, v, u) {
  sum(xi == v & xj == u)
}

# CH_ij^t(u, v)
compute_bivar_cumsum_targets = function(xi, xj, y, v, u) {
  sum(y[xi <= v & xj <= u])
}

# CH_ij^t(u, v)
compute_bivar_cumsum_counts = function(xi, xj, v, u) {
  sum(xi <= v & xj <= u)
}

compute_values_targets = function(xi, xj, y, v, u, a) {
  cumsum_targets_i = compute_cumsum_targets(xi, v, y)
  cumsum_targets_j = compute_cumsum_targets(xj, u, y)
  cumsum_targets_i_upper = compute_cumsum_targets(xi, v, y, lower.tail = FALSE)
  b = cumsum_targets_i - a
  c = cumsum_targets_j - a
  d = cumsum_targets_i_upper - c
  c(a, b, c, d)
}

compute_values_counts = function(xi, xj, v, u, a) {
  cumsum_counts_i = compute_cumsum_counts(xi, v)
  cumsum_counts_j = compute_cumsum_counts(xj, u)
  cumsum_counts_i_upper = compute_cumsum_counts(xi, v, lower.tail = FALSE)
  b = cumsum_counts_i - a
  c = cumsum_counts_j - a
  d = cumsum_counts_i_upper - c
  c(a, b, c, d)
}

na.replace = function(x, value = 0) {
  x[is.na(x) | !is.finite(x)] = value
  x
}

compute_best_rss_pairwise = function(xi, xj, y, cuts_i, cuts_j) {
  n_cuts_i = length(cuts_i)
  n_cuts_j = length(cuts_j)

  # ConstructLookupTable targets
  sum = 0
  a_list_t = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  lookup_list_t = lapply(seq_len(n_cuts_i), function(x) vector("list", length = n_cuts_j))
  
  for (q in 1:n_cuts_j) {
    sum = sum + compute_bivar_sum_targets(xi, xj, y, cuts_i[1L], cuts_j[q])
    a_list_t[[1L]][q] = sum
    lookup_list_t[[1L]][[q]] = compute_values_targets(xi, xj, y, cuts_i[1L], cuts_j[q], a_list_t[[1L]][q])
  }
  
  for (p in 2:n_cuts_i) {
    sum = 0
    for (q in 1:n_cuts_j) {
      sum = sum + compute_bivar_sum_targets(xi, xj, y, cuts_i[p], cuts_j[q])
      a_list_t[[p]][q] = sum + a_list_t[[p - 1L]][q]
      lookup_list_t[[p]][[q]] = compute_values_targets(xi, xj, y, cuts_i[p], cuts_j[q], a_list_t[[p]][q])
    }
  }
  
  # ConstructLookupTable counts
  sum = 0
  a_list_w = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  lookup_list_w = lapply(seq_len(n_cuts_i), function(x) vector("list", length = n_cuts_j))
  
  for (q in 1:n_cuts_j) {
    sum = sum + compute_bivar_sum_counts(xi, xj, cuts_i[1L], cuts_j[q])
    a_list_w[[1L]][q] = sum
    lookup_list_w[[1L]][[q]] = compute_values_counts(xi, xj, cuts_i[1L], cuts_j[q], a_list_w[[1L]][q])
  }
  
  for (p in 2:n_cuts_i) {
    sum = 0
    for (q in 1:n_cuts_j) {
      sum = sum + compute_bivar_sum_counts(xi, xj, cuts_i[p], cuts_j[q])
      a_list_w[[p]][q] = sum + a_list_w[[p - 1L]][q]
      lookup_list_w[[p]][[q]] = compute_values_counts(xi, xj, cuts_i[p], cuts_j[q], a_list_w[[p]][q])
    }
  }
  
  
  rss = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  for (i in seq_len(n_cuts_i)) {
    for (j in seq_len(n_cuts_j)) {
      tijs = na.replace(lookup_list_t[[i]][[j]] / lookup_list_w[[i]][[j]])
      rss[[i]][j] = sum(tijs^2 *  lookup_list_w[[i]][[j]]) - 2 * sum(tijs * lookup_list_w[[i]][[j]])
    }
  }
  
  best_rss = min(unlist(rss))
  best_rss
}


########## test
if (FALSE) {
  task = tsk("wine")
  detector = InteractionDetector$new(task)
  detector$compute_best_rss()
  detector$get_eqcs_from_top_k(1)
}
