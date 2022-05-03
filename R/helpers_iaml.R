# function to get the eqcs
get_eqcs = function(x) {
  # x is output like sample_random
  map(x$eqcs, function(eqc) {
    x$features[x$belonging == eqc]
  })
}

# function to get the interaction matrix based on eqcs
get_matrix = function(x) {
  # x is output like get_eqcs
  features = unique(unlist(x))
  n_features = length(features)
  I = diag(1, nrow = n_features, ncol = n_features)
  rownames(I) = colnames(I) = features
  for (i in seq_along(x)) {
    indices = match(x[[i]], features)
    # if eqc only has one member do nothing
    if (length(indices) >= 2L) {
      interactions = utils::combn(indices, m = 2L)
      I[t(interactions)] = 1
    }
  }
  I[lower.tri(I)] = t(I)[lower.tri(I)]
  I
}

# sampling features to eqcs
sample_interactions_random = function(features) {
  n_eqcs = sample(seq_along(features), size = 1L)
  belonging = sample(seq_len(n_eqcs), size = length(features), replace = TRUE)
  belonging = match(belonging, sort(unique(belonging)))  # ordering without tie breaks
  list(features = features, eqcs = unique(belonging), belonging = belonging)
}

# sampling monotonicity
sample_m = function(I) {
  m = integer(nrow(I$I))
  n_classes = length(I$classes)
  m_sample = sample(c(-1L, 0L, 1L), size = n_classes, replace = TRUE)
  for (i in seq_len(n_classes)) {
    m[I$classes[[i]]] = m_sample[i]
  }
  m
}

mult_max_to_min = function(codomain) {
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), 1, -1)
}
