# ideen:
# eqcs Gruppen der groesse absteigend nach sortieren so werden beim crossover dann eher gleich grosse gruppen eingecrosst
# shuffle groups
IAMLPointNEW = R6Class("IAMLPoint",
  public = list(
    initialize = function(task, n_selected = NULL, scores = NULL, interaction_detector = NULL) {
      # checks and feature names
      assert_task(task, feature_types = c("integer", "numeric"))
      assert_int(n_selected, lower = 1L, upper = nrow(task$feature_types), null.ok = TRUE)
      assert_data_table(scores, null.ok = TRUE)
      assert_r6(interaction_detector, classes = "InteractionDetector", null.ok = TRUE)
      feature_names = task$feature_names
      n_features = length(feature_names)
      self$feature_names = feature_names

      # sample selected features based on scores
      selected_features = sort(sample(scores$feature, size = n_selected, replace = FALSE, prob = scores$score))

      # eqcs
      if (n_selected == 1L) {
        eqcs = list(features = selected_features, eqcs = 1L, belonging = 1L)
      } else {
        k = sample(seq_len((n_selected * (n_selected - 1L)) / 2L), size = 1L)
        belonging = interaction_detector$get_eqcs_from_top_k(k = k, features = selected_features)
        belonging = match(belonging, sort(unique(belonging)))  # make eqcs start at 1

        eqcs = unique(belonging)
        eqcs = list(features = selected_features, eqcs = sort(eqcs), belonging = belonging)
      }

      # create group structure (including the first group being unselected)
      self$groups = self$get_full_group_structure(eqcs, unselected_features = setdiff(feature_names, selected_features))

      # FIXME:
      # randomly sample monotonicity constraints of partitions
      monotone_eqcs = c(NA_integer_, sample(c(0L, 1L), size = length(self$groups$eqcs) - 1L, replace = TRUE))  # first one is the not selected group
      self$monotone_eqcs = data.table(eqcs = self$groups$eqcs, monotonicity = monotone_eqcs)
    },

    feature_names = NULL,

    get_groups = function() {
      map(self$groups$eqcs, function(eqc) {
        self$groups$features[self$groups$belonging == eqc]
      })
    },

    get_matrix = function() {
      x = self$get_groups()[-1L]  # first one is the not selected

      features = unique(unlist(x))
      stopifnot(setequal(features, self$selected_features))
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
      reorder = match(rownames(I), self$selected_features)
      I[reorder, reorder]
    },

    # FIXME: create_* should be ABs? or shouldn't they?

    create_selector = function() {
      s = selector_name(self$selected_features)
      # FIXME:
      attr(s, "n_selected") = self$n_selected
      attr(s, "n_selected_total") = self$n_features
      s
    },

    create_interaction_constraints = function() {
      I = list(I = self$get_matrix(), classes = map(self$get_groups()[-1L], function(x) match(x, self$selected_features)))
      interaction_constraints = I$classes
      n_interactions = sum(I$I[upper.tri(I$I)])
      stopifnot(nrow(I$I) == self$n_selected)  # I is only dim n_selected x n_selected
      n_interactions_total = self$n_selected * (self$n_selected - 1L) / 2L  # number of elements of upper tri without diag
      interaction_constraints = map(interaction_constraints, function(x) x - 1L)  # must start at 0
      # FIXME:
      attr(interaction_constraints, "n_interactions") = n_interactions
      attr(interaction_constraints, "n_interactions_total") = n_interactions_total
      interaction_constraints
    },

    create_monotonicity_constraints = function() {
      monotone_features = na.omit(self$monotone_features)
      stopifnot(setequal(monotone_features$feature, self$selected_features))
      monotonicity_constraints = setNames(monotone_features[["monotonicity"]], nm = monotone_features[["feature"]])[self$selected_features]
      n_non_monotone = sum(monotonicity_constraints == 0)
      n_non_monotone_total = length(monotonicity_constraints)
      # FIXME:
      attr(monotonicity_constraints, "n_non_monotone") = n_non_monotone
      attr(monotonicity_constraints, "n_non_monotone_total") = n_non_monotone_total
      monotonicity_constraints
    },

    get_full_group_structure = function(eqcs, unselected_features) {
      eqcs$eqcs = eqcs$eqcs + 1L  # 1 is now the unselected group
      eqcs$belonging = eqcs$belonging + 1L

      # add unselected as first group
      eqcs$features = c(unselected_features, eqcs$features)
      eqcs$eqcs = c(1L, eqcs$eqcs)
      eqcs$belonging = c(rep(1L, length(unselected_features)), eqcs$belonging)

      # reorder features and belonging alphabetically
      reorder = match(self$feature_names, eqcs$features)
      eqcs$features = eqcs$features[reorder]
      eqcs$belonging = eqcs$belonging[reorder]
      eqcs
    },

    mutate = function() {
      # create a new group, taking members from 1 (monotone_eqcs then 0)
      # destroy a group, put members in 1
      # shuffle membership
      # mutate group membership

      old_groups = self$groups
      old_monotone_eqcs = self$monotone_eqcs

      tryCatch(
      {
        groups = self$groups
        group_lookup = data.table(groups = groups$eqcs, reordered = seq_along(groups$eqcs))
        p = 0.2
        change_belonging = runif(length(groups$features)) < p
        if (sum(change_belonging) > 0) {
          new_group_id = max(groups$eqcs) + 1L
          group_lookup = rbind(group_lookup, data.table(groups = new_group_id, reordered = new_group_id))
          monotone_eqcs = rbind(self$monotone_eqcs, data.table(eqcs = new_group_id, monotonicity = 0L))
          new_belonging = sample(c(groups$eqcs, new_group_id), size = sum(change_belonging), replace = TRUE)
          belonging = groups$belonging
          belonging[change_belonging] = new_belonging

          # some older groups may now be unselected
          group_lookup = group_lookup[groups %in% unique(c(1L, belonging)), ]  # it may happend that "1_1" is empty
          group_lookup[, reordered := seq_len(.N)]

          belonging = group_lookup$reordered[match(belonging, group_lookup$groups)]
          eqcs = unique(belonging)
          self$groups = list(features = groups$features, eqcs = unique(c(1L, sort(eqcs))), belonging = belonging)

          # update group attributes (monotonicity)
          orig_eqcs = group_lookup$groups[match(eqcs, group_lookup$reordered)]
          monotone_eqcs = monotone_eqcs[match(unique(c(1L, orig_eqcs)), monotone_eqcs$eqcs), ]  # it may happend that "1_1" is empty
          monotone_eqcs[, eqcs := group_lookup$reordered[match(monotone_eqcs$eqcs, group_lookup$groups)]]
          setorderv(monotone_eqcs, "eqcs")
          self$monotone_eqcs = monotone_eqcs
        }
        change_monotonicity = runif(nrow(self$monotone_eqcs) -1L) < p
        if (sum(change_monotonicity) > 0) {
          new_monotonicity = sample(c(0L, 1L), size = sum(change_monotonicity), replace = TRUE)
          # + 1 because first row is always NA
          self$monotone_eqcs = self$monotone_eqcs[which(change_monotonicity) + 1L, monotonicity := new_monotonicity]
        }
      }, error = function(error_condition) {
        warning(error_condition$message)
        warning("Resetting due to error in mutation.")
        self$groups = old_groups
        self$monotone_eqcs = old_monotone_eqcs
      })
    },

    crossover = function(parent2, crossing_sections) {
      # parent1 is self$groups
      # parent2 is $groups of another IAMLPoint
      # GGA, groups are {not_selected, eqc1, ..., eqcl} not_selected must be the first
      old_groups = self$groups
      old_monotone_eqcs = self$monotone_eqcs

      tryCatch(
      {
        parent1_monotone_eqcs = copy(self$monotone_eqcs)
        parent1_monotone_eqcs[, eqcs := paste0("1_", eqcs)]
        parent2_monotone_eqcs = copy(parent2$monotone_eqcs)
        parent2_monotone_eqcs[, eqcs := paste0("2_", eqcs)]

        parent1 = copy(self$groups)
        parent1$eqcs = paste0("1_", parent1$eqcs)
        parent1$belonging = paste0("1_", parent1$belonging)
        parent2 = copy(parent2$groups)
        parent2$eqcs = paste0("2_", parent2$eqcs)
        parent2$belonging = paste0("2_", parent2$belonging)

        stopifnot(parent1$features == parent2$features)

        crossing_section1 = crossing_sections[[1L]]
        crossing_section2 = crossing_sections[[2L]]
        if (length(parent1$eqcs) == 1L) {
          left_section = 1
          right_section = 1
        } else {
          left_section = parent1$eqcs[1:crossing_section1[1L]]  # AB
          right_section = parent1$eqcs[(crossing_section1[1L] + 1):length(parent1$eqcs)]  # DE
        }

        groups_to_be_injected = parent2$eqcs[crossing_section2[1L]:crossing_section2[2L]]  # bcd
        features_in_injected_groups = which(parent2$belonging %in% groups_to_be_injected)  # which features are contained in the to be injected groups of parent2
        if ("2_1" %in% groups_to_be_injected) {
          # if the unselected group should be injected we add it to the unselected group of the parent instead of
          # inserting it as a new group
          groups_to_be_injected[groups_to_be_injected == "2_1"] = "1_1"
          parent2$belonging[parent2$belonging == "2_1"] = "1_1"
        }

        # new groups would look like this
        groups = c(left_section, groups_to_be_injected, right_section)  # AB | bcd | DE
        groups = unique(groups)  # due to "2_1" %in% groups_to_be_injected handling above
        group_lookup = data.table(groups = groups, reordered = seq_along(groups))

        # belonging of features now has changed after insertion of groups
        belonging = parent1$belonging
        for (group in groups_to_be_injected) {
          belonging[parent2$belonging == group] = group
        }

        # some older groups may now be unselected
        group_lookup = group_lookup[groups %in% unique(c("1_1", belonging)), ]  # it may happend that "1_1" is empty
        group_lookup[, reordered := seq_len(.N)]
        stopifnot("1_1" %in% group_lookup$groups && group_lookup[groups == "1_1", ][["reordered"]] == 1)

        belonging = group_lookup$reordered[match(belonging, group_lookup$groups)]
        eqcs = unique(belonging)
        self$groups = list(features = parent1$features, eqcs = unique(c(1L, sort(eqcs))), belonging = belonging)

        # update group attributes (monotonicity)
        orig_eqcs = group_lookup$groups[match(eqcs, group_lookup$reordered)]
        all_monotone_eqcs = rbind(parent1_monotone_eqcs, parent2_monotone_eqcs)
        monotone_eqcs = all_monotone_eqcs[match(unique(c("1_1", orig_eqcs)), all_monotone_eqcs$eqcs), ]  # it may happend that "1_1" is empty
        monotone_eqcs[, eqcs := group_lookup$reordered[match(monotone_eqcs$eqcs, group_lookup$groups)]]
        setorderv(monotone_eqcs, "eqcs")
        self$monotone_eqcs = monotone_eqcs
      }, error = function(error_condition) {
        warning(error_condition$message)
        warning("Resetting due to error in crossover.")
        self$groups = old_groups
        self$monotone_eqcs = old_monotone_eqcs
      })
    },

    # function to get crossing sections for grouped GA crossover below
    get_crossing_sections = function(parent2) {
      # FIXME: what if unselected group is empty, check this
      # parent1 is self$groups
      # parent2 is $groups of another IAMLPoint
      parent1 = self$groups
      parent2 = parent2$groups
      stopifnot(parent1$features == parent2$features)

      # some special handling needed if only one eqc and all features belong to it
      # eqc 1 is then empty (
      crossing_section1 = if (length(parent1$eqcs) == 1) c(1, 1) else sort(sample(seq_along(parent1$eqcs), size = 2L))  # AB |  C  | DE
      crossing_section2 = if (length(parent2$eqcs) == 1) c(1, 1) else sort(sample(seq_along(parent2$eqcs), size = 2L))  # a  | bcd | e

      list(crossing_section1 = crossing_section1, crossing_section2 = crossing_section2)
    }
  ),

  active = list(
    n_features = function() {
      length(self$feature_names)
    },
    n_selected = function() {
      length(self$selected_features)
    },
    selected_features = function() {
      self$feature_names[self$groups$belonging != 1]
    },
    unselected_features = function() {
      setdiff(self$feature_names, self$selected_features)
    },
    n_groups = function() {
      length(self$groups$eqcs)  # first group is the unselected
    },
    monotone_features = function() {
      data.table(feature = self$groups$features, monotonicity = self$monotone_eqcs[self$groups$belonging]$monotonicity)
    },
    groups = function(rhs) {
      if (!missing(rhs)) {
        assert_list(rhs, len = 3L, types = c("character", "integer", "integer"), any.missing = FALSE, names = "named")
        assert_true(all(names(rhs) == c("features", "eqcs", "belonging")))
        assert_set_equal(rhs[["features"]], self$feature_names)
        # there must be at least 2 groups: the not selected one and at least one selected
        # upper is p + 1 because we may have the empty unselected group + each feature being in its own
        assert_integer(rhs[["eqcs"]], lower = 1L, upper = length(self$feature_names) + 1L, any.missing = FALSE, min.len = 2L, unique = TRUE)
        assert_true(all(rhs[["belonging"]] %in% rhs[["eqcs"]]))
        private$.groups = rhs
      } else {
        private$.groups
      }
    },
    monotone_eqcs = function(rhs) {
      if (!missing(rhs)) {
        assert_data_table(rhs, min.rows = 1L, max.rows = length(self$feature_names) + 1L, types = "integer")
        assert_true(rhs[1L, ][["eqcs"]] == 1L && is.na(rhs[1L, ][["monotonicity"]]))
        assert_integer(rhs[["eqcs"]], lower = 1L, upper = length(self$feature_names) + 1L, any.missing = FALSE, min.len = 2L, unique = TRUE)
        assert_true(all(rhs[["eqcs"]] == seq_len(nrow(rhs))))
        assert_integer(rhs[["monotonicity"]], lower = 0L, upper = 1L)
        private$.monotone_eqcs = rhs
      } else {
        private$.monotone_eqcs
      }
    }
  ),
  private = list(
    .groups = NULL,
    .monotone_eqcs = NULL
  )
)

