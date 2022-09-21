# https://github.com/dmlc/xgboost/blob/master/R-package/R/xgb.model.dt.tree.R
xgb_model_dt_tree <- function(feature_names = NULL, model = NULL, text = NULL, trees = NULL, use_int_id = FALSE, ...) {
  if (!inherits(model, "xgb.Booster") && !is.character(text)) {
    stop("Either 'model' must be an object of class xgb.Booster\n",
         "  or 'text' must be a character vector with the result of xgb.dump\n",
         "  (or NULL if 'model' was provided).")
  }

  if (is.null(feature_names) && !is.null(model) && !is.null(model$feature_names))
    feature_names <- model$feature_names

  if (!(is.null(feature_names) || is.character(feature_names))) {
    stop("feature_names: must be a character vector")
  }

  if (!(is.null(trees) || is.numeric(trees))) {
    stop("trees: must be a vector of integers.")
  }

  if (is.null(text)){
    text <- xgboost::xgb.dump(model = model, with_stats = TRUE)
  }

  if (length(text) < 2 ||
      sum(grepl('leaf=([+-]\\d+)|leaf=(\\d+)', text)) < 1) {
    stop("Non-tree model detected! This function can only be used with tree models.")
  }

  position <- which(grepl("booster", text, fixed = TRUE))

  add.tree.id <- function(node, tree) if (use_int_id) node else paste(tree, node, sep = "-")

  anynumber_regex <- "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"

  td <- data.table(t = text)
  td[position, Tree := 1L]
  td[, Tree := cumsum(ifelse(is.na(Tree), 0L, Tree)) - 1L]

  if (is.null(trees)) {
    trees <- 0:max(td$Tree)
  } else {
    trees <- trees[trees >= 0 & trees <= max(td$Tree)]
  }
  td <- td[Tree %in% trees & !grepl('^booster', t)]

  td[, Node := as.integer(sub("^([0-9]+):.*", "\\1", t))]
  if (!use_int_id) td[, ID := add.tree.id(Node, Tree)]
  td[, isLeaf := grepl("leaf", t, fixed = TRUE)]

  # parse branch lines
  branch_rx <- paste0("f(\\d+)<(", anynumber_regex, ")\\] yes=(\\d+),no=(\\d+),missing=(\\d+),",
                      "gain=(", anynumber_regex, "),cover=(", anynumber_regex, ")")
  branch_cols <- c("Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover")
  td[
    isLeaf == FALSE,
    (branch_cols) := {
      matches <- regmatches(t, regexec(branch_rx, t))
      # skip some indices with spurious capture groups from anynumber_regex
      xtr <- do.call(rbind, matches)[, c(2, 3, 5, 6, 7, 8, 10), drop = FALSE]
      xtr[, 3:5] <- add.tree.id(xtr[, 3:5], Tree)
      if (length(xtr) == 0) {
        as.data.table(
          list(Feature = "NA", Split = "NA", Yes = "NA", No = "NA", Missing = "NA", Quality = "NA", Cover = "NA")
        )
      } else {
        as.data.table(xtr)
      }
    }
  ]

  # assign feature_names when available
  is_stump <- function() {
    return(length(td$Feature) == 1 && is.na(td$Feature))
  }
  if (!is.null(feature_names) && !is_stump()) {
    if (length(feature_names) <= max(as.numeric(td$Feature), na.rm = TRUE))
      stop("feature_names has less elements than there are features used in the model")
    td[isLeaf == FALSE, Feature := feature_names[as.numeric(Feature) + 1]]
  }

  # parse leaf lines
  leaf_rx <- paste0("leaf=(", anynumber_regex, "),cover=(", anynumber_regex, ")")
  leaf_cols <- c("Feature", "Quality", "Cover")
  td[
    isLeaf == TRUE,
    (leaf_cols) := {
      matches <- regmatches(t, regexec(leaf_rx, t))
      xtr <- do.call(rbind, matches)[, c(2, 4)]
      if (length(xtr) == 2) {
        c("Leaf", as.data.table(xtr[1]), as.data.table(xtr[2]))
      } else {
        c("Leaf", as.data.table(xtr))
      }
    }
  ]

  # convert some columns to numeric
  numeric_cols <- c("Split", "Quality", "Cover")
  td[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
  if (use_int_id) {
    int_cols <- c("Yes", "No", "Missing")
    td[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  }

  td[, t := NULL]
  td[, isLeaf := NULL]

  td[order(Tree, Node)]
}

interactions <- function(xgb_model, option = "pairs") {
  Child <- Parent <- Feature <- sumGain <- . <- NULL

  if (option == "pairs") {
    gainTable <- calculatePairsGainTable(xgb_model)
  }
  class(gainTable) <- c("interactions", "data.table")
  return(gainTable)

}

calculatePairsGainTable <- function(xgb_model) {
  name_pair <- childsGain <- Parent <- Child <- sumGain <- N <- . <- NULL

  treeList <- calculateGain(xgb_model)
  trees <- rbindlist(treeList, fill = TRUE)
  if ("name_pair" %nin% colnames(trees)) {
    trees[, name_pair := NA_character_]
    importance = data.table(Parent = character(), Child = character(), sumGain = numeric(), frequency = integer())
    return(importance)
  }

  importanceCount <- data.table(table(trees[, "name_pair"],dnn = "name_pair"))
  importanceGain <- trees[, .(sumGain = sum(childsGain)), by = "name_pair"]
  importance <- merge(importanceCount, importanceGain, by = "name_pair")
  importance <-
  importance[, `:=`(Parent = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 1))),
                     Child = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 2 ))))]
  importance <- importance[, -1]
  setorderv(importance, "sumGain", -1)

  return(importance[,.(Parent, Child, sumGain, frequency = N)])
}

calculateGain <- function(xgb.model) {

  leaf <- Feature <- Yes <- No <- ID <- parentsGain <- Quality <- parentsCover <-
    Cover <- name_pair <- childsGain <- depth <- parentsName <- NULL

  trees = tableOfTrees(xgb.model)
  trees[, leaf := Feature == "Leaf"]
  trees$depth <- 0
  treeList = split(trees, as.factor(trees$Tree))

  for (tree in treeList) {
    num_nodes = nrow(tree)
    non_leaf_rows = which(tree[, leaf] == F)
    for (r in non_leaf_rows) {
      left = tree[r, Yes]
      right = tree[r, No]
      if (tree[ID == left, leaf] == F) {
       # newDepth <- tree[r , depth] + 1
        tree[ID == left,`:=`(parentsGain = tree[r, Quality],
                             parentsCover = tree[r, Cover],
                             name_pair = paste(tree[r, Feature], tree[ID == left, Feature], sep = ":"),
                             childsGain = Quality,
                             depth = tree[r , depth] + 1,
                             parentsName = tree[r, Feature])]
        tree[ID == left, interaction := ((parentsGain < childsGain) & (Feature != parentsName))]
      }

      if (tree[ID == right, leaf]==F) {

        #newDepth <- tree[r , depth] + 1
        tree[ID == right, `:=`(parentsGain = tree[r, Quality],
                               parentsCover = tree[r, Cover],
                               name_pair = paste(tree[r, Feature], tree[ID == right, Feature], sep = ":"),
                               childsGain = Quality,
                               depth = tree[r , depth] + 1,
                               parentsName = tree[r, Feature])]
        tree[ID == right, interaction := ((parentsGain < childsGain) & (Feature != parentsName))]
      }
    }
  }

  return(treeList)
}

tableOfTrees <- function(model, ...) {
  count <- split_feature <- leaf_count <- internal_count <-
    split_index <- tree_index <- leaf_index <- threshold <-
    leaf_value <- split_gain <- flag <- node_parent <- leaf_parent<-
    Node <- Feature <- . <- Cover <- Yes <- No <- ID <-
    Tree<- Quality <- Missing <-Leaf_old_num<- Split <- NULL
  if(class(model)[1] == "xgb.Booster") {
    return(xgb_model_dt_tree(model = model, ...)[])
  }
}

# walk up a leaf in an xgboost tree to get its feature region (min max corners for each feature)
walk_up_leaf = function(leaf_id, tree, model) {
  node_to_check_leaf = leaf_id
  min_feature_value_leaf = setNames(rep(-Inf, length(model$feature_names)), model$feature_names)
  max_feature_value_leaf = setNames(rep(Inf, length(model$feature_names)), model$feature_names)
  
  tmp = tree[Yes == node_to_check_leaf | No == node_to_check_leaf | Missing == node_to_check_leaf]
  while(nrow(tmp) > 0) {
    stopifnot(nrow(tmp) == 1)
    if (tmp$Yes == node_to_check_leaf) {
      if (tmp$Split < max_feature_value_leaf[tmp$Feature]) {
        max_feature_value_leaf[tmp$Feature] = tmp$Split
      }
    } else if (tmp$No == node_to_check_leaf) {
      if (tmp$Split > min_feature_value_leaf[tmp$Feature]) {
        min_feature_value_leaf[tmp$Feature] = tmp$Split
      }
    }
    node_to_check_leaf = tmp$Node
    tmp = tree[Yes == node_to_check_leaf | No == node_to_check_leaf | Missing == node_to_check_leaf]
  }
  list(min = min_feature_value_leaf, max = max_feature_value_leaf)
}

# check whether an xgboost tree is not monotone
monotonicity_violated = function(model) {
  # https://www.kdd.org/exploration_files/potharst.pdf
  trees = tableOfTrees(model, use_int_id = TRUE)
  any(unlist(
    map(unique(trees$Tree), function(tree_id) {
      tree = trees[Tree == tree_id]
      leaves = tree[Feature == "Leaf"]
      leaf_ids = leaves$Node
      min_max_per_leaf = map(leaf_ids, function(leaf_id) {
        walk_up_leaf(leaf_id, tree = tree, model = model)
      })
      names(min_max_per_leaf) = leaf_ids

      not_monotone = map(leaf_ids, function(leaf_id1) {
        value1 = leaves[Node == leaf_id1, Quality]
        # value1 > value2 but min_value1 < max_value2 --> violates
        potential_leaves2 = leaves[Quality < value1, Node]
        not_monotone1  = map(potential_leaves2, function(leaf_id2) {
          #names(which(min_max_per_leaf[[as.character(leaf_id1)]]$min < min_max_per_leaf[[as.character(leaf_id2)]]$max))
          all(min_max_per_leaf[[as.character(leaf_id1)]]$min < min_max_per_leaf[[as.character(leaf_id2)]]$max)
        })
        # value1 < value2 but max_value1 > min_value2 --> violates
        potential_leaves2 = leaves[Quality > value1, Node]
        not_monotone2 = map(potential_leaves2, function(leaf_id2) {
          #names(which(min_max_per_leaf[[as.character(leaf_id1)]]$max > min_max_per_leaf[[as.character(leaf_id2)]]$min))
          all(min_max_per_leaf[[as.character(leaf_id1)]]$max > min_max_per_leaf[[as.character(leaf_id2)]]$min)
        })
        unlist(not_monotone1, not_monotone2)
      })
      unlist(not_monotone)
    })
  ))
}

