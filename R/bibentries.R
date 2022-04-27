format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  molnar_2020 = bibentry("inproceedings",
    title     = "Quantifying Model Complexity via Functional Decomposition for Better Post-Hoc Interpretability",
    author    = "Molnar, Christoph and Casalicchio, Giuseppe and Bischl, Bernd",
    year      = "2020",
    pages     = "193--204",
    booktitle = "Machine Learning and Knowledge Discovery in Databases"
  )
)

