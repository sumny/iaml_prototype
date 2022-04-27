# from https://github.com/slds-lmu/paper_2019_iml_measures
# Compute sum of squares
ssq = function(x) {
  assert_numeric(x, any.missing = FALSE, min.len = 1)
  sum(x^2)
}
