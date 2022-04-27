# from https://github.com/slds-lmu/paper_2019_iml_measures
# TODO: Test
get_r2 = function(seg.predictions, ale.values) {
  SST = ssq(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = ssq(seg.predictions - ale.values)
  SSE / SST
}
