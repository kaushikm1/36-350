generate_data = function(n,p) {
  res = list()
  res[[1]] = matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  res[[2]] = rnorm(n, 0, 1)
  names(res) = c("covariates", responses)
  return (res)
}
