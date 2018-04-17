## 2a)

generate_data = function(n,p) {
  res = list()
  res[[1]] = matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  res[[2]] = rnorm(n, 0, 1)
  names(res) = c("covariates", "responses")
  return (res)
}

## 2b)

model_select = function (covariates, responses, cutoff) {
  
  temp.lm = lm(responses ~ covariates)
  index = which((summary(temp.lm)$coefficients)[,4] < 0.05) 
  return((summary(lm(responses~covariates[, index]))$coefficients)[,4])
}


