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
  index = which((summary(temp.lm)$coefficients)[,4] < cutoff) 
  return((summary(lm(responses~covariates[, index]))$coefficients)[,4])
}

## 2c)

run_simulation = function (n_trials, n, p, cutoff) {
  res = c()
  for (i in 1:n_trials) {
    dat = generate_data(n[i], p[i])
    responses = dat$responses
    covariates = dat$covariates
    Ps = model_select(covariates, responses, cutoff)
    res = c(res, Ps)
    
  }
  return(hist(res, main = "Distribution of P-values"), xlab = "P-values")
  
} 

run_simulation(3,c(100, 1000, 10000), c(10, 20, 50), 0.05)
