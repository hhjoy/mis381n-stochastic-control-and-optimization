constructFund <- function(rho, q, sharesMat, priceMat, unique_tickers, unique_dates) {
  n = length(unique_tickers)
  reshaped_rho = melt(rho, id = c('unique_tickers', 'unique_tickers')) # reshape similar matrix into vector
  C = c(reshaped_rho$value, rep(0, n))
  A1 = c(rep(0, n*n), rep(1, n)) # constraint 1, only 1 constraint
  A2x = matrix(0, n, n*n)
  for (j in 1:n) {
    A2x[j, (n*(j-1)+1):(n*j)] = 1
  }
  A2y = matrix(0, n, n)
  A2 = cbind(A2x, A2y) # constraint 2, 100 constraints
  A3x = diag(1, n*n)
  A3y1 = diag(-1, n)
  A3y = NULL
  for (j in 1:n) {
    A3y = rbind(A3y, A3y1)
  }
  A3 = cbind(A3x, A3y) # constraint 3, 10000 constraints
  A = rbind(A1, A2, A3)
  b = c(q, rep(1, n), rep(0, n*n))
  dir = c(rep('=', n+1), rep('<=', n*n))
  s = lp("max", C, A, dir, b, all.bin=TRUE)
  solutionMat = matrix(s$solution, n, n, byrow = TRUE)
  
  price = priceMat[dim(priceMat)[1], ]
  shares = sharesMat[dim(sharesMat)[1], ]
  value = price * shares
  weights = NULL
  for (j in 1:n) {
    weights = c(weights, sum(solutionMat[, j] * value))
  }
  sum_weights = sum(weights)
  real_weights = weights / sum_weights
  return (real_weights)
}