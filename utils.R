medidas <- function(x, rf = 0) {
  # Annualized Average
  AV <- mean(x)
  # Annualized SD
  SD <- sd(x)
  # Information (or Sharpe) Ratio
  SR <- (mean(x) - rf)/sd(x)
  # Adjusted Sharpe Ratio
  ASR <- SR*(1 + (moments::skewness(x)/6)*SR - ((moments::kurtosis(x) - 3)/24)*SR^2)
  # Sortino Ratio
  SO <- (mean(x) - rf)/sqrt(mean(ifelse(x - rf < 0, 0, (x - rf)^2)))
  output <- c(12*AV, sqrt(12)*SD, sqrt(12)*SR, sqrt(12)*ASR, sqrt(12)*SO)
  return(output)
}

calculate_to<- function(previous_weights, desired_weights, oos_returns, p) {
  oos_returns[is.na(oos_returns)] <- 0
  num <- previous_weights*(1 + oos_returns/100)
  den <- sum(num, na.rm = TRUE)
  updated_weights <- num/den
  to <- sum(abs(desired_weights - updated_weights))
  return(to)
}

cvc_shrinkage <- function(R) {
  S <- cov(R)
  R_c <- scale(R, scale = FALSE)
  p <- ncol(S)
  n <- nrow(R)
  ones <- matrix(1, p, p)
  
  phi <- mean(diag(S))
  nu <- mean(S[row(S) != col(S)]) 
  
  Fcvc <-  matrix(nu, p, p)
  diag(Fcvc) <- phi
  
  zeta2 <- sum((Fcvc- S)^2)
  tau2 <- min(sum(sapply(1:n, function(t) sum((tcrossprod(R_c[t, ]) - S)^2)))/n^2,  zeta2)
  delta <- tau2 / zeta2
  
  return(delta * Fcvc + (1 - delta) * S)
  
}


cov_nlshrink <- function(X) {
  results <- tryCatch({
    nlshrink_cov(X, method = "nlminb")
  }, error = function(e) {
    nlshrink_cov(X, method = "nloptr")
  })
  return(results)
}
