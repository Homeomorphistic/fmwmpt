attainable_set <- function(mu_1, mu_2, sigma_1, sigma_2, rho, short_selling = TRUE) {
  if (short_selling) {
    from <- -2
    to <- 2
  } else {
    from <- 0
    to <- 1
  }

  w = seq(from, to, length.out = 1000)

  mu = w*mu_1 + (1-w)*mu_2
  sigma2 = w^2*sigma_1^2 + (1-w)^2*sigma_2^2 + 2*w*(1-w)*rho*sigma_1*sigma_2
  sigma = sqrt(sigma2)

  data.frame(mu=mu, sigma=sigma)
}

minimal_variance_portfolio <- function(sigma_1, sigma_2, rho) {
  correlations <- matrix(c(1, rho, rho, 1), 2, 2)

  covariances <- cor_to_cov(volatilities = c(sigma_1, sigma_2),
                            correlations = correlations)

  C_inv <- solve(covariances)
  ones <- rep(1, ncol(covariances))
  weights <- (C_inv %*% ones ) / as.numeric(t(ones) %*% C_inv %*% ones)

  return(as.vector(weights))
}

cor_to_cov <- function(volatilities, correlations) {
  vol <- diag(volatilities)
  covariances <- vol %*% correlations %*% vol

  return(covariances)
}

portfolio_volatility <- function(sigma_1, sigma_2, rho, weights) {
  vol <- diag(c(sigma_1, sigma_2))
  correlations <- matrix(c(1, rho, rho, 1), 2, 2)

  covariances <- vol %*% correlations %*% vol

  return(sqrt(as.numeric(t(weights) %*% covariances %*% weights)))
}

market_portfolio <- function(mu_1, mu_2, sigma_1, sigma_2, rho, risk_free) {
  correlations <- matrix(c(1, rho, rho, 1), 2, 2)
  covariances <- cor_to_cov(volatilities = c(sigma_1, sigma_2),
                                 correlations = correlations)
  c_inv <- solve(covariances)
  ones <- rep(x = 1, times = ncol(correlations))

  excess_returns <- c(mu_1, mu_2) - risk_free
  num <- c_inv %*% excess_returns
  den <- as.numeric(t(ones) %*% c_inv %*% excess_returns)
  weights <- as.vector(num / den)

  return(weights)
}




