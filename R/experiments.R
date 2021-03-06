#' Simplex distribution
#' @description Probability density function of a random variable with simplex distribution.
#' @param x Value in the domain of the function, such that \eqn{0 < x < 1}.
#' @param mu Lease parameter, such that \eqn{0 < \mu < 1}.
#' @param sigma2 Dispersion parameter, such that \eqn{\sigma^2 > 0}.
#' @examples
#' integrate(f = pdf_simplex, lower = 0, upper = 1, mu = 0.5, sigma2 = 1.2)
#' @export
pdf_simplex <- function(x, mu, sigma2){

  stopifnot(mu > 0, mu < 1, sigma2 > 0)

  d <- (x - mu)^2 / (x * (1 - x) * mu^2 * (1 - mu)^2)

  (2 * pi * sigma2 * (x * (1 - x))^3)^(-1/2) * exp(-1/(2 * sigma2) * d)
}

#' Beta distribution (reparameterized)
#' @description Probability density function of a random variable with beta distribution.
#' @param x Value in the domain of the function, such that \eqn{0 < x < 1}.
#' @param mu Lease parameter, such that \eqn{0 < \mu < 1}.
#' @param psi Dispersion parameter, such that \eqn{\phi^2 > 0}.
#' @examples
#' integrate(f = pdf_beta, lower = 0, upper = 1, mu = 0.5, phi = 1.2)
#' @export
pdf_beta <- function(x, mu, phi){

  stopifnot(mu > 0, mu < 1)

  (x^(mu * phi - 1) * (1 - x)^((1 - mu) * phi - 1)) / beta(mu * phi, (1 - mu) * phi)
}

#' Unit gamma distribution
#' @description Probability density function of a random variable with unit gamma distribution.
#' @param x Value in the domain of the function, such that \eqn{0 < x < 1}.
#' @param mu Lease parameter, such that \eqn{0 < \mu < 1}.
#' @param tau Dispersion parameter, such that \eqn{\tau^2 > 0}.
#' @examples
#' integrate(f = pdf_gamma_u, lower = 0, upper = 1, mu = 0.5, tau = 1.2)
#' @export
pdf_gamma_u <- function(x, mu, tau) {

  stopifnot(mu > 0, mu < 1, tau > 0)

  d <- (mu^(1/tau) / (1 - mu^(1/tau)))^(tau)
  d^tau / gamma(tau) * x^(d - 1) * log(1/x)^(tau - 1)
}

#' Acceptance and rejection method for generating pseudo-random numbers
#' @description
#' Acceptance and rejection method for generating pseudo-random numbers from any
#' distribution in the interval \eqn{0 < x < 1}.
#' @param n Number of generations to be generated.
#' @param pdf Probability density function defined on the interval \eqn{0 < x < 1}.
#' @param ... Probability density function parameters.
#' @details The accept and reject method implemented by this function is
#' intended to generate pseudo random observations of random variables in the
#' interval \eqn{0 < x < 1}. Therefore, the passed probability density function pdf must
#' be a function with supported defined in this range.
#' @examples
#' # Example (Simplex)
#' acceptance_rejection(n = 1e3L, pdf = pdf_simplex, mu = 0.9, sigma2 = 0.9) |>
#' hist(prob = TRUE)
#' x <- seq(0, 1, length.out = 200)
#' y <- pdf_simplex(x = x, mu = 0.9, sigma2 = 0.9)
#' lines(x, y, type = "l")
#' # Example (Beta)
#' acceptance_rejection(n = 1e3, pdf = pdf_beta, mu = 0.2, phi = 0.2) |>
#' hist(prob = TRUE)
#' x <- seq(0, 1, length.out = 200)
#' y <- pdf_beta(x = x, mu = 0.2, phi = 0.2)
#' lines(x, y, type = "l")
#' # Example (Unit Gamma)
#' acceptance_rejection(n = 1e3, pdf = pdf_gamma_u, mu = 0.7, tau = 1.34) |>
#' hist(prob = TRUE)
#' x <- seq(0, 1, length.out = 200)
#' y <- pdf_gamma_u(x = x, mu = 0.7, tau = 1.34)
#' lines(x, y, type = "l")
#' @export
acceptance_rejection <- function(n = 1L, pdf,...){

  cond_c <- function(x, ...){
    num <- pdf(x, ...)
    den <- dbeta(x, shape1 = 0.5, shape2 = 0.5)
    -num / den
  }

  x_max <- optim(fn = cond_c, method = "SANN", par = 0.5, ...)$par

  c <- pdf(x_max, ...) / dbeta(x_max, shape1 = 0.5, shape2 = 0.5)

  criterion <- function(y, u){
    num <- pdf(y, ...)
    den <- dbeta(y, shape1 = 0.5, shape2 = 0.5)
    u < num / (c * den)
  }

  values <- double(n)
  i <- 1L
  repeat{
    y <- rbeta(n = 1L, shape1 = 0.5, shape2 = 0.5)
    u <- runif(n = 1L, min = 0, max = 1)

    if (criterion(y, u)) {
      values[i] <- y
      i <- i + 1L
    }
    if(i > n) break
  }
  values
}

likelihood <- function(pdf, x, ...){
  -sum(log(pdf(x, ...)))
}

mc_limitchart <- function(N = 10L,
                          pdf = pdf_simplex,
                          sig = 0.0027,
                          n = 5L,
                          location_par = list(mu = 0.1),
                          dispersion_par = list(sigma2 =  1.37),
                          delta = 1) {

  ARL0 <- 1/sig
  SDRL0 <- sqrt((1 - sig)/sig^2)
  MRL0 <- log(0.5)/log(1 - sig)

  lot <- do.call(
    acceptance_rejection,
    c(n = n, pdf = pdf, location_par, dispersion_par)
  )

  likelihood <- function(x, ...){
    -sum(log(pdf(x, ...)))
  }

}
