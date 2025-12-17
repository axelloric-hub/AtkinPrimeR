#' Create Rayleigh damping matrix
#' @param K Stiffness matrix
#' @param M Mass matrix
#' @param xi Damping ratio (e.g., 0.05 for 5%)
#' @param omega1 First natural frequency (rad/s)
#' @param omega2 Second natural frequency (rad/s)
#' @export
rayleigh_damping <- function(K, M, xi = 0.05, omega1 = NULL, omega2 = NULL) {
  if(is.null(omega1) || is.null(omega2)) {
    # Use first two modes if not specified
    eig <- eigen(solve(M) %*% K)
    omega1 <- sqrt(eig$values[1])
    omega2 <- sqrt(eig$values[2])
  }

  # Rayleigh damping coefficients
  A <- matrix(c(1/omega1, omega1, 1/omega2, omega2), nrow = 2, byrow = TRUE)
  b <- 2 * xi * c(1, 1)
  alpha_beta <- solve(A, b)

  # Damping matrix
  C <- alpha_beta[1] * M + alpha_beta[2] * K

  return(C)
}

#' Create modal damping matrix
#' @export
modal_damping <- function(M, K, xi_values) {
  # Solve eigenvalue problem
  eig <- eigen(solve(M) %*% K)
  Phi <- eig$vectors  # Mode shapes
  omega <- sqrt(eig$values)  # Natural frequencies

  # Modal damping matrix
  C_modal <- diag(2 * xi_values * omega)

  # Transform to physical coordinates
  C <- t(Phi) %*% C_modal %*% Phi

  return(list(C = C, frequencies = omega/(2*pi), modes = Phi))
}
