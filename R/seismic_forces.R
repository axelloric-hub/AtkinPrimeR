#' Generate earthquake acceleration record
#' @param duration Total duration (s)
#' @param dt Time step (s)
#' @param type Type of record: "el_centro", "kobe", "sine", "white_noise"
#' @param PGA Peak Ground Acceleration (g)
#' @export
generate_earthquake <- function(duration = 30, dt = 0.01,
                                type = "el_centro", PGA = 0.3) {
  time <- seq(0, duration, by = dt)
  n <- length(time)

  if(type == "el_centro") {
    # Simplified El Centro 1940 NS component
    accel <- PGA * 9.81 * sin(2*pi*2*time) * exp(-0.2*time) *
      (1 + 0.5*sin(2*pi*0.5*time))
  } else if(type == "kobe") {
    # Simplified Kobe 1995
    accel <- PGA * 9.81 * sin(2*pi*2.5*time) * exp(-0.25*time) *
      (1 + 0.7*sin(2*pi*0.8*time))
  } else if(type == "sine") {
    accel <- PGA * 9.81 * sin(2*pi*2*time)
  } else if(type == "white_noise") {
    accel <- PGA * 9.81 * rnorm(n, 0, 0.5)
    # Filter high frequencies
    accel <- filter(accel, rep(1/10, 10), circular = TRUE)
  } else {
    stop("Unknown earthquake type")
  }

  return(data.frame(
    time = time,
    acceleration = accel,
    velocity = cumsum(accel) * dt,
    displacement = cumsum(cumsum(accel)) * dt^2
  ))
}

#' Calculate seismic forces using response spectrum
#' @param M Mass matrix
#' @param K Stiffness matrix
#' @param spectrum Response spectrum function
#' @export
response_spectrum_analysis <- function(M, K, spectrum, directions = c(1,0,0)) {
  # Solve eigenvalue problem
  eig <- eigen(solve(M) %*% K)
  omega <- sqrt(eig$values)
  Phi <- eig$vectors

  n_modes <- length(omega)
  forces <- matrix(0, nrow = nrow(M), ncol = 1)

  for(i in 1:min(10, n_modes)) {
    # Modal participation factor
    L <- t(Phi[,i]) %*% M %*% directions
    M_star <- t(Phi[,i]) %*% M %*% Phi[,i]
    Gamma <- L / M_star

    # Spectral acceleration
    Sa <- spectrum(omega[i]/(2*pi))

    # Modal forces
    forces <- forces + Gamma * Sa * M %*% Phi[,i]
  }

  return(forces)
}
