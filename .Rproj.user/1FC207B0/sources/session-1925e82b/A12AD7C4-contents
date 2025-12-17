#' Solve static analysis
#' @param K Global stiffness matrix
#' @param F Force vector
#' @param constrained_dof Constrained degrees of freedom
#' @export
solve_static <- function(K, F, constrained_dof) {
  # Apply boundary conditions
  K_bc <- apply_boundary_conditions(K, constrained_dof)
  F_bc <- F
  F_bc[constrained_dof] <- 0

  # Solve linear system
  displacement <- solve(K_bc, F_bc)

  # Calculate reactions
  reactions <- K %*% displacement - F

  return(list(
    displacement = displacement,
    reactions = reactions
  ))
}

#' Solve modal analysis
#' @param M Mass matrix
#' @param K Stiffness matrix
#' @param n_modes Number of modes to compute
#' @export
solve_modes <- function(M, K, n_modes = 10) {
  # Résoudre le problème aux valeurs propres généralisé
  eig <- eigen(solve(M) %*% K)

  # Trier par valeurs propres croissantes
  idx <- order(eig$values)
  eigenvalues <- eig$values[idx]
  eigenvectors <- eig$vectors[, idx]

  # Prendre les n_modes premiers
  eigenvalues <- eigenvalues[1:n_modes]
  eigenvectors <- eigenvectors[, 1:n_modes]

  # Fréquences naturelles (rad/s et Hz)
  omega <- sqrt(Re(eigenvalues))  # Fréquences angulaires (rad/s)
  frequencies <- omega / (2*pi)    # Fréquences en Hz
  periods <- 1 / frequencies       # Périodes en secondes

  # Nettoyer les valeurs complexes (garder seulement la partie réelle)
  eigenvectors <- Re(eigenvectors)

  # Facteurs de participation modale (simplifié)
  n_dof <- nrow(M)
  participation <- numeric(n_modes)
  modal_mass <- numeric(n_modes)
  total_mass <- sum(diag(M))

  for(i in 1:n_modes) {
    phi <- eigenvectors[, i]

    # Facteur de participation modal
    participation[i] <- sum(phi)^2 / sum(phi^2)

    # Masse modale
    modal_mass[i] <- (sum(phi))^2 / sum(phi^2)
  }

  return(list(
    frequencies = frequencies,
    periods = periods,
    modes = eigenvectors,  # Matrice n_dof x n_modes
    participation = participation,
    modal_mass = modal_mass,
    modal_mass_ratio = modal_mass / total_mass
  ))
}

#' Solve time history analysis (Newmark-beta method)
#' @param M Mass matrix
#' @param C Damping matrix
#' @param K Stiffness matrix
#' @param F_time Time-varying force matrix
#' @param dt Time step
#' @param beta Newmark-beta parameter
#' @param gamma Newmark-gamma parameter
#' @export
newmark_beta <- function(M, C, K, F_time, dt,
                         beta = 0.25, gamma = 0.5) {
  n_steps <- ncol(F_time)
  n_dof <- nrow(M)

  # Initialize
  u <- matrix(0, n_dof, n_steps)
  v <- matrix(0, n_dof, n_steps)
  a <- matrix(0, n_dof, n_steps)

  # Initial acceleration
  a[,1] <- solve(M, F_time[,1] - C %*% v[,1] - K %*% u[,1])

  # Effective stiffness matrix
  K_eff <- K + (gamma/(beta*dt)) * C + (1/(beta*dt^2)) * M

  for(i in 2:n_steps) {
    # Effective force
    F_eff <- F_time[,i] +
      M %*% ((1/(beta*dt^2))*u[,i-1] + (1/(beta*dt))*v[,i-1] +
               ((1/(2*beta))-1)*a[,i-1]) +
      C %*% ((gamma/(beta*dt))*u[,i-1] + ((gamma/beta)-1)*v[,i-1] +
               dt*((gamma/(2*beta))-1)*a[,i-1])

    # Solve for displacement
    u[,i] <- solve(K_eff, F_eff)

    # Update velocity and acceleration
    a[,i] <- (1/(beta*dt^2))*(u[,i] - u[,i-1]) -
      (1/(beta*dt))*v[,i-1] - ((1/(2*beta))-1)*a[,i-1]

    v[,i] <- v[,i-1] + dt*((1-gamma)*a[,i-1] + gamma*a[,i])
  }

  return(list(
    displacement = u,
    velocity = v,
    acceleration = a
  ))
}
