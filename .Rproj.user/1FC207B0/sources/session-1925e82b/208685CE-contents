#' Beam element (Euler-Bernoulli)
#' @param L Length (m)
#' @param E Young's modulus (Pa)
#' @param I Moment of inertia (m4)
#' @param A Cross-sectional area (m2)
#' @param rho Density (kg/m3)
#' @export
BeamElement <- function(L, E, I, A, rho) {
  # Local stiffness matrix (12x12 for 3D beam)
  k_local <- matrix(0, nrow = 12, ncol = 12)

  # Axial stiffness
  EA_L <- E * A / L
  k_local[c(1,7), c(1,7)] <- EA_L * matrix(c(1, -1, -1, 1), 2, 2)

  # Bending about y-axis
  EIy_L3 <- 12 * E * I / L^3
  k_local[c(2,6,8,12), c(2,6,8,12)] <- EIy_L3 * matrix(c(
    1, L/2, -1, L/2,
    L/2, L^2/3, -L/2, L^2/6,
    -1, -L/2, 1, -L/2,
    L/2, L^2/6, -L/2, L^2/3
  ), 4, 4, byrow = TRUE)

  # Bending about z-axis (simplified)
  EIz_L3 <- 12 * E * I / L^3
  k_local[c(3,5,9,11), c(3,5,9,11)] <- EIz_L3 * matrix(c(
    1, -L/2, -1, -L/2,
    -L/2, L^2/3, L/2, L^2/6,
    -1, L/2, 1, L/2,
    -L/2, L^2/6, L/2, L^2/3
  ), 4, 4, byrow = TRUE)

  # Torsion
  GJ_L <- E * A / (2*(1+0.3)) / L  # Simplified
  k_local[c(4,10), c(4,10)] <- GJ_L * matrix(c(1, -1, -1, 1), 2, 2)

  # Mass matrix (consistent mass matrix)
  m_local <- matrix(0, nrow = 12, ncol = 12)
  m <- rho * A * L

  # Translational masses
  for(i in c(1:3, 7:9)) {
    m_local[i, i] <- m/2
  }

  # Rotational inertia (simplified)
  for(i in c(4:6, 10:12)) {
    m_local[i, i] <- m * L^2 / 12
  }

  structure(list(
    type = "beam",
    L = L,
    E = E,
    I = I,
    A = A,
    rho = rho,
    k_local = k_local,
    m_local = m_local,
    dof_per_node = 6
  ), class = "FEMElement")
}

#' Column element (special case of beam)
#' @export
ColumnElement <- function(L, E, I, A, rho) {
  BeamElement(L, E, I, A, rho)
}

#' Shear wall element
#' @param thickness Wall thickness (m)
#' @param height Wall height (m)
#' @param width Wall width (m)
#' @export
ShearWallElement <- function(height, width, thickness, E, rho, nu = 0.2) {
  # Simplified shear wall as shell element
  A = width * thickness  # Cross-sectional area
  I = width * thickness^3 / 12  # Moment of inertia

  # Create as a vertical beam with high stiffness
  element <- BeamElement(height, E, I, A, rho)
  element$type <- "shear_wall"
  element$width <- width
  element$thickness <- thickness

  # Increase stiffness for shear wall behavior
  element$k_local <- element$k_local * 10  # Simplified

  return(element)
}
