#' Check material properties
#' @param E Young's modulus (Pa)
#' @param nu Poisson's ratio
#' @param rho Density (kg/m3)
#' @param fy Yield strength (Pa)
#' @export
check_material <- function(E, nu, rho, fy = NULL) {
  if(E <= 0) stop("Young's modulus must be positive")
  if(nu <= 0 || nu >= 0.5) stop("Poisson's ratio must be between 0 and 0.5")
  if(rho <= 0) stop("Density must be positive")
  return(TRUE)
}

#' Create nodes matrix
#' @param coordinates Matrix of coordinates (n x 3)
#' @export
create_nodes <- function(coordinates) {
  if(ncol(coordinates) != 3) {
    stop("Coordinates must have 3 columns: x, y, z")
  }
  nodes <- data.frame(
    id = 1:nrow(coordinates),
    x = coordinates[,1],
    y = coordinates[,2],
    z = coordinates[,3],
    constraint_x = FALSE,
    constraint_y = FALSE,
    constraint_z = FALSE,
    constraint_rx = FALSE,
    constraint_ry = FALSE,
    constraint_rz = FALSE
  )
  return(nodes)
}

#' Apply boundary conditions
#' @param nodes Nodes data frame
#' @param node_ids Vector of node IDs
#' @param constraints List of constraints to apply
#' @export
apply_bc <- function(nodes, node_ids, constraints) {
  for(id in node_ids) {
    for(constraint in names(constraints)) {
      nodes[nodes$id == id, constraint] <- constraints[[constraint]]
    }
  }
  return(nodes)
}
#git remote add origin https://github.com/TP2k2005/M-thode-des-l-ments-finis.git
#git branch -M main
#git push -u origin main
