#' Assemble global stiffness matrix
#' @param elements List of elements
#' @param connectivity Connectivity matrix (n x 2)
#' @param total_dof Total degrees of freedom
#' @export
assemble_global_stiffness <- function(elements, connectivity, total_dof) {
  K_global <- matrix(0, nrow = total_dof, ncol = total_dof)

  for(i in 1:length(elements)) {
    elem <- elements[[i]]
    node1 <- connectivity[i, 1]
    node2 <- connectivity[i, 2]

    # Degrees of freedom indices
    dof1 <- ((node1-1)*elem$dof_per_node + 1):(node1*elem$dof_per_node)
    dof2 <- ((node2-1)*elem$dof_per_node + 1):(node2*elem$dof_per_node)
    dof_indices <- c(dof1, dof2)

    # Add element stiffness to global matrix
    K_global[dof_indices, dof_indices] <-
      K_global[dof_indices, dof_indices] + elem$k_local
  }

  return(K_global)
}

#' Apply boundary conditions to stiffness matrix
#' @param K Global stiffness matrix
#' @param constrained_dof Degrees of freedom to constrain
#' @export
apply_boundary_conditions <- function(K, constrained_dof) {
  K_reduced <- K
  # Set constrained DOFs to identity (penalty method)
  for(dof in constrained_dof) {
    K_reduced[dof, ] <- 0
    K_reduced[, dof] <- 0
    K_reduced[dof, dof] <- 1
  }
  return(K_reduced)
}
