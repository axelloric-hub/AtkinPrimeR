#' Assemble global mass matrix
#' @param elements List of elements
#' @param connectivity Connectivity matrix
#' @param total_dof Total degrees of freedom
#' @export
assemble_global_mass <- function(elements, connectivity, total_dof) {
  M_global <- matrix(0, nrow = total_dof, ncol = total_dof)

  for(i in 1:length(elements)) {
    elem <- elements[[i]]
    node1 <- connectivity[i, 1]
    node2 <- connectivity[i, 2]

    # Degrees of freedom indices
    dof1 <- ((node1-1)*elem$dof_per_node + 1):(node1*elem$dof_per_node)
    dof2 <- ((node2-1)*elem$dof_per_node + 1):(node2*elem$dof_per_node)
    dof_indices <- c(dof1, dof2)

    # Add element mass to global matrix
    M_global[dof_indices, dof_indices] <-
      M_global[dof_indices, dof_indices] + elem$m_local
  }

  return(M_global)
}

#' Create diagonal (lumped) mass matrix
#' @export
create_lumped_mass <- function(M_consistent) {
  M_lumped <- diag(colSums(M_consistent))
  return(M_lumped)
}
