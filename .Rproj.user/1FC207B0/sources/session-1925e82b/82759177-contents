#' Create a simple 2D frame building
#' @param n_stories Number of stories
#' @param story_height Height per story (m)
#' @param bay_width Bay width (m)
#' @param n_bays Number of bays
#' @export
create_2D_frame <- function(n_stories = 3, story_height = 3,
                            bay_width = 5, n_bays = 3) {

  # Create nodes
  nodes <- data.frame()
  node_id <- 1

  for(story in 0:n_stories) {
    for(bay in 0:n_bays) {
      nodes <- rbind(nodes, data.frame(
        id = node_id,
        x = bay * bay_width,
        y = story * story_height,
        z = 0
      ))
      node_id <- node_id + 1
    }
  }

  # Create elements (beams and columns)
  elements <- list()
  connectivity <- matrix(nrow = 0, ncol = 2)
  element_id <- 1

  # Columns
  for(bay in 0:n_bays) {
    for(story in 0:(n_stories-1)) {
      node_bottom <- bay + story*(n_bays+1) + 1
      node_top <- bay + (story+1)*(n_bays+1) + 1

      connectivity <- rbind(connectivity, c(node_bottom, node_top))
      elements[[element_id]] <- ColumnElement(
        L = story_height,
        E = 2.5e10,  # Concrete
        I = 0.002,   # m4
        A = 0.25,    # m2
        rho = 2500   # kg/m3
      )
      element_id <- element_id + 1
    }
  }

  # Beams
  for(story in 1:n_stories) {
    for(bay in 0:(n_bays-1)) {
      node_left <- bay + story*(n_bays+1) + 1
      node_right <- (bay+1) + story*(n_bays+1) + 1

      connectivity <- rbind(connectivity, c(node_left, node_right))
      elements[[element_id]] <- BeamElement(
        L = bay_width,
        E = 2.5e10,
        I = 0.001,
        A = 0.2,
        rho = 2500
      )
      element_id <- element_id + 1
    }
  }

  # Apply boundary conditions (fixed base)
  for(bay in 0:n_bays) {
    node_base <- bay + 1
    nodes$constraint_x[node_base] <- TRUE
    nodes$constraint_y[node_base] <- TRUE
    nodes$constraint_z[node_base] <- TRUE
    nodes$constraint_rx[node_base] <- TRUE
    nodes$constraint_ry[node_base] <- TRUE
    nodes$constraint_rz[node_base] <- TRUE
  }

  return(list(
    nodes = nodes,
    elements = elements,
    connectivity = connectivity,
    total_dof = nrow(nodes) * 6
  ))
}

#' Create a 3D building model
#' @export
create_3D_building <- function(n_stories = 3, plan_width = 10,
                               plan_depth = 10, story_height = 3) {

  # Create nodes at four corners
  nodes <- data.frame()
  node_id <- 1

  for(story in 0:n_stories) {
    for(i in 0:1) {
      for(j in 0:1) {
        nodes <- rbind(nodes, data.frame(
          id = node_id,
          x = i * plan_width,
          y = j * plan_depth,
          z = story * story_height
        ))
        node_id <- node_id + 1
      }
    }
  }

  # Create elements
  elements <- list()
  connectivity <- matrix(nrow = 0, ncol = 2)

  # Columns (8 columns total)
  column_nodes <- matrix(c(
    1,5, 2,6, 3,7, 4,8,
    5,9, 6,10, 7,11, 8,12
  ), ncol = 2, byrow = TRUE)

  for(i in 1:nrow(column_nodes)) {
    connectivity <- rbind(connectivity, column_nodes[i,])
    elements[[i]] <- ColumnElement(
      L = story_height,
      E = 2.5e10,
      I = 0.003,
      A = 0.3,
      rho = 2500
    )
  }

  # Beams (per story)
  beam_id <- length(elements) + 1
  for(story in 0:n_stories) {
    base_node <- story * 4 + 1
    beam_connections <- matrix(c(
      base_node, base_node+1,
      base_node+1, base_node+3,
      base_node+3, base_node+2,
      base_node+2, base_node,
      base_node, base_node+3,  # Diagonals for stability
      base_node+1, base_node+2
    ), ncol = 2, byrow = TRUE)

    for(j in 1:nrow(beam_connections)) {
      connectivity <- rbind(connectivity, beam_connections[j,])
      elements[[beam_id]] <- BeamElement(
        L = ifelse(j <= 4, plan_width, sqrt(plan_width^2 + plan_depth^2)),
        E = 2.5e10,
        I = 0.0015,
        A = 0.25,
        rho = 2500
      )
      beam_id <- beam_id + 1
    }
  }

  # Boundary conditions (fixed base)
  base_nodes <- 1:4
  for(node in base_nodes) {
    nodes$constraint_x[node] <- TRUE
    nodes$constraint_y[node] <- TRUE
    nodes$constraint_z[node] <- TRUE
    nodes$constraint_rx[node] <- TRUE
    nodes$constraint_ry[node] <- TRUE
    nodes$constraint_rz[node] <- TRUE
  }

  return(list(
    nodes = nodes,
    elements = elements,
    connectivity = connectivity,
    total_dof = nrow(nodes) * 6
  ))
}
