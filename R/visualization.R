#' Plot mode shapes
#' @param nodes Nodes data frame
#' @param modes Mode shapes matrix
#' @param mode_number Which mode to plot
#' @param scale_factor Scale factor for deformation
#' @export
plot_mode_shape <- function(nodes, modes, mode_number = 1, scale_factor = 10) {
  require(ggplot2)
  require(plotly)

  n_nodes <- nrow(nodes)
  total_dof <- nrow(modes)  # Le nombre total de DOF dans le système

  # Nombre de DOF par nœud
  dof_per_node <- total_dof / n_nodes

  # Vérification que le calcul est valide
  if(dof_per_node != round(dof_per_node)) {
    stop("Le nombre de DOF total n'est pas divisible par le nombre de nœuds")
  }

  # Créer une version modifiée avec la bonne logique d'extraction
  deformed <- nodes

  # Extraire les déplacements pour ce mode
  # Les DOF sont organisés comme: [ux1, uy1, uz1, rx1, ry1, rz1, ux2, uy2, ...]
  ux_indices <- seq(1, total_dof, by = dof_per_node)  # ux pour chaque nœud
  uy_indices <- seq(2, total_dof, by = dof_per_node)  # uy pour chaque nœud
  uz_indices <- seq(3, total_dof, by = dof_per_node)  # uz pour chaque nœud

  # Appliquer les déplacements (mode shape)
  deformed$x <- nodes$x + scale_factor * Re(modes[ux_indices, mode_number])
  deformed$y <- nodes$y + scale_factor * Re(modes[uy_indices, mode_number])
  deformed$z <- nodes$z + scale_factor * Re(modes[uz_indices, mode_number])

  # Visualisation
  p <- plot_ly() %>%
    add_trace(
      data = nodes,
      x = ~x, y = ~y, z = ~z,
      type = 'scatter3d',
      mode = 'markers+lines',
      name = 'Original',
      marker = list(size = 5, color = 'blue'),
      line = list(color = 'blue', width = 2)
    ) %>%
    add_trace(
      data = deformed,
      x = ~x, y = ~y, z = ~z,
      type = 'scatter3d',
      mode = 'markers+lines',
      name = paste('Mode', mode_number),
      marker = list(size = 5, color = 'red'),
      line = list(color = 'red', width = 2)
    ) %>%
    layout(
      title = paste('Mode Shape', mode_number),
      scene = list(
        xaxis = list(title = 'X (m)'),
        yaxis = list(title = 'Y (m)'),
        zaxis = list(title = 'Z (m)'),
        aspectmode = 'data'
      )
    )

  return(p)
}
#' Plot time history response
#' @param time Time vector
#' @param response Response vector (displacement, velocity, or acceleration)
#' @param node_id Node ID to plot
#' @param dof Degree of freedom (1-6)
#' @export
plot_time_history <- function(time, response, node_id = 1, dof = 1,
                              title = "Time History Response") {

  # Extract specific DOF
  if(is.matrix(response)) {
    response_plot <- response[(node_id-1)*6 + dof, ]
  } else {
    response_plot <- response
  }

  df <- data.frame(
    time = time,
    response = response_plot
  )

  p <- ggplot(df, aes(x = time, y = response)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(
      title = title,
      x = "Time (s)",
      y = "Response"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90")
    )

  return(p)
}

#' Create building visualization
#' @export
visualize_building <- function(nodes, elements, connectivity,
                               deformation = NULL, scale = 10) {
  require(plotly)

  # Plot nodes
  p <- plot_ly(
    data = nodes,
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'markers',
    name = 'Nodes',
    marker = list(size = 4, color = 'blue')
  )

  # Add elements (beams/columns)
  for(i in 1:nrow(connectivity)) {
    node1 <- nodes[nodes$id == connectivity[i, 1], ]
    node2 <- nodes[nodes$id == connectivity[i, 2], ]

    p <- p %>% add_trace(
      x = c(node1$x, node2$x),
      y = c(node1$y, node2$y),
      z = c(node1$z, node2$z),
      type = 'scatter3d',
      mode = 'lines',
      line = list(color = 'black', width = 3),
      showlegend = i == 1,
      name = 'Elements'
    )
  }

  # Add deformed shape if provided
  if(!is.null(deformation)) {
    deformed_nodes <- nodes
    n_nodes <- nrow(nodes)

    deformed_nodes$x <- nodes$x + scale * deformation[seq(1, n_nodes*6, by = 6)]
    deformed_nodes$y <- nodes$y + scale * deformation[seq(2, n_nodes*6, by = 6)]
    deformed_nodes$z <- nodes$z + scale * deformation[seq(3, n_nodes*6, by = 6)]

    p <- p %>% add_trace(
      data = deformed_nodes,
      x = ~x, y = ~y, z = ~z,
      type = 'scatter3d',
      mode = 'markers',
      name = 'Deformed',
      marker = list(size = 4, color = 'red')
    )

    # Add deformed elements
    for(i in 1:nrow(connectivity)) {
      node1 <- deformed_nodes[deformed_nodes$id == connectivity[i, 1], ]
      node2 <- deformed_nodes[deformed_nodes$id == connectivity[i, 2], ]

      p <- p %>% add_trace(
        x = c(node1$x, node2$x),
        y = c(node1$y, node2$y),
        z = c(node1$z, node2$z),
        type = 'scatter3d',
        mode = 'lines',
        line = list(color = 'red', width = 3, dash = 'dash'),
        showlegend = i == 1,
        name = 'Deformed Elements'
      )
    }
  }

  p <- p %>% layout(
    title = 'Building Structure',
    scene = list(
      xaxis = list(title = 'X (m)'),
      yaxis = list(title = 'Y (m)'),
      zaxis = list(title = 'Z (m)'),
      aspectmode = 'data'
    ),
    showlegend = TRUE
  )

  return(p)
}
