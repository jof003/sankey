library(ggplot2)
library(ggalluvial)
library(reshape2)

#' Sankey diagram
#'
#' @param df A dataframe of risk factors over time points
#' @param include_borders The option to include white boarders around the alluvium and stratums or no boarders. Either TRUE or FALSE
#' @export


# The function to create a Sankey diagram chart, including the option to choose
# whether white borders are wanted or not.
sankey_diagram <- function(df, include_borders = TRUE) {
  borders <- ifelse(include_borders, 1, 0)

  # Transform the data set so that we have three columns: risk factor, year,
  #and risk factor value estimates.
  df_v2 <- reshape2::melt(df, id = "Risk Factors for Stroke in Blacks")
  names(df_v2) <- c("risk_factor", "year", "value")

  # Identify the initial variables.
  ggplot2::ggplot(df_v2, ggplot2::aes(x = df_v2$year,
                                      y = df_v2$value,
                                      alluvium = df_v2$risk_factor,
                                      stratum = df_v2$risk_factor)) +
    # Adjust the alluvium (the connections between the stratums).
    ggalluvial::geom_alluvium(ggplot2::aes(fill = df_v2$risk_factor),
                              alpha = 1,
                              width = 1/2,
                              size = borders,
                              color = "white",
                              decreasing = FALSE) +
    # Adjust the stratums (the nodes/rectangles between alluvium).
    ggalluvial::geom_stratum(ggplot2::aes(fill = df_v2$risk_factor),
                             alpha = 1,
                             width = 2/3,
                             size = borders,
                             color = "white",
                             decreasing = FALSE) +
    # Change the colors of the diagram.
    ggplot2::scale_fill_manual(values = c("#CB6C56", "#CD925E", "#94B993", "#546C91", "#88ABC2")) +
    # Label the risk values onto the stratums.
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = df_v2$value), colour = "white", size = 3, fontface = "bold", decreasing = FALSE) +
    # Add the title.
    ggplot2::ggtitle("Risk Factor for Stroke in Blacks") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = (10), hjust = 0.5, margin = ggplot2::margin(0,0,20,0))) +
    # Move the x-axis (years) to the top and remove the axis label.
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", color = "black"), axis.title.x = ggplot2::element_blank()) +
    # Add the labels of the risk factors to the left of the graph and remove the axis label.
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = ifelse(df_v2$year == "1990", df_v2$risk_factor," ")), hjust = 0, nudge_x = -1.75, color = "black", size = 3,  decreasing = FALSE) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
    # Finally, remove the legend and background elements.
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}
