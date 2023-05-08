#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias
#' @import ggplot2
#' @import patchwork
#' @export

plot_validt <- function(Cis, plots_path) {
  groups <- Cis$Ci %>%
    group_split(Practice_Category, Climate_zones_IPCC, CFGs)

  for (i in 1:length(groups)) {
    p1 <- ggplot(groups[[i]], aes(x = SOC_End_Converted, y = soc_end_predicted)) +
      geom_point(size = 1) +
      geom_abline(slope = 1, intercept = 0, colour = "blue", linetype = 2, linewidth = 0.5) +
      labs(
        x = "Measured (tC/ha)", y = "Modeled (tC/ha)"
      ) +
      ggtitle(paste("A")) +
      coord_fixed(xlim = c(0, 70), ylim = c(0, 70), ratio = 1) +
      theme(plot.title = element_text(face = "bold"))

    p2 <- ggplot(groups[[i]], aes(x = model_error)) +
      geom_histogram(binwidth = 1, bins = 5) +
      labs(title = paste("B"), x = "Residual Error", y = "Count") +
      theme(plot.title = element_text(face = "bold"))

    p3 <- ggplot(groups[[i]], aes(x = 1:nrow(groups[[i]]), y = predicted_diff)) +
      geom_point(aes(colour = as.factor(fill))) +
      geom_errorbar(aes(ymin = cilower, ymax = ciupper), width = 0.4) +
      scale_color_manual(values = c("black", "red")) +
      labs(
        title = paste("C"),
        x = expression(paste("i"^{
          "th"
        }, " modeled effect")),
        y = "t C/ha",
        colour = "Category",
        caption =
          paste0(
            "\n",
            "\n",
            paste(groups[[i]][1, c("Practice_Category", "Climate_zones_IPCC", "CFGs")], collapse = " ")
          )
      ) +
      theme(plot.title = element_text(face = "bold")) +
      theme(plot.caption = element_text(hjust = 0, size = 12))

    p <- (p1 + p2) / p3
    p <- p + plot_layout(heights = c(2, 1))
    ggsave(filename = paste(paste(groups[[i]][1, c("Practice_Category", "Climate_zones_IPCC", "CFGs")], collapse = "_"), ".jpg", collapse = "_"), width = 7.5, height = 7, plot = p, path = plots_path)

  }
}
