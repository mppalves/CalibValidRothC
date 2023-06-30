#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias
#' @import patchwork
#' @import ggplot2
#' @importFrom dplyr group_split
#' @export

plot_validt <- function(Cis, plots_path) {
  groups <- Cis$Ci %>%
    dplyr::group_split(Practice_Category, Climate_zones_IPCC, CFGs)

  p_list <- list()

  for (i in 1:length(groups)) {
    p1 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = SOC_End_Converted, y = soc_end_predicted)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "blue", linetype = 2, linewidth = 0.5) +
      ggplot2::labs(
        x = "Measured (tC/ha)", y = "Modeled (tC/ha)"
      ) +
      ggplot2::ggtitle(paste("A")) +
      ggplot2::coord_fixed(xlim = c(0, 70), ylim = c(0, 70), ratio = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    p2 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = model_error)) +
      ggplot2::geom_histogram(binwidth = 1, bins = 5) +
      ggplot2::labs(title = paste("B"), x = "Residual Error", y = "Count") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    p3 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = 1:nrow(groups[[i]]), y = predicted_diff)) +
      ggplot2::geom_point(ggplot2::aes(colour = as.factor(fill))) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = cilower, ymax = ciupper), width = 0.4) +
      ggplot2::scale_color_manual(
        values = c(
          "Inside C.I." = "black",
          "Outside C.I." = "red"
        )
      ) +
      ggplot2::labs(
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
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 12))

    p <- (p1 + p2) / p3
    p <- p + patchwork::plot_layout(heights = c(2, 1))
    p_list[[i]] <- p
    ggplot2::ggsave(
      filename = paste(
        paste(groups[[i]][1, c("Practice_Category", "Climate_zones_IPCC", "CFGs")], collapse = "_"),
        ".jpg",
        collapse = "_"
      ),
      width = 7.5,
      height = 7,
      plot = p,
      path = plots_path
    )
  }

  return(invisible(p_list))
}
