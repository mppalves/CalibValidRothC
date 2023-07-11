#' Plot Validation Results
#'
#' This function generates a set of validation plots for the confidence intervals.
#'
#' @param Cis A list containing two data frames: Ci_percent and Ci. These data frames
#'   are the output of the `conf_int` function.
#' @param plots_path Character string specifying the path where the generated plots will be saved.
#'
#' @return A list of ggplot objects representing the validation plots.
#'
#' @details The function generates multiple plots for each group in the data.
#'   Three types of plots are generated:
#'   1. Scatter plot comparing the measured and modeled values.
#'   2. Histogram of the residual errors.
#'   3. Plot showing the modeled effect with confidence intervals.
#'
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @usage plot_validt(Cis, plots_path)
#' @examples
#' # Example usage with the output of conf_int function
#' Cis <- list(
#'   Ci_percent = data.frame(Practice_Category = c("A", "B"), CFGs = c("CFG1", "CFG2"),
#'                           Climate_zones_IPCC = c("Zone1", "Zone2"),
#'                           ci_percent = c(80, 60), fields_inside = c(4, 3),
#'                           total_fields = c(5, 5), addtional_fields_to_90 = c(1, 2)),
#'   Ci = data.frame(Practice_Category = c("A", "A", "B", "B"),
#'                   CFGs = c("CFG1", "CFG2", "CFG1", "CFG2"),
#'                   Climate_zones_IPCC = c("Zone1", "Zone1", "Zone2", "Zone2"),
#'                   observed_diff = c(10, 12, 15, 8),
#'                   predicted_diff = c(9, 11, 13, 6),
#'                   model_error = c(-1, -1, -2, -2),
#'                   sd = c(0.5, 0.7, 0.8, 0.6),
#'                   n = c(3, 4, 5, 6),
#'                   cilower = c(8, 9, 11, 6),
#'                   ciupper = c(12, 13, 17, 10),
#'                   fill = c("Outside C.I.", "Inside C.I.", "Outside C.I.", "Inside C.I."))
#' )
#' plots_path <- "path/to/save/plots"
#' plot_validt(Cis, plots_path)
#'
#' @seealso See \code{\link{conf_int}} for calculating the confidence intervals.
#'@export

plot_validt3 <- function(Cis, plots_path, lim = NULL) {
  groups <- Cis$Ci %>%
    dplyr::group_split(Practice_Category, Climate_zones_IPCC, CFGs)
  if(is.null(lim)) lim <- max(Cis$Ci$soc_end_predicted, Cis$Ci$SOC_End_Converted)
  p_list <- list()

  for (i in 1:length(groups)) {
    p1 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = SOC_End_Converted, y = soc_end_predicted)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "blue", linetype = 2, linewidth = 0.2) +
      ggplot2::labs(
        x = "Measured (tC/ha)", y = "Modeled (tC/ha)"
      ) +
      ggplot2::ggtitle(paste("A")) +
      ggplot2::coord_fixed(xlim = c(0, lim), ylim = c(0, lim), ratio = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    p2 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = model_error)) +
      ggplot2::geom_histogram(binwidth = 1, bins = 5) +
      ggplot2::labs(title = paste("B"), x = "Residual Error", y = "Count") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    # Manually set the color levels for the legend
    colors <- c("Inside C.I." = "black", "Outside C.I." = "red")
    
    p3 <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = 1:nrow(groups[[i]]), y = predicted_diff)) +
      ggplot2::geom_point(ggplot2::aes(colour = as.factor(fill))) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = cilower, ymax = ciupper), width = 0.4) +
      ggplot2::scale_color_manual(values = colors, drop = FALSE, limits = names(colors)) +
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
