#' @title Calibration
#'
#' @description Function that performs the calibration of RothC for the data set
#' input
#'
#' @param data_source data source pro processed via \link{expand_data_set}
#' @param stopval Finishing criteria by error size
#' @param maxeval Finihsing criteria by the number of algorithmic iterations
#' @param lb lower boundary
#' @param ub upper boundary
#' @return return all_metrics:
#' @author Marcos Alves
#' @import caret
#' @import dplyr
#' @import nloptr
#' @import Metrics
#' @import AgreenaRothC2
#' @export

calibration <- function(data_source, stopval = 0.01, maxeval = 100, ub = 1.5, lb = 0.3) {
  names(data_source) <- names(data_source) %>% gsub(" ", "_", .)
  names(data_source) <- names(data_source) %>% gsub("\\(", "", .)
  names(data_source) <- names(data_source) %>% gsub("\\)", "", .)
  names(data_source) <- names(data_source) %>% gsub("\\.", "", .)

  cz <- unique(data_source$Climate_zones_IPCC)
  solution <- matrix(NA, length(cz), 4, dimnames = list(cz, c("k", "cf3", "cf4", "cf5")))
  predictions <- list()
  data_out <- NULL
  metrics <- list()
  all_metrics <- list()
  predicted <- list()

  for (i in 1:length(cz)) {
    x <- filter(data_source, Climate_zones_IPCC == cz[i])
    # skip the groups that have no observations
    if (dim(x)[1] <= 1) {
      next
    }

    # Defining kfold groups (k) as the same numbner as the observations is equivalent to the leave-one-out validation approach used for small data sets
    if (dim(x)[1] <= 10) {
      k <- nrow(x)
      klog <- "LOOUT"
    } else {
      k <- 5
      klog <- "5"
    }

    y <- unlist(x[, "SOC_End_Converted"])
    folds <- createFolds(y, k = k, list = TRUE)

    # Define the lower, upper bounds and starting values
    lb <- rep(lb, 3)
    ub <- rep(ub, 3)
    x0 <- c(1, 1, 1)

    for (j in 1:length(folds)) {
      test_indices <- folds[[j]]
      train_indices <- setdiff(seq_len(nrow(x)), test_indices)
      x_train <- x[train_indices, ]
      y_train <- y[train_indices]
      x_test <- x[test_indices, ]
      y_test <- y[test_indices]
      train_data <- list("x_train" = x_train, "y_train" = y_train)
      # fit calibration parameters
      res <- nloptr(
        x0 = x0, eval_f = optm_fun,
        lb = lb, ub = ub,
        opts = list(
          "algorithm" = "NLOPT_GN_ORIG_DIRECT",
          "stopval" = stopval,
          "maxeval" = maxeval
        ),
        "train_data" = train_data
      )
      y_pred <- simulations(res$solution, test_data = x_test)
      metrics[[j]] <- bind_cols(bias_rmse(x_test, y_test, y_pred), "fold" = j)
    }
    all_metrics[[i]] <- bind_cols(bind_rows(metrics), "climate_zone" = cz[i])

    res_all <- nloptr(
      x0 = x0, eval_f = optm_fun,
      lb = lb, ub = ub,
      opts = list(
        "algorithm" = "NLOPT_GN_ORIG_DIRECT",
        "stopval" = stopval,
        "maxeval" = maxeval
      ),
      "train_data" = list("x_train" = x, "y_train" = y)
    )$solution

    solution[i, c("cf3", "cf4", "cf5")] <- res_all
    solution[i, "k"] <- klog
    data_out[[i]] <- bind_cols(x, tibble(soc_end_predicted = simulations(res_all, test_data = x)))
  }
  all_metrics <- bind_rows(all_metrics)
  data_out <- bind_rows(data_out)

  suppressWarnings(rm(list = c("base_yields", "resid_mgmt", "crop_names", "yld2bio", "tillage_convert")))

  return(list("all_metrics" = all_metrics, "data_out" = data_out, "solution" = solution))
}
