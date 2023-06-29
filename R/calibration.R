#' @title Calibration
#'
#' @description Function that performs the calibration of RothC for the data set input
#'
#' @param data_source data source pro processed via \link{expand_data_set}
#' @param stopval Finishing criteria by error size
#' @param maxeval Finishing criteria by the number of algorithmic iterations
#' @param lb lower boundary
#' @param ub upper boundary
#' @param print_nlopt Bool: print the output from nlopt to check optimisation progress.
#' @return return all_metrics:
#' @author Marcos Alves
#' @export

calibration <- function(
    data_source,
    stopval = 0.01,
    maxeval = 100,
    ub = 1.2,
    lb = 0.8,
    print_nlopt = FALSE
) {
  names(data_source) <- names(data_source) %>% gsub(" ", "_", .)
  names(data_source) <- names(data_source) %>% gsub("\\(", "", .)
  names(data_source) <- names(data_source) %>% gsub("\\)", "", .)
  names(data_source) <- names(data_source) %>% gsub("\\.", "", .)

  cz <- unique(data_source$Climate_zones_IPCC)
  solution <- matrix(NA, length(cz), 4, dimnames = list(cz, c("k", "cf3", "cf4", "cf5")))
  predictions <- list()
  data_out <- NULL
  # metrics <- list()
  all_metrics <- list()
  predicted <- list()

  # Define the lower, upper bounds and starting values
  lb <- rep(lb, 3)
  ub <- rep(ub, 3)
  x0 <- c(1, 1, 1)

  for (i in 1:length(cz)) {
    x <- dplyr::filter(data_source, Climate_zones_IPCC == cz[i])
    pub_id <- as.character(dplyr::pull(x[, "Publication_ID"]))
    n_unique_pub <- length(unique(pub_id))
    # skip the groups that have only one study
    if (n_unique_pub < 2) {
      logger::log_warn("Fewer than 2 unique studies in climate zone: {cz[i]}")
      next
    }

    # if n_unique_pub (i.e. the number of studies) is less than 10, do LOGO
    # (leave one group out, otherwise do 5-fold group CV)
    if (n_unique_pub <= 10) {
      k <- n_unique_pub
      klog <- "LOGO"
    } else {
      k <- 5
      klog <- "5"
    }

    y <- unlist(x[, "SOC_End_Converted"])
    folds <- caret::groupKFold(pub_id, k = k)

    metrics <- lapply(seq_len(length(folds)), function(j) {
      logger::log_trace("Calibrating on fold {j} of {length(folds)} in climate zone: {cz[i]}")
      train_indices <- folds[[j]]
      test_indices <- setdiff(seq_len(nrow(x)), train_indices)
      logger::log_trace("Fold {j} contains {length(train_indices)} training points and {length(test_indices)} test points")

      x_train <- x[train_indices, ]
      y_train <- y[train_indices]
      x_test <- x[test_indices, ]
      y_test <- y[test_indices]
      n_test <- length(test_indices)
      train_data <- list(x_train = x_train, y_train = y_train)

      # fit calibration parameters
      res <- nloptr::nloptr(
        x0 = x0,
        eval_f = optm_fun,
        lb = lb,
        ub = ub,
        opts = list(
          algorithm = "NLOPT_GN_ORIG_DIRECT",
          stopval = stopval,
          maxeval = maxeval,
          print_level = ifelse(print_nlopt, 1, 0)
        ),
        train_data = train_data
      )

      y_pred <- simulations(res$solution, test_data = x_test)
      res_tbl <- dplyr::bind_cols(
        bias_rmse(x_test, y_test, y_pred), 
        fold = j, 
        fold_test_size = n_test
      )

      return(res_tbl)
    })

    all_metrics[[i]] <- dplyr::bind_cols(dplyr::bind_rows(metrics), "climate_zone" = cz[i])

    logger::log_trace("Starting final calibration")
    res_all <- nloptr::nloptr(
      x0 = x0,
      eval_f = optm_fun,
      lb = lb,
      ub = ub,
      opts = list(
        algorithm = "NLOPT_GN_ORIG_DIRECT",
        stopval = stopval,
        maxeval = maxeval,
        print_level = ifelse(print_nlopt, 1, 0)
      ),
      train_data = list(x_train = x, y_train = y)
    )

    solution[i, c("cf3", "cf4", "cf5")] <- res_all$solution
    solution[i, "k"] <- klog
    data_out[[i]] <- dplyr::bind_cols(
      x,
      dplyr::tibble(soc_end_predicted = simulations(res_all$solution, test_data = x))
    )
  }
  all_metrics <- dplyr::bind_rows(all_metrics)
  data_out <- dplyr::bind_rows(data_out)
  return(list(all_metrics = all_metrics, data_out = data_out, solution = solution))
}
