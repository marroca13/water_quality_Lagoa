## Functions used in DGAM.Rmd

plot_mvgam_trend_green = function (object, series = 1, newdata, data_test, realisations = FALSE,
                                   n_realisations = 15, n_cores = 1, derivatives = FALSE, hide_xlabels = FALSE,
                                   xlab, ylab, ...)
{
  if (!(inherits(object, "mvgam"))) {
    stop("argument \"object\" must be of class \"mvgam\"")
  }
  if (sign(series) != 1) {
    stop("argument \"series\" must be a positive integer",
         call. = FALSE)
  }
  else {
    if (series%%1 != 0) {
      stop("argument \"series\" must be a positive integer",
           call. = FALSE)
    }
  }
  if (series > NCOL(object$ytimes)) {
    stop(paste0("object only contains data / predictions for ",
                NCOL(object$ytimes), " series"), call. = FALSE)
  }
  if (attr(object$model_data, "trend_model") == "None" & !object$use_lv) {
    stop("no trend was estimated in object", call. = FALSE)
  }
  if (!missing("newdata")) {
    data_test <- newdata
  }
  data_train <- object$obs_data
  ends <- seq(0, dim(mvgam:::mcmc_chains(object$model_output, "ypred"))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes) + 1))])
  ends <- ends[-1]
  if (object$fit_engine == "stan") {
    preds <- mvgam:::mcmc_chains(object$model_output, "trend")[,
                                                       seq(series, dim(mvgam:::mcmc_chains(object$model_output,
                                                                                   "trend"))[2], by = NCOL(object$ytimes))]
  }
  else {
    preds <- mvgam:::mcmc_chains(object$model_output, "trend")[,
                                                       starts[series]:ends[series]]
  }
  s_name <- levels(data_train$series)[series]
  if (!missing(data_test)) {
    if (!"y" %in% names(data_test)) {
      data_test$y <- rep(NA, NROW(data_test))
    }
    if (!"series" %in% names(data_test)) {
      data_test$series <- factor("series1")
    }
    if (!"time" %in% names(data_test)) {
      stop("data_test does not contain a \"time\" column")
    }
    if (inherits(data_test, "list")) {
      all_obs <- c(data.frame(y = data_train$y, series = data_train$series,
                              time = data_train$time) %>% dplyr::filter(series ==
                                                                          s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                     dplyr::arrange(time) %>% dplyr::pull(y), data.frame(y = data_test$y,
                                                                         series = data_test$series, time = data_test$time) %>%
                     dplyr::filter(series == s_name) %>% dplyr::select(time,
                                                                       y) %>% dplyr::distinct() %>% dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }
    else {
      all_obs <- c(data_train %>% dplyr::filter(series ==
                                                  s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                     dplyr::arrange(time) %>% dplyr::pull(y), data_test %>%
                     dplyr::filter(series == s_name) %>% dplyr::select(time,
                                                                       y) %>% dplyr::distinct() %>% dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }
    if (dim(preds)[2] != length(all_obs)) {
      fc_preds <- forecast(object, data_test = data_test,
                           type = "trend", n_cores = n_cores)$forecasts[[series]]
      preds <- cbind(preds, fc_preds)
    }
  }
  preds_last <- preds[1, ]
  pred_vals <- seq(1:length(preds_last))
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds), function(n) quantile(preds[,
                                                           n], probs = probs))
  # Tonalidades verdes
  c_light <- c("#BCDCBC")
  c_light_highlight <- c("#99C799")
  c_mid <- c("#7CB97C")
  c_mid_highlight <- c("#50A250")
  c_dark <- c("#278F27")
  c_dark_highlight <- c("#007C00")

  if (missing(xlab)) {
    xlab <- "Time"
  }
  if (missing(ylab)) {
    ylab <- paste0("Estimated trend for ", levels(data_train$series)[series])
  }
  if (derivatives) {
    .pardefault <- par(no.readonly = T)
    on.exit(par(.pardefault))
    par(mfrow = c(2, 1))
    plot(1, type = "n", bty = "L", xlab = xlab, ylab = ylab,
         xlim = c(0, length(preds_last)), ylim = range(cred),
         ...)
    if (realisations) {
      for (i in 1:n_realisations) {
        lines(x = pred_vals, y = preds[i, ], col = "white",
              lwd = 2.5)
        lines(x = pred_vals, y = preds[i, ], col = sample(c("#BCDCBC",
                                                            "#99C799", "#7CB97C", "#50A250", "#007C00"),
                                                          1), lwd = 2.25)
      }
    }
    else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1, ],
                                              rev(cred[9, ])), col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2, ],
                                              rev(cred[8, ])), col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3, ],
                                              rev(cred[7, ])), col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4, ],
                                              rev(cred[6, ])), col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5, ], col = c_dark, lwd = 2.5)
    }
    box(bty = "L", lwd = 2)
    if (!missing(data_test)) {
      if (class(data_train)[1] == "list") {
        abline(v = length(data_train$y)/NCOL(object$ytimes),
               col = "#FFFFFF60", lwd = 2.85)
        abline(v = length(data_train$y)/NCOL(object$ytimes),
               col = "black", lwd = 2.5, lty = "dashed")
      }
      else {
        abline(v = NROW(data_train)/NCOL(object$ytimes),
               col = "#FFFFFF60", lwd = 2.85)
        abline(v = NROW(data_train)/NCOL(object$ytimes),
               col = "black", lwd = 2.5, lty = "dashed")
      }
    }
    first_derivs <- cbind(rep(NA, NROW(preds)), t(apply(preds,
                                                        1, diff)))
    cred <- sapply(1:NCOL(first_derivs), function(n) quantile(first_derivs[,
                                                                           n], probs = probs, na.rm = T))
    plot(1, type = "n", bty = "L", xlab = xlab, ylab = "1st derivative",
         xlim = c(min(pred_vals), max(pred_vals)), ylim = c(min(cred,
                                                                na.rm = T) - sd(first_derivs, na.rm = T), max(cred,
                                                                                                              na.rm = T) + sd(first_derivs, na.rm = T)), ...)
    if (realisations) {
      for (i in 1:n_realisations) {
        lines(x = pred_vals, y = first_derivs[i, ], col = "white",
              lwd = 2.5)
        lines(x = pred_vals, y = first_derivs[i, ], col = sample(c("#BCDCBC",
                                                                   "#99C799", "#7CB97C", "#50A250", "#007C00"),
                                                                 1), lwd = 2.25)
      }
    }
    else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1, ],
                                              rev(cred[9, ])), col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2, ],
                                              rev(cred[8, ])), col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3, ],
                                              rev(cred[7, ])), col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4, ],
                                              rev(cred[6, ])), col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5, ], col = c_dark, lwd = 2.5)
    }
    box(bty = "L", lwd = 2)
    abline(h = 0, lty = "dashed", lwd = 2)
    invisible()
  }
  else {
    if (hide_xlabels) {
      plot(1, type = "n", bty = "L", xlab = "", xaxt = "n",
           ylab = ylab, xlim = c(0, length(preds_last)),
           ylim = range(cred))
    }
    else {
      plot(1, type = "n", bty = "L", xlab = xlab, ylab = ylab,
           xlim = c(0, length(preds_last)), ylim = range(cred),
           ...)
    }
    if (realisations) {
      for (i in 1:n_realisations) {
        lines(x = pred_vals, y = preds[i, ], col = "white",
              lwd = 2.5)
        lines(x = pred_vals, y = preds[i, ], col = sample(c("#BCDCBC",
                                                            "#99C799", "#7CB97C", "#50A250", "#007C00"),
                                                          1), lwd = 2.25)
      }
    }
    else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1, ],
                                              rev(cred[9, ])), col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2, ],
                                              rev(cred[8, ])), col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3, ],
                                              rev(cred[7, ])), col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4, ],
                                              rev(cred[6, ])), col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5, ], col = c_dark, lwd = 2.5)
    }
    box(bty = "L", lwd = 2)
    if (!missing(data_test)) {
      if (class(data_train)[1] == "list") {
        abline(v = length(data_train$y)/NCOL(object$ytimes),
               col = "#FFFFFF60", lwd = 2.85)
        abline(v = length(data_train$y)/NCOL(object$ytimes),
               col = "black", lwd = 2.5, lty = "dashed")
      }
      else {
        abline(v = NROW(data_train)/NCOL(object$ytimes),
               col = "#FFFFFF60", lwd = 2.85)
        abline(v = NROW(data_train)/NCOL(object$ytimes),
               col = "black", lwd = 2.5, lty = "dashed")
      }
    }
  }
}


plot_mvgam_fc_green = function (object, series = 1, newdata, data_test, realisations = FALSE,
                                n_realisations = 15, hide_xlabels = FALSE, xlab, ylab, ylim,
                                n_cores = 1, return_forecasts = FALSE, return_score = FALSE,
                                ...)
{
  if (!(inherits(object, "mvgam"))) {
    stop("argument \"object\" must be of class \"mvgam\"")
  }
  if (sign(series) != 1) {
    stop("argument \"series\" must be a positive integer",
         call. = FALSE)
  }
  else {
    if (series%%1 != 0) {
      stop("argument \"series\" must be a positive integer",
           call. = FALSE)
    }
  }
  if (series > NCOL(object$ytimes)) {
    stop(paste0("object only contains data / predictions for ",
                NCOL(object$ytimes), " series"), call. = FALSE)
  }
  if (sign(n_realisations) != 1) {
    stop("argument \"n_realisations\" must be a positive integer",
         call. = FALSE)
  }
  else {
    if (n_realisations%%1 != 0) {
      stop("argument \"n_realisations\" must be a positive integer",
           call. = FALSE)
    }
  }
  if (return_score) {
    return_forecasts <- TRUE
  }
  if (missing(data_test) & missing("newdata")) {
    if (!is.null(object$test_data)) {
      data_test <- object$test_data
    }
  }
  if (!missing("newdata")) {
    data_test <- newdata
    if (terms(formula(object$call))[[2]] != "y") {
      data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
    }
  }
  data_train <- object$obs_data
  ends <- seq(0, dim(mvgam:::mcmc_chains(object$model_output, "ypred"))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes) + 1))])
  ends <- ends[-1]
  if (object$fit_engine == "stan") {
    preds <- mvgam:::mcmc_chains(object$model_output, "ypred")[,
                                                       seq(series, dim(mvgam:::mcmc_chains(object$model_output,
                                                                                   "ypred"))[2], by = NCOL(object$ytimes)), drop = FALSE]
  }
  else {
    preds <- mvgam:::mcmc_chains(object$model_output, "ypred")[,
                                                       starts[series]:ends[series], drop = FALSE]
  }
  s_name <- levels(data_train$series)[series]
  if (!missing(data_test)) {
    if (terms(formula(object$call))[[2]] != "y") {
      if (object$family %in% c("binomial", "beta_binomial")) {
        resp_terms <- as.character(terms(formula(object$call))[[2]])
        resp_terms <- resp_terms[-grepl("cbind", resp_terms)]
        trial_name <- resp_terms[2]
        data_test$y <- data_test[[resp_terms[1]]]
        if (!exists(trial_name, data_test)) {
          stop(paste0("Variable ", trial_name, " not found in newdata"),
               call. = FALSE)
        }
      }
      else {
        data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
      }
    }
    if (!"y" %in% names(data_test)) {
      data_test$y <- rep(NA, NROW(data_test))
    }
    if (inherits(data_test, "list")) {
      if (!"time" %in% names(data_test)) {
        stop("data_test does not contain a \"time\" column")
      }
      if (!"series" %in% names(data_test)) {
        data_test$series <- factor("series1")
      }
    }
    else {
      if (!"time" %in% colnames(data_test)) {
        stop("data_test does not contain a \"time\" column")
      }
      if (!"series" %in% colnames(data_test)) {
        data_test$series <- factor("series1")
      }
    }
    if (inherits(data_test, "list")) {
      all_obs <- c(data.frame(y = data_train$y, series = data_train$series,
                              time = data_train$time) %>% dplyr::filter(series ==
                                                                          s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                     dplyr::arrange(time) %>% dplyr::pull(y), data.frame(y = data_test$y,
                                                                         series = data_test$series, time = data_test$time) %>%
                     dplyr::filter(series == s_name) %>% dplyr::select(time,
                                                                       y) %>% dplyr::distinct() %>% dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }
    else {
      all_obs <- c(data_train %>% dplyr::filter(series ==
                                                  s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                     dplyr::arrange(time) %>% dplyr::pull(y), data_test %>%
                     dplyr::filter(series == s_name) %>% dplyr::select(time,
                                                                       y) %>% dplyr::distinct() %>% dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }
    if (dim(preds)[2] != length(all_obs)) {
      s_name <- levels(object$obs_data$series)[series]
      if (attr(object$model_data, "trend_model") == "None") {
        if (class(object$obs_data)[1] == "list") {
          series_obs <- which(data_test$series == s_name)
          series_test <- lapply(data_test, function(x) {
            if (is.matrix(x)) {
              matrix(x[series_obs, ], ncol = NCOL(x))
            }
            else {
              x[series_obs]
            }
          })
        }
        else {
          series_test = data_test %>% dplyr::filter(series ==
                                                      s_name)
        }
        fc_preds <- predict.mvgam(object, newdata = series_test,
                                  type = "response", n_cores = n_cores)
      }
      else {
        fc_preds <- forecast.mvgam(object, data_test = data_test,
                                   n_cores = n_cores)$forecasts[[series]]
      }
      preds <- cbind(preds, fc_preds)
    }
  }
  preds_last <- preds[1, ]
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds), function(n) quantile(preds[,
                                                           n], probs = probs, na.rm = TRUE))
  # Tonalidades verdes
  c_light <- c("#BCDCBC")
  c_light_highlight <- c("#99C799")
  c_mid <- c("#7CB97C")
  c_mid_highlight <- c("#50A250")
  c_dark <- c("#278F27")
  c_dark_highlight <- c("#007C00")

  if (missing(ylim)) {
    ytrain <- data.frame(series = data_train$series, time = data_train$time,
                         y = data_train$y) %>% dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>% dplyr::distinct() %>%
      dplyr::arrange(time) %>% dplyr::pull(y)
    if (tolower(object$family) %in% c("beta", "bernoulli")) {
      ylim <- c(min(cred, min(ytrain, na.rm = TRUE)), max(cred,
                                                          max(ytrain, na.rm = TRUE)))
      ymin <- max(0, ylim[1])
      ymax <- min(1, ylim[2])
      ylim <- c(ymin, ymax)
    }
    else if (tolower(object$family) %in% c("lognormal", "gamma")) {
      ylim <- c(min(cred, min(ytrain, na.rm = TRUE)), max(cred,
                                                          max(ytrain, na.rm = TRUE)))
      ymin <- max(0, ylim[1])
      ymax <- max(ylim)
      ylim <- c(ymin, ymax)
    }
    else {
      ylim <- c(min(cred, min(ytrain, na.rm = TRUE)), max(cred,
                                                          max(ytrain, na.rm = TRUE)))
    }
  }
  if (missing(ylab)) {
    ylab <- paste0("Predicitons for ", levels(data_train$series)[series])
  }
  if (missing(xlab)) {
    xlab <- "Time"
  }
  pred_vals <- seq(1:length(preds_last))
  if (hide_xlabels) {
    plot(1, type = "n", bty = "L", xlab = "", xaxt = "n",
         ylab = ylab, xlim = c(0, length(preds_last)), ylim = ylim,
         ...)
  }
  else {
    plot(1, type = "n", bty = "L", xlab = xlab, ylab = ylab,
         xaxt = "n", xlim = c(0, length(preds_last)), ylim = ylim,
         ...)
    if (!missing(data_test)) {
      axis(side = 1, at = floor(seq(0, max(data_test$time) -
                                      (min(object$obs_data$time) - 1), length.out = 6)),
           labels = floor(seq(min(object$obs_data$time),
                              max(data_test$time), length.out = 6)))
    }
    else {
      axis(side = 1, at = floor(seq(0, max(object$obs_data$time) -
                                      (min(object$obs_data$time) - 1), length.out = 6)),
           labels = floor(seq(min(object$obs_data$time),
                              max(object$obs_data$time), length.out = 6)))
    }
  }
  if (realisations) {
    for (i in 1:n_realisations) {
      lines(x = pred_vals, y = preds[i, ], col = "white",
            lwd = 2.5)
      lines(x = pred_vals, y = preds[i, ], col = sample(c("#BCDCBC",
                                                          "#99C799", "#7CB97C", "#50A250", "#007C00"),
                                                        1), lwd = 2.25)
    }
  }
  else {
    polygon(c(pred_vals, rev(pred_vals)), c(cred[1, ], rev(cred[9,
    ])), col = c_light, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[2, ], rev(cred[8,
    ])), col = c_light_highlight, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[3, ], rev(cred[7,
    ])), col = c_mid, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[4, ], rev(cred[6,
    ])), col = c_mid_highlight, border = NA)
    lines(pred_vals, cred[5, ], col = c_dark, lwd = 2.5)
  }
  box(bty = "L", lwd = 2)
  if (!missing(data_test)) {
    if (class(data_train)[1] == "list") {
      data_train <- data.frame(series = data_train$series,
                               y = data_train$y, time = data_train$time)
      data_test <- data.frame(series = data_test$series,
                              y = data_test$y, time = data_test$time)
    }
    last_train <- (NROW(data_train)/NCOL(object$ytimes))
    if (!realisations) {
      polygon(c(pred_vals[1:(NROW(data_train)/NCOL(object$ytimes))],
                rev(pred_vals[1:(NROW(data_train)/NCOL(object$ytimes))])),
              c(cred[1, 1:(NROW(data_train)/NCOL(object$ytimes))],
                rev(cred[9, 1:(NROW(data_train)/NCOL(object$ytimes))])),
              col = "grey70", border = NA)
      lines(pred_vals[1:(NROW(data_train)/NCOL(object$ytimes))],
            cred[5, 1:(NROW(data_train)/NCOL(object$ytimes))],
            col = "grey70", lwd = 2.5)
    }
    points(dplyr::bind_rows(data_train, data_test) %>% dplyr::filter(series ==
                                                                       s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
             dplyr::arrange(time) %>% dplyr::pull(y), pch = 16,
           col = "white", cex = 0.8)
    points(dplyr::bind_rows(data_train, data_test) %>% dplyr::filter(series ==
                                                                       s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
             dplyr::arrange(time) %>% dplyr::pull(y), pch = 16,
           col = "black", cex = 0.65)
    abline(v = last_train, col = "#FFFFFF60", lwd = 2.85)
    abline(v = last_train, col = "black", lwd = 2.5, lty = "dashed")
    truth <- as.matrix(data_test %>% dplyr::filter(series ==
                                                     s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                         dplyr::arrange(time) %>% dplyr::pull(y))
    last_train <- length(data_train %>% dplyr::filter(series ==
                                                        s_name) %>% dplyr::select(time, y) %>% dplyr::distinct() %>%
                           dplyr::arrange(time) %>% dplyr::pull(y))
    fc <- preds[, (last_train + 1):NCOL(preds)]
    if (all(is.na(truth))) {
      score <- NULL
      message("No non-missing values in data_test$y; cannot calculate forecast score")
    }
    else {
      if (object$family %in% c("poisson", "negative binomial",
                               "tweedie", "binomial", "beta_binomial")) {
        if (max(fc, na.rm = TRUE) > 50000) {
          score <- sum(crps_mcmc_object(as.vector(truth),
                                        fc)[, 1], na.rm = TRUE)
          message(paste0("Out of sample CRPS:\n", score))
        }
        else {
          score <- sum(drps_mcmc_object(as.vector(truth),
                                        fc)[, 1], na.rm = TRUE)
          message(paste0("Out of sample DRPS:\n", score))
        }
      }
      else {
        score <- sum(crps_mcmc_object(as.vector(truth),
                                      fc)[, 1], na.rm = TRUE)
        message(paste0("Out of sample CRPS:\n", score))
      }
    }
  }
  else {
    if (class(data_train)[1] == "list") {
      data_train <- data.frame(series = data_train$series,
                               y = data_train$y, time = data_train$time)
    }
    points(data_train %>% dplyr::filter(series == s_name) %>%
             dplyr::select(time, y) %>% dplyr::distinct() %>%
             dplyr::arrange(time) %>% dplyr::pull(y), pch = 16,
           col = "white", cex = 0.8)
    points(data_train %>% dplyr::filter(series == s_name) %>%
             dplyr::select(time, y) %>% dplyr::distinct() %>%
             dplyr::arrange(time) %>% dplyr::pull(y), pch = 16,
           col = "black", cex = 0.65)
  }
  if (return_forecasts) {
    if (return_score) {
      if (!missing(data_test)) {
        return(list(forecast = preds[, (last_train +
                                          1):NCOL(preds)], score = score))
      }
      else {
        return(list(forecast = preds, score = NULL))
      }
    }
    else {
      if (!missing(data_test)) {
        return(preds[, (last_train + 1):NCOL(preds)])
      }
      else {
        return(preds)
      }
    }
  }
}


# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
# Functions for derivatives of GAM(M) models ##

Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  newDF <- data.frame(newD) ## needs to be a data frame for predict
  X0 <- predict(mod, newDF, type = "lpmatrix")
  newDF <- newDF + eps
  X1 <- predict(mod, newDF, type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  # bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  lD ##return
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { ## how many attempts to get this right!?!?
    ##term <- match(term, term.labs)
    ##term <- term[match(term, term.labs)]
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else {
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(miss))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")
    names(xlab) <- xlab
  }
  if (missing(main)) {
    main <- term
    names(main) <- term
  }
  ## compute confidence interval
  CI <- confint(x, term = term)
  ## plots
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  for(i in term) {
    upr <- CI[[i]]$upper
    lwr <- CI[[i]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,i], x[[i]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,i], rev(x$eval[,i])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,i], upr, lty = "dashed")
      lines(x$eval[,i], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
      S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,i], S$incr, lwd = lwd, col = "red")
      lines(x$eval[,i], S$decr, lwd = lwd, col = "blue")
    } else {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}

