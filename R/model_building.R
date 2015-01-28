#' Function doing base part of study
#' 
#' @param input_data A data frame
#' @param p An integer, number of max AR terms
#' @param q An integer, number of max MA terms
#' 
#' @export
select_arima <- function(input_frame, max.p = 7, max.q = 7) {  
  
  calc_bic <- function(model, p, q) {
    data.frame(p = p,
               q = q,
               aic = -2 * model$loglik + 2 * length(model$coef),
               bic = -2 * model$loglik + length(model$coef) *
                 log(length(model$residuals)),
               hic = -2 * model$loglik + 2 * length(model$coef) *
                 log(log(length(model$residuals))))     
  }
  
  out <- lapply(0:max.p, function(x) {
    lapply(0:max.q, function(y) {
      arima(input_frame$price,
            order = c(x, 0, y),
            optim.control = list(maxit = 2000)) %>% calc_bic(x, y)
    }
    ) %>% rbind_all
  }
  ) %>% rbind_all
}

#' @export
select_garch <- function(input_frame, max.p = 7, max.q = 7) {
  out <- lapply(1:max.p, function(p) {
    lapply(1:max.q, function(q) {
      cat(paste0("Estimating garch model with p ", p, " and q ", q, "\n"))
      model <- ugarchspec(
        variance.model = list(model = "sGARCH",
                              garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(6, 5)),
        distribution = "norm")
      ugarchfit(spec = model,
                data = input_frame$price) %>%
        infocriteria %>% 
        as.data.frame %>%
        add_rownames %>% 
        transmute(info_crit = rowname,
                  value = V1,
                  p = p,
                  q = q)      
    }) %>% rbind_all
  }) %>% rbind_all
}

#' Function basically performing the same task as e1071::tune, but here made
#' clear.
#' @param input_frame A data frame containing a date and a price.
#' @param max.cost An integer containing the maximum allowed cost. If left as
#' NULL, it will default to floor(max(abs(price)) + 3 * sd(price))
#' @param max.epsilon An interger being the max allowed epsilon parameter. If 
#' left as null, it will default to 4, being the value where no support vectors
#' are returned for the base spot price for Germany.
#' @param cachesize cache memory in MB (default 4000)
#' 
#' @export
select_svm <- function(input_frame, kernel = "linear", max.polynomial = 2,
                       cachesize = 500, min_lag = 7,
                       eps_grid = 10 ^ c(0.5, 0.75, 1, 1.25, 1.5),
                       gamma_grid = 10 ^ c(0.5, 0.75, 1, 1.25, 1.5),
                       sigma_grid = -10 ^ seq(-1, 1, 0.5),
                       cores = 8L) {
  
  cost <- max(input_frame$price)
  
  if (is.null(eps_grid)) {
    noise_param <- input_frame %>%
      use_series(price) %>%
      stats::filter(rep(1/7, 7), sides = 2) %>%
      subtract(input_frame %>% use_series(price), .) %>%
      sd(na.rm = TRUE)
    eps_grid = noise_param * c(0.5, 0.75, 1, 1.25, 1.5)
  }
  
  if (is.null(gamma_grid)) {
    spread_param <- input_frame %>%
      use_series(price) %>%
      sd(na.rm = TRUE)
    gamma_grid = spread_param * c(0.5, 0.75, 1, 1.25, 1.5)
  }
  
  train_frame <- data.frame(y = input_frame$price,
                            x1 = lag(input_frame$price, 1),
                            x2 = lag(input_frame$price, 2),
                            x3 = lag(input_frame$price, 3),
                            x4 = lag(input_frame$price, 4),
                            x5 = lag(input_frame$price, 5),
                            x6 = lag(input_frame$price, 6),
                            x7 = lag(input_frame$price, 7)) %>%
    na.omit
  
  if (kernel == "linear") {
    svm_out <- 
      mclapply((min_lag + 1):ncol(train_frame), function(t) {
        mclapply(eps_grid, function(e) {
          cat(paste0("Calculating svm regression with linear kernel, ", t - 1,
                     " lags, ", cost, " cost, and ", e, " epsilon"))
          
          out <- try(svm(y ~ .,
                         data = train_frame[, 1:t],
                         kernel = kernel,
                         scale = TRUE,
                         type = "eps-regression",
                         cost = cost,
                         epsilon = e,
                         cachesize = cachesize,
                         tolerence = 0.1)) %>%
            {
              if (inherits(., "try-error") %>% `!`) {
                data.frame(mse = fitted(.) %>%
                             as.numeric %>%
                             subtract(train_frame[, 1], .) %>%
                             raise_to_power(2) %>%
                             mean,
                           support_vectors = use_series(., SV) %>%
                             length)
                } else {
                  data.frame(mse = NA,
                             support_vectors = NA)
                }
              } %>%
            transmute(kernel = kernel,
                      lags = t - 1,
                      cost = cost,
                      epsilon = e,
                      mse,
                      support_vectors)
          
          cat(" ... Done\n")
          if (length(save_path) > 0 ) write.csv(out, file = paste0(save_path, "/", kernel, "_lags_", t - 1, "_lags_", cost, "_cost_", e, "_epsilon.csv"))
          return(out)
          }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
        }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
    } else if (kernel == "polynomial") {
      gamma <- 1
      coef0 <- 1
      
      svm_out <- 
        mclapply((min_lag + 1):ncol(train_frame), function(t) {
          out <- mclapply(2:max.polynomial, function(d) {
            lapply(eps_grid, function(e) {
              cat(paste0("Calculating svm regression with polynomial kernel ",
                         "of degree, ", d, " with ", t - 1, " lags, ", cost,
                         " cost, and ", e, " epsilon"))
              
              out <- try(svm(y ~ .,
                             data = train_frame[, 1:t],
                             kernel = kernel,
                             scale = TRUE,
                             type = "eps-regression",
                             cost = cost,
                             epsilon = e,
                             gamma = gamma,
                             coef0 = coef0,
                             degree = d,
                             cachesize = cachesize)) %>%
                {
                  if (inherits(., "try-error") %>% `!`) {
                    data.frame(mse = fitted(.) %>%
                                 as.numeric %>%
                                 subtract(train_frame[, 1], .) %>%
                                 raise_to_power(2) %>%
                                 mean,
                               support_vectors = use_series(., SV) %>%
                                 length)
                    } else {
                      data.frame(mse = NA,
                                 support_vectors = NA)
                    }
                  } %>%
                transmute(kernel = kernel,
                          lags = t - 1,
                          cost = cost,
                          epsilon = e,
                          degree = d,
                          mse,
                          support_vectors)
              
              cat(" ... Done\n")
              if (length(save_path) > 0 ) write.csv(out, file = paste0(save_path, "/", kernel, "_of_degree_", d, "_lags_", t - 1, "_lags_", cost, "_cost_", e, "_epsilon.csv"))
              return(out)
              }) %>% rbind_all
            }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
          }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
      } else if (kernel == "radial") {
        svm_out <-
          mclapply((min_lag + 1):ncol(train_frame), function(t) {
            lapply(gamma_grid, function(g) {
              lapply(eps_grid, function(e) {
                cat(paste0("Calculating svm regression with radial basis ker",
                           "nel with ", t - 1, " lags, ", g, " gamma, ", cost,
                           " cost, and ", e, " epsilon"))
                
                out <- try(svm(y ~ .,
                               data = train_frame[, 1:t],
                               kernel = kernel,
                               scale = TRUE,
                               type = "eps-regression",
                               cost = cost,
                               epsilon = e,
                               gamma = g,
                               cachesize = cachesize)) %>%
                  {
                    if (inherits(., "try-error") %>% `!`) {
                      data.frame(mse = fitted(.) %>%
                                   as.numeric %>%
                                   subtract(train_frame[, 1], .) %>%
                                   raise_to_power(2) %>%
                                   mean,
                                 support_vectors = use_series(., SV) %>%
                                   length)
                      } else {
                        data.frame(mse = NA,
                                   support_vectors = NA)
                      }
                    } %>%
                  transmute(kernel = kernel,
                            lags = t - 1,
                            cost = cost,
                            epsilon = e,
                            gamma = g,
                            mse,
                            support_vectors)
                
                cat(" ... Done\n")
                if (length(save_path) > 0 ) write.csv(out, file = paste0(save_path, "/", kernel, "_gamma_", g, "_lags_", t - 1, "_lags_", cost, "_cost_", e, "_epsilon.csv"))
                return(out)
                }) %>% rbind_all
              }) %>% rbind_all
            }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
        } else if (kernel == "sigmoid") {
    svm_out <- 
      mclapply((min_lag + 1):ncol(train_frame), function(t) {
        mclapply(sigma_grid, function(s) {
          lapply(gamma_grid, function(g) {
                   lapply(eps_grid,
                          function(e) {
              cat(paste0("Calculating svm regression with sigmoid kernel w",
                         "ith ", t - 1, " lags, ", s, " coef0, ", g,
                         " gamma, ", cost, " cost, and ", e, " epsilon"))
              
              out <- try(svm(y ~ .,
                             data = train_frame[, 1:t],
                             kernel = kernel,
                             scale = TRUE,
                             type = "eps-regression",
                             cost = cost,
                             epsilon = e,
                             gamma = g,
                             coef0 = s,
                             cachesize = cachesize)) %>%
                {
                  if (inherits(., "try-error") %>% `!`) {
                    data.frame(mse = fitted(.) %>%
                                 as.numeric %>%
                                 subtract(train_frame[, 1], .) %>%
                                 raise_to_power(2) %>%
                                 mean,
                               support_vectors = use_series(., SV) %>%
                                 length)
                    } else {
                      data.frame(mse = NA,
                                 support_vectors = NA)
                    }
                  } %>%
                transmute(kernel = kernel,
                          lags = t - 1,
                          cost = cost,
                          epsilon = e,
                          gamma = g,
                          coef0 = s,
                          mse,
                          support_vectors)
              
              cat(" ... Done\n")
              if (length(save_path) > 0 ) write.csv(out, file = paste0(save_path, "/", kernel, "_gamma_", g, "_sigma_", s, "_lags_", t - 1, "_lags_", cost, "_cost_", e, "_epsilon.csv"))
              return(out)
              }) %>% rbind_all
            }) %>% rbind_all
          }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
        }, mc.cores = getOption("mc.cores", cores)) %>% rbind_all
      }
}

#' Function doing oos forecast using a given model type over a given horison
#' @export
oos_forecast <- function(input_frame, forecast_type = "arima",
                         test_start = "2012-11-01", h = 1, save_path = NULL,
                         outlier_filt = TRUE) {
  
  test_start %<>% as.Date
  
  input_frame %<>% mutate(date = date %>% as.Date) %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  forecast_frame <- input_frame %>%
    left_join(get_holi_dum(head(input_frame$date, 1) %>% as.character,
                           tail(input_frame$date, 1) %>% as.character),
              by = "date") %>%
    transmute(date,
              is_holiday,
              week_day = date %>% as.Date %>% format("%a") %>% as.factor)
  forecast_frame <- cbind(forecast_frame,
                          data.frame(model.matrix(~ week_day - 1,
                                                  data = forecast_frame))) %>%
    select(-week_day, -week_dayMon)
  
  date_vec <- seq(test_start, tail(input_frame$date, 1), by = "day")
   
  fore_out <- mclapply(date_vec, function(x) {
    
    cat(paste0("\n\nForecasting ", x, "\n\n"))
    
    tmp_input_frame <- input_frame %>%
      filter(date < x)
    tmp_forecast_frame <- forecast_frame %>%
      filter(date == x) %>%
      select(-date) %>%
      as.matrix %>%
      cbind(1, .)
    
    cost <- max(abs(tmp_input_frame$price))
    
    mean_data <- mean(tmp_input_frame$price, na.rm = TRUE)
    demean_data_frame <- tmp_input_frame %>%
      transmute(date,
                price = price - mean(price, na.rm = TRUE))
    
    trend_seas_fit <- long_run_trend_season(demean_data_frame)
    de_lrts_data_frame <- demean_data_frame %>% 
      transmute(date,
                trend = 1:n(),
                ewma = ewma(price),
                price = price - (trend_seas_fit[1] * 
                                   sin(2 * pi * (trend / 365 + trend_seas_fit[2])) -
                                   trend_seas_fit[3] + trend_seas_fit[4] * ewma))
    
    short_seas_fit <- short_run_season(de_lrts_data_frame)
    deseason_data_frame <- data.frame(date = de_lrts_data_frame$date,
                                      price = short_seas_fit$residuals)
    
    if (outlier_filt) {
      deseason_filtered_frame <- outlier_filt(deseason_data_frame, 3)
    } else {
      deseason_filtered_frame <- deseason_data_frame
    }
    
    svm_input <- data.frame(y = deseason_filtered_frame$price,
                            x1 = lag(deseason_filtered_frame$price, 1),
                            x2 = lag(deseason_filtered_frame$price, 2),
                            x3 = lag(deseason_filtered_frame$price, 3),
                            x4 = lag(deseason_filtered_frame$price, 4),
                            x5 = lag(deseason_filtered_frame$price, 5),
                            x6 = lag(deseason_filtered_frame$price, 6),
                            x7 = lag(deseason_filtered_frame$price, 7))
    
    svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                x2 = lag(deseason_filtered_frame$price, 1),
                                x3 = lag(deseason_filtered_frame$price, 2),
                                x4 = lag(deseason_filtered_frame$price, 3),
                                x5 = lag(deseason_filtered_frame$price, 4),
                                x6 = lag(deseason_filtered_frame$price, 5),
                                x7 = lag(deseason_filtered_frame$price, 6)) %>%
      tail(1)
    
    if (forecast_type == "arima") {
      forecast <- arima(deseason_filtered_frame$price,
                              order = c(6, 0, 5),
                              optim.control = list(maxit = 2000)) %>%
        predict(n.ahead = h) %>%
        use_series(pred) %>%
        as.numeric
    } else if (forecast_type == "garch") {
      forecast <- 
        ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(2, 6)),
                   mean.model = list(armaOrder = c(6, 5)),
                   distribution = "norm") %>%
        ugarchfit(spec = .,
                  data = deseason_filtered_frame$price) %>%
        ugarchforecast(fitORspec = .,
                       n.ahead = h) %>%
        fitted %>%
        as.numeric
    } else if (forecast_type == "svm_linear") {
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "linear",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = 0.5490,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_polynomial") {
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "polynomial",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      degree = 2,
                      gamma = 1,
                      coef0 = 1,
                      epsilon = 0.6862,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input)
    } else if (forecast_type == "svm_radial") {
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "radial",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = 0.1372,
                      gamma = 5.0678,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input)
    } else if (forecast_type == "svm_sigmoid") {
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "sigmoid",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = 0.4117,
                      gamma = 0.0203,
                      coef0 = -1.5625,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input)
    }
    
    forecast_out <- data.frame(
      date = x,
      type = forecast_type,
      forecast = (forecast +
                    tmp_forecast_frame %*% (short_seas_fit$coef %>% as.matrix) +
                    trend_seas_fit[1] * sin(2 * pi *
                                              ((tail(de_lrts_data_frame$trend, 1) + h) / 365 + trend_seas_fit[2])) -
                    trend_seas_fit[3] + trend_seas_fit[4] * tail(de_lrts_data_frame$ewma, 1) +
                    mean_data) / (1 - trend_seas_fit[4] * (1 - 0.975)))
    cat("\n\nForecast: \n\n")
    print(forecast_out)
    if (length(save_path) > 0 ) write.csv(forecast_out, file = paste0(save_path, "/", forecast_type, "_", x, ".csv"))
    return(forecast_out)
  },
  mc.cores = getOption("mc.cores", 8L)
  ) %>%
    rbind_all %>%
    left_join(input_frame %>% filter(date >= test_start), ., by = "date")
}

#' @export
select_binary_eco <- function(input_frame, method = "logit") {
  calc_bic <- function(model, lag) {
    data.frame(lag = lag - 1,
               aic = -2 * logLik(model) + 2 * length(model$coefficients),
               bic = -2 * logLik(model) + length(model$coefficients) *
                 log(length(model$fitted.values)),
               hic = -2 * logLik(model) + 2 * length(model$coefficients) *
                 log(log(length(model$fitted.values))))     
  }
  
  train_frame <- data.frame(y = input_frame$direction,
                            x1 = lag(input_frame$spread, 1),
                            x2 = lag(input_frame$spread, 2),
                            x3 = lag(input_frame$spread, 3),
                            x4 = lag(input_frame$spread, 4),
                            x5 = lag(input_frame$spread, 5),
                            x6 = lag(input_frame$spread, 6),
                            x7 = lag(input_frame$spread, 7)) %>%
    na.omit
  
  lapply(2:8, function(i) {
    glm(y ~ .,
        data = train_frame[, 1:i],
        family = binomial(method)) %>%
      calc_bic(lag = i)
  }) %>% rbind_all
}

#' Function to cross validate binary model
#'
#' @export
select_svm_bin <- function(input_frame, kernel = "linear", min.lag = 7,
                           cost = 10^seq(-2, 1, 0.1), max.polynomial = 4,
                           gamma = 10^seq(-6, 1, 0.5), 
                           coef0 = -10^seq(-1, 1, 0.1),
                           cachesize = 500, cores = 8L) {
  
  lags <- 1:min.lag + 1
  
  train_frame <- data.frame(y = input_frame$direction,
                            x1 = lag(input_frame$spread, 1),
                            x2 = lag(input_frame$spread, 2),
                            x3 = lag(input_frame$spread, 3),
                            x4 = lag(input_frame$spread, 4),
                            x5 = lag(input_frame$spread, 5),
                            x6 = lag(input_frame$spread, 6),
                            x7 = lag(input_frame$spread, 7)) %>%
    na.omit
  
  if (kernel == "linear") {
    svm_out <- mclapply(lags, function(t) {
      mclapply(cost, function(c) {
        cat(paste0("Calculating svm classification with linear kernel, ",
                   t - 1, " lags, ", c, " and cost."))
        
        out <- try(svm(y ~ .,
                       data = train_frame[, 1:t],
                       kernel = kernel,
                       scale = TRUE,
                       type = "C-classification",
                       cost = c,
                       cachesize = cachesize)) %>% 
          {
            if (inherits(., "try-error") %>% `!`) {
              data.frame(class_error = fitted(.) %>%
                           as.numeric %>%
                           subtract(train_frame[, 1] %>% as.numeric, .) %>%
                           abs %>%
                           mean,
                         support_vectors = use_series(., SV) %>%
                           length)
              } else {
                data.frame(clas_error = NA,
                           support_vectors = NA)
              }
            } %>%
          transmute(kernel = kernel,
                    lags = t - 1,
                    cost = c,
                    class_error,
                    support_vectors)
        
        cat(" ... Done\n")
        return(out)
      }, mc.cores = getOption("mc.cores", cores)
      ) %>% rbind_all
    }, mc.cores = getOption("mc.cores", cores)
    ) %>% rbind_all
  } else if (kernel == "polynomial") {
    gamma <- 1
    coef0 <- 1
    poly <- 2:max.polynomial
    
    svm_out <- mclapply(lags, function(t) {
      mclapply(poly, function(d) {
        lapply(cost, function(c) {
          cat(paste0("Calculating svm classification with polynomial kernel ",
                     "of degree, ", d, " with ", t - 1, " lags, and", c,
                     " cost."))
          out <- try(svm(y ~ .,
                         data = train_frame[, 1:t],
                         kernel = kernel,
                         scale = TRUE,
                         type = "C-classification",
                         cost = c,
                         gamma = gamma,
                         coef0 = coef0,
                         degree = d,
                         cachesize = cachesize)) %>%
            {
              if (inherits(., "try-error") %>% `!`) {
                data.frame(class_error = fitted(.) %>%
                             as.numeric %>%
                             subtract(train_frame[, 1] %>% as.numeric, .) %>%
                             abs %>%
                             mean,
                           support_vectors = use_series(., SV) %>%
                             length)
                } else {
                  data.frame(class_error = NA,
                             support_vectors = NA)
                }
              } %>%
            transmute(kernel = kernel,
                      lags = t - 1,
                      cost = c,
                      degree = d,
                      class_error,
                      support_vectors)          
          cat(" ... Done\n")
          return(out)
        }
        ) %>% rbind_all
      }, mc.cores = getOption("mc.cores", cores)
      ) %>%rbind_all
    }, mc.cores = getOption("mc.cores", cores)
    ) %>% rbind_all
  } else if (kernel == "radial") {    
    svm_out <- mclapply(lags, function(t) {
      lapply(gamma,function(g) {
        lapply(cost, function(c) {
          cat(paste0("Calculating svm classification with radial basis kernel ",
                     "with ", t - 1, " lags, ", g, " gamma, and", c, " cost."))
          out <- try(svm(y ~ .,
                         data = train_frame[, 1:t],
                         kernel = kernel,
                         scale = TRUE,
                         type = "C-classification",
                         cost = c,
                         gamma = g,
                         cachesize = cachesize)) %>%
            {
              if (inherits(., "try-error") %>% `!`) {
                data.frame(class_error = fitted(.) %>%
                             as.numeric %>%
                             subtract(train_frame[, 1] %>% as.numeric, .) %>%
                             abs %>%
                             mean,
                           support_vectors = use_series(., SV) %>%
                             length)
                } else {
                  data.frame(class_error = NA,
                             support_vectors = NA)
                }
              } %>%
            transmute(kernel = kernel,
                      lags = t - 1,
                      cost = c,
                      gamma = g,
                      class_error,
                      support_vectors)
          cat(" ... Done\n")
          return(out)
        }
        ) %>% rbind_all
      }
      ) %>% rbind_all
    }, mc.cores = getOption("mc.cores", cores)
    ) %>% rbind_all
  } else if (kernel == "sigmoid") {
    
    svm_out <- mclapply(lags, function(t) {
      mclapply(coef0, function(s) {
        lapply(gamma, function(g) {
          lapply(cost, function(c) {
            cat(paste0("Calculating svm regression with sigmoid kernel with ",
                       t - 1, " lags, ", s, " coef0, ", g, " gamma, and ", c,
                       " cost."))
            out <- try(svm(y ~ .,
                           data = train_frame[, 1:t],
                           kernel = kernel,
                           scale = TRUE,
                           type = "C-classification",
                           cost = c,
                           gamma = g,
                           coef0 = s,
                           cachesize = cachesize)) %>%
              {
                if (inherits(., "try-error") %>% `!`) {
                  data.frame(class_error = fitted(.) %>%
                               as.numeric %>%
                               subtract(train_frame[, 1] %>% as.numeric, .) %>%
                               abs %>%
                               mean,
                             support_vectors = use_series(., SV) %>%
                               length)
                  } else {
                    data.frame(class_error = NA,
                               support_vectors = NA)
                  }
                } %>%
              transmute(kernel = kernel,
                        lags = t - 1,
                        cost = c,
                        gamma = g,
                        coef0 = s, 
                        class_error,
                        support_vectors)
            cat(" ... Done\n")
            return(out)
          }
          ) %>% rbind_all
        }
        ) %>% rbind_all
      }, mc.cores = getOption("mc.cores", cores)
      ) %>% rbind_all
    }, mc.cores = getOption("mc.cores", cores)
    ) %>% rbind_all
  }
}

#' @export
oos_classification <- function(input_frame, test_start = "2012-11-01",
                               cachesize = 500, cores = 8L, save_path = NULL) {
  
  test_start %<>% as.Date
  
  data_frame <- input_frame %>%
    group_by(date) %>%
    summarise(spread = mean(spread, na.rm = TRUE)) %>%
    ungroup
  
  date_vec <- seq(test_start, tail(data_frame$date, 1), by = "day")
  
  fore_out <- mclapply(date_vec, function(x) {
    cat(paste0("Forecasting ", x, "\n"))
    
    tmp_input_frame <- data_frame %>%
      filter(date < x) %>%
      transmute(date,
                price = spread) %>%
      outlier_filt(3) %>%
      transmute(date,
                direction = ifelse(price > 0, 1, 0) %>% as.factor(),
                spread = price)
    
    train_frame <- data.frame(y = tmp_input_frame$direction,
                              x1 = lag(tmp_input_frame$spread, 1),
                              x2 = lag(tmp_input_frame$spread, 2),
                              x3 = lag(tmp_input_frame$spread, 3),
                              x4 = lag(tmp_input_frame$spread, 4),
                              x5 = lag(tmp_input_frame$spread, 5),
                              x6 = lag(tmp_input_frame$spread, 6),
                              x7 = lag(tmp_input_frame$spread, 7)) %>%
      na.omit
    
    test_frame <- data.frame(x1 = tmp_input_frame$spread,
                             x2 = lag(tmp_input_frame$spread, 1),
                             x3 = lag(tmp_input_frame$spread, 2),
                             x4 = lag(tmp_input_frame$spread, 3),
                             x5 = lag(tmp_input_frame$spread, 4),
                             x6 = lag(tmp_input_frame$spread, 5),
                             x7 = lag(tmp_input_frame$spread, 6)) %>%
      tail(1)
  
    probit_forecast <- glm(y ~ .,
                           data = train_frame[, 1:2, drop = FALSE],
                           family = binomial("probit")) %>%
      predict(newdata = test_frame[, 1, drop = FALSE],
              type = "response") %>%
      as.numeric
    
    logit_forecast <- glm(y ~ .,
                           data = train_frame[, 1:2, drop = FALSE],
                           family = binomial("logit")) %>%
      predict(newdata = test_frame[, 1, drop = FALSE],
              type = "response") %>%
      as.numeric
    
    svm_linear_forecast <- svm(y ~ .,
                               data = train_frame[, 1:5],
                               kernel = "linear",
                               scale = TRUE,
                               type = "C-classification",
                               cost = 0.4,
                               cachesize = cachesize) %>%
      predict(newdata = test_frame[, 1:4, drop = FALSE]) %>%
      as.character %>%
      as.numeric
    
    svm_polynomial_forecast <- svm(y ~ .,
                                   data = train_frame[, 1:8],
                                   kernel = "polynomial",
                                   scale = TRUE,
                                   type = "C-classification",
                                   cost = 0.8,
                                   degree = 3,
                                   gamma = 1,
                                   coef0 = 1,
                                   cachesize = cachesize) %>%
      predict(newdata = test_frame[, 1:7, drop = FALSE]) %>%
      as.character %>%
      as.numeric
    
    svm_radial_forecast <- svm(y ~ .,
                               data = train_frame[, 1:8],
                               kernel = "sigmoid",
                               scale = TRUE,
                               type = "C-classification",
                               cost = 1,
                               gamma = 0.1,
                               cachesize = cachesize) %>%
      predict(newdata = test_frame[, 1:7, drop = FALSE]) %>%
      as.character %>%
      as.numeric
    
    svm_sigmoid_forecast <- svm(y ~ .,
                                   data = train_frame[, 1:8],
                                   kernel = "sigmoid",
                                   scale = TRUE,
                                   type = "C-classification",
                                   cost = 7.2560,
                                   gamma = 0.3162,
                                   coef0 = -5.0119,
                                   cachesize = cachesize) %>%
      predict(newdata = test_frame[, 1:7, drop = FALSE]) %>%
      as.character %>%
      as.numeric
    
    forecast <- data.frame(date = x,
                           probit = probit_forecast,
                           logit = logit_forecast,
                           svm_linear = svm_linear_forecast,
                           svm_polynomial = svm_polynomial_forecast,
                           svm_radial = svm_radial_forecast,
                           svm_sigmoid = svm_sigmoid_forecast)
    
    if (length(save_path) > 0 ) write.csv(forecast, file = paste0(save_path, "/classification_", x, ".csv"))
    return(forecast)
  }, mc.cores = getOption("mc.cores", cores)) %>%
    rbind_all %>%
    left_join(data_frame %>% filter(date >= test_start), ., by = "date") %>%
    mutate(spread,
           direction = ifelse(spread > 0, 1, 0),
           probit = ifelse(probit > 0.5, 1, 0),
           logit = ifelse(logit > 0.5, 1, 0))
  
  return(fore_out)
}