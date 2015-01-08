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

arima_desc <- function(input_frame, p = 6, q = 5, path_figure) {
  residuals_arima <- arima(test_treated_frame$price, order = c(p, 0, q)) %>%
    use_series(residuals) %>%
    data.frame(date = test_treated_frame$date,
               price = . %>% as.numeric) %>%
    select(date, price)
  
  draw_line_plot(residuals_arima,
                 input = "price",
                 xlabel = "Year",
                 ylabel = "Deseasonalised data",
                 file_name = "6_arima(6,0,6)_line_plot.eps",
                 save_path = path_figure,
                 do_print = TRUE)
  
  draw_acf(residuals_arima,
           lags = 72,
           file_name = "6_arima(6,0,6)_acf_pacf.eps",
           save_path = path_figure,
           do_print = TRUE)
  
  
  draw_periodogram(residuals_arima,
                   log = FALSE,
                   file_name = "6_arima(6,0,6)_periodogram.eps",
                   save_path = path_figure,
                   do_print = TRUE)
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
select_svm <- function(input_frame, kernel = "linear", max.cost = NULL,
                       min.cost = NULL, gamma = NULL, coef0 = NULL,
                       max.polynomial = 5, max.epsilon = 1, min.epsilon = 0,
                       cachesize = 8000) {
  
  if (max.cost %>% is.null) {
    max.cost <- (max(abs(input_frame$price)) + sd(input_frame$price)) %>%
      floor
  }
  
  if (min.cost %>% is.null) {
    min.cost <- (max(abs(input_frame$price)) - sd(input_frame$price)) %>%
      floor
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
      lapply(2:ncol(train_frame), function(j) {
        out <- 
          lapply(min.cost:max.cost, function(h) {
            lapply(seq(min.epsilon, max.epsilon, 0.1), function(k) {
              cat(paste0("Calculating svm regression with linear kernel, ", j - 1,
                         " lags, ", h, " cost, and ", k, " epsilon"))
              
              out <- try(svm(y ~ .,
                             data = train_frame[, 1:j],
                             kernel = kernel,
                             scale = TRUE,
                             type = "eps-regression",
                             cost = h,
                             epsilon = k,
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
                          lags = j - 1,
                          cost = h,
                          epsilon = k,
                          mse,
                          support_vectors)
              
              cat(" ... Done\n")
              return(out)
              }) %>% rbind_all
            }) %>% rbind_all
          write.csv(out, paste0("C:/Users/akl/Dropbox/Economics/Final_Thesis/Topic/SVM Outputs/SVM_Linear_lag_",
                                j - 1, ".csv"), sep = ";")
          return(out)
        }) %>% rbind_all
    } else if (kernel == "polynomial") {
      gamma <- 1
      coef0 <- 1
      
      svm_out <- 
        lapply(2:ncol(train_frame), function(j) {
          out <- 
            lapply(2:max.polynomial, function(i) {
              lapply(min.cost:max.cost, function(h) {
                lapply(seq(min.epsilon, max.epsilon, 0.1), function(k) {
                  cat(paste0("Calculating svm regression with polynomial kernel ",
                             "of degree, ", i, " with ", j - 1, " lags, ", h,
                             " cost, and ", k, " epsilon"))
                  
                  out <- try(svm(y ~ .,
                                 data = train_frame[, 1:j],
                                 kernel = kernel,
                                 scale = TRUE,
                                 type = "eps-regression",
                                 cost = h,
                                 epsilon = k,
                                 gamma = gamma,
                                 coef0 = coef0,
                                 degree = i,
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
                              lags = j - 1,
                              cost = h,
                              epsilon = k,
                              degree = i,
                              mse,
                              support_vectors)
                  
                  cat(" ... Done\n")
                  return(out)
                  }) %>% rbind_all
                }) %>% rbind_all
              }) %>% rbind_all
            write.csv(out, paste0("C:/Users/akl/Dropbox/Economics/Final_Thesis/Topic/SVM Outputs/SVM_Polynomial_lag_",
                                  j - 1, ".csv"), sep = ";")
            return(out)
          }) %>% rbind_all
      } else if (kernel == "radial") {
        svm_out <-
          lapply(2:ncol(train_frame), function(j) {
            out <- 
              lapply(seq(0.1, 1, 0.1), function(i) {
                lapply(min.cost:max.cost, function(h) {
                  lapply(seq(min.epsilon, max.epsilon, 0.1), function(k) {
                    cat(paste0("Calculating svm regression with radial basis ker",
                               "nel with ", j - 1, " lags, ", i, " gamma, ", h,
                               " cost, and ", k, " epsilon"))
                    
                    out <- try(svm(y ~ .,
                                   data = train_frame[, 1:j],
                                   kernel = kernel,
                                   scale = TRUE,
                                   type = "eps-regression",
                                   cost = h,
                                   epsilon = k,
                                   gamma = i,
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
                                lags = j - 1,
                                cost = h,
                                epsilon = k,
                                gamma = i,
                                mse,
                                support_vectors)
                    
                    cat(" ... Done\n")
                    return(out)
                    }) %>% rbind_all
                  }) %>% rbind_all
                }) %>% rbind_all
              write.csv(out, paste0("C:/Users/akl/Dropbox/Economics/Final_Thesis/Topic/SVM Outputs/SVM_Radial_lag_",
                                    j - 1, ".csv"), sep = ";")
              return(out)
            }) %>% rbind_all
        } else if (kernel == "sigmoid") {
    svm_out <- 
      lapply(2:ncol(train_frame), function(j) {
        out <- 
          lapply(1:10, function(t) {
            lapply(seq(0.1, 1, 0.1), function(i) {
              lapply(min.cost:max.cost, function(h) {
                lapply(seq(min.epsilon, max.epsilon, 0.1), function(k) {
                  cat(paste0("Calculating svm regression with sigmoid basis ker",
                             "nel with ", j - 1, " lags, ", t, " coef0, ", i,
                             " gamma, ", h, " cost, and ", k, " epsilon"))
                  
                  out <- try(svm(y ~ .,
                                 data = train_frame[, 1:j],
                                 kernel = kernel,
                                 scale = TRUE,
                                 type = "eps-regression",
                                 cost = h,
                                 epsilon = k,
                                 gamma = i,
                                 coef0 = t,
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
                              lags = j - 1,
                              cost = h,
                              epsilon = k,
                              gamma = i,
                              mse,
                              support_vectors)
                  
                  cat(" ... Done\n")
                  return(out)
                  }) %>% rbind_all
                }) %>% rbind_all
              }) %>% rbind_all
            }) %>% rbind_all
          write.csv(out, paste0("C:/Users/akl/Dropbox/Economics/Final_Thesis/Topic/SVM Outputs/SVM_Sigmoid_lag_",
                                j - 1, ".csv"), sep = ";")
          return(out)
        }) %>% rbind_all
      }
}

#' Function doing oos forecast using a given model type over a given horison
#' 
oos_forecast <- function(input_frame, test_start = "2013-11-01", p = 6, q = 5,
                         h = 1) {
  
  test_start %<>% as.Date
  
  input_frame %<>%mutate(date = date %>% as.Date) %>%
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
  
  fore_out <- lapply(date_vec, function(x) {
    
    cat(paste0("Forecasting ", x, "\n"))
    
    tmp_input_frame <- input_frame %>%
      filter(date < x)
    tmp_forecast_frame <- forecast_frame %>%
      filter(date == x) %>%
      select(-date) %>%
      as.matrix %>%
      cbind(1, .)
    
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
    
    deseason_filtered_frame <- outlier_filt(deseason_data_frame, 3)
    
    arima_forecast <- arima(deseason_filtered_frame$price,
                            order = c(p, 0, q),
                            optim.control = list(maxit = 2000)) %>%
      predict(n.ahead = h) %>%
      use_series(pred) %>%
      as.numeric
    
    svm_input <- data.frame(y = deseason_filtered_frame$price,
                            x1 = lag(deseason_filtered_frame$price, 1),
                            x2 = lag(deseason_filtered_frame$price, 2),
                            x3 = lag(deseason_filtered_frame$price, 3),
                            x4 = lag(deseason_filtered_frame$price, 4),
                            x5 = lag(deseason_filtered_frame$price, 5),
                            x6 = lag(deseason_filtered_frame$price, 6))
    
    svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                x2 = lag(deseason_filtered_frame$price, 1),
                                x3 = lag(deseason_filtered_frame$price, 2),
                                x4 = lag(deseason_filtered_frame$price, 3),
                                x5 = lag(deseason_filtered_frame$price, 4),
                                x6 = lag(deseason_filtered_frame$price, 5)) %>%
      tail(1)
    
    svm_forecast  <- svm(y ~ .,
                         data = svm_input,
                         kernel = "linear",
                         scale = TRUE,
                         type = "eps-regression",
                         cost = 1,
                         epsilon = 0.1) %>%
      predict.svm(newdata = svm_for_input)
    
    forecast_arima <- data.frame(
      date = x,
      forecast_arima = (arima_forecast +
                          tmp_forecast_frame %*% (short_seas_fit$coef %>% as.matrix) +
                          trend_seas_fit[1] * sin(2 * pi *
                                                    ((tail(de_lrts_data_frame$trend, 1) + h) / 365 + trend_seas_fit[2])) -
                          trend_seas_fit[3] + trend_seas_fit[4] * tail(de_lrts_data_frame$ewma, 1) +
                          mean_data) / (1 - trend_seas_fit[4] * (1 - 0.975)),
      forecast_svm = (svm_forecast +
                        tmp_forecast_frame %*% (short_seas_fit$coef %>% as.matrix) +
                        trend_seas_fit[1] * sin(2 * pi *
                                                  ((tail(de_lrts_data_frame$trend, 1) + h) / 365 + trend_seas_fit[2])) -
                        trend_seas_fit[3] + trend_seas_fit[4] * tail(de_lrts_data_frame$ewma, 1) +
                        mean_data) / (1 - trend_seas_fit[4] * (1 - 0.975)))
  }
  ) %>%
    rbind_all %>%
    left_join(input_frame %>% filter(date <= test_start), ., by = "date")
  
}