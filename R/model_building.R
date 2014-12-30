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

select_svm <- function(input_frame, max.cost = 100,
                       max.epsilon = 10) {
  
  train_frame <- data.frame(y = input_frame$price,
                            x1 = lag(input_frame$price, 1),
                            x2 = lag(input_frame$price, 2),
                            x3 = lag(input_frame$price, 3),
                            x4 = lag(input_frame$price, 4),
                            x5 = lag(input_frame$price, 5),
                            x6 = lag(input_frame$price, 6),
                            x7 = lag(input_frame$price, 7))
  
  kernel <- c("linear", "polynomial", "radial basis", "sigmoid")
  
  svm_out <- train_frame %>%
    lapply(1:length(kernel), function(i) {
      lapply(2:ncol(train_frame), function(j) {
        lapply(1:max.cost, function(h) {
          lapply(seq(0.1, max.epsilon, 0.1) function(k) {
            svm(y ~ .,
                data = train_frame[, 2:j],
                kernel = kernel[i],
                scale = TRUE,
                type = "eps-regression",
                cost = h,
                epsilon = k) %>%
              predict.svm(newdata = train_frame[, 2:j]) %>%
              summarise(mse = mean((train_frame[, 1] - XXX) ** 2)) %>%>
              data.frame(kernel = kernel[i],
                         lags = j - 1,
                         cost = h,
                         epsilon = k,
                         mse = .)
          }) %>% rbind_all
        }) %>% rbind_all
      }) %>% rbind_all
    }) %>% rbind_all
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