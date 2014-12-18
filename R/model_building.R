#' Function doing base part of study
#' 
#' @param input_data A data frame
#' @param p An integer, number of max AR terms
#' @param q An integer, number of max MA terms
#' 
#' @export
select_arima <- function(input_frame, p = 7, q = 7) {  
  
  calc_bic <- function(model, p, q) {
      data.frame(p = p,
                 q = q,
                 aic = -2 * model$loglik + 2 * length(model$coef),
                 bic = -2 * model$loglik + length(model$coef) *
                   log(length(model$residuals)),
                 hic = -2 * model$loglik + 2 * length(model$coef) *
                   log(log(length(model$residuals))))     
  }
  
  out <- lapply(0:p, function(x) {
    lapply(0:q, function(y) {
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

#' Function doing oos forecast using a given model type over a given horison
#' 
oos_forecast <- function(input_frame, test_start = "2013-11-01", p = 6, q = 6,
                         h = 1) {
  
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
  
  fore_out <- lapply(date_vec, function(x) {
    
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
    
    arima_forecast <- predict(arima(deseason_filtered_frame$price,
                                    order = c(p, 0, q)),
                              n.ahead = h) %>%
      use_series(pred) %>%
      as.numeric
    
    forecast <- data.frame(
      date = x,
      forecast = arima_forecast +
        tmp_forecast_frame %*% (short_seas_fit$coef %>% as.matrix) +
        trend_seas_fit[1] * sin(2 * pi *
        ((tail(de_lrts_data_frame$trend, 1) + h) / 365 + trend_seas_fit[2])) -
        trend_seas_fit[3] + trend_seas_fit[4] * tail(de_lrts_data_frame$ewma, 1) +
        mean_data) / (1 - trend_seas_fit[4] * (1 - 0.975))
  }
  ) %>%
    rbind_all %>%
    left_join(input_frame %>% filter(date <= test_start), ., by = "date")
  
  
  
}