#' Comput a number of statistical loss functions for forecasts
#' 
#' @export
#' 
loss_functions <- function(input_frame, table_path = NULL) {
  out <- input_frame %>%
    transmute(date = date %>% as.Date,
              price = price %>% as.character %>% as.numeric,
              Type = type %>% as.character,
              forecast = forecast %>% as.character %>% as.numeric) %>%
    group_by(Type) %>%
    summarise(MSE = mean((forecast - price) ^ 2),
              MAE = mean(abs(forecast - price)),
              MPE = mean((forecast - price) / price) ,
              MAPE = mean(abs((forecast - price) / price)) * 100,
              sMAPE = mean(abs((forecast - price) /
                                 (price + forecast))) * 100) %>%
    ungroup %>%
    mutate(Type = ifelse(grepl("_", Type), paste0("$\\text{", gsub("_", " ", Type), "}$"), paste0("$\\text{", Type, "}$")),
           MSE = paste0("$", MSE %>% round(2), "$"),
           MAE = paste0("$", MAE %>% round(2), "$"),
           MPE = paste0("$", MPE %>% round(4), "\\%$"),
           MAPE = paste0("$", MAPE %>% round(2), "\\%$"),
           sMAPE = paste0("$", sMAPE %>% round(2), "\\%$"))
  
  if (!is.null(table_path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(table_path, "/regress_loss.tex"),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
plot_class_measures <- function(input_frame, path_table = NULL) {
  out <- input_frame %>%
    tidyr::gather(type, forecast, probit:svm_sigmoid) %>%
    group_by(type) %>%
    summarise(TP = sum(direction == forecast & direction == 1) %>% as.numeric,
              TN = sum(direction == forecast & direction == 0) %>% as.numeric,
              FP = sum(direction != forecast & forecast == 1) %>% as.numeric,
              FN = sum(direction != forecast & forecast == 0) %>% as.numeric) %>%
    ungroup %>%
    transmute(Type = type,
              Accuracy = (TP + TN) / (TP + TN + FN + FP),
              TPR = TP / (TP + FN),
              TNR = TN / (FP + TN),
              PPV = TP / (TP + FP),
              NPV = TN / (TN + FN),
              MCC = ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))) %>%
    mutate(Type = ifelse(grepl("_", Type), paste0("$\\text{", gsub("_", " ", Type), "}$"), paste0("$\\text{", Type, "}$")),
           Accuracy = paste0("$", Accuracy %>% multiply_by(100) %>% round(2), "\\%$"),
           TPR = paste0("$", TPR %>% multiply_by(100) %>% round(2), "\\%$"),
           TNR = paste0("$", TNR %>% multiply_by(100) %>% round(2), "\\%$"),
           PPV = paste0("$", PPV %>% multiply_by(100) %>% round(2), "\\%$"),
           NPV = paste0("$", NPV %>% multiply_by(100) %>% round(2), "\\%$"),
           MCC = paste0("$", MCC %>% round(4), "$"))
  
  if (!is.null(path_table)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path_table, "/class_measures.tex"),
            floating = FALSE,
            include.rownames = FALSE,
            sanitize.text.function = function(x){x})
  }
}

#' @export
arima_desc <- function(input_frame, p = 6, q = 5, path_figure = NULL,
                       path_table = NULL) {
  
  residuals_arima <- arima(input_frame$price,
                           order = c(p, 0, q),
                           optim.control = list(maxit = 2000)) %>%
    use_series(residuals) %>%
    as.numeric %>%
    data.frame(date = input_frame$date,
               price = .)
  
  arma_model <- arima(input_frame$price,
                      order = c(p, 0, q),
                      optim.control = list(maxit = 2000)) %>%
    tidy %>%
    {
      rbind(tail(., 1), head(., -1))
    } %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = (Estimate / Std.Error) %>% round(4),
              p.value = 2 * pt(abs(t.statistic),
                               nrow(input_frame) - n() - 1,
                               lower = FALSE) %>% round(4)) %>%
    mutate(Parameter = ifelse(Parameter == "intercept", "Intercept",
                              ifelse(grepl("ar", Parameter), paste0(gsub("ar", "AR(", Parameter), ")"),
                                     ifelse(grepl("ma", Parameter), paste0(gsub("ma", "MA(", Parameter), ")"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, "**",
                                     ifelse(0.05 <= p.value & p.value < 0.1, "***", paste0(Parameter, "")))))
  
  if (!is.null(path_table)) {
    xtable(arma_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path_table, "/arma_model_de.tex"),
            floating = FALSE,
            include.rownames = FALSE)
  }
  
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(residuals_arima,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Deseasonalised data",
                   file_name = "6_arima(6,0,6)_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  if (path_figure %>% is.null %>% `!`) {
    draw_acf(residuals_arima,
             lags = 72,
             file_name = "6_arima(6,0,6)_acf_pacf.eps",
             save_path = path_figure,
             do_print = TRUE)
  }  
  
  if (path_figure %>% is.null %>% `!`) {
    draw_periodogram(residuals_arima,
                     log = FALSE,
                     file_name = "6_arima(6,0,6)_periodogram.eps",
                     save_path = path_figure,
                     do_print = TRUE)
  }
}

#' @export
garch_desc <- function(input_frame, p = 1, q = 1, path_figure = NULL,
                       path_table = NULL) {
  
  arima_start_val <- arima(input_frame$price,
                           order = c(6, 0, 5),
                           optim.control = list(maxit = 2000)) %>%
    coef %>%
    as.list
  
  names(arima_start_val) <- c("ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ma1",
                              "ma2", "ma3", "ma4", "ma5", "mu")
  
  model <- ugarchspec(
    variance.model = list(model = "sGARCH",
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(6, 5)),
    start.pars = arima_start_val,
    distribution = "norm")
  
  garch_residuals <- ugarchfit(spec = model,
                               data = input_frame$price) %>%
    residuals %>%
    as.numeric %>%
    data.frame(date = input_frame$date,
               price = .)
  
  garch_model <- ugarchfit(spec = model,
                           data = input_frame$price) %>%
    `@`(fit) %>%
    `$`(matcoef) %>%
    as.data.frame %>%
    add_rownames("Parameter") %>%
    filter(!grepl("mu", Parameter),
           !grepl("ar", Parameter),
           !grepl("ma", Parameter)) %>%
    rename(Estimate = ` Estimate`,
           Std.Error = ` Std. Error`,
           t.value = ` t value`,
           p.value = `Pr(>|t|)`) %>%
    mutate(Parameter = ifelse(Parameter == "omega", "Intercept",
                              ifelse(grepl("alpha", Parameter), paste0("$\\", gsub("alpha", "alpha_", Parameter), "$"),
                                     ifelse(grepl("beta", Parameter), paste0("$\\", gsub("beta", "beta_", Parameter), "$"), NA)))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1, paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  if (!is.null(path_table)) {
    xtable(garch_model,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(path_table, "/garch_model_de.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
  
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(garch_residuals,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Deseasonalised data",
                   file_name = "/6_garch_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  if (path_figure %>% is.null %>% `!`) {
    draw_acf(garch_residuals,
             lags = 72,
             file_name = "6_garch_acf_pacf.eps",
             save_path = path_figure,
             do_print = TRUE)
  }  
  
  if (path_figure %>% is.null %>% `!`) {
    draw_periodogram(garch_residuals,
                     log = FALSE,
                     file_name = "6_garch_periodogram.eps",
                     save_path = path_figure,
                     do_print = TRUE)
  }
}

#' Binary output
#' 
#' @export
#' 
bin_out <- function(input_frame, method = "logit", save_path = NULL) {
  out <- data.frame(y = input_frame$direction,
                    x1 = lag(input_frame$spread, 1)) %>%
    na.omit %>%
    glm(y ~ .,
        data = .,
        family = binomial(method))%>%
    tidy %>%
    transmute(Parameter = term,
              Estimate = estimate %>% round(4),
              Std.Error = std.error %>% round(4),
              t.statistic = statistic %>% round(4),
              p.value) %>%
    mutate(Parameter = ifelse(Parameter == "(Intercept)", "Intercept",
                              ifelse(grepl("x", Parameter), paste0("$\\", gsub("x", "beta_", Parameter), "$"), NA))) %>%
    mutate(Parameter = ifelse(p.value < 0.01, paste0(Parameter, "*"),
                              ifelse(0.01 <= p.value & p.value < 0.05, paste0(Parameter, "**"),
                                     ifelse(0.05 <= p.value & p.value < 0.1,  paste0(Parameter, "***"), paste0(Parameter, "")))))
  
  if (!is.null(save_path)) {
    xtable(out,
           digits = 4) %>%
      print(type = "latex",
            file = paste0(save_path, "/", method, ".tex"),
            floating = FALSE,
            sanitize.text.function=function(x){x},
            include.rownames = FALSE)
  }
  
}
