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