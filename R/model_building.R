#' Function doing base part of study
#' 
#' @param input_data A data frame
#' 
#' @export
select_arima <- function(input_frame, p = 7, q = 7) {  
  
  calc_bic <- function(model) {
    -2 * log(model$loglik) + length(model$coef) * 
      (log(length(model$residuals) - log(2 * pi)))
  }
  
  out <- lapply(1:3, function(x) {
    lapply(1:3, function(y) {
      arima(input_frame$price, order = c(1, 0, 1)) %>%
        calc_bic() %>%
        data.frame(p = x,
                   q = y,
                   BIC = .)
      }
      ) %>% rbind_all
  }
  ) %>% rbind_all
    
}