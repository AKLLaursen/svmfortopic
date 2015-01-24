#' Function performing all calculation and creating all tables for topic report
#' 
#' 
#' @export
#' 
do_unit_root_test <- function(input_frame, save_path = NULL) {
  
  # Stationarity test, augmented Dickey-Fuller, tested for up to 14 lags.
  out <- data.frame(ADF = c(adf.test(input_frame$price) %>%
                              `$`(statistic) %>% as.numeric,
                            trunc((length(input_frame$price) - 1) ^ (1 / 3)) %>%
                              round(0),
                            NA),
                    Phillips_Perron = c(pp.test(input_frame$price) %>%
                                         `$`(statistic) %>% as.numeric,
                                       NA,
                                       trunc(4 * (length(input_frame$price) / 
                                                    100) ^ 0.25) %>%
                                         round(0)),
                    KPSS = c(kpss.test(input_frame$price) %>%
                               `$`(statistic) %>% as.numeric,
                             NA,
                             trunc(3 * sqrt(length(input_frame$price)) /
                                     13) %>%
                               round(0)))
  
  rownames(out) <- c("Statistic", "Lags", "Bandwith")
  colnames(out) <- c("ADF", "Phillips-Perron", "KPSS")
  
  if (!is.null(save_path)) {
    xtable(out) %>%
      print(type = "latex",
            file = paste0(save_path, "/unit_root.tex"),
            floating = FALSE)
  }
  
  return(out)
}