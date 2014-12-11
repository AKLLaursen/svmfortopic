#' Function performing all calculation and creating all tables for topic report
#' 
do_study <- function(spot_data) {
  
  # Filter spot data
  spot_data_filtered <- spot_data %>%
    deseason_price() %>%
    outlier_filt()
  
  # Stationarity test, augmented Dickey-Fuller
  DF <- vector()
  for (ii in 1:48) {
    DF[ii] <- tseries::adf.test(spot_data_filtered$price, k = ii) %>%
      use_series(p.value)
  }
  
  # ARCH test
  
  
  # Forecasting 24 hours in one model
  
  # Forecasting each of the 24 hours in 24 models model
  
  # Forecasting the base using the base of all days
  
  # Forecasting the base using 24 previous observations (multicollinearity)
}