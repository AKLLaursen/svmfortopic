#' Function scraping various spotprices and volumes from the Epexspot website
#'
#' @param from_date A string with the start date of the desired series
#' @param to_date A string with the end date of the desired series
#' @param country A string with the desired country iso id. Takes the values DE, 
#' FR and CH. Default is DE.
#' @param market A string indicating market type. Takes the values Spot and
#' Intraday. Deafult is Spot.
#' @param contract A string indicating contract type for intraday. Take the
#' values H and Q. Default is H.
#' @param filter_missing A bolean, determining wether or not to replace missing
#' values.
#' @return a dataframe containing the dates, hours, spot and volumes
#' @export
scrape_epex <- function(from_date, to_date, country = "DE", market = "Spot",
                        contract = "H", filter_missing = TRUE)
{
  cat("Initialising scraping routine, getting dates ...\n")
  time_stamp <- Sys.time()
  
  if (market == "Spot") {
    date_scr <- seq(as.Date(from_date) -6, as.Date(to_date) -6, by = 1) %>%
      as.character
  } else if (market == "Intraday") {
    date_scr <- seq(as.Date(from_date), as.Date(to_date), by = 1) %>%
      as.character
  } else {
    stop("Market not recognised")
  }
  
  data_out <- list()
  for (ii in 1:length(date_scr)) {
    if (market == "Spot") {
      
      epex_sp <-
        html(paste0("http://www.epexspot.com/en/market-data/auction/auction-ta",
                    "ble/",
                    date_scr[ii],
                    "/",
                    country))
      
      data_scr <- epex_sp %>%
        html_nodes("#tab_de td:nth-child(9)") %>%
        html_text %>%
        gsub(",", "", .) %>% 
        as.numeric
      
      # Summarise to handle October daylights savings, i.e. two hour 3.
      data_out[[ii]] <- data.frame(
        date = date_scr[ii] %>% as.Date %>% add(6),
        hour = if (length(data_scr) > 48) c(1:3, 3:24) else 1:24,
        spot = data_scr[c(TRUE, FALSE)],
        volume = data_scr[c(FALSE, TRUE)],
        created = time_stamp) %>%
        group_by(date, hour) %>%
        summarise(spot = mean(spot, na.rm = TRUE),
                  volume = mean(volume, na.rm = TRUE)) %>%
        ungroup
      
      cat(paste0(as.character(as.Date(date_scr[ii]) + 6), " ...\n"))
      
    } else if (market == "Intraday") {
      
      epex_sp <-
        html(paste0("http://www.epexspot.com/en/market-data/intraday/intraday-",
                    "table/",
                    date_scr[ii],
                    "/",
                    country))
      
      data_scr <- epex_sp %>%
        html_nodes("td:nth-child(6)") %>%
        html_text %>%
        gsub(",", "", .) %>% 
        as.numeric %>%
        head(-1)
      
      if (contract == "H") {
        
        data_scr <- data_scr[seq(1, length(data_scr), 5)]
        
        # Summarise to handle October daylights savings, i.e. two hour 3.
        data_out[[ii]] <- data.frame(
          date = date_scr[ii] %>% as.Date,
          hour = if (length(data_scr) > 24) c(1:3, 3:24) else 1:24,
          vwap = data_scr,
          created = time_stamp) %>%
          group_by(date, hour) %>%
          summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
          ungroup
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else if (contract == "Q") {
        
        data_scr <- data_scr[-seq(1, length(data_scr), 5)]
        
        # Summarise to handle October daylights savings, i.e. two hour 3.
        data_out[[ii]] <- data.frame(
          date = date_scr[ii] %>% as.Date,
          quarter = if (length(data_scr) > 96) c(1:12, 9:12, 13:96) else 1:96,
          vwap = data_scr,
          created = time_stamp) %>%
          group_by(date, quarter) %>%
          summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
          ungroup
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else {
        stop("Contract not recognised")
      }
      
    } else {
      stop("Market not recognised\n")
    }
  }
  if (market == "Spot") {
    data_out <- data_out %>%
      rbind_all %>%
      transmute(date,
                hour,
                price = if (filter_missing == TRUE) na_filter(spot) else spot,
                volume = if (filter_missing == TRUE) na_filter(volume) else volume)
  } else if (market == "Intraday") {
    data_out <- data_out %>%
      rbind_all %>%
      transmute(date,
                hour,
                price = if (filter_missing == TRUE) na_filter(vwap) else vwap)
  } else {
    stop("Please specify correct market type.")
  }
  
  cat("Data downloaded, exiting function\n")
  return(data_out)
}

#' Function replacing missing values (NA) using linear interpolation between
#' closest finite observations. If start/end point is missing, they are
#' replaced by a mean of the 2 the closets observations possible.
#'
#' @param input_data An atomic vector containing data to be fixed.
#' @return An atomic vector filtered for missing values.
na_filter <- function(input_data) {
  cat("Replacing missing values\n")
  
  .idx <- which(is.na(input_data) == TRUE)
  
  if (length(.idx) > 0) {
    for (ii in 1:length(.idx)) {
      for (jj in 1:(length(input_data) - .idx[ii])) {
        if (!is.na(input_data[.idx[ii] + jj]) == TRUE) {
          .bp <- jj
          break
        }
      }
      if (length(input_data[.idx[ii] - 1]) == 0) {
        input_data[.idx[ii]] <- mean(input_data[(.idx[ii] + .bp):(.idx[ii] +
                                                                    .bp + 1)])
      } else if (.idx[ii] + 1 > length(input_data)) {
        input_data[.idx[ii]] <- mean(input_data[(.idx[ii] - 2):(.idx[ii] - 1)])
        cat(paste0("Replaced ", ii, "\n"))
      } else {
        input_data[.idx[ii]] <- input_data[.idx[ii] - 1] +
          (input_data[.idx[ii] + .bp] - input_data[.idx[ii] - 1]) / (.bp + 1)
        cat(paste0("Replaced ", ii, "\n"))
      }
    }
  }
  cat("... Done\n")
  return(input_data)
}

#' Function getting holiday dummies
#' @param from_date A string with the start date of the desired series
#' @param to_date A string with the end date of the desired series
#' @param country A string with the desired country iso id. Takes the values DE, 
#' FR and CH. Default is DE.
#' @return output_frame A dataframe containing date and seleced countries.
#' @export
#' 
get_holi_dum <- function(from_date, to_date, country = "de") {
  cat("Getting dummies")
  
  csv_file <- system.file("csv/holidays.csv", package = "svmfortopic")
  output_frame <- read.csv(csv_file, sep = ";", stringsAsFactors = FALSE) %>%
    select(date, one_of(country)) %>%
    filter(date >= from_date & date <= to_date) %>%
    mutate(date = as.Date(date)) %>%
    set_names(c("date", "is_holiday"))
  
  cat(" ... Done\n")
  return(output_frame)
}

#' Function calculating the exponentially weighted moving average of the input
#' time series.
#' 
#' @param input_vector An atomic vector containing a time series.
#' @param lambda A double, the lambda parameter for the ewma. Default value is
#' 0.975
#' @return An atomic vector with the transformed data.
#' @export
ewma <- function(input_vector, lambda = 0.975) {
  output_vector <- input_vector
  for (ii in 2:length(input_vector)) {
    output_vector[ii] <- (1 - lambda) * input_vector[ii] +
      lambda * output_vector[ii - 1]
  }
  return(output_vector)
}

#' Function for filtering seasonalities based on XXX
#' 
#' @param input_frame A data frame containing the times series data.
#' @return data_filt Same dataframe with deseasonalised price
#' @export
long_run_trend_season <- function(input_frame) {  
  data_frame <- input_frame %>%
    transmute(price = price - mean(price),
              trend = 1:n(),
              ewma = ewma(price))
  
  # Run linear and non-linear combinations to obtain starting values.
  data_frame_init <- data_frame %>%
    transmute(price,
              trend = sin(2 * pi * (trend / 365 + 1)),
              ewma = ewma(price))
  out_init <- lm(price ~ trend + ewma,
                 data = data_frame_init) %>%
    tidy %>%
    use_series(estimate)    
  
  out_init_1 <- nls(price ~ (out_init[2] * sin(2 * pi * (trend / 365 + beta_2)) +
                               out_init[1] + out_init[3] * ewma),
                    data = data_frame,
                    start = list(
                      beta_2 = 1),
                    trace = TRUE,
                    control = list(
                      maxiter = 5000)) %>%
    tidy %>%
    use_series(estimate)
  
  data_frame_init_1 <- data_frame %>%
    transmute(price,
              trend = sin(2 * pi * (trend / 365 + out_init_1[1])),
              ewma = ewma(price))
  out_init_2 <- lm(price ~ trend + ewma,
                   data = data_frame_init_1) %>%
    tidy %>%
    use_series(estimate)
  
  # Run model
  trend_seas_fit <- nls(price ~ (beta_1 * sin(2 * pi * (trend / 365 + beta_2)) +
                                   beta_3 + beta_4 * ewma),
                        data = data_frame,
                        start = list(
                          beta_1 = out_init_2[2],
                          beta_2 = out_init_1[1],
                          beta_3 = out_init_2[1],
                          beta_4 = out_init_2[3]),
                        trace = TRUE,
                        algorithm = "default", # GN. Same convergence for "port"
                        control = list(
                          maxiter = 100,
                          tol = 1e-06,
                          printEval = TRUE)) %>%
    tidy %>%
    use_series(estimate)  
}

#' Function calculating short run season
#' @param input_frame A data frame with a trend and a price
#' @export
#' 
short_run_season <- function(input_frame) {
  data_frame <- input_frame %>%
    left_join(get_holi_dum(head(input_frame$date, 1),
                           tail(input_frame$date, 1)),
              by = "date") %>%
    transmute(price,
              is_holiday,
              week_day = date %>% as.Date %>% format("%a") %>% as.factor)
  
  data_frame <- cbind(data_frame,
                      data.frame(model.matrix(~ week_day - 1,
                                              data = data_frame))) %>%
    select(-week_day, -week_dayMon)
  
  short_seas_fit <- lm(price ~ .,
                       data = data_frame)
  
  return(short_seas_fit)
}

#' Outlier filter function
#' 
#' @param data_input A data frame containing a price to be outlier filtered.
#' @param std_filt Number of standard deviations to filter on.
#' @export
outlier_filt <- function(input_frame, std_filt = 3)
{
  cat("Filtering Outliers\n")
  cat(paste0("Outliers outside of ", std_filt, " being replaced by NA"))
  data_transform <- input_frame
  data_transform$price[which(data_transform$price >
                               std_filt * sd(data_transform$price) |
                               data_transform$price <
                               - std_filt * sd(data_transform$price))
                       ] <- NA
  cat(" ... Done")
  output_frame <- data_transform %>% 
    mutate(price = na_filter(price))
  
  cat(" ... Outliers filtered")
  return(output_frame)
}

#' Function carrying out the pre-processing of the data described in study data-
#' section. That is saving all plots and returning a filtered data_frame of the
#' entire period.
#' 
#' @param data_frame A data frame containing the spot price
#' @param path_figure A string with path to save figures
#' @param path_table A string with path to save tables
#' 
#' @export
pre_model_processing_spot <- function(input_data, path_figure = NULL,
                                 path_table = NULL) {
  
  # Take the mean across each day to find the base spot price
  data_frame <- input_data %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  # Draw line plot to get feel of data series
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(data_frame,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Base spot price, EUR/MWh",
                   file_name = "5_spot_untreated_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  # Get unfiltered descriptive statistics
  if (path_table %>% is.null %>% `!`) {
    descriptives <- data_frame %>%
      summarise(series = "Eex day ahead (spot)",
                n = n(),
                Max = max(price) %>% round(2),
                Min = min(price) %>% round(2),
                Median = median(price, na.rm = TRUE) %>% round(2),
                Mean = mean(price, na.rm = TRUE) %>% round(2),
                Sd = sd(price, na.rm = TRUE) %>% round(2)) %>%
      stargazer(type = "latex",
                style = "default",
                summary = FALSE,
                out = paste0(path_table, "/5_descriptives_spot.tex"),
                out.header = FALSE)
  }
  
  demean_data_frame <- data_frame %>%
    transmute(date,
              price = price - mean(price, na.rm = TRUE))
  
  # Plot autocorrelation
  if (path_figure %>% is.null %>% `!`) {
    draw_acf(demean_data_frame,
             lags = 72,
             input = "price",
             file_name = "5_spot_untreated_acf_pacf.eps",
             save_path = path_figure,
             do_print = TRUE)
  }
  
  # Plot parellogram
  if (path_figure %>% is.null %>% `!`) {
    draw_periodogram(demean_data_frame,
                     log = FALSE,
                     input = "price",
                     file_name = "5_spot_untreated_periodogram.eps",
                     save_path = path_figure,
                     do_print = TRUE)
  }
  
  # Clearly we see a seasonal pattern at lag 7 and at freqency 0.14 -> period
  # 1/0.14 = 7.14. As such, we deseasonalise.
  
  # We separate the deseason into two parts, a LR trendseasonal term and a SR
  # (weekly) seasonal term. Starting with LR
  trend_seas_fit <- long_run_trend_season(demean_data_frame)
  de_lrts_data_frame <- demean_data_frame %>% 
    transmute(date,
              trend = 1:n(),
              ewma = ewma(price),
              price = price - (trend_seas_fit[1] * 
                                 sin(2 * pi * (trend / 365 + trend_seas_fit[2])) -
                                 trend_seas_fit[3] + trend_seas_fit[4] * ewma),
              trend_seas = (trend_seas_fit[1] * 
                              sin(2 * pi * (trend / 365 + trend_seas_fit[2])) -
                              trend_seas_fit[3] + trend_seas_fit[4] * ewma)) %>%
    select(-trend, -ewma)
  
  # Plot estimated trendseas:
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(de_lrts_data_frame,
                   input = "trend_seas",
                   xlabel = "Year",
                   ylabel = "Long run seasonal trend",
                   file_name = "5_lrts_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  # Plot detrended series
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(de_lrts_data_frame,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Long run seasonal trend",
                   file_name = "5_spot_treated_lrts_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  short_seas_fit <- short_run_season(de_lrts_data_frame)
  deseason_data_frame <- data.frame(date = de_lrts_data_frame$date,
                                    price = short_seas_fit$residuals)
  
  # Plot estimated deseason:
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(deseason_data_frame,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Deseasonalised data",
                   file_name = "5_spot_treated_srs_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  # Finally we remove outliers
  deseason_filtered_frame <- outlier_filt(deseason_data_frame, 3)
  
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(deseason_filtered_frame,
                   input = "price",
                   xlabel = "Year",
                   ylabel = "Deseasonalised data",
                   file_name = "5_spot_treated_filter_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  # Plot autocorrelation
  if (path_figure %>% is.null %>% `!`) {
    draw_acf(deseason_filtered_frame,
             lags = 72,
             input = "price",
             file_name = "5_spot_treated_acf_pacf.eps",
             save_path = path_figure,
             do_print = TRUE)
  }
  
  # Plot parellogram
  if (path_figure %>% is.null %>% `!`) {
    draw_periodogram(deseason_filtered_frame,
                     log = FALSE,
                     input = "price",
                     file_name = "5_spot_treated_periodogram.eps",
                     save_path = path_figure,
                     do_print = TRUE)
  }
  
  return(deseason_filtered_frame)
}

#' @export
pre_model_processing_intraday <- function(input_data, path_figure = NULL,
                                      path_table = NULL) {
  # Take the mean across each day to find the base spread
  data_frame <- input_data %>%
    group_by(date) %>%
    summarise(spread = mean(spread, na.rm = TRUE)) %>%
    ungroup
  
  # Draw line plot to get feel of data series
  if (path_figure %>% is.null %>% `!`) {
    draw_line_plot(data_frame,
                   input = "spread",
                   xlabel = "Year",
                   ylabel = "Base spread price, EUR/MWh",
                   file_name = "5_spread_untreated_line_plot.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  }
  
  # Plot autocorrelation
  if (path_figure %>% is.null %>% `!`) {
    draw_acf(data_frame,
             lags = 72,
             input = "spread",
             file_name = "5_spread_untreated_acf_pacf.eps",
             save_path = path_figure,
             do_print = TRUE)
  }
  
  # Plot parellogram
  if (path_figure %>% is.null %>% `!`) {
    draw_periodogram(data_frame,
                     log = FALSE,
                     input = "spread",
                     file_name = "5_spread_untreated_periodogram.eps",
                     save_path = path_figure,
                     do_print = TRUE)
  }
  
  # Filter outliers
  outlier_filtered_frame <- data_frame %>%
    transmute(date,
              price = spread) %>%
    outlier_filt(3) %>%
    transmute(date,
              spread = price)
  
  # Create data structure
  out <- outlier_filtered_frame %>%
    transmute(date,
              direction = ifelse(spread > 0, 1, 0) %>% as.factor,
              spread)
  
  return(out)
}