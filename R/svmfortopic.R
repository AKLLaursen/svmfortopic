#' @name svmfortopic
#' @title Package providing all data and programs used in the topic XXX by
#' Andreas Keller Leth Laursen
#' @docType package
#' @description Provides all data used in the project topic by Andreas Keller
#' Leth Laursen from public sources. Further all programs are included as well
#' as tables and graphs.
#' @details
#' \tabular{ll}{
#'   Type:    \tab Package     \cr
#'   Version: \tab 1.0         \cr
#'   Date:    \tab 2014-10-24  \cr
#'   License: \tab GPL         \cr
#'   }
#' @author Andreas Keller Leth Laursen
#' Maintainer Andreas Keller Leth Laursen <andreas.keller[at]gmail.com>
NULL

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
#' @return a dataframe containing the dates, hours, spot and volumes
#' @export
scrape_epex <- function(from_date, to_date, country = "DE", market = "Spot",
                        contract = "H")
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
        html(paste0("http://www.epexspot.com/en/market-data/auction/auction-ta,"
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
        html(paste0("http://www.epexspot.com/en/market-data/intraday/intraday-,"
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
      rbind_all%>%
      mutate(spot = na_filter(spot),
             volume = na_filter(volume))
  } else if (market == "Intraday") {
    data_out <- data_out %>%
      rbind_all %>%
      mutate(vwap = na_filter(vwap))
  } else {
    stop("Please specify correct market type.")
  }
  
  cat("Data downloaded, exiting function\n")
  return(data_out)
}

#' Function replacing missing values (NA) using linear interpolation between
#' closest finite observations. If starting points are missing, they are
#' replaced by the closets observation possible.
#'
#' @param input_data An atomic vector containing data to be fixed.
#' @return An atomic vector filtered for missing values.
na_filter <- function(input_data) {
  cat("Replacing missing values\n")
    
  .idx <- which(is.na(input_data))
  
  ii = 1
  for (ii in 1:length(.idx)) {
    for (jj in 1:(length(input_data) - .idx[ii])) {
      if (!is.na(input_data[.idx[ii] + jj]) == TRUE) {
        .bp <- jj
        break
      }
    }
    if (is.null(.idx[ii - 1]) == TRUE) {
      input_data[.idx[ii]] <- input_data[.idx[ii] + .bp]
    } else {
      input_data[.idx[ii]] <- input_data[.idx[ii] - 1] +
        (input_data[.idx[ii] + .bp] - input_data[.idx[ii] - 1]) / (.bp + 1)
      cat(paste0("Replaced ", ii, "\n"))
    }
  }
  cat("... Done\n")
  return(input_data)
}

#' Function getting holiday dummies
#' @param start_date 
#' 
get_holi_dum <- function(start_date, end_date, country) {
  
}

#' Function calculating the exponentially weighted moving average of the input
#' time series.
#' 
#' @param input_vector An atomic vector containing a time series.
#' @param lambda A double, the lambda parameter for the ewma. Default value is
#' 0.975
#' @return An atomic vector with the transformed data.
ewma <- function(input_vector, lambda = 0.975) {
  output_vector <- input_vector
  for (ii in 2:length(input_vector)) {
    output_vector[ii] <- (1 - lambda) * input_vector[ii] +
      lambda * output_vector[ii - 1]
  }
  return(output_vector)
}

#' Function calculating the long run trend seasonalities and short run
#' seasonalities based on Janczura et. al. 2012.
#' 
#' @param beta An atomic vector containing the parameters.
#' @param x A dataframe containing the variables, built as [trend, ones, ewma,
#' dummies].
season_func <- function(beta, x) {
  beta[1] * sin(2 * pi * (x[, 1] / 365 + x[, 2])) + beta[3] * x[, 2] +
    beta[4:nrows(beta)] %*% t(x[, 3:ncol(x)])
}

#' Squared error function
#' 
#' @param beta An atomic vector containing the parameters.
#' @param x A dataframe containing the variables, built as [time_series, trend,
#' ones, ewma, dummies].
error_func <- function(beta, x) {
  sum((x[, 1] - season_func(beta, x[, 2:ncol(x)])) ** 2)
}

#' Function for filtering seasonalities
#' 
#' @param input_frame A data frame containing the times series data.
deseason <- function(input_frame) {
  
}