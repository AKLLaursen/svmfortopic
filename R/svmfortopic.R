#'@name svmfortopic
#'@title Package providing all data and programs used in the topic XXX by
#'Andreas Keller Leth Laursen
#'@docType package
#'@description Provides all data used in the project topic by Andreas Keller
#'Leth Laursen from public sources. Further all programs are included as well as
#'tables and graphs.
#'@details
#'\tabular{ll}{
#'  Package: \tab svmfortopic \cr
#'  Type:    \tab Package     \cr
#'  Version: \tab 1.0         \cr
#'  Date:    \tab 2014-10-24  \cr
#'  License: \tab GPL         \cr
#'  }
#'@author Andreas Keller Leth Laursen
#'Maintainer Andreas Keller Leth Laursen <andreas.keller[at]gmail.com>
NULL

#'@importFrom magrittr %>%
`%>%` <- magrittr::`%>%`

#'Function scraping various spotprices and volumes from the Epexspot website
#'
#'@importFrom rvest html
#'@importFrom rvest html_nodes
#'@importFrom rvest html_text
#'@importFrom dplyr rbind_all
#'@importFrom dplyr group_by
#'@importFrom dplyr ungroup
#'@importFrom dplyr summarise
#'@importFrom magrittr add
#'@param from_date A string with the start date of the desired series
#'@param to_date A string with the end date of the desired series
#'@param country A string with the desired country iso id. Takes the values DE, 
#'FR and CH. Default is DE.
#'@param market A string indicating market type. Takes the values Spot and
#'Intraday. Deafult is Spot.
#'@param contract A string indicating contract type for intraday. Take the
#'values H and Q. Default is H.
#'@return a dataframe containing the dates, hours, spot and volumes
#'@export
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
        rvest::html(paste0("http://www.epexspot.com/en/market-data/auction/auc",
                           "tion-table/",
                           date_scr[ii],
                           "/",
                           country))
      
      data_scr <- epex_sp %>%
        rvest::html_nodes("#tab_de td:nth-child(9)") %>%
        rvest::html_text() %>%
        gsub(",", "", .) %>% 
        as.numeric
      
      data_out[[ii]] <- data.frame(
        date = date_scr[ii] %>% as.Date %>% magrittr::add(6),
        hour = if (length(data_scr) > 48) c(1:3, 3:24) else 1:24,
        spot = data_scr[c(TRUE, FALSE)],
        volume = data_scr[c(FALSE, TRUE)],
        created = time_stamp) %>%
        dplyr::group_by(date, hour) %>%
        dplyr::summarise(spot = mean(spot, na.rm = TRUE),
                         volume = mean(volume, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      cat(paste0(as.character(as.Date(date_scr[ii]) + 6), " ...\n"))
      
    } else if (market == "Intraday") {
      
      epex_sp <-
        rvest::html(paste0("http://www.epexspot.com/en/market-data/intraday/in",
                           "traday-table/",
                           date_scr[ii],
                           "/",
                           country))
      
      data_scr <- epex_sp %>%
        rvest::html_nodes("td:nth-child(6)") %>%
        rvest::html_text() %>%
        gsub(",", "", .) %>% 
        as.numeric %>%
        head(-1)
      
      if (contract == "H") {
        
        data_scr <- data_scr[seq(1, length(data_scr), 5)]
        
        data_out[[ii]] <- data.frame(
          date = date_scr[ii] %>% as.Date,
          hour = if (length(data_scr) > 24) c(1:3, 3:24) else 1:24,
          vwap = data_scr,
          created = time_stamp) %>%
          dplyr::group_by(date, hour) %>%
          dplyr::summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
          dplyr::ungroup()
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else if (contract == "Q") {
        
        data_scr <- data_scr[-seq(1, length(data_scr), 5)]
        
        data_out[[ii]] <- data.frame(
          date = date_scr[ii] %>% as.Date,
          quarter = if (length(data_scr) > 96) c(1:12, 9:12, 13:96) else 1:96,
          vwap = data_scr,
          created = time_stamp) %>%
          dplyr::group_by(date, quarter) %>%
          dplyr::summarise(vwap = mean(vwap, na.rm = TRUE)) %>%
          dplyr::ungroup()
        
        cat(paste0(date_scr[ii], " ...\n"))
        
      } else {
        stop("Contract not recognised")
      }
      
    } else {
      stop("Market not recognised\n")
    }
  }
  data_out <- dplyr::rbind_all(data_out)
  cat("Data downloaded, exiting function\n")
  return(data_out)
}

#'Funtion fixing daylights savings issues by replacing missing hours with mean
#'of neighbouring hours and collecting extra hour to single observation by
#'taking the mean.
#'
#'@importFrom dplyr slice
#'@param input_frame A dataframe containing data to be fixed.
#'@return A dataframe, the original imported dataframe fixed.
daylights_filter <- function(input_data) {
  cat("Correcting daylights savings issues")
  
  # Find position of daylights savings hours
  find_hour <- function(x, season) {
    if (season == "summer") {
      out <- which(x$date %in% c("2010-03-28", "2011-03-27", "2012-03-25",
                                 "2013-03-31", "2014-03-30", "2015-03-29",
                                 "2016-03-27", "2017-03-26", "2018-03-25") &
                     x$hour == 2)
    } else {
      out <- which(x$date %in% c("2010-10-31", "2011-10-30", "2012-10-28",
                                 "2013-10-27", "2014-10-26", "2015-10-25",
                                 "2016-10-30", "2017-10-29", "2018-10-28") &
                     x$hour == 2) + 1
    }
  }
  
  # Replace missing hour 3 with mean of hour 2 and 4.
  insert_rows <- function(x) {
    pos <- find_hour(x, "summer") + 0:(length(find_hour(x, "summer")) - 1)
    for (ii in 1:length(pos)) {
      x <- rbind(head(x, pos[ii]),
                 `[<-` (dplyr::slice(x, pos[ii]), , 2:ncol(x),
                        colMeans(x[pos[ii]:(pos[ii] +1), 2:ncol(x)],
                                 na.rm = TRUE)),
                 tail(x, -pos[ii]))
    }
    return(x)
  }
} 