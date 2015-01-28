set_data_time <- function () {
  if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_TIME", "English")
  } else if (.Platform$OS.type == "unix") {
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
  }
}

# library(svmfortopic)
# library(magrittr)
# library(dplyr)
# library(e1071)
# Sys.setlocale("LC_TIME", "English")
# data_spot <- readRDS("C:/git/r/svmfortopic/inst/rds/data_spot.rds")
# train_data <- data_spot %>% filter(date <= "2012-10-31")
# filtered_frame <- pre_model_processing_spot(train_data)
# linear_svm <- select_svm(filtered_frame)