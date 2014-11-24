# Imports from magrittr

#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`
#' @importFrom magrittr add
magrittr <- magrittr::add
#' @importFrom magrittr use_series
use_series <- magrittr::use_series
#' @importFrom magrittr extract
extract <- magrittr::extract
#' @importFrom magrittr set_names
set_names <- magrittr::set_names

# Imports from rvest

#' @importFrom rvest html
html <- rvest::html
#' @importFrom rvest html_nodes
html_nodes <- rvest::html_nodes
#' @importFrom rvest html_text
html_text <- rvest::html_text

# Imports from dplyr

#' @importFrom dplyr rbind_all
rbind_all <- dplyr::rbind_all
#' @importFrom dplyr group_by
group_by <- dplyr::group_by
#' @importFrom dplyr ungroup
ungroup <- dplyr::ungroup
#' @importFrom dplyr summarise
summarise <- dplyr::summarise
#' @importFrom dplyr mutate
mutate <- dplyr::mutate
#' @importFrom dplyr n
n <- dplyr::n
#' @importFrom dplyr select
select <- dplyr::select
#' @importFrom dplyr filter
filter <- dplyr::filter
#' @importFrom dplyr transmute
transmute <- dplyr::transmute
#' @importFrom dplyr left_join
left_join <- dplyr::left_join

# Imports from broom

#' @importFrom broom tidy
tidy <- broom::tidy

# Imports from e1071

#' @importFrom e1071 svm
svm <- e1071::svm

# Imports from ggplot2

#' @importFrom ggplot2 ggplot
ggplot <- ggplot2::ggplot
#' @importFrom ggplot2 geom_point
geom_point <- ggplot2::geom_point
#' @importFrom ggplot2 scale_color_manual
scale_color_manual <- ggplot2::scale_color_manual
#' @importFrom ggplot2 geom_abline
geom_abline <- ggplot2::geom_abline
#' @importFrom ggplot2 ggsave
ggsave <- ggplot2::ggsave

# Imports from tseries
#' @importFrom tseries adf.test
adf.test <- tseries::adf.test