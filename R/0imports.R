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

# Imports from broom

#' @importFrom broom tidy
tidy <- broom::tidy