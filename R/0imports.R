# Imports from magrittr

#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`
#' @importFrom magrittr %<>%
`%<>%` <- magrittr::`%<>%`
#' @importFrom magrittr add
magrittr <- magrittr::add
#' @importFrom magrittr use_series
use_series <- magrittr::use_series
#' @importFrom magrittr extract
extract <- magrittr::extract
#' @importFrom magrittr set_names
set_names <- magrittr::set_names
#' @importFrom magrittr subtract
subtract <- magrittr::subtract
#' @importFrom magrittr raise_to_power
raise_to_power <- magrittr::raise_to_power

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
#' @importFrom dplyr add_rownames
add_rownames <- dplyr::add_rownames

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
#' @importFrom ggplot2 stat_function
stat_function <- ggplot2::stat_function
#' @importFrom ggplot2 geom_hline
geom_hline <- ggplot2::geom_hline
#' @importFrom ggplot2 geom_vline
geom_vline <- ggplot2::geom_vline
#' @importFrom ggplot2 annotate
annotate <- ggplot2::annotate
#' @importFrom ggplot2 xlab
xlab <- ggplot2::xlab
#' @importFrom ggplot2 ylab
ylab <- ggplot2::ylab
#' @importFrom ggplot2 geom_line
geom_line <- ggplot2::geom_line
#' @importFrom ggplot2 geom_segment
geom_segment <- ggplot2::geom_segment
#' @importFrom ggplot2 aes_string
aes_string <- ggplot2::aes_string
#' @importFrom ggplot2 aes
aes <- ggplot2::aes
#' @importFrom ggplot2 ggplotGrob
ggplotGrob <- ggplot2::ggplotGrob
#' @importFrom ggplot2 ylim
ylim <- ggplot2::ylim

# Imports from gridExtra

#' @importFrom gridExtra arrangeGrob
arrangeGrob <- gridExtra::arrangeGrob

# Imports from tseries

#' @importFrom tseries adf.test
adf.test <- tseries::adf.test

# Imports from xtable

#' @importFrom xtable xtable
xtable <- xtable::xtable

# Imports from rugarch

#' @importFrom rugarch ugarchspec
ugarchspec <- rugarch::ugarchspec
#' @importFrom rugarch ugarchfit
ugarchfit <- rugarch::ugarchfit
#' @importFrom rugarch infocriteria
infocriteria <- rugarch::infocriteria
#' @importFrom rugarch residuals
residuals <- rugarch::residuals

# Imports from e1071

#' @importFrom e1071 svm
svm <- e1071::svm

#' @importFrom parallel mclapply
parallel <- parallel::mclapply