% Generated by roxygen2 (4.0.2): do not edit by hand
\name{scrape_epex}
\alias{scrape_epex}
\title{Function scraping various spotprices and volumes from the Epexspot website}
\usage{
scrape_epex(from_date, to_date, country = "DE", market = "Spot",
  contract = "H", filter_missing = TRUE, cores = 8L)
}
\arguments{
\item{from_date}{A string with the start date of the desired series}

\item{to_date}{A string with the end date of the desired series}

\item{country}{A string with the desired country iso id. Takes the values DE,
FR and CH. Default is DE.}

\item{market}{A string indicating market type. Takes the values Spot and
Intraday. Deafult is Spot.}

\item{contract}{A string indicating contract type for intraday. Take the
values H and Q. Default is H.}

\item{filter_missing}{A bolean, determining wether or not to replace missing
values.}
}
\value{
a dataframe containing the dates, hours, spot and volumes
}
\description{
Function scraping various spotprices and volumes from the Epexspot website
}

