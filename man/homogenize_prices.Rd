% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/market_data_to_ccl.R
\name{homogenize_prices}
\alias{homogenize_prices}
\title{Transform the price columns of a data.table by applying the exchange rate}
\usage{
homogenize_prices(dt)
}
\arguments{
\item{dt}{A data.table with the following columns: currency, lastPrice, openPrice, maxPrice, minPrice, rate}
}
\value{
The same data.table with the price columns transformed.
}
\description{
Use this function to convert the currencies of a data.table with market data
that already includes the exchange rate for each date.
Modifies the original data.table in place.
}
