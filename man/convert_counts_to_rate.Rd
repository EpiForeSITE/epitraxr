% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{convert_counts_to_rate}
\alias{convert_counts_to_rate}
\title{Convert case counts to rate}
\usage{
convert_counts_to_rate(counts, pop, digits, rate_adj_pop = 1e+05)
}
\arguments{
\item{counts}{Integer(s). Case counts to convert.}

\item{pop}{Integer. Population size where cases were counted.}

\item{digits}{Integer. Number of decimals to round to.}

\item{rate_adj_pop}{Integer. Optional target population to use for rate.
Defaults to 100k.}
}
\value{
The count(s) as rates per rate_adj_pop.
}
\description{
'convert_counts_to_rate' converts case counts for a given population to an
adjusted per population of size X and rounds to the given number of digits.
}
\examples{
convert_counts_to_rate(50, 200000, 2)
convert_counts_to_rate(c(10, 20), 100000, 1, 10000)
}
