% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reports.R
\name{create_report_annual_counts}
\alias{create_report_annual_counts}
\title{Create annual counts report}
\usage{
create_report_annual_counts(data, disease_names)
}
\arguments{
\item{data}{Dataframe. Input data with columns: disease, year, counts.}

\item{disease_names}{Character vector. List of diseases to include in the
report.}
}
\value{
Dataframe of annual counts with one row per disease and one column
per year.
}
\description{
'create_report_annual_counts' generates a data frame of annual case
counts for each disease, with years as columns.
}
\examples{
data <- data.frame(
  disease = c("A", "A", "B"),
  year = c(2020, 2021, 2020),
  counts = c(5, 7, 8)
)
create_report_annual_counts(data, disease_names = c("A", "B", "C"))
}
