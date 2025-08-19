# ------------------------------------------------------------------------------
# title: EpiTrax Sample Data Generator
# author: Andrew Pulsipher
# date: 2025-07-21
#
# This script generates realistic sample data for testing the EpiTrax reporting
# package. It creates disease case data with appropriate MMWR week numbers and
# realistic seasonal patterns for common diseases.
#
# The script is maintained by the Insight Net center ForeSITE. All the code is
# available on GitHub at
# https://github.com/EpiForeSITE/epitrax-report-automation.
#
# For questions, updates, or bug reports, please visit the GitHub repository.
# ------------------------------------------------------------------------------

library(lubridate)

set.seed(123)  # For reproducible examples

# Setup parameters
years <- 2019:2024
diseases <- c("Coronavirus, Novel (2019-nCoV)", "Influenza-associated hospitalization", "Measles (rubeola)", "Syphilis, primary", "Chickenpox (Varicella)")

# Function to get MMWR week from a date
get_mmwr_week <- function(date) {
  # Simple approximation - actual MMWR weeks have special rules
  week <- week(date)
  if (week == 53) week <- 52
  week
}

# Generate dates and count patterns for each disease and year
cases <- list()

for (year in years) {
  for (disease in diseases) {
    # Determine number of cases based on disease type
    yearly_cases <- switch(disease,
      "Coronavirus, Novel (2019-nCoV)" = sample(600:2400, 1),  # 50-200 per month average
      "Influenza-associated hospitalization" = sample(600:2400, 1),
      "Measles (rubeola)" = sample(120:600, 1),    # 10-50 per month average
      "Syphilis, primary" = sample(120:600, 1),
      "Chickenpox (Varicella)" = sample(120:360, 1)  # 10-30 per month average
    )

    # Generate random dates throughout the year
    dates <- as.Date(paste0(year, "-01-01")) + sample(0:364, yearly_cases, replace = TRUE)

    # Create cases data frame
    year_cases <- data.frame(
      patient_mmwr_week = sapply(dates, get_mmwr_week),
      patient_mmwr_year = year,
      patient_disease = disease
    )

    cases[[paste(disease, year)]] <- year_cases
  }
}

# Combine all cases and randomize order
epitrax_data <- do.call(rbind, cases)
epitrax_data <- epitrax_data[sample(nrow(epitrax_data)), ]
rownames(epitrax_data) <- NULL

# Write to CSV
utils::write.csv(
  epitrax_data,
  "sample_epitrax_data.csv",
  row.names = FALSE
)
