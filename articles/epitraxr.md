# epitraxr

## Introduction to epitraxr

EpiTrax is a central repository for epidemiological data developed by
Utah State’s Department of Health and Human Services (DHHS). It is now
used by several other states. Through EpiTrax, public health officials
have access to many different types of disease surveillance data, which
they use to produce regular (e.g., weekly, monthly, annual) reports on
their respective jurisdictions. This can be a tedious, time-intensive
process.

The epitraxr package makes it fast and easy to process EpiTrax data and
produce multiple reports.

### EpiTrax Data

To explore basic report functions in epitraxr, we’ll use this sample
dataset:

``` r

data_file <- "vignette-data/epitrax_data.csv"
head(read.csv(data_file))
#>   patient_mmwr_week patient_mmwr_year                      patient_disease
#> 1                31              2020 Influenza-associated hospitalization
#> 2                51              2019 Influenza-associated hospitalization
#> 3                27              2021 Influenza-associated hospitalization
#> 4                30              2024       Coronavirus, Novel (2019-nCoV)
#> 5                24              2020 Influenza-associated hospitalization
#> 6                13              2022 Influenza-associated hospitalization
```

When you export data from EpiTrax, each row corresponds to a single
disease case. EpiTrax can provide many different data points, but
epitraxr only cares about three:

- `patient_disease`: The disease
- `patient_mmwr_week`: The week number (1-52) of disease onset
- `patient_mmwr_year`: The year of disease onset

The package ignores all other columns.

Read in the data with the
[`read_epitrax_data()`](https://epiforesite.github.io/epitraxr/reference/read_epitrax_data.md)
function:

``` r

epitrax_data <- read_epitrax_data(data_file)
head(epitrax_data)
#>                                disease month year counts
#> 1 Influenza-associated hospitalization     8 2020      1
#> 2 Influenza-associated hospitalization    12 2019      1
#> 3 Influenza-associated hospitalization     7 2021      1
#> 4       Coronavirus, Novel (2019-nCoV)     7 2024      1
#> 5 Influenza-associated hospitalization     6 2020      1
#> 6 Influenza-associated hospitalization     4 2022      1
```

This validates the input data and converts the week number to a month
number (1-12), because reports generally use months instead of weeks.
The function also adds the `counts` column (initially all rows have a
count of 1), which is used internally in manipulating the data while
generating reports.

### Disease Lists

Before you can generate reports from the data, epitraxr needs a list of
diseases to include in the report. Often, you’ll have two lists, one for
internal reports and one for public reports. Read these files in using
the functions
[`get_report_diseases_internal()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_internal.md)
and
[`get_report_diseases_public()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_public.md).

``` r

internal_disease_list <- "vignette-data/ireport_diseases.csv"
internal_diseases <- get_report_diseases_internal(internal_disease_list)
head(internal_diseases)
#>                      EpiTrax_name                      Group_name
#> 1                         Anthrax                Zoonotic Disease
#> 2             Botulism, foodborne                  Enteric Toxins
#> 3              Campylobacteriosis       Bacterial Enteric Disease
#> 4          Chickenpox (Varicella)    Vaccine-Preventable Diseases
#> 5 Chlamydia trachomatis infection Sexually Transmitted Infections
#> 6                         Cholera                  Enteric Toxins
```

`internal_diseases` has two columns. `EpiTrax_name` is the disease name
*as reported by EpiTrax*. All internal reports need the values in this
column. `Group_name` is for grouping diseases, which is only used by
[`create_report_grouped_stats()`](https://epiforesite.github.io/epitraxr/reference/create_report_grouped_stats.md).
If you aren’t creating grouped reports, you don’t need `Group_name` in
your internal disease list.

``` r

public_disease_list <- "vignette-data/preport_diseases.csv"
public_diseases <- get_report_diseases_public(public_disease_list)
head(public_diseases)
#>                           EpiTrax_name                       Public_name
#> 1               Chickenpox (Varicella)                        Chickenpox
#> 2      Chlamydia trachomatis infection                         Chlamydia
#> 3                  Colorado Tick Fever               Colorado Tick Fever
#> 4       Coronavirus, Novel (2019-nCoV)                          COVID-19
#> 5                 HIV Infection, adult Human Immondeficiency Virus (HIV)
#> 6 Influenza-associated hospitalization       Influenza (hospitalization)
```

`public_diseases` also has two columns. Like `internal_diseases`,
`EpiTrax_name` is the disease name *as reported by EpiTrax*. All public
reports need the values in this column to properly compute statistics
from the data. `Public_name` is used by certain functions (prefixed with
`create_public_report_`) to translate the EpiTrax disease name to
something more accessible to the public. `Public_name` is also used to
combine related diseases in the final report (e.g., “Syphilis, primary”
and “Syphilis, secondary” publicly reported by the collected statistic
of “Syphilis”).

### Generating Reports: Standard Mode

We can now call the report generation functions, such as
[`create_report_annual_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_annual_counts.md),
providing the list of diseases we want to include in our report.

``` r

report <- create_report_annual_counts(
  data = epitrax_data,
  diseases = internal_diseases$EpiTrax_name
)

head(report)
#>                           disease 2019 2020 2021 2022 2023 2024
#> 1                         Anthrax    0    0    0    0    0    0
#> 2             Botulism, foodborne    0    0    0    0    0    0
#> 3              Campylobacteriosis    0    0    0    0    0    0
#> 4          Chickenpox (Varicella)  218  318  263  234  249  292
#> 5 Chlamydia trachomatis infection    0    0    0    0    0    0
#> 6                         Cholera    0    0    0    0    0    0
```

This gives us a data frame containing a row for each disease in our
disease list and a column showing the case counts for each year in the
dataset.

Let’s call the report function again, but this time give it the public
disease list.

``` r

report <- create_report_annual_counts(
  data = epitrax_data,
  diseases = public_diseases$EpiTrax_name
)

head(report)
#>                                disease 2019 2020 2021 2022 2023 2024
#> 1               Chickenpox (Varicella)  218  318  263  234  249  292
#> 2      Chlamydia trachomatis infection    0    0    0    0    0    0
#> 3                  Colorado Tick Fever    0    0    0    0    0    0
#> 4       Coronavirus, Novel (2019-nCoV) 1014 1627 2398 1855  908 1191
#> 5                 HIV Infection, adult    0    0    0    0    0    0
#> 6 Influenza-associated hospitalization  625 1733 1889 2289 1664 1466
```

### Generating Reports: Piped Mode (recommended)

The epitraxr package includes a separate piped mode to make it easy to
chain together multiple reports without needing to specify the disease
list and input data each time. **This is our recommended mode for
epitraxr.** See
[`vignette("piped-mode")`](https://epiforesite.github.io/epitraxr/articles/piped-mode.md)
for more information.

Here is a brief example of how the same annual counts report generation
would work in piped mode.

``` r

# Data and configuration files
data_file <- "vignette-data/epitrax_data.csv"
config_file <- "vignette-data/config.yaml"
disease_lists <- list(
  internal = "vignette-data/ireport_diseases.csv",
  public = "vignette-data/preport_diseases.csv"
)

# Run pipe
epitrax <- create_epitrax_from_file(data_file) |>
  epitrax_set_config_from_file(config_file) |>
  epitrax_set_report_diseases(disease_lists) |>
  epitrax_ireport_annual_counts()

# View report
head(epitrax$internal_reports$annual_counts)
#>                           disease 2019 2020 2021 2022 2023 2024
#> 1                         Anthrax    0    0    0    0    0    0
#> 2             Botulism, foodborne    0    0    0    0    0    0
#> 3              Campylobacteriosis    0    0    0    0    0    0
#> 4          Chickenpox (Varicella)  218  318  263  234  249  292
#> 5 Chlamydia trachomatis infection    0    0    0    0    0    0
#> 6                         Cholera    0    0    0    0    0    0
```

Piped mode really shines when we’re creating multiple reports all at
once.

``` r

epitrax <- create_epitrax_from_file(data_file) |>
  epitrax_set_config_from_file(config_file) |>
  epitrax_set_report_diseases(disease_lists) |>
  epitrax_ireport_annual_counts() |>
  epitrax_ireport_monthly_avgs() |>
  epitrax_ireport_ytd_counts_for_month()

list(epitrax$internal_reports)
#> [[1]]
#> [[1]]$annual_counts
#>                                 disease 2019 2020 2021 2022 2023 2024
#> 1                               Anthrax    0    0    0    0    0    0
#> 2                   Botulism, foodborne    0    0    0    0    0    0
#> 3                    Campylobacteriosis    0    0    0    0    0    0
#> 4                Chickenpox (Varicella)  218  318  263  234  249  292
#> 5       Chlamydia trachomatis infection    0    0    0    0    0    0
#> 6                               Cholera    0    0    0    0    0    0
#> 7                   Colorado Tick Fever    0    0    0    0    0    0
#> 8        Coronavirus, Novel (2019-nCoV) 1014 1627 2398 1855  908 1191
#> 9        E. coli - Carbapenem resistant    0    0    0    0    0    0
#> 10                 HIV Infection, adult    0    0    0    0    0    0
#> 11 Influenza-associated hospitalization  625 1733 1889 2289 1664 1466
#> 12                         Lyme disease    0    0    0    0    0    0
#> 13                    Measles (rubeola)  211  326  292  414  586  304
#> 14                            Monkeypox    0    0    0    0    0    0
#> 15                            Pertussis    0    0    0    0    0    0
#> 16                        Salmonellosis    0    0    0    0    0    0
#> 17                    Syphilis, primary  278  356  439  398  577  269
#> 18                  Syphilis, secondary    0    0    0    0    0    0
#> 19                 Tuberculosis, Active    0    0    0    0    0    0
#> 20              West Nile virus disease    0    0    0    0    0    0
#> 21                         Yellow Fever    0    0    0    0    0    0
#> 
#> [[1]]$`monthly_avgs_2019-2024`
#>                                 disease    Jan    Feb    Mar    Apr    May
#> 1                               Anthrax   0.00   0.00   0.00   0.00   0.00
#> 2                   Botulism, foodborne   0.00   0.00   0.00   0.00   0.00
#> 3                    Campylobacteriosis   0.00   0.00   0.00   0.00   0.00
#> 4                Chickenpox (Varicella)  23.50  22.33  21.00  26.67  19.83
#> 5       Chlamydia trachomatis infection   0.00   0.00   0.00   0.00   0.00
#> 6                               Cholera   0.00   0.00   0.00   0.00   0.00
#> 7                   Colorado Tick Fever   0.00   0.00   0.00   0.00   0.00
#> 8        Coronavirus, Novel (2019-nCoV) 114.17 137.67 115.67 140.33 113.50
#> 9        E. coli - Carbapenem resistant   0.00   0.00   0.00   0.00   0.00
#> 10                 HIV Infection, adult   0.00   0.00   0.00   0.00   0.00
#> 11 Influenza-associated hospitalization 121.83 140.17 129.33 162.67 133.67
#> 12                         Lyme disease   0.00   0.00   0.00   0.00   0.00
#> 13                    Measles (rubeola)  27.00  31.50  24.67  34.83  25.67
#> 14                            Monkeypox   0.00   0.00   0.00   0.00   0.00
#> 15                            Pertussis   0.00   0.00   0.00   0.00   0.00
#> 16                        Salmonellosis   0.00   0.00   0.00   0.00   0.00
#> 17                    Syphilis, primary  32.67  35.33  31.67  38.67  27.50
#> 18                  Syphilis, secondary   0.00   0.00   0.00   0.00   0.00
#> 19                 Tuberculosis, Active   0.00   0.00   0.00   0.00   0.00
#> 20              West Nile virus disease   0.00   0.00   0.00   0.00   0.00
#> 21                         Yellow Fever   0.00   0.00   0.00   0.00   0.00
#>       Jun    Jul    Aug    Sep    Oct    Nov    Dec
#> 1    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 2    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 3    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 4   19.17  24.17  20.00  18.83  25.33  19.00  22.50
#> 5    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 6    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 7    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 8  114.50 139.17 110.17 122.17 146.67 123.17 121.67
#> 9    0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 10   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 11 119.17 158.33 128.67 117.33 146.67 119.83 133.33
#> 12   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 13  24.67  37.33  27.50  28.17  36.33  29.17  28.67
#> 14   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 15   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 16   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 17  33.50  28.50  30.00  26.67  38.00  26.17  37.50
#> 18   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 19   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 20   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 21   0.00   0.00   0.00   0.00   0.00   0.00   0.00
#> 
#> [[1]]$ytd_counts
#>                                 disease Current_YTD_Counts Avg_5yr_YTD_Counts
#> 1                               Anthrax                  0                0.0
#> 2                   Botulism, foodborne                  0                0.0
#> 3                    Campylobacteriosis                  0                0.0
#> 4                Chickenpox (Varicella)                292              256.4
#> 5       Chlamydia trachomatis infection                  0                0.0
#> 6                               Cholera                  0                0.0
#> 7                   Colorado Tick Fever                  0                0.0
#> 8        Coronavirus, Novel (2019-nCoV)               1191             1560.4
#> 9        E. coli - Carbapenem resistant                  0                0.0
#> 10                 HIV Infection, adult                  0                0.0
#> 11 Influenza-associated hospitalization               1466             1640.0
#> 12                         Lyme disease                  0                0.0
#> 13                    Measles (rubeola)                304              365.8
#> 14                            Monkeypox                  0                0.0
#> 15                            Pertussis                  0                0.0
#> 16                        Salmonellosis                  0                0.0
#> 17                    Syphilis, primary                269              409.6
#> 18                  Syphilis, secondary                  0                0.0
#> 19                 Tuberculosis, Active                  0                0.0
#> 20              West Nile virus disease                  0                0.0
#> 21                         Yellow Fever                  0                0.0
```
