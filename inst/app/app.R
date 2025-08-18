library(shiny)
library(DT)
library(devtools)
library(writexl)
library(shinyjs)
library(stringr)
library(epitraxr)

source("../../R/epitrax.R")
source("../../R/validation.R")
source("../../R/data.R")
source("../../R/helpers.R")
source("../../R/filesystem.R")
source("../../R/reports.R")

# Dictionary constant for multiselect options matching epitraxr functions
REPORT_OPTIONS <- list(
  "Annual Counts" = "annual_counts",
  "Monthly Counts All Years" = "monthly_counts_all_yrs", 
  "Monthly Averages" = "monthly_avgs",
  "YTD Counts" = "ytd_counts",
  "YTD Rates" = "ytd_rates",
  "Monthly Cross-sections" = "month_crosssections",
  "Public YTD Rates" = "public_ytd_rates",
  "Combined Month/YTD" = "combined_month_ytd",
  "Monthly Medians" = "monthly_medians",
  "YTD Medians" = "ytd_medians",
  "Grouped Stats" = "grouped_stats"
)

# Function to display DataFrame as table
display_dataframe_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return("No data available")
  }
  
  # Return DT datatable for interactive display
  DT::datatable(df, 
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  autoWidth = TRUE
                ),
                class = 'cell-border stripe')
}

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs functionality
  titlePanel("EpiTrax Report Generator"),

  sidebarLayout(
    sidebarPanel(
      # File upload field for CSV
      fileInput("epitrax_file",
                "EpiTrax Data Export:",
                accept = c(".csv", ".CSV"),
                placeholder = "Choose CSV file..."),
      
      # File upload for internal disease list
      fileInput("internal_disease_file",
                "Internal Disease List:",
                accept = c(".csv", ".CSV"),
                placeholder = "Choose internal disease CSV..."),
      
      # File upload for public disease list  
      fileInput("public_disease_file",
                "Public Disease List:",
                accept = c(".csv", ".CSV"),
                placeholder = "Choose public disease CSV..."),
      
      # Multiselect with epitraxr function options
      selectInput("report_options",
        "Reports to Generate:",
        choices = REPORT_OPTIONS,
        multiple = TRUE,
        selected = NULL
        ),
      
      # Current Population input
      numericInput("current_population",
                   "Current Population:",
                   value = 100000,
                   min = 1,
                   step = 1000),
      
      # Average 5-Year Population input
      numericInput("avg_population",
                   "Average 5-Year Population:",
                   value = 100000,
                   min = 1,
                   step = 1000),
      
      # Rounding Decimal Places input
      numericInput("decimal_places",
                   "Rounding Decimal Places:",
                   value = 2,
                   min = 0,
                   max = 10,
                   step = 1),
      
      # Generate Reports button
      actionButton("generate",
                   "Generate Reports",
                   class = "btn-success"),
      
      br(), br(),
      
      # Download buttons in a row
      fluidRow(
        column(3, 
               downloadButton("download_csv",
                              "Download CSV",
                              class = "btn-primary",
                              style = "width: 100%;")),
        column(3, 
               downloadButton("download_excel", 
                              "Download Excel",
                              class = "btn-info",
                              style = "width: 100%;")),
        column(3, 
               downloadButton("download_pdf",
                              "Download PDF",
                              class = "btn-secondary",
                              style = "width: 100%;")),
        column(3, 
               downloadButton("download_all",
                              "Download All",
                              class = "btn-warning",
                              style = "width: 100%;"))
      )
    ),
    
    mainPanel(
      h3("Generated Reports"),
      
      # Dynamic tabset panel based on selected reports
      uiOutput("report_tabs")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store data and reports
  epitrax_obj <- reactiveVal(NULL)
  
  # Disable download buttons initially
  observe({
    shinyjs::disable("download_csv")
    shinyjs::disable("download_excel")
    shinyjs::disable("download_pdf")
    shinyjs::disable("download_all")
  })
  
  # Generate Reports button click handler
  observeEvent(input$generate, {
    req(input$epitrax_file, input$report_options)
    
    tryCatch({
      # Show progress
      showNotification("Generating reports...", type = "message", duration = 2)
      
      # Create config list from frontend inputs
      config_list <- list(
        current_population = input$current_population,
        avg_5yr_population = input$avg_population,
        rounding_decimals = input$decimal_places
      )
      
      # Conditionally build disease_list_files based on provided files
      disease_list_files <- list()
      if (!is.null(input$internal_disease_file)) {
        disease_list_files$internal <- input$internal_disease_file$datapath
      }
      if (!is.null(input$public_disease_file)) {
        disease_list_files$public <- input$public_disease_file$datapath
      }
      
      # Setup epitrax object
      epitrax <- setup_epitrax(
        epitrax_file = input$epitrax_file$datapath,
        config_list = config_list,
        disease_list_files = disease_list_files
      )
      
      # Generate selected reports
      selected_reports <- input$report_options
      
      # Generate internal reports
      if ("annual_counts" %in% selected_reports) {
        epitrax <- epitrax_ireport_annual_counts(epitrax)
      }
      
      if ("monthly_counts_all_yrs" %in% selected_reports) {
        epitrax <- epitrax_ireport_monthly_counts_all_yrs(epitrax)
      }
      
      if ("monthly_avgs" %in% selected_reports) {
        epitrax <- epitrax_ireport_monthly_avgs(epitrax, exclude.report.year = TRUE)
      }
      
      if ("ytd_counts" %in% selected_reports) {
        epitrax <- epitrax_ireport_ytd_counts_for_month(epitrax, as.rates = FALSE)
      }
      
      if ("ytd_rates" %in% selected_reports) {
        epitrax <- epitrax_ireport_ytd_counts_for_month(epitrax, as.rates = TRUE)
      }
      
      # Generate public reports
      if ("month_crosssections" %in% selected_reports) {
        epitrax <- epitrax_preport_month_crosssections(epitrax, month_offsets = 0:3)
      }
      
      if ("public_ytd_rates" %in% selected_reports) {
        epitrax <- epitrax_preport_ytd_rates(epitrax)
      }
      
      if ("combined_month_ytd" %in% selected_reports) {
        epitrax <- epitrax_preport_combined_month_ytd(epitrax)
      }
      
      # Generate additional internal reports
      if ("monthly_medians" %in% selected_reports) {
        epitrax <- epitrax_report_monthly_medians(epitrax, is.public = FALSE, exclude.report.year = TRUE)
      }
      
      if ("ytd_medians" %in% selected_reports) {
        epitrax <- epitrax_report_ytd_medians(epitrax, is.public = FALSE, exclude.report.year = TRUE)
      }
      
      if ("grouped_stats" %in% selected_reports) {
        epitrax <- epitrax_report_grouped_stats(epitrax, is.public = FALSE)
      }
      
      # Store the epitrax object
      epitrax_obj(epitrax)
      
      # Enable download buttons
      shinyjs::enable("download_csv")
      shinyjs::enable("download_excel")
      shinyjs::enable("download_pdf")
      shinyjs::enable("download_all")
      
      showNotification("Reports generated successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error generating reports:", e$message), type = "error")
      epitrax_obj(NULL)
      shinyjs::disable("download_csv")
      shinyjs::disable("download_excel")
      shinyjs::disable("download_pdf")
      shinyjs::disable("download_all")
    })
  })
  
  # Download CSV files as ZIP
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("epitrax_csv_reports_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(epitrax_obj())
      
      # Create temporary directory
      temp_dir <- tempdir()
      # Create a directory with the same name as the ZIP file (without extension)
      zip_name <- tools::file_path_sans_ext(basename(paste0("epitrax_csv_reports_", Sys.Date(), ".zip")))
      csv_dir <- file.path(temp_dir, zip_name)
      dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
      
      tryCatch({
        epitrax <- epitrax_obj()
        
        # Write internal reports to CSV
        if (!is.null(epitrax$internal_reports) && length(epitrax$internal_reports) > 0) {
          internal_dir <- file.path(csv_dir, "internal")
          dir.create(internal_dir, showWarnings = FALSE)
          
          for (name in names(epitrax$internal_reports)) {
            write.csv(epitrax$internal_reports[[name]], 
                     file.path(internal_dir, paste0(name, ".csv")), 
                     row.names = FALSE)
          }
        }
        
        # Write public reports to CSV
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          public_dir <- file.path(csv_dir, "public")
          dir.create(public_dir, showWarnings = FALSE)
          
          for (name in names(epitrax$public_reports)) {
            write.csv(epitrax$public_reports[[name]], 
                     file.path(public_dir, paste0(name, ".csv")), 
                     row.names = FALSE)
          }
        }
        
        # Create ZIP file with proper structure
        # Change to temp_dir and zip the named directory
        old_wd <- getwd()
        setwd(temp_dir)
        zip(file, zip_name, flags = "-r")
        setwd(old_wd)
        
      }, error = function(e) {
        showNotification(paste("Error creating CSV download:", e$message), type = "error")
      })
    },
    contentType = "application/zip"
  )
  
  # Download Excel files
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("epitrax_excel_reports_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(epitrax_obj())
      
      # Create temporary directory
      temp_dir <- tempdir()
      # Create a directory with the same name as the ZIP file (without extension)
      zip_name <- tools::file_path_sans_ext(basename(paste0("epitrax_excel_reports_", Sys.Date(), ".zip")))
      excel_dir <- file.path(temp_dir, zip_name)
      dir.create(excel_dir, showWarnings = FALSE, recursive = TRUE)
      
      tryCatch({
        epitrax <- epitrax_obj()
        
        # Write internal reports to Excel
        if (!is.null(epitrax$internal_reports) && length(epitrax$internal_reports) > 0) {
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(epitrax$internal_reports, 
                               path = file.path(excel_dir, "internal_reports_combined.xlsx"))
          } else {
            # Fallback to individual CSV files if writexl not available
            internal_dir <- file.path(excel_dir, "internal_csv")
            dir.create(internal_dir, showWarnings = FALSE)
            for (name in names(epitrax$internal_reports)) {
              write.csv(epitrax$internal_reports[[name]], 
                       file.path(internal_dir, paste0(name, ".csv")), 
                       row.names = FALSE)
            }
          }
        }
        
        # Write public reports to Excel
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(epitrax$public_reports, 
                               path = file.path(excel_dir, "public_reports_combined.xlsx"))
          } else {
            # Fallback to CSV files if writexl not available
            public_dir <- file.path(excel_dir, "public_csv")
            dir.create(public_dir, showWarnings = FALSE)
            for (name in names(epitrax$public_reports)) {
              write.csv(epitrax$public_reports[[name]], 
                       file.path(public_dir, paste0(name, ".csv")), 
                       row.names = FALSE)
            }
          }
        }
        
        # Create ZIP file with proper structure
        # Change to temp_dir and zip the named directory
        old_wd <- getwd()
        setwd(temp_dir)
        zip(file, zip_name, flags = "-r")
        setwd(old_wd)
        
      }, error = function(e) {
        showNotification(paste("Error creating Excel download:", e$message), type = "error")
      })
    },
    contentType = "application/zip"
  )

  # Download PDF files
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("epitrax_pdf_reports_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(epitrax_obj())
      
      # Create temporary directory
      temp_dir <- tempdir()
      # Create a directory with the same name as the ZIP file (without extension)
      zip_name <- tools::file_path_sans_ext(basename(paste0("epitrax_pdf_reports_", Sys.Date(), ".zip")))
      pdf_dir <- file.path(temp_dir, zip_name)
      dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
      
      tryCatch({
        epitrax <- epitrax_obj()
        
        # Create filesystem structure for PDF generation
        fsys <- list(
          internal = file.path(pdf_dir, "internal"),
          public = file.path(pdf_dir, "public"),
          settings = pdf_dir
        )
        
        # Create directories
        dir.create(fsys$internal, showWarnings = FALSE, recursive = TRUE)
        dir.create(fsys$public, showWarnings = FALSE, recursive = TRUE)
        dir.create(fsys$settings, showWarnings = FALSE, recursive = TRUE)
        
        # PDF report parameters
        params <- list(
          title = "Disease Surveillance Report",
          author = "EpiTrax Report Generator"
        )
        
        # Generate PDFs for public reports (excluding grouped stats)
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          # Check if rmarkdown is available before attempting PDF generation
          if (requireNamespace("rmarkdown", quietly = TRUE)) {
            tryCatch({
              epitrax_write_pdf_public_reports(epitrax, params, fsys)
            }, error = function(e) {
              # PDF generation failed, but continue
              showNotification("PDF generation failed for public reports", type = "warning")
            })
          } else {
            showNotification("rmarkdown package required for PDF generation", type = "warning")
          }
        }
        
        # Generate PDFs for grouped stats reports (both internal and public)
        if ((!is.null(epitrax$internal_reports) && any(grepl("^grouped_stats_", names(epitrax$internal_reports)))) ||
            (!is.null(epitrax$public_reports) && any(grepl("^grouped_stats_", names(epitrax$public_reports))))) {
          # Check if rmarkdown is available before attempting PDF generation
          if (requireNamespace("rmarkdown", quietly = TRUE)) {
            tryCatch({
              epitrax_write_pdf_grouped_stats(epitrax, params, fsys)
            }, error = function(e) {
              # PDF generation failed, but continue
              showNotification("PDF generation failed for grouped stats reports", type = "warning")
            })
          } else {
            showNotification("rmarkdown package required for PDF generation", type = "warning")
          }
        }
        
        # Remove empty directories
        if (length(list.files(fsys$internal)) == 0) {
          unlink(fsys$internal, recursive = TRUE)
        }
        if (length(list.files(fsys$public)) == 0) {
          unlink(fsys$public, recursive = TRUE)
        }
        
        # Create ZIP file with proper structure
        # Change to temp_dir and zip the named directory
        old_wd <- getwd()
        setwd(temp_dir)
        zip(file, zip_name, flags = "-r")
        setwd(old_wd)
        
      }, error = function(e) {
        showNotification(paste("Error creating PDF download:", e$message), type = "error")
      })
    },
    contentType = "application/zip"
  )

  # Download All files (both CSV and Excel)
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("epitrax_all_reports_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(epitrax_obj())
      
      # Create temporary directory
      temp_dir <- tempdir()
      # Create a directory with the same name as the ZIP file (without extension)
      zip_name <- tools::file_path_sans_ext(basename(paste0("epitrax_all_reports_", Sys.Date(), ".zip")))
      all_dir <- file.path(temp_dir, zip_name)
      dir.create(all_dir, showWarnings = FALSE, recursive = TRUE)
      
      tryCatch({
        epitrax <- epitrax_obj()
        
        # Create CSV subdirectory
        csv_dir <- file.path(all_dir, "csv_reports")
        dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Write internal reports to CSV
        if (!is.null(epitrax$internal_reports) && length(epitrax$internal_reports) > 0) {
          internal_csv_dir <- file.path(csv_dir, "internal")
          dir.create(internal_csv_dir, showWarnings = FALSE)
          
          for (name in names(epitrax$internal_reports)) {
            write.csv(epitrax$internal_reports[[name]], 
                     file.path(internal_csv_dir, paste0(name, ".csv")), 
                     row.names = FALSE)
          }
        }
        
        # Write public reports to CSV
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          public_csv_dir <- file.path(csv_dir, "public")
          dir.create(public_csv_dir, showWarnings = FALSE)
          
          for (name in names(epitrax$public_reports)) {
            write.csv(epitrax$public_reports[[name]], 
                     file.path(public_csv_dir, paste0(name, ".csv")), 
                     row.names = FALSE)
          }
        }
        
        # Create Excel subdirectory
        excel_dir <- file.path(all_dir, "excel_reports")
        dir.create(excel_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Write internal reports to Excel
        if (!is.null(epitrax$internal_reports) && length(epitrax$internal_reports) > 0) {
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(epitrax$internal_reports, 
                               path = file.path(excel_dir, "internal_reports_combined.xlsx"))
          } else {
            # Fallback to individual CSV files if writexl not available
            internal_dir <- file.path(excel_dir, "internal_csv")
            dir.create(internal_dir, showWarnings = FALSE)
            for (name in names(epitrax$internal_reports)) {
              write.csv(epitrax$internal_reports[[name]], 
                       file.path(internal_dir, paste0(name, ".csv")), 
                       row.names = FALSE)
            }
          }
        }
        
        # Write public reports to Excel
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(epitrax$public_reports, 
                               path = file.path(excel_dir, "public_reports_combined.xlsx"))
          } else {
            # Fallback to CSV files if writexl not available
            public_dir <- file.path(excel_dir, "public_csv")
            dir.create(public_dir, showWarnings = FALSE)
            for (name in names(epitrax$public_reports)) {
              write.csv(epitrax$public_reports[[name]], 
                       file.path(public_dir, paste0(name, ".csv")), 
                       row.names = FALSE)
            }
          }
        }
        
        # Create PDF subdirectory
        pdf_dir <- file.path(all_dir, "pdf_reports")
        dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Create filesystem structure for PDF generation
        pdf_fsys <- list(
          internal = file.path(pdf_dir, "internal"),
          public = file.path(pdf_dir, "public"),
          settings = pdf_dir
        )
        
        # Create directories
        dir.create(pdf_fsys$internal, showWarnings = FALSE, recursive = TRUE)
        dir.create(pdf_fsys$public, showWarnings = FALSE, recursive = TRUE)
        dir.create(pdf_fsys$settings, showWarnings = FALSE, recursive = TRUE)
        
        # PDF report parameters
        params <- list(
          title = "Disease Surveillance Report",
          author = "EpiTrax Report Generator"
        )
        
        # Generate PDFs for public reports (excluding grouped stats)
        if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
          # Check if rmarkdown is available before attempting PDF generation
          if (requireNamespace("rmarkdown", quietly = TRUE)) {
            tryCatch({
              epitrax_write_pdf_public_reports(epitrax, params, pdf_fsys)
            }, error = function(e) {
              # PDF generation failed, but continue with other formats
              showNotification("PDF generation failed for public reports", type = "warning")
            })
          }
        }
        
        # Generate PDFs for grouped stats reports (both internal and public)
        if ((!is.null(epitrax$internal_reports) && any(grepl("^grouped_stats_", names(epitrax$internal_reports)))) ||
            (!is.null(epitrax$public_reports) && any(grepl("^grouped_stats_", names(epitrax$public_reports))))) {
          # Check if rmarkdown is available before attempting PDF generation
          if (requireNamespace("rmarkdown", quietly = TRUE)) {
            tryCatch({
              epitrax_write_pdf_grouped_stats(epitrax, params, pdf_fsys)
            }, error = function(e) {
              # PDF generation failed, but continue with other formats
              showNotification("PDF generation failed for grouped stats reports", type = "warning")
            })
          }
        }
        
        # Remove empty PDF directories
        if (length(list.files(pdf_fsys$internal)) == 0) {
          unlink(pdf_fsys$internal, recursive = TRUE)
        }
        if (length(list.files(pdf_fsys$public)) == 0) {
          unlink(pdf_fsys$public, recursive = TRUE)
        }
        if (length(list.files(pdf_dir)) == 0) {
          unlink(pdf_dir, recursive = TRUE)
        }
        
        # Create ZIP file with proper structure
        # Change to temp_dir and zip the named directory
        old_wd <- getwd()
        setwd(temp_dir)
        zip(file, zip_name, flags = "-r")
        setwd(old_wd)
        
      }, error = function(e) {
        showNotification(paste("Error creating combined download:", e$message), type = "error")
      })
    },
    contentType = "application/zip"
  )

  # Dynamic UI for report tabs
  output$report_tabs <- renderUI({
    epitrax <- epitrax_obj()
    if (is.null(epitrax)) {
      return(div(h4("No reports generated yet. Please upload files and click 'Generate Reports'.")))
    }
    
    # Create top-level tabs for Internal and Public reports
    top_level_tabs <- list()
    
    # Internal reports tab
    if (!is.null(epitrax$internal_reports) && length(epitrax$internal_reports) > 0) {
      internal_sub_tabs <- list()
      
      for (report_name in names(epitrax$internal_reports)) {
        internal_sub_tabs[[length(internal_sub_tabs) + 1]] <- tabPanel(
          title = gsub("_", " ", str_to_title(report_name)),
          value = paste0("internal_", report_name),
          div(style = "margin-top: 10px;",
              DT::dataTableOutput(paste0("table_", report_name)))
        )
      }
      
      top_level_tabs[[length(top_level_tabs) + 1]] <- tabPanel(
        title = "Internal Reports",
        value = "internal_tab",
        div(style = "margin-top: 10px;",
            do.call(tabsetPanel, internal_sub_tabs))
      )
    }
    
    # Public reports tab
    if (!is.null(epitrax$public_reports) && length(epitrax$public_reports) > 0) {
      public_sub_tabs <- list()
      
      for (report_name in names(epitrax$public_reports)) {
        public_sub_tabs[[length(public_sub_tabs) + 1]] <- tabPanel(
          title = gsub("_", " ", str_to_title(report_name)),
          value = paste0("public_", report_name),
          div(style = "margin-top: 10px;",
              DT::dataTableOutput(paste0("table_", report_name)))
        )
      }
      
      top_level_tabs[[length(top_level_tabs) + 1]] <- tabPanel(
        title = "Public Reports",
        value = "public_tab",
        div(style = "margin-top: 10px;",
            do.call(tabsetPanel, public_sub_tabs))
      )
    }
    
    if (length(top_level_tabs) == 0) {
      return(div(h4("No report data available.")))
    }
    
    do.call(tabsetPanel, top_level_tabs)
  })
  
  # Dynamic table outputs for each report
  observe({
    epitrax <- epitrax_obj()
    if (!is.null(epitrax)) {
      
      # Render internal report tables
      if (!is.null(epitrax$internal_reports)) {
        for (report_name in names(epitrax$internal_reports)) {
          local({
            local_name <- report_name
            output[[paste0("table_", local_name)]] <- DT::renderDataTable({
              display_dataframe_table(epitrax$internal_reports[[local_name]])
            })
          })
        }
      }
      
      # Render public report tables  
      if (!is.null(epitrax$public_reports)) {
        for (report_name in names(epitrax$public_reports)) {
          local({
            local_name <- report_name
            output[[paste0("table_", local_name)]] <- DT::renderDataTable({
              display_dataframe_table(epitrax$public_reports[[local_name]])
            })
          })
        }
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 4747))