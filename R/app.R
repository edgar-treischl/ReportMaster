library(ReportMaster)
library(shiny)
library(bslib)

# Set environment and initialize parameters
Sys.setenv(R_CONFIG_ACTIVE = "default")
tmp.snr <- "8934"
tmp.ubb <- FALSE
tmp.audience <- "sus"
tmp.results <- "sus"
tmp.stype <- "gm"
tmp.ganztag <- FALSE

# Get initial parameters and data
get_parameter(
  snr = tmp.snr,
  audience = tmp.audience,
  ubb = tmp.ubb,
  ganztag = tmp.ganztag,
  stype = tmp.stype
)

# Get school name
schoolname <- get_sname(snr = tmp.snr)

# Check if report already exists
initial_report_path <- base::file.path("res", base::paste0(tmp.snr, "_2024"),
                                       base::paste0(tmp.snr, "_results_", tmp.audience, ".pdf"))
initial_report_exists <- base::file.exists(initial_report_path)

ui <- bslib::page_sidebar(
  title = "OES Report Viewer",

  sidebar = bslib::sidebar(
    width = "33%",

    # School information at the top of sidebar
    bslib::card(
      class = "mb-3",
      bslib::card_header(
        "School Information",
        class = "bg-light"
      ),
      shiny::div(
        class = "p-3",
        shiny::h4(schoolname),
        shiny::tags$small(class = "text-muted", base::paste("Befragung:", tmp.audience)),
        shiny::tags$br(),
        shiny::tags$small(class = "text-muted", base::paste("N:", tmp.n[1]))
        )
    ),

    # Plot selection dropdown
    shiny::selectInput("selected_plot", "Select Plot:",
                       choices = tmp.meta,
                       selected = tmp.meta[1]),

    shiny::downloadButton("download", "Download Plot", class = "btn-standard w-100"),

    shiny::hr(),

    shiny::uiOutput("generate_report_ui"),
    shiny::uiOutput("progress_ui"),
    shiny::uiOutput("download_report_ui")
  ),

  # Main panel with tabs for plot and information
  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Plot",
      shiny::uiOutput("dynamic_card_header"),
      shiny::plotOutput("plot1")
    ),
    bslib::nav_panel(
      "Information",
      shiny::uiOutput("plot_explanation_accordion")
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive values
  report_available <- shiny::reactiveVal(initial_report_exists)
  is_running <- shiny::reactiveVal(FALSE)

  # Dynamic card header for plot
  output$dynamic_card_header <- shiny::renderUI({
    shiny::req(input$selected_plot)
    bslib::card_header(input$selected_plot)
  })

  # Generate plot function
  generate_plot <- function() {
    ReportMaster::export_plot(
      meta = input$selected_plot,
      snr = tmp.snr,
      audience = tmp.audience,
      report = tmp.report,
      ubb = tmp.ubb,
      data = tmp.data,
      export = FALSE
    )
  }

  # Display the plot
  output$plot1 <- shiny::renderPlot({
    shiny::req(input$selected_plot)
    generate_plot()
  })

  # Add new output for plot explanations with accordion
  output$plot_explanation_accordion <- shiny::renderUI({
    shiny::req(input$selected_plot)

    # Create a named list of explanations for each plot type
    explanations <- base::list(
      "plot1" = base::list(
        purpose = "This plot shows the distribution of student performance.",
        interpretation = "The x-axis represents score ranges, while the y-axis shows frequency.",
        key_points = shiny::tags$ul(
          shiny::tags$li("Peaks indicate common score ranges"),
          shiny::tags$li("Wide distribution suggests varied performance"),
          shiny::tags$li("Compare with reference lines for context")
        )
      ),
      "plot2" = base::list(
        purpose = "Comparative analysis of different subject areas.",
        interpretation = "Compare performance across different subjects and identify patterns.",
        key_points = shiny::tags$ul(
          shiny::tags$li("Each bar represents a subject area"),
          shiny::tags$li("Height indicates average performance"),
          shiny::tags$li("Error bars show confidence intervals")
        )
      )
    )

    current_explanation <- explanations[[input$selected_plot]]

    if (!base::is.null(current_explanation)) {
      bslib::accordion(
        bslib::accordion_panel(
          "Purpose",
          shiny::p(current_explanation$purpose)
        ),
        bslib::accordion_panel(
          "How to Interpret",
          shiny::p(current_explanation$interpretation)
        ),
        bslib::accordion_panel(
          "Key Points",
          current_explanation$key_points
        ),
        multiple = TRUE
      )
    } else {
      bslib::accordion(
        bslib::accordion_panel(
          "About ...",
          shiny::p("This plot shows specific analysis results for your school.",
                   "Select different plots from the dropdown to explore various aspects",
                   "of the data.")
        )
      )
    }
  })

  # Generate Report UI
  output$generate_report_ui <- shiny::renderUI({
    if (!report_available()) {
      shiny::div(
        shiny::actionButton("generate_report",
                            label = shiny::span(
                              shiny::icon("file-pdf"),
                              "Generate Report"
                            ),
                            class = "btn-primary w-100"),
        shiny::div(
          style = "margin-top: 5px; font-size: 0.8em; color: #666;",
          shiny::icon("clock"),
          "This process may take several minutes"
        )
      )
    }
  })

  # Download handler for plots
  output$download <- shiny::downloadHandler(
    filename = function() {
      base::paste0("plot-", input$selected_plot, ".png")
    },
    content = function(file) {
      base::tryCatch({
        grDevices::png(file, width = 8, height = 6, units = "in", res = 150)
        base::print(generate_plot())
        grDevices::dev.off()
      }, error = function(e) {
        if(grDevices::dev.cur() > 1) grDevices::dev.off()
        base::stop("Error generating plot: ", e$message)
      })
    },
    contentType = "image/png"
  )

  # Progress UI
  output$progress_ui <- shiny::renderUI({
    if (is_running()) {
      shiny::div(
        style = "margin-top: 15px;",
        shiny::div(class = "text-center",
                   shiny::tags$i(class = "fa fa-spinner fa-spin fa-2x"),
                   shiny::tags$p("Generating report...")
        )
      )
    }
  })

  # Download Report UI
  output$download_report_ui <- shiny::renderUI({
    if (report_available()) {
      shiny::div(
        style = "margin-top: 15px;",
        shiny::downloadButton("download_report", "Download Report", class = "btn-success w-100")
      )
    }
  })

  # Download handler for report
  output$download_report <- shiny::downloadHandler(
    filename = function() {
      base::paste0(tmp.snr, "_results_", tmp.audience, ".pdf")
    },
    content = function(file) {
      report_path <- base::file.path("res", base::paste0(tmp.snr, "_2024"),
                                     base::paste0(tmp.snr, "_results_", tmp.audience, ".pdf"))

      if (!base::file.exists(report_path)) {
        base::stop("Report file not found at: ", report_path)
      }
      base::file.copy(report_path, file)
    },
    contentType = "application/pdf"
  )

  # Handle report generation
  shiny::observeEvent(input$generate_report, {
    is_running(TRUE)
    report_available(FALSE)

    base::tryCatch({
      ReportMaster::run_Parallel(
        snr = tmp.snr,
        audience = tmp.audience,
        stype = tmp.stype,
        ubb = tmp.ubb,
        ganztag = tmp.ganztag,
        results = tmp.results
      )
      is_running(FALSE)
      report_available(TRUE)
      shiny::showNotification("Report generation completed!", type = "message")
    },
    error = function(e) {
      is_running(FALSE)
      report_available(FALSE)
      shiny::showNotification(
        base::paste("Error generating report:", e$message),
        type = "error"
      )
    })
  })
}

shiny::shinyApp(ui, server)
