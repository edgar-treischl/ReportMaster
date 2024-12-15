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
initial_report_path <- file.path("res", paste0(tmp.snr, "_2024"),
                                 paste0(tmp.snr, "_results_", tmp.audience, ".pdf"))
initial_report_exists <- file.exists(initial_report_path)

ui <- page_sidebar(
  title = "Report Plots Viewer",

  sidebar = sidebar(
    width = "33%",

    # School information at the top of sidebar
    card(
      class = "mb-3",
      card_header(
        "School Information",
        class = "bg-light"
      ),
      div(
        class = "p-3",
        h4(schoolname),
        tags$small(
          class = "text-muted",
          paste("School ID:", tmp.snr)
        )
      )
    ),

    # Plot selection dropdown
    selectInput("selected_plot", "Select Plot:",
                choices = tmp.meta,
                selected = tmp.meta[1]),

    downloadButton("download", "Download Plot", class = "btn-standard w-100"),

    hr(),

    uiOutput("generate_report_ui"),
    uiOutput("progress_ui"),
    uiOutput("download_report_ui")
  ),

  # Main panel with tabs for plot and information
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Plot",
      uiOutput("dynamic_card_header"),
      plotOutput("plot1")
    ),
    nav_panel(
      "Information",
      uiOutput("plot_explanation_accordion")
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive values
  report_available <- reactiveVal(initial_report_exists)
  is_running <- reactiveVal(FALSE)

  # Dynamic card header for plot
  output$dynamic_card_header <- renderUI({
    req(input$selected_plot)
    card_header(input$selected_plot)
  })

  # Generate plot function
  generate_plot <- function() {
    export_plot(
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
  output$plot1 <- renderPlot({
    req(input$selected_plot)
    generate_plot()
  })

  # Add new output for plot explanations with accordion
  output$plot_explanation_accordion <- renderUI({
    req(input$selected_plot)

    # Create a named list of explanations for each plot type
    explanations <- list(
      "plot1" = list(
        purpose = "This plot shows the distribution of student performance.",
        interpretation = "The x-axis represents score ranges, while the y-axis shows frequency.",
        key_points = tags$ul(
          tags$li("Peaks indicate common score ranges"),
          tags$li("Wide distribution suggests varied performance"),
          tags$li("Compare with reference lines for context")
        )
      ),
      "plot2" = list(
        purpose = "Comparative analysis of different subject areas.",
        interpretation = "Compare performance across different subjects and identify patterns.",
        key_points = tags$ul(
          tags$li("Each bar represents a subject area"),
          tags$li("Height indicates average performance"),
          tags$li("Error bars show confidence intervals")
        )
      )
    )

    # Get the explanation for the current plot
    current_explanation <- explanations[[input$selected_plot]]

    if (!is.null(current_explanation)) {
      accordion(
        accordion_panel(
          "Purpose",
          p(current_explanation$purpose)
        ),
        accordion_panel(
          "How to Interpret",
          p(current_explanation$interpretation)
        ),
        accordion_panel(
          "Key Points",
          current_explanation$key_points
        ),
        multiple = TRUE  # Allow multiple panels to be open simultaneously
      )
    } else {
      # Default accordion for plots without specific explanations
      accordion(
        accordion_panel(
          "About this Plot",
          p("This plot shows specific analysis results for your school.",
            "Select different plots from the dropdown to explore various aspects",
            "of the data.")
        )
      )
    }
  })

  # Generate Report UI
  output$generate_report_ui <- renderUI({
    if (!report_available()) {
      div(
        actionButton("generate_report",
                     label = span(
                       icon("file-pdf"),
                       "Generate Report"
                     ),
                     class = "btn-primary w-100"),
        div(
          style = "margin-top: 5px; font-size: 0.8em; color: #666;",
          icon("clock"),
          "This process may take several minutes"
        )
      )
    }
  })

  # Download handler for plots
  output$download <- downloadHandler(
    filename = function() {
      paste0("plot-", input$selected_plot, ".png")
    },
    content = function(file) {
      tryCatch({
        png(file, width = 8, height = 6, units = "in", res = 150)
        print(generate_plot())
        dev.off()
      }, error = function(e) {
        if(dev.cur() > 1) dev.off()
        stop("Error generating plot: ", e$message)
      })
    },
    contentType = "image/png"
  )

  # Progress UI
  output$progress_ui <- renderUI({
    if (is_running()) {
      div(
        style = "margin-top: 15px;",
        div(class = "text-center",
            tags$i(class = "fa fa-spinner fa-spin fa-2x"),
            tags$p("Generating report...")
        )
      )
    }
  })

  # Download Report UI
  output$download_report_ui <- renderUI({
    if (report_available()) {
      div(
        style = "margin-top: 15px;",
        downloadButton("download_report", "Download Report", class = "btn-success w-100")
      )
    }
  })

  # Download handler for report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(tmp.snr, "_results_", tmp.audience, ".pdf")
    },
    content = function(file) {
      report_path <- file.path("res", paste0(tmp.snr, "_2024"),
                               paste0(tmp.snr, "_results_", tmp.audience, ".pdf"))

      if (!file.exists(report_path)) {
        stop("Report file not found at: ", report_path)
      }
      file.copy(report_path, file)
    },
    contentType = "application/pdf"
  )

  # Handle report generation
  observeEvent(input$generate_report, {
    is_running(TRUE)
    report_available(FALSE)

    tryCatch({
      run_Parallel(
        snr = tmp.snr,
        audience = tmp.audience,
        stype = tmp.stype,
        ubb = tmp.ubb,
        ganztag = tmp.ganztag,
        results = tmp.results
      )
      is_running(FALSE)
      report_available(TRUE)
      showNotification("Report generation completed!", type = "message")
    },
    error = function(e) {
      is_running(FALSE)
      report_available(FALSE)
      showNotification(
        paste("Error generating report:", e$message),
        type = "error"
      )
    })
  })
}

shinyApp(ui, server)
