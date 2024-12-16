library(ReportMaster)
library(shiny)
library(bslib)

# Initialize reactive values globally
appParams <- reactiveVal(NULL)

# Initialize reactiveValues for meta and data
meta_data <- reactiveValues(
  meta = NULL,
  data = NULL
)

ui <- function(request) {
  fluidPage(
    uiOutput("mainOrModal")
  )
}

server <- function(input, output, session) {
  # Initialize reactive values
  report_available <- reactiveVal(FALSE)
  is_running <- reactiveVal(FALSE)

  # Show the modal dialog at startup
  observe({
    if (is.null(appParams())) {
      showModal(modalDialog(
        title = "Set Initial Parameters",
        textInput("modal_snr", "School Number (SNR)", value = "8934"),
        selectInput("modal_audience", "Audience",
                    choices = c("sus" = "sus", "other_option" = "other_option", value = "sus")),
        selectInput("modal_stype", "School Type",
                    choices = c("gm" = "gm", "other_type" = "other_type", value = "gm")),
        checkboxInput("modal_ubb", "UBB", value = FALSE),
        checkboxInput("modal_ganztag", "Ganztag", value = FALSE),
        footer = tagList(
          actionButton("modal_submit", "Start Application", class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE,
        closeButton = FALSE
      ))
    }
  })

  # When modal submit button is clicked
  observeEvent(input$modal_submit, {
    validate(
      need(input$modal_snr != "", "Please enter a school number")
    )

    # Store parameters
    appParams(list(
      snr = input$modal_snr,
      audience = input$modal_audience,
      stype = input$modal_stype,
      ubb = input$modal_ubb,
      ganztag = input$modal_ganztag,
      results = "Blala"
    ))

    # Initialize with new parameters
    params <- appParams()
    get_parameter(
      snr = params$snr,
      audience = params$audience,
      ubb = params$ubb,
      ganztag = params$ganztag,
      stype = params$stype
    )

    # Store meta and data in reactiveValues
    meta_data$meta <- get("tmp.meta", envir = .GlobalEnv)
    meta_data$data <- get("tmp.data", envir = .GlobalEnv)

    # Check if report exists
    report_path <- file.path("res", paste0(params$snr, "_2024"),
                             paste0(params$snr, "_results_", params$audience, ".pdf"))
    report_available(file.exists(report_path))

    removeModal()
  })

  # Render either the modal or main UI
  output$mainOrModal <- renderUI({
    if (is.null(appParams()) || is.null(meta_data$meta)) {
      return(NULL)
    }

    params <- appParams()
    schoolname <- get_sname(snr = params$snr)

    page_sidebar(
      title = "Report Plots Viewer",

      sidebar = sidebar(
        width = "33%",

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
              paste("School ID:", params$snr)
            )
          )
        ),

        selectInput("selected_plot", "Select Plot:",
                    choices = meta_data$meta,
                    selected = meta_data$meta[1]),

        downloadButton("download", "Download Plot", class = "btn-standard w-100"),

        hr(),

        uiOutput("generate_report_ui"),
        uiOutput("progress_ui"),
        uiOutput("download_report_ui")
      ),

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
  })

  # Dynamic card header for plot
  output$dynamic_card_header <- renderUI({
    req(input$selected_plot)
    card_header(input$selected_plot)
  })

  # Generate plot function
  generate_plot <- function() {
    params <- appParams()
    export_plot(
      meta = input$selected_plot,
      snr = params$snr,
      audience = params$audience,
      report = params$audience,
      ubb = params$ubb,
      data = tmp.data,
      export = FALSE
    )
  }

  # Display the plot
  output$plot1 <- renderPlot({
    req(input$selected_plot, meta_data$data)
    generate_plot()
  })

  # Plot explanations accordion
  output$plot_explanation_accordion <- renderUI({
    req(input$selected_plot)

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
        multiple = TRUE
      )
    } else {
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
      params <- appParams()
      paste0(params$snr, "_results_", params$audience, ".pdf")
    },
    content = function(file) {
      params <- appParams()
      report_path <- file.path("res", paste0(params$snr, "_2024"),
                               paste0(params$snr, "_results_", params$audience, ".pdf"))

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

    params <- appParams()

    tryCatch({
      ReportMaster::run_Parallel(
        snr = params$snr,
        audience = params$audience,
        stype = params$stype,
        ubb = params$ubb,
        ganztag = params$ganztag,
        results = params$results
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
