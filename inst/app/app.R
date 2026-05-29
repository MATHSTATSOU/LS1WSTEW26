library(shiny)
library(plotly)
library(gt)
library(fs)
library(DT)
library(ggplot2)
library(bslib)

# Use package function
analyze_dir <- LS1WSTEW26::analyze_dir

# ---- Read defaults from run_app ----
default_dir  <- getOption("dirspect.dir_path", fs::path_home())
default_top  <- getOption("dirspect.top_n", 10)
default_hash <- getOption("dirspect.hash", FALSE)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),

  titlePanel("📁 Directory Explorer App"),

  sidebarLayout(

    sidebarPanel(

      textInput("root", "Root Folder:", value = default_dir),

      actionButton("load", "Load Folders"),

      selectInput("dir", "Select Folder:", choices = NULL),

      actionButton("up", "Go Up"),

      hr(),

      numericInput("top_n", "Top N", value = default_top),

      checkboxInput("hash", "Detect duplicates", value = default_hash),

      actionButton("run", "Analyze")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Files", DTOutput("files")),
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Summary", gt_output("table")),
        tabPanel("Duplicates", DTOutput("dup"))
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(path = NULL)

  # ---- Initialize automatically ----
  observe({

    if (dir.exists(default_dir)) {

      dirs <- fs::dir_ls(default_dir, type = "directory")

      updateSelectInput(session, "dir",
                        choices = dirs,
                        selected = dirs[1])

      rv$path <- dirs[1]
    }
  })

  observeEvent(input$dir, {
    rv$path <- input$dir
  })

  observeEvent(input$load, {

    req(dir.exists(input$root))

    dirs <- fs::dir_ls(input$root, type = "directory")

    rv$path <- input$root

    updateSelectInput(session, "dir",
                      choices = dirs,
                      selected = dirs[1])
  })

  observeEvent(input$up, {

    req(rv$path)

    parent <- dirname(rv$path)

    if (dir.exists(parent)) {
      dirs <- fs::dir_ls(parent, type = "directory")

      rv$path <- parent

      updateSelectInput(session, "dir",
                        choices = dirs,
                        selected = parent)
    }
  })

  result <- eventReactive(input$run, {

    req(rv$path)

    analyze_dir(
      path = rv$path,
      top_n = input$top_n,
      hash = input$hash
    )
  })

  output$files <- renderDT({
    req(result())
    datatable(result()$df)
  })

  output$plot <- renderPlotly({
    req(result())
    plotly::ggplotly(result()$plot, tooltip = "text")
  })

  output$table <- render_gt({
    req(result())
    result()$table
  })

  output$dup <- renderDT({
    req(result())
    if (is.null(result()$dup)) return(NULL)
    datatable(result()$dup)
  })
}

shinyApp(ui, server)
