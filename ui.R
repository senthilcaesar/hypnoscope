library(shiny)
library(luna)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)
library(datamods)
library(lubridate)
library(shinydashboard)

ui <- navbarPage(
  useShinyjs(),
  id = "tabset",
  title = "Hypnoscope",


  # Component 1
  tabPanel(
    title = "N = 1",
    fluidRow(
      add_busy_spinner(spin = "fading-circle"),
      column( # Sidebar
        2,
        # style = "background-color:#f2f0eb; border-top: 100px; height: 800px; width: 250px;",
        # Input: Select a file ----
        fileInput("upload1",
          label = NULL,
          multiple = TRUE,
          accept = c(
            ".annot",
            ".eannot",
            ".xml",
            ".hypnos"
          )
        ),
        textOutput("text.header1a"),
        hr(style = "border-color: #d9d9d9"),
        actionButton("load.default", "Example"),
        hr(style = "border-color: #d9d9d9"),
        import_copypaste_ui(id = "myid", title = NULL, name_field = F)
      ),
      column( # Main
        10,
        plotOutput("hypno1",
          width = "100%", height = "200px",
          dblclick = "hypno_dblclick",
          brush = brushOpts(id = "hypno_brush", direction = "x", resetOnNew = F)
        ),
        div(style = "margin-top: 50px"),
        tabsetPanel(
          id = "maintabs",
          tabPanel(
            "Summaries",
            fluidRow(
              column(7, DT::dataTableOutput("table.hypno")),
              column(5, DT::dataTableOutput("table.hypno.stages"))
            )
          ),
          tabPanel("Times", DT::dataTableOutput("table.hypno.times")),
          tabPanel("Cycles", DT::dataTableOutput("table.hypno.cycles")),
          tabPanel("Epochs", DT::dataTableOutput("table.hypno.epochs")),
        ),
      )
    )
  ),


  # Component 2


  tabPanel(
    title = "N > 1",
    fluidRow(
      add_busy_spinner(spin = "fading-circle"),
      column(
        2,
        # Input: Select a file ----
        fileInput("upload2",
          label = NULL,
          multiple = TRUE,
          accept = c(
            ".csv",
            ".tsv",
            ".hypnos"
          )
        ),
        textOutput("text.header2a"),
        hr(style = "border-color: #d9d9d9"),
        actionButton("load.default2", "Example"),
        hr(style = "border-color: #d9d9d9"),
        selectInput(inputId = "ultradian2", label = "Select Ultradian dynamics", choices = c("CLOCK_TIME", "ONSET"), selected = "CLOCK_TIME", multiple = F),
        selectInput("sort", label = h5("Sort by"), choices = c("Choose" = "", c("NULL")), multiple = F, selectize = T),
        uiOutput(outputId = "n")
      ),
      column(
        10,
        tags$head(tags$style(
          type = "text/css",
          "#myImage img {max-width: 100%; width: 1200; height: auto}"
        )),
        box(
          style = "width:1200px;overflow-x: scroll;height:800px;overflow-y: scroll;",
          imageOutput("myImage")
        )
      )
    )
  )
)
