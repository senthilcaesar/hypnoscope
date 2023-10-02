ui <- navbarPage(
  useShinyjs(),
  id = "tabset",
  title = "Hypnoscope",

  # Component 1
  tabPanel(
    title = "N = 1",
    fluidRow(
      column( # Sidebar
        2,
        # style = "background-color:#f2f0eb; border-top: 100px; height: 800px; width: 250px;",
        # Input: Select a file ----
        fileInput("upload1",
          label = NULL,
          multiple = TRUE,
          accept = c(
            ".csv",
            ".eannot",
            ".hypnos"
          )
        ),
        textOutput("text.header1a"),
        hr(style = "border-color: #d9d9d9"),
        actionButton("load.default", "Example")
      ),
      column( # Main
        10,
        plotOutput("hypno1",
          width = "100%", height = "40px",
          dblclick = "hypno_dblclick",
          brush = brushOpts(id = "hypno_brush", direction = "x", resetOnNew = F)
        ),
        div(style = "margin-top: 10px"),
        tabsetPanel(
          id = "maintabs",
          tabPanel(
            "Headers",
            fluidRow(
              column(4, DT::dataTableOutput("table.header3")),
              column(8, DT::dataTableOutput("table.header2"))
            )
          ),
          tabPanel(
            "Hypnogram",
            tabsetPanel(
              tabPanel("Summaries", DT::dataTableOutput("table.hypno", width = "100%")),
              tabPanel("Times", DT::dataTableOutput("table.hypno.times", width = "100%")),
              tabPanel("Stages", DT::dataTableOutput("table.hypno.stages")),
              tabPanel("Cycles", DT::dataTableOutput("table.hypno.cycles")),
              tabPanel("Epochs", DT::dataTableOutput("table.hypno.epochs")),
            )
          )
        ),
      )
    )
  ),


  # Component 2
  tabPanel(
    title = "N > 1",
    fluidRow(
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
        selectInput(inputId = "ultradian2", label = "Select Ultradian dynamics", choices = c("CLOCK_TIME", "ONSET"), selected = "CLOCK_TIME", multiple = F)
      ),
      column(
        10,
        plotOutput("hypno2")
      )
    )
  )
)
