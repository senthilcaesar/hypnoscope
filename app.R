library(shiny)
library(luna)
library(lubridate)
library(shinybusy)

options(shiny.maxRequestSize = 2000 * 1024^2)

ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("Hypnoscope"), # Add a title panel
  tags$hr(),
  fluidRow(
    column(
      2,
      # Input: Select a file ----
      fileInput("upload", "Choose File",
        multiple = TRUE,
        accept = c(
          ".csv",
          ".tsv",
          ".hypnos"
        )
      ),
      selectInput(inputId = "ultradian", label = "Select Ultradian dynamics", choices = c("CLOCK_TIME", "ONSET"), selected = "CLOCK_TIME", multiple = F)
    ),
    column(
      10,
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Values, an object that is created outside a reactive expression is updated depending
  # on inputs but is not re-executed whenever an input changes.
  values <- reactiveValues(opt = list())

  observeEvent(input$upload, {
    d <- read.table(input$upload$datapath, header = T, stringsAsFactors = F)
    names(d)[4] <- "SS"

    ni <- length(unique(d$ID))
    nf <- length(d$ID[d$E == 1])
    if (ni != nf) {
      cat("not all indivs have first epoch (E==1)...\n")
      d <- d[d$ID %in% d$ID[d$E == 1], ]
    }

    # Get first epoch for each individual
    d1 <- d[d$E == 1, ]

    # Convert HMS to seconds
    secs <- lubridate::period_to_seconds(lubridate::hms(d1$CLOCK_TIME))

    # Align to 30 sec epoch
    secs <- 30 * floor(secs / 30)

    # Need clarification ( secs less than 12 hrs )
    secs[secs < 43200] <- secs[secs < 43200] + 86400

    # Get earliest time point
    td <- seconds_to_period(min(secs))
    first.epoch <- sprintf("%02d:%02d:%02d", td@hour, minute(td), second(td))

    # Get epoch-wise starts for all people
    d1$E1 <- (secs - min(secs)) / 30

    # Adjust epoch counts : EA = aligned epochs (by clock)
    d <- merge(d, d1[, c("ID", "E1")], by = "ID")
    d$EA <- d$E + d$E1

    # Get key anchors for each individual: sleep onset / offset, lights, sleep midpoint
    d$SLEEP <- as.integer(d$SS %in% c("N1", "N2", "N3", "R"))
    d1 <- as.data.frame(tapply(d$EA[d$SLEEP == 1], d$ID[d$SLEEP == 1], min))
    names(d1) <- "T2"
    d1$ID <- rownames(d1) # Timed epoch when subject starts to sleep

    # nb. do not assume all individuals will have sleep... thus all.x = T
    d <- merge(d, d1[, c("ID", "T2")], by = "ID", all.x = T)

    # Align to T2 == 0
    d$E2 <- d$EA - d$T2

    values$opt[["data"]] <- d
    values$opt[["ni"]] <- ni
    values$opt[["stgpal"]] <- c(lstgcols("N3"), lstgcols("N2"), lstgcols("N1"), lstgcols("R"), lstgcols("W"), lstgcols("?"))
  }) # End of observeEvent (input$upload)

  # Reaction expression for ONSET computation
  # Results are cached the first time the expression is called
  onset <- reactive({
    dmin <- tapply(values$opt[["data"]]$E2, values$opt[["data"]]$ID, min)
    dmax <- tapply(values$opt[["data"]]$E2, values$opt[["data"]]$ID, max)
    ids <- unique(values$opt[["data"]]$ID)
    mindmin <- min(dmin)
    dmin <- dmin - mindmin + 1
    dmax <- dmax - mindmin + 1
    ne <- max(values$opt[["data"]]$E2) - min(values$opt[["data"]]$E2) + 1
    m <- matrix(NA, nrow = ne, ncol = values$opt[["ni"]])
    for (i in 1:values$opt[["ni"]]) m[(dmin[i]):(dmax[i]), i] <- 4 + lstgn(values$opt[["data"]]$SS[values$opt[["data"]]$ID == ids[i]])
    m
  })

  # Reaction expression for CLOCK_TIME computation
  # Results are cached the first time the expression is called
  clockTime <- reactive({
    dmin <- tapply(values$opt[["data"]]$EA, values$opt[["data"]]$ID, min)
    dmax <- tapply(values$opt[["data"]]$EA, values$opt[["data"]]$ID, max)
    ids <- unique(values$opt[["data"]]$ID)
    ne <- max(values$opt[["data"]]$EA) - min(values$opt[["data"]]$EA) + 1
    m <- matrix(NA, nrow = ne, ncol = values$opt[["ni"]])
    for (i in 1:values$opt[["ni"]]) m[(dmin[i]):(dmax[i]), i] <- 4 + lstgn(values$opt[["data"]]$SS[values$opt[["data"]]$ID == ids[i]])
    m
  })

  # Default plot CLOCK_TIME
  output$plot <- renderPlot({
    req(input$upload)
    data <- switch(input$ultradian,
      CLOCK_TIME = clockTime(),
      ONSET = onset()
    )
    image(data, useRaster = T, col = values$opt[["stgpal"]], xaxt = "n", yaxt = "n", axes = F, breaks = 0.5 + (0:6))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
