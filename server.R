# server.R

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Values, an object that is created outside a reactive expression is updated depending
  # on inputs but is not re-executed whenever an input changes.
  values <- reactiveValues(opt = list())

  #-------------------------------- Component 1 -------------------------------#
  observeEvent(input$upload1, {
    # Set to null
    values$file.details <- NULL
    edf.name <- edf.path <- NULL
    annot.names <- annot.paths <- NULL

    idx <- grep(".eannot", ignore.case = T, input$upload1$name)
    annot.names <- input$upload1$name[idx]
    annot.paths <- input$upload1$datapath[idx]

    # Take a single EDF
    edf.name <- "shhs1-204679.edf"
    edf.path <- "data/shhs1-204679.edf"

    # Input file details
    values$file.details <- list(
      edf.name = edf.name,
      edf.path = edf.path,
      annot.names = annot.names,
      annot.paths = annot.paths
    )
  })

  # --------- Copy and Paste module----------------

  imported <- import_copypaste_server("myid",
    btn_show_data = FALSE,
    fread_args = list(col.names = "Annots", header = F, blank.lines.skip = T)
  )

  toListen <- reactive({
    list(imported$status(), imported$data())
  })

  observeEvent(toListen(), {
    if (!is.null(imported$status()) && !is.null(imported$data())) {
      reset(id = "upload1")
      txtPath <- tempfile(fileext = ".eannot")
      for (stage in imported$data()) cat(stage, "\n", file = txtPath, sep = "\n", append = TRUE)

      edf.name <- "shhs1-204679.edf"
      edf.path <- "data/shhs1-204679.edf"
      annot.names <- basename(txtPath)
      annot.paths <- txtPath

      # Input file details
      values$file.details <- list(
        edf.name = edf.name,
        edf.path = edf.path,
        annot.names = annot.names,
        annot.paths = annot.paths
      )
    }
  })



  #--------------------------------------------


  # initiate view w/ an example data set
  observeEvent(input$load.default, {
    reset(id = "upload1")

    values$file.details <-
      list(
        edf.name = "learn-nsrr02.edf",
        edf.path = "data/learn-nsrr02.edf",
        annot.names = "learn-nsrr02.xml",
        annot.paths = "data/learn-nsrr02.xml"
      )
    # Reset Shiny UI components
  })


  load.data <- observeEvent(values$file.details, {
    # pull file names
    edf.name <- values$file.details[["edf.name"]]
    edf.path <- values$file.details[["edf.path"]]
    annot.names <- values$file.details[["annot.names"]]
    annot.paths <- values$file.details[["annot.paths"]]

    # clear old data out
    try(lrefresh())
    try(ldrop())

    # clear all
    values$opt <- NULL
    values$elen <- 30

    updateTabsetPanel(inputId = "tabset", selected = "N = 1")

    # register that we have new data attached
    values$hasedf <- !is.null(edf.name)
    values$hasannots <- !is.null(annot.names)
    values$hasdata <- values$hasedf | values$hasannots

    if (values$hasedf) {
      values$opt[["edfname"]] <- edf.name
      values$opt[["edfpath"]] <- edf.path
    }

    if (values$hasannots) {
      values$opt[["annotnames"]] <- annot.names
      values$opt[["annotpaths"]] <- annot.paths
    }

    # some brief console output
    cat(" has data?", values$hasdata, "\n")
    cat(" has edf?", values$hasedf, "\n")
    cat(" has annotations?", values$hasannots, "\n")

    if (values$hasedf) {
      # attach EDF
      cat("attaching", values$opt[["edfpath"]], "\n")
      ledf(values$opt[["edfpath"]])

      # read all EDF+ annotations as class-level
      # so that they show in the display
      lset("edf-annot-class-all", "T")

      # add any annotations
      for (a in values$opt[["annotpaths"]]) {
        cat("attaching", a, "\n")
        ladd.annot.file(a)
      }

      # kick off initial analyses
      init()
    }
  })

  init <- function() {
    values$elen <- 30
    ne <- lepoch()
    if (ne == 0) values$elen <- 1

    ret <- leval(paste("EPOCH dur=", values$elen, " verbose & SEGMENTS", sep = ""))
    values$opt[["init.epochs"]] <- ret$EPOCH$E
    values$opt[["ne"]] <- dim(ret$EPOCH$E)[1]
    values$opt[["init.secs"]] <- max(ret$EPOCH$E$STOP)
    values$opt[["init.segidx"]] <- ret$SEGMENTS$SEG[, c("START", "STOP")]
    session$resetBrush("hypno_brush")
    # Get stage-aligned epochs and hypnogram
    ret <- leval(paste("EPOCH align verbose dur=", values$elen, sep = ""))
    values$opt[["ne.aligned"]] <- dim(ret$EPOCH$E)[1]
    values$opt[["init.epochs.aligned"]] <- ret$EPOCH$E
    ret <- leval("HYPNO epoch")

    stgs <- lstages()
    values$hasstaging <- !is.null(stgs)
    values$variable.staging <- F
    if (values$hasstaging) {
      stgs <- stgs[stgs == "N1" | stgs == "N2" | stgs == "N3" | stgs == "R" | stgs == "W"]
      nstgs10 <- sum(table(stgs) >= 10)
      values$variable.staging <- nstgs10 >= 2
    }
    if (values$hasstaging) {
      values$opt[["hypno.stats"]] <- ret$HYPNO$BL
      values$opt[["hypno.epochs"]] <- ret$HYPNO$E
      values$opt[["all.hypno.epochs"]] <- ret$HYPNO$E
      values$opt[["ss"]] <- ret$HYPNO$E[, c("E", "STAGE", "START_SEC")]
      values$opt[["ss"]]$STOP_SEC <- values$opt[["ss"]]$START_SEC + values$elen
      values$opt[["hypno.cycles"]] <- ret$HYPNO$C
      values$opt[["hypno.stages"]] <- ret$HYPNO$SS
    }

    # report to console
    cat(" # epochs (raw)", values$opt[["ne"]], "\n")
    cat(" # epochs (stage-aligned)", values$opt[["ne.aligned"]], "\n")
    cat(" has-staging?", values$hasstaging, "\n")

    output$hypno1 <- renderPlot({
      req(values$hasedf, values$hasstaging)
      par(mar = c(0, 0, 0, 0))
      lhypno(values$opt[["hypno.epochs"]]$STAGE,
        cycles = values$opt[["hypno.epochs"]]$CYCLE,
        times = values$opt[["init.epochs.aligned"]]$START
      )
    })

    output$text.header1a <- renderText({
      req(values$hasstaging)
      values$opt[["annotnames"]]
    })

    # Base-level EDF headers output
    output$table.header3 <- DT::renderDataTable({
      req(values$hasedf)
      df <- values$opt[["header1"]]
      df$ID <- df$EDF_ID <- NULL
      df <- df[, c("EDF_TYPE", "NS", "START_DATE", "START_TIME", "STOP_TIME", "REC_DUR_HMS", "REC_DUR_SEC", "EPOCH", "TOT_DUR_HMS", "TOT_DUR_SEC", "NR", "REC_DUR")]
      df <- data.frame(t(df))
      df$VAR <- rownames(df)
      names(df) <- c("Value", "Variable")
      df <- df[, c("Variable", "Value")]
      df$Variable <- c(
        "EDF type", "Number of signals", "Start date", "Start time", "Stop time", "Duration (h:m:s)", "Duration (sec)", "Duration (epochs)",
        "Total duration (h:m:s)", "Total duration (sec)", "Number of records", "Record duration (sec)"
      )

      DT::datatable(df,
        extensions = c("Buttons"),
        options = list(
          scrollY = "375px",
          paging = F,
          info = FALSE,
          searching = FALSE,
          dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        ),
        rownames = FALSE
      )
    })

    # Channel-wise EDF headers output
    output$table.header2 <- DT::renderDataTable({
      req(values$hasedf)
      DT::datatable(values$opt[["header2"]],
        extensions = c("Buttons"),
        options = list(
          scrollY = "375px",
          paging = F,
          info = FALSE,
          searching = FALSE,
          dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      )
    })
  }

  # -------------------Hypnogram statistics------------------------------------#

  output$table.hypno <- DT::renderDataTable({
    req(values$hasedf, values$hasstaging, values$variable.staging)

    m <- as.data.frame(matrix(
      c(
        "TRT", "Total Recording Time, based on scored epochs (T0 – T6) (mins)",
        "TIB", "Time In Bed: Lights Off to Lights On (mins) (T1 – T5) (mins)",
        "SPT", "Sleep period time: Sleep Onset to Final Wake (T2 – T4) (mins)",
        "SPT_PER", "Persistent Sleep Period time: Persistent Sleep Onset to Final Wake (mins)",
        "LOT", "Lights On Time (mins)",
        "TWT", "Total Wake time during Lights Off = SLP_LAT + WASO + FWT (mins)",
        "TST", "Total Sleep Time (mins)",
        "TST_PER", "Total Persistent Sleep Time (mins)",
        " ", "",
        "SME", "Sleep maintenance efficiency, TST / SPT (denom. = T2 – T4)",
        "SE", "Sleep efficiency, TST / TIB (denom. = T1 – T5)",
        "WASO", "Wake time between sleep onset and final wake onset (T2 – T4)",
        "FWT", "Duration of wake from final wake onset to Lights on (T4 – T5)",
        "SOL", "Sleep latency (T1 – T2)",
        "SOL_PER", "Persistent sleep latency (T1 to onset of persistent sleep)",
        "REM_LAT", "REM latency (sleep onset T2 – first REM epoch)",
        "REM_LAT2", "REM latency excluding W, i.e. elapsed NR at REM onset",
        "NREMC", "Number of NREM cycles",
        "NREMC_MINS", "Mean NREM cycle duration (mins)",
        "  ", "",
        "CONF", "Number of epochs w/ conflicting stages (should be 0)",
        "SINS", "Recording starts in sleep (0=N, 1=Y)",
        "EINS", "Recording ends in sleep (0=N, 1=Y)",
        "OTHR", "Duration of non-sleep/non-wake annotations (unknown/movement)",
        "FIXED_WAKE", "Excessive leading/trailing Wake epochs set to L",
        "FIXED_LIGHTS", "Number of epochs set or L before/after lights out/on",
        "FIXED_SLEEP", "Sleep epochs set to L due to extreme WASO intervals",
        "LOT", "Lights On duration (mins)",
        "LOST", "Lights On Sleep duration (mins) : sleep set to L (should be 0)",
        "   ", "",
        "SFI", "Sleep Fragmentation Index: Sleep/W transition count / TST",
        "TI_S", "Stage Transition Index (excludes W): N1/N2/N3/R transition count / TST",
        "TI_S3", "3-class Stage Transition Index: NR/R/W transition count / SPT",
        "TI_RNR", "REM/NREM Transition Index, NR/R transition count / TST",
        "LZW", "LZW complexity index"
      ),
      ncol = 2, byrow = T
    ))

    # spacers
    values$opt[["hypno.stats"]][, " "] <- NA
    values$opt[["hypno.stats"]][, "  "] <- NA
    values$opt[["hypno.stats"]][, "   "] <- NA

    # add in values
    m$VALUE <- round(as.numeric(values$opt[["hypno.stats"]][, m[, 1]]), 3)

    DT::datatable(m,
      options = list(
        scrollY = "380px",
        dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
        ordering = F, paging = F,
        info = FALSE,
        searching = FALSE
      ),
      rownames = FALSE, colnames = rep("", 3)
    )
  })

  output$table.hypno.times <- DT::renderDataTable({
    req(values$hasedf, values$hasstaging, values$variable.staging)

    m <- as.data.frame(matrix(
      c(
        "X0_START", "Study Start",
        "X1_LIGHTS_OFF", "Lights Off time (or start of recording)",
        "X2_SLEEP_ONSET", "Sleep Onset time",
        "X3_SLEEP_MIDPOINT", "Mid-point of T2 & T4",
        "X4_FINAL_WAKE", "Final Wake Onset time",
        "X5_LIGHTS_ON", "Lights On time (or end of recording)",
        "X6_STOP", "Study Stop"
      ),
      ncol = 2, byrow = T
    ))
    m$HMS <- as.character(values$opt[["hypno.stats"]][, gsub("X", "HMS", m[, 1])])
    m$E <- round(as.numeric(values$opt[["hypno.stats"]][, gsub("X", "E", m[, 1])]), 3)
    #    m$T <- round(as.numeric(values$opt[["hypno.stats"]][, gsub("X", "T", m[, 1])]), 3)
    m$EPOCH <- floor(m$E * 2) + 1

    DT::datatable(m,
      options = list(
        ordering = F, pageLength = dim(m)[1],
        buttons = list(list(extend = "copy", text = "Copy")),
        lengthChange = FALSE,
        dom = "tB", info = FALSE,
        searching = FALSE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      ),
      rownames = FALSE,
      colnames = c("Time-point", "Description", "Clock-time", "Elapsed (mins)", "Epoch")
    )
  })

  # ------------------------------------------------------------
  # Hypnogram stage statistics
  #

  output$table.hypno.stages <- DT::renderDataTable({
    req(values$hasedf, values$hasstaging, values$variable.staging)

    dt <- values$opt[["hypno.stages"]]
    dt <- dt[dt$SS %in% c("?", "N1", "N2", "N3", "R", "S", "W", "WASO"), ]
    dt$EDUR <- as.integer(dt$MINS * 2)
    dt <- dt[, c("SS", "MINS", "EDUR", "PCT", "BOUT_MD", "BOUT_N")]
    dt$PCT <- round(100 * dt$PCT, 2)
    names(dt) <- c("Stage", "Duration (m)", "Duration (e)", "Duration (%)", "Median-bout(m)", "N-bouts")
    DT::datatable(dt,
      options = list(
        scrollY = "300px",
        dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
        info = FALSE,
        searching = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  })

  # ------------------------------------------------------------
  # Hypnogram cycle statistics
  #

  output$table.hypno.cycles <- DT::renderDataTable({
    req(values$hasedf, values$hasstaging, values$variable.staging)
    dt <- values$opt[["hypno.cycles"]]
    dt$NUM <- 1:(dim(dt)[1])
    dt <- dt[, c("NUM", "NREMC_START", "NREMC_N", "NREMC_MINS", "NREMC_NREM_MINS", "NREMC_REM_MINS")]
    names(dt) <- c("Cycle", "Start(E)", "Duration(E)", "Duration(m)", "NREM-duration(m)", "REM-duration(m)")
    DT::datatable(dt,
      options = list(
        scrollY = "300px",
        dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
        info = FALSE,
        searching = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  })

  # ------------------------------------------------------------
  # Hypnogram epoch-statistics

  output$table.hypno.epochs <- DT::renderDataTable({
    req(values$hasedf, values$hasstaging, values$variable.staging)

    dt <- values$opt[["hypno.epochs"]]
    dt <- dt[, c("E", "CLOCK_TIME", "MINS", "STAGE", "CYCLE", "PERSISTENT_SLEEP", "WASO", "E_N1", "E_N2", "E_N3", "E_REM", "E_SLEEP", "E_WASO")]
    names(dt) <- c("E", "Clock", "Mins", "Stage", "Cycle", "Per-Sleep", "WASO", "E(N1)", "E(N2)", "E(N3)", "E(REM)", "E(S)", "E(WASO)")
    #    dt$PCT <- round( dt$PCT , 2 )

    DT::datatable(dt,
      options = list(
        scrollY = "350px",
        dom = "tB", buttons = list(list(extend = "copy", text = "Copy")),
        info = FALSE, paging = F,
        searching = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  })
  # -------------------- End of Hypnogram statistics ---------------------------#

  #-------------------------------- End of component1--------------------------#


  #-------------------------------- Component1---------------------------------#
  observeEvent(input$upload2, {
    d <- read.table(input$upload2$datapath, header = T, stringsAsFactors = F)
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

  # Create reactive expressions with reactive({ })
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
  # The Hypnoscope visualization app has one reactive object, a plot named "hypno"
  # The plot is built with the image function
  output$hypno2 <- renderPlot({
    req(input$upload2)

    # Shiny will automatically re-build the data object if an input value in the renderPlot function changes
    # data is a reactive object the depends on user widget input ("ultradian")
    data <- switch(input$ultradian2,
      CLOCK_TIME = clockTime(), # Call reactive expression
      ONSET = onset()
    )
    image(data, useRaster = T, col = values$opt[["stgpal"]], xaxt = "n", yaxt = "n", axes = F, breaks = 0.5 + (0:6))
  })
}
