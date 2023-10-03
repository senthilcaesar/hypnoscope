library(shiny)
library(luna)
library(lubridate)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)
library(datamods)

options(shiny.maxRequestSize = 2000 * 1024^2)
source('/Users/sq566/hypnoscope/ui.R', local = TRUE)
source('/Users/sq566/hypnoscope/server.R')

shinyApp(ui = ui, server = server)