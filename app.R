library(shiny)
library(lubridate)
library(DT)
library(dplyr)
library(RSQLite)
library(dbplyr)
library(glue)

source("R/shinyinbox.R")
source("R/composeBox.R")
source("R/utils.R")


# Configuratie object voor messages module
msg <- list(
  connection = dbConnect(RSQLite::SQLite(), "data/messages.sqlite"),
  table = "messages",
  edit_rights = TRUE,
  delete_rights = TRUE,
  poll_delay = 500)




ui <- fluidPage(
  
  includeScript("www/selectedMessages.js"),
  
  tags$h3("Main inbox"),
  tags$p("Alle berichten komen hier terecht."),
      fluidRow(
        column(6,
               tags$div(
                 style = "padding: 50px; border: 1px solid gray; width: 500px;",
                 
                 shinyinboxUI("box1")
               )
        ),
        column(6, 
               tags$div(
                 style = "padding: 50px; border: 1px solid gray; width: 500px;",
                 
                 composeBoxUI("send1", users = letters),
                 
               )
        )  
      ),
    
  tags$hr(),
  tags$h3("Local inbox"),
  tags$p("Berichten hier geschreven komen hier terecht, en in de main inbox (maar niet andersom)"),
      fluidRow(
        column(6,
               tags$div(
                 style = "padding: 50px; border: 1px solid aqua; width: 500px;",
                 
                 shinyinboxUI("box2")
               )
        ),
        column(6, 
               tags$div(
                 style = "padding: 50px; border: 1px solid aqua; width: 500px;",
                 
                 composeBoxUI("send2", users = c("Bob","Marie","Peter","Floor"))
                 
               )
        )  
      
    )
  

)


server <- function(input, output, session) {

  callModule(composeBox, "send1", msg)
  callModule(shinyinbox, "box1", msg)
  
  callModule(composeBox, "send2", msg, attachment = "pandmessage")
  callModule(shinyinbox, "box2", msg, filter_attachment = "pandmessage")

}

shinyApp(ui, server)

