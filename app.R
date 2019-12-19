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
  delete_rights = TRUE)




ui <- fluidPage(
  
  includeScript("www/selectedMessages.js"),
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
             
             composeBoxUI("send1")
            
           )
    )  
  )

  
  
  
)

server <- function(input, output, session) {

  
  
  messages_db <- reactivePoll(500, session,
                              
                              checkFunc = function(){
                                collect(tbl(msg$connection, 
                                            sql(glue("SELECT max(timestamp_modification) FROM {msg$table} ")))
                                )[[1]]
                              },
                              valueFunc = function(){
                                dbReadTable(msg$connection, msg$table)
                              }
  )
  
  
  callModule(composeBox, "send1", msg)
  
  observe({
    callModule(shinyinbox, "box1", 
               messages = messages_db(), 
               connection = msg$connection, 
               tableName = msg$table,
               edit_rights = msg$edit_rights, 
               delete_rights = msg$delete_rights)  
  })
  
    
}

shinyApp(ui, server)

