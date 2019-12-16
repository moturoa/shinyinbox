library(shiny)
library(lubridate)
library(DT)
library(dplyr)
library(RSQLite)
library(dbplyr)
library(glue)

source("shinyinbox.R")

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
             
             
             textAreaInput("txt_message", "Bericht"),
             
             selectInput("txt_message_user_tags",
                         label="Notificatie gebruiker",
                         choices = letters, #all_users,
                         selected = NULL,
                         multiple=TRUE),
             
             actionButton("txt_send", "Opslaan", icon = icon("envelope")),
             tags$br(),
             textOutput("txt_bericht_user"),
             actionButton("testbtn","test"),
             textOutput("testtxt")
           )
    )  
  )

  
  
  
)

server <- function(input, output, session) {

  output$txt_bericht_user <- renderText({
    paste("Bericht wordt verstuurd als: ", "get_naam_user(current_user)")
  })
  
  output$testtxt <- renderText(input$testid)
  
  
  observeEvent(input$txt_send, {
    
    if(input$txt_message != ""){
      
      msg_in <- tibble(id = uuid::UUIDgenerate(),
                        msg = input$txt_message,
                        users = "", #paste(replace_null_emptychar(input$txt_message_user_tags),collapse=";"),
                        sender = "current_user",
                        attachment = "",
                        timestamp = now(tz="UTC"),
                        timestamp_modification = now(tz="UTC"),
                        deleted = FALSE)
      
      dbWriteTable(msg$connection, msg$table, msg_in, append = TRUE)
      
      updateTextAreaInput(session, "txt_message", value = "")
      updateSelectInput(session, "txt_message_user_tags", selected = "")
      
      
    }
    
  })  
  
  messages_db <- reactivePoll(2000, session,
                              
                              checkFunc = function(){
                                collect(tbl(msg$connection, 
                                            sql(glue("SELECT max(timestamp_modification) FROM {msg$table} ")))
                                )[[1]]
                              },
                              valueFunc = function(){
                                dbReadTable(msg$connection, msg$table)
                              }
  )
  
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

