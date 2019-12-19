
composeBoxUI <- function(id, labels = list(bericht = "Bericht",
                                           tag_gebruiker = "Notificatie gebruiker"),
                         users = NULL
                         ){
  
  ns <- NS(id)
  
  tagList(
    textAreaInput(ns("txt_message"), labels$bericht),
    
    if(!is.null(users)){
      selectInput(ns("txt_message_user_tags"),
                  label = labels$tag_gebruiker,
                  choices = letters, #all_users,
                  selected = NULL,
                  multiple=TRUE)  
    },
    
    actionButton(ns("txt_send"), "Opslaan", icon = icon("envelope")),
    
    tags$br(),
    textOutput("txt_compose_comment")
    
  )
  
}

composeBox <- function(input, output, session, msg){
  
  
  output$txt_compose_comment <- renderText({
    paste("Bericht wordt verstuurd als: ", "<<current user>>")
  })
  
  
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
  
}
