#' A Shiny module for composing messages
#' @description A module (consisting of a UI and a server function) for composing messages. 
#' @param id A unique ID for the module instance.
#' @param labels A list with labels.
#' @param users Optionally, a vector with user names. If provided, the compose box will include an option 
#' to 'tag' users for the current message.
#' @rdname composeBox
#' @export
composeBoxUI <- function(id, labels = list(bericht = "Bericht",
                                           tag_gebruiker = "Notificatie gebruiker",
                                           verstuur = "Verstuur"),
                         users = NULL
                         ){
  
  ns <- NS(id)
  
  tagList(
    textAreaInput(ns("txt_message"), labels$bericht),
    
    if(!is.null(users)){
      selectInput(ns("txt_message_user_tags"),
                  label = labels$tag_gebruiker,
                  choices = users,
                  selected = NULL,
                  multiple = TRUE)
    },
    
    actionButton(ns("txt_send"), labels$verstuur, icon = icon("envelope")),
    
    tags$br(),
    textOutput(ns("txt_compose_comment"))
    
  )
  
}

#' @rdname composeBox
#' @export
composeBox <- function(input, output, session, msg, attachment = "", current_user = NULL){
  
  
  output$txt_compose_comment <- renderText({
    req(current_user)
    paste("Bericht wordt verstuurd als: ", current_user)
  })
  
  if(is.null(attachment)){
    attachment <- ""
  }
  
  if(is.null(current_user)){
    current_user <- ""
  }
  
  observeEvent(input$txt_send, {
    
    print(paste("sending", input$txt_send))
    
    if(input$txt_message != ""){
      
      msg_in <- tibble(id = uuid::UUIDgenerate(),
                       msg = input$txt_message,
                       users = paste(replace_null_emptychar(input$txt_message_user_tags),
                                     collapse=";"),
                       sender = current_user,
                       attachment = isolate(attachment),
                       timestamp = as.integer(lubridate::now(tz="UTC")),
                       timestamp_modification = as.integer(now(tz="UTC")),
                       deleted = 0)
      
      # --> naar een schema config voor msg database
      if(!is.null(msg$attachment_column)){
        names(msg_in)[names(msg_in) == "attachment"] <- msg$attachment_column
      }
      
      dbWriteTable(msg$connection, msg$table, msg_in, append = TRUE)
      
      updateTextAreaInput(session, "txt_message", value = "")
      updateSelectInput(session, "txt_message_user_tags", selected = "")
      
    }
    
  })  
  
}
