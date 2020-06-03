
#' Inbox and Compose box Shiny module
#' @export
#' @rdname inboxCompose
inboxComposeUI <- function(id){
  
  
  
  ns <- NS(id)
  
  uiOutput(ns("ui_composebox"))
  
}


#' Inbox and Compose box Shiny module
#' @export
#' @rdname inboxCompose
inboxCompose <- function(input, output, session,
                         
                         msg,   # configuration object
                         
                         users = NULL,
                         current_user = NULL,
                         filter_user = NULL,
                         
                         attachment = reactive(NULL), 
                         attachment_function = NULL,
                         filter_attachment = reactive(NULL),
                         
                         label_inbox = getOption("sib_inbox", "Inbox"),
                         label_delete = getOption("sib_delete", "Verwijder selectie"),
                         
                         label_compose_box = getOption("sib_compose_titel", "Schrijf bericht"),
                         label_tag_user = getOption("sib_tag_user", "Notificatie gebruiker"),
                         label_send = getOption("sib_send", "Verstuur"),
                         
                         label_message = getOption("sib_message", "Bericht"),
                         label_undo = getOption("sib_undo", "Undo"),
                         label_tagged = getOption("sib_tagged", "Tagged"),
                         label_close = getOption("sib_close", "Sluiten"),
                         label_edit = getOption("sib_edit", "Edit"),
                         label_save = getOption("sib_save", "Opslaan"),
                         width = 4,
                         ...){
  
  
  
  ping_berichten_ui <- reactiveVal()
  
  output$ui_composebox <- renderUI({
    
    cur <- filter_attachment()
    ping_berichten_ui(runif(1))
    
    
    tagList(
      
      shinydashboard::box(width = width, status = "info", 
                          collapsible = TRUE, collapsed = FALSE,
                          title = "Berichten",
                          
                          shinyinboxUI(session$ns("inbox"), 
                                       label_inbox = label_inbox,
                                       label_delete = label_delete
                          )
                          
      ),
      shinydashboard::box(width = width, status = "info", 
                          collapsible = TRUE, collapsed = FALSE,
                          title = label_compose_box,
                          
                          composeBoxUI(session$ns("compose"), 
                                       labels = list(
                                         bericht = label_message,
                                         tag_gebruiker = label_tag_user,
                                         verstuur = label_send
                                       ),
                                       users = users
                          )
                          
      )
      
    )
    
  })
  
  # nodig om backwards compatibility niet te breken
  rv <- reactiveValues(
    attachment = NULL
  )
  
  observeEvent(attachment(), {
    rv$attachment <- attachment()
  })
  
  
  observeEvent(ping_berichten_ui(), {
    
    req(ping_berichten_ui())
    
    callModule(composeBox, "compose", msg, 
               attachment = rv$attachment,
               current_user = current_user)
    
    callModule(shinyinbox, "inbox", msg,
      filter_attachment = rv$attachment,
      attachment_function = attachment_function)
  })
  
  
}
