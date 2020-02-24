#' A Shiny module for an inbox with messages
#' @description Inbox for messages stored in a local database with edit and delete functionality. 
#' For use in Shiny apps.
#' @param id A unique ID for the inbox UI.
#' @param language A list of labels to be used in the UI.
#' @param input For the server function, Shiny usage.
#' @param output For the server function, Shiny usage.
#' @param session For the server function, Shiny usage.
#' @param msg A configuration object.
#' @param filter_attachment If set, only shows messages that include this value in the attachment.
#' @rdname shinyinbox
#' @examples
#' # The configuration object may look like:
#' msg <- list(
#'   connection = dbConnect(RSQLite::SQLite(), "data/messages.sqlite"),
#'   table = "messages",
#'   edit_rights = TRUE,
#'   delete_rights = TRUE,
#'   poll_delay = 500)
#' @export
#' @importFrom shiny tabsetPanel tabPanel insertTab hideTab
shinyinboxUI <- function(id,
                         label_inbox = getOption("sib_inbox", "Inbox"),
                         label_delete = getOption("sib_delete", "Verwijder selectie")
                         
                         ){
  
  ns <- NS(id)

  out <- shiny::tabsetPanel(id = ns("mail_container"),
                     selected = "tab_inbox",
           shiny::tabPanel(title = label_inbox, 
                    icon = icon("envelope"),
                    value = "tab_inbox",
                    #tabName = "tab_inbox",
                    
                    tags$br(),
                    actionButton(ns("btn_del"), 
                                 label_delete, 
                                 class = "btn btn-light btn-sm", 
                                 icon = icon("trash")),
                    
                    tags$div(style = "width: 100% !important;",
                             DT::dataTableOutput(ns("table_inbox"), 
                                                 width = "100%")
                    )
           )
           
    )
  
attachShinyInboxDependencies(out)
}


#' @rdname shinyinbox
#' @export
shinyinbox <- function(input, output, session, msg, 
                       filter_attachment = NULL,
                       filter_user = NULL,
                       attachment_function = NULL,
                       label_message = getOption("sib_message", "Bericht"),
                       label_undo = getOption("sib_undo", "Undo"),
                       label_tagged = getOption("sib_tagged", "Tagged"),
                       label_close = getOption("sib_close", "Sluiten"),
                       label_edit = getOption("sib_edit", "Edit"),
                       label_save = getOption("sib_save", "Opslaan")
                       ){
  
  
  shiny::hideTab("mail_container", target = "tab_bericht")
  shiny::hideTab("mail_container", target = "tab_edit")
  
  
  # --> naar R6 config
  if(is.null(msg$attachment_column)){
    msg$attachment_column <- "attachment"
  }
  
  # Read messages from database.
  messages_raw <- reactivePoll(msg$poll_delay, 
                              session,
                              
                              checkFunc = function(){
                                collect(tbl(msg$connection, 
                                            sql(glue("SELECT max(timestamp_modification) FROM {msg$table} ")))
                                )[[1]]
                              },
                              
                              valueFunc = function(){
                                dbReadTable(msg$connection, msg$table)
                              }
                  )
  
  # Messages: filtered, formatted
  messages_db <- reactive({
    
    out <- messages_raw() %>%
      dplyr::filter(deleted == 0) %>%
      mutate(
        Select = shinyInput(checkboxInput, nrow(.), 
                            paste0("checkmsg_", id), 
                            value = FALSE, width = "10px"),
        Datum = format(as_time(timestamp), "%d %b '%y"),
        Message = shinyInput(actionLink, nrow(.), id = paste0("link_",id),
                             label = shorten(msg),
                             onclick = paste0('Shiny.onInputChange("', session$ns("message_click"),  
                                              '",{id: this.id, nonce: Math.random()})')),
        Actie = make_actie(id, session)
      )
    
    if(!is.null(filter_attachment)){
      out <- dplyr::filter(out, !!sym(msg$attachment_column) %in% filter_attachment)  
    }
    
    if(!is.null(filter_user)){
      # Use word boundary (\b), users are ;-separated
      out <- dplyr::filter(out, grepl(paste0("\\b",filter_user,"\\b"), users))  
    }
  
  return(out)
  })
  
  # id is vector
  shinyInput <- function(FUN, len, id, ..., label = NULL, expand_label = TRUE) {
    inputs <- character(len)
    
    if(expand_label){
      if(length(label) < len){
        label <- rep(label, len)
      }
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(id[i], label = label[i], ...))
      }
    } else {
      for (i in seq_len(len)) {
        suppressWarnings(
          inputs[i] <- as.character(FUN(id[i], label = label, ...))
        )
      }
    }
    
    inputs
  }
  
  # no delete button if no rights
  if(!msg$delete_rights){
    shinyjs::hide("btn_del")
  }
  
  # show these columns in the inbox
  inbox_def <- tibble::tibble(column = c("Select","Message","sender", "Datum", "Actie"),
                          width = c(5,58,15,12,5))
  
  if(!msg$edit_rights){
    inbox_def <- dplyr::filter(inbox_def, column != "Actie")
  }
  if(!msg$delete_rights){
    inbox_def <- dplyr::filter(inbox_def, column != "Select")
  }
  
  # Prepare horrible format for column widths to datatable.
  el <- function(i){
    list(width = paste0(inbox_def$width[i],"%"),
                        targets = list(i))
  }
  
  width_setting <- c(
    list(list(width = "5%", targets = list(0))),
    lapply(1:nrow(inbox_def), el)
  )
    
  # Edit 'buttons' for each row in the table
  make_actie <- function(ids, session){
    
    len <- length(ids)
    out <- character(len)
    id_click <- session$ns("edit_click")
    
    for(i in seq_len(len)){
      
      out[i] <- as.character(
        actionLink(ids[i], label = icon("edit"), 
                   onclick = glue("Shiny.onInputChange('{{id_click}}',",
                                  "{id: '{{ids[i]}}', nonce: Math.random()})",
                                  .open = "{{", .close = "}}"))
      )
      
    }
    out
  }
  
  # Click on Inbox, hide Edit
  observeEvent(input$mail_container, {
    if(input$mail_container == "tab_inbox"){
      removeTab("mail_container", target = "tab_edit")
      removeTab("mail_container", target = "tab_bericht")
    }
  })
  
  # Inbox table
  output$table_inbox <- DT::renderDataTable(
    
    messages_db()[,inbox_def$column] %>%
      reverse_dataframe() %>%
      empty_colnames() %>%
      DT::datatable(., 
                    escape = FALSE,
                    selection = "none",
                    options = list(
                      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                      ordering = FALSE,
                      dom = "tpi",
                      language = list(search = "Zoek",
                                      emptyTable = "Nog geen berichten.",
                                      zeroRecords = "Geen resultaten.",
                                      info = "_START_ tot _END_ van _TOTAL_ berichten",
                                      infoFiltered = "(gefilterd uit _MAX_ rijen)",
                                      infoEmpty = "",
                                      paginate = list(
                                        first = "Eerste",
                                        last = "Laatste",
                                        `next` = "Volgende",
                                        previous = "Vorige"
                                      )
                      ),
                      columnDefs = width_setting
                    )),
    
    server = FALSE
    
  )
  
  # Klik op delete, vind geselecteerde rijen (JS)
  observeEvent(input$btn_del, {
    
    session$sendCustomMessage("selectedMessages", list(inbox_id = session$ns("table_inbox"), 
                                                       shiny_id = session$ns("msgchecked")))  

  })
  
  # Messages zijn geselecteerd, en er is op delete gedrukt.
  observeEvent(input$msgchecked, {
    
    ids <- gsub("checkmsg_", "", input$msgchecked)
    
    query <- glue("UPDATE {msg$table} SET ",
                  "[deleted] = 1, timestamp_modification = {time_now_int()} ",
                  "WHERE id IN {to_sql_string(ids)}")
    
    dbExecute(
     msg$connection, query
    )
    
  })
  
  # Klik op message om te lezen.
  observeEvent(input$message_click, {

    link_id <- input$message_click$id
    id <- strsplit(link_id, "_")[[1]][2]
    
    attach_col <- ifelse(is.null(msg$attachment_column),
                         "attachment",
                         msg$attachment_column) 
    
    data <- tbl(msg$connection, 
                sql(glue("select * from {msg$table} where id = '{id}'"))) %>% 
            collect

    msg_html <- tagList(
      tags$div(style="border: 1px solid lightgray; padding: 10px;",
               tags$span(data$sender, style = "font-weight: bold;"),
               tags$span(format(as_time(data$timestamp), "%d-%m-%Y"),
                         style = "float: right;"),
               tags$hr(),
               tags$p(data$msg)
               
      ),
      
      # Optionally, render some UI for the attachment.
      tags$div(style="padding:10px",
               
               if(data[[attach_col]] != ""){
                 if(!is.null(attachment_function)){
                   
                   attachment_function(data[[attach_col]])
                 }
                 
               }
      ),
      tags$div(style="padding:10px;",
               
               if(data$users != ""){
                 tagList(
                   tags$span(tags$strong(glue("{label_tagged}: ")), data$users)
                 )
                 
               }
      )
      
    )
    
    pane <- shiny::tabPanel(title = label_message,
             icon = icon("envelope-open-o"),
             value = "tab_bericht",
             
             tags$div(class = "message_box",
                      style = "font-size: 1.1em;
                                      padding: 30px;",
                      
                      msg_html,
                      
                      tags$br(),
                      
                      actionButton(session$ns("btn_message_close"), 
                                   label_close, 
                                   class = "btn btn-sm",
                                   icon = icon("close"))
             )
             
    )
    
    shiny::appendTab("mail_container", pane, select = TRUE)
    
  })
  
  
  # Klik op Edit knopje
  observeEvent(input$edit_click, {
    
    data <- tbl(msg$connection, 
                sql(glue("select * from {msg$table} where id = '{input$edit_click$id}'"))) %>% 
      collect
    
    
    pane <- shiny::tabPanel(title = label_edit, 
             icon = icon("edit"),
             value = "tab_edit",
             
             tags$div(style = "font-size: 1.1em;
                                      padding: 30px;",
                      
                      shiny::textAreaInput(session$ns("txt_edit_message"), 
                                    label_message, 
                                    resize="vertical",
                                    value = data$msg,
                                    height="300px"),
                      
                      shiny::actionButton(session$ns("btn_edit_save"), 
                                   label_save, 
                                   icon = icon("save"),
                                   class = "btn btn-sm"),
                      
                      shiny::actionButton(session$ns("btn_edit_undo"), 
                                   label_undo, 
                                   icon = icon("undo"),
                                   class = "btn btn-sm")
             )
             
    )
    
    shiny::appendTab("mail_container", pane, select = TRUE)
    
  })
  
  observeEvent(input$btn_edit_save, {
    
    dbExecute(
      msg$connection,
      glue("UPDATE {msg$table} SET ",
           "msg = '{input$txt_edit_message}', ",
           "timestamp_modification = {time_now_int()} ",
           "WHERE id = '{input$edit_click$id}'")
    )
    
    shiny::updateTabsetPanel(session, "mail_container", selected = "tab_inbox")
    shiny::removeTab("mail_container", target = "tab_edit")
    
    
  })
  
  observeEvent(input$btn_edit_undo,{
    
    data <- tbl(msg$connection, 
               sql(glue("select * from {msg$table} where id = '{input$edit_click$id}'"))) %>% 
      collect
    
    shiny::updateTextAreaInput(session, "txt_edit_message", value = data$msg)
    
  })
  
  observeEvent(input$btn_message_close, {
    
    shiny::updateTabsetPanel(session, "mail_container", selected = "tab_inbox")
    shiny::removeTab("mail_container", target = "tab_bericht")
    
  })
  
}
