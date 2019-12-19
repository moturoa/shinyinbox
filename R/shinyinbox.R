

shinyinboxUI <- function(id,
                         
                         language = list(tab_inbox = "Inbox",
                                         tab_message = "Bericht",
                                         tab_edit = "Edit",
                                         btn_delete = "Verwijder selectie",
                                         btn_close = "Sluiten",
                                         btn_save = "Opslaan",
                                         btn_undo = "Undo",
                                         txt_message = "Bericht"
                                         )
                         ){
  
  ns <- NS(id)

  tabsetPanel(id = ns("mail_container"),
           tabPanel(language$tab_inbox, icon = icon("envelope"),
                    value="tab_inbox",
                    tags$br(),
                    
                    actionButton(ns("btn_del"), 
                                 language$btn_delete, 
                                 class = "btn btn-light btn-sm", 
                                 icon = icon("trash")),
                    
                    tags$div(style = "width: 100% !important;",
                             DT::dataTableOutput(ns("table_inbox"), 
                                                 width = "100%")
                    )
           ),
           tabPanel(language$tag$message, icon = icon("envelope-open-o"),
                    
                    tags$div(class = "message_box",
                             style = "font-size: 1.1em;
                                      padding: 30px;",
                             
                             htmlOutput(ns("txt_message")),
                             
                             tags$br(),
                             
                             actionButton(ns("btn_message_close"), 
                                          language$btn_close, 
                                          class = "btn btn-sm",
                                          icon = icon("close"))
                    ),
                    value = "tab_bericht"
                    
           ),
           tabPanel("Edit", icon = icon("edit"),
                    value = "tab_edit",
                    tags$div(style = "font-size: 1.1em;
                                      padding: 30px;",
                             
                             textAreaInput(ns("txt_edit_message"), 
                                           language$txt_message, 
                                           resize="vertical",
                                           #width = "500px", 
                                           height="300px"),
                             
                             actionButton(ns("btn_edit_save"), 
                                          language$btn_save, 
                                          icon = icon("save"),
                                          class = "btn btn-sm"),
                             
                             actionButton(ns("btn_edit_undo"), 
                                          language$btn_undo, 
                                          icon = icon("undo"),
                                          class = "btn btn-sm")
                    )
                    
           )
    )
  
}



shinyinbox <- function(input, output, session, msg){
  
  
  messages_db <- reactivePoll(msg$poll_delay, 
                              session,
                              
                              checkFunc = function(){
                                collect(tbl(msg$connection, 
                                            sql(glue("SELECT max(timestamp_modification) FROM {msg$table} ")))
                                )[[1]]
                              },
                              valueFunc = function(){
                                dbReadTable(msg$connection, msg$table) %>%
                                  dplyr::filter(deleted == 0) %>%
                                  mutate(
                                    Select = shinyInput(checkboxInput, nrow(.), paste0("checkmsg_", id), 
                                                         value = FALSE, width = "10px"),
                                     Datum = format(as_time(timestamp), "%d %b '%y"),
                                     Message = shinyInput(actionLink, nrow(.), id = paste0("link_",id),
                                                          label = shorten(msg),
                                                          onclick = paste0('Shiny.onInputChange("', session$ns("message_click"),  
                                                                           '",{id: this.id, nonce: Math.random()})')),
                                     Actie = make_actie(id, session)
                                  )
                              }
                              )
  
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
  
  
  
  # Make message links, checkboxes, formatting.
  hideTab("mail_container", target = "tab_bericht")
  hideTab("mail_container", target = "tab_edit")
  
  # Klik op Inbox, hide Edit
  observeEvent(input$mail_container, {
    if(input$mail_container == "tab_inbox"){
      hideTab("mail_container", target = "tab_edit")
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
    
    data <- tbl(msg$connection, 
                sql(glue("select * from {msg$table} where id = '{id}'"))) %>% 
            collect

    output$txt_message <- renderUI({
      
      tagList(
        tags$div(style="border: 1px solid lightgray; padding: 10px;",
                 tags$span(data$sender, style = "font-weight: bold;"),
                 tags$span(format(as_time(data$timestamp), "%d-%m-%Y"),
                           style = "float: right;"),
                 tags$hr(),
                 tags$p(data$msg)
                 
        ),
        # tags$div(style="padding:10px",
        #    
        #   if(messages$object_link_label[ii] != ""){
        #     tagList(
        #       tags$span(tags$strong("Link: ")),
        #       tags$a(href = messages$object_link[ii], messages$object_link_label[ii])  
        #     )
        #     
        #   }
        # ),
        tags$div(style="padding:10px;",
                 
                 if(data$users != ""){
                   tagList(
                     tags$span(tags$strong("Tagged: "), data$users)
                   )
                   
                 }
        )
        
      )
      
    })
    
    showTab("mail_container", target = "tab_bericht")
    
    updateTabsetPanel(session, "mail_container", selected = "tab_bericht")
    
  })
  
  
  # Klik op Edit knopje
  observeEvent(input$edit_click, {
    
    data <- tbl(msg$connection, 
                sql(glue("select * from {msg$table} where id = '{input$edit_click$id}'"))) %>% 
      collect
    
    updateTextAreaInput(session, "txt_edit_message", value = data$msg)
    
    showTab("mail_container", target = "tab_edit")
    
    updateTabsetPanel(session, "mail_container", selected = "tab_edit")
    
  })
  
  observeEvent(input$btn_edit_save, {
    
    dbExecute(
      msg$connection,
      glue("UPDATE {msg$table} SET ",
           "msg = '{input$txt_edit_message}', ",
           "timestamp_modification = {time_now_int()} ",
           "WHERE id = '{input$edit_click$id}'")
    )
    
    updateTabsetPanel(session, "mail_container", selected = "tab_inbox")
    hideTab("mail_container", target = "tab_edit")
    
    
  })
  
  observeEvent(input$btn_edit_undo,{
    
    data <- tbl(msg$connection, 
               sql(glue("select * from {msg$table} where id = '{input$edit_click$id}'"))) %>% 
      collect
    
    updateTextAreaInput(session, "txt_edit_message", value = data$msg)
    
  })
  
  observeEvent(input$btn_message_close, {
    
    updateTabsetPanel(session, "mail_container", selected = "tab_inbox")
    hideTab("mail_container", target = "tab_bericht")
    
  })
  
  
}
