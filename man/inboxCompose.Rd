% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/composeInbox.R
\name{inboxComposeUI}
\alias{inboxComposeUI}
\alias{inboxCompose}
\title{Inbox and Compose box Shiny module}
\usage{
inboxComposeUI(id)

inboxCompose(
  input,
  output,
  session,
  msg,
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
  ...
)
}
\description{
Inbox and Compose box Shiny module

Inbox and Compose box Shiny module
}
