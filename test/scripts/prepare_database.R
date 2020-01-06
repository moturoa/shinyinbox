library(DBI)
library(purrr)
library(tibble)

empty_tibble <- function(column_names) {
  args <- purrr::map(column_names, ~ character())
  names(args) <- column_names
  do.call(tibble::tibble, args)
}

tab <- empty_tibble(c("id", "msg", "users", "sender", "attachment", 
                      "timestamp", "timestamp_modification", "deleted"))


mydb <- dbConnect(RSQLite::SQLite(), "data/messages.sqlite")
dbWriteTable(mydb, "messages", tab, overwrite = TRUE)
dbDisconnect(mydb)