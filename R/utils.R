

shorten <- function(x, n = 70){
  
  if(length(x) == 1){
    if(nchar(x) >= n){
      paste0(substr(x, 1, n-3), "...")
    } else {
      x
    }
  } else {
    sapply(x, shorten, n = n)
  }
  
}


# vector to e.g. "('aap','noot','mies')"
to_sql_string <- function(x){
  
  paste0(
    "('",
    paste(x, collapse="','"),
    "')"
  )
  
}


time_now_int <- function(){
  as.numeric(lubridate::now(tz="UTC"))
}

reverse_dataframe <- function(x){
  if(nrow(x)){
    x[nrow(x):1,]  
  } else {
    x
  }
  
}

as_time <- function(x){
  as.POSIXct(as.numeric(x), origin="1970-1-1", tz="UTC")
}

empty_colnames <- function(dfr){
  setNames(dfr, rep(" ", ncol(dfr)))
}
