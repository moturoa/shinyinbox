#' Attach dependencies
#'
#' @noRd
#' @importFrom utils packageVersion
#' @importFrom htmltools htmlDependency attachDependencies
attachShinyInboxDependencies <- function(tag){

  version <- as.character(packageVersion("shinyinbox")[[1]])

  dep <- list(
    htmltools::htmlDependency(
      "selectedMessages", version,
      src = c(href = "shinyinbox/selectedMessages"),
      script = "selectedMessages.js"
    )
  )

htmltools::attachDependencies(tag, dep, append = TRUE)
}


