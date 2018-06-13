# https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
# https://github.com/dreamRs/shinyWidgets


choose_by <- function(type) {
  switch(type,
         single_select = FALSE,
         multiple_select = TRUE,
         match = FALSE)
}

is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}
removeMeButton <- function(i) {
  actionButton(paste("removeFactor",i,sep=""), "", 
               icon=icon("times", class = NULL, lib = "font-awesome"),
               onclick = paste0('Shiny.onInputChange("remove", ', i, ')'))
}
dropdownButton2 <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL, class = NULL, id = NULL, icon = NULL) {
  
  status <- match.arg(
    arg = status,
    choices = c("default", "primary", "success", "info", "warning", "danger")
  )
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(list(icon(icon)),html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = paste0("dropdown ", class),
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script("$('.dropdown-menu').click(function(e) {e.stopPropagation();});")
  )
}
