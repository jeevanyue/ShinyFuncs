#' Create numeric range input
#'
#' Create an input control for numeric range values
#'
#' @examples
#'
#' if (interactive()) {
#' library(shiny)
#' library(shinydashboard)
#' shinyApp(
#'   ui = fluidPage({
#'     title = "Num Range Input"
#'     fluidRow(
#'       numRangeInput("num", label = "数值：", start = 12, end = 20000),
#'       sliderInput("obs", "Number of observations:", min = 0, max = 2000, value = c(13,1900)),
#'       actionButton("update", label = "Update"),
#'       verbatimTextOutput("value1"),
#'       verbatimTextOutput("value2")
#'     )
#'   }),
#'   server = function(input, output, session) {
#'     output$value1 <- renderText({
#'       paste(input$num_1,input$num_2)
#'     })
#'     observeEvent(input$update, {
#'       updatenumRangeInput(session, "num", range = 1, value = list(input$obs[[1]]))
#'       updatenumRangeInput(session, "num", range = 2,value = list(input$obs[[2]]))
#'     })
#'     output$value2 <- renderText({
#'       input$obs
#'     })
#'   }
#' )
#' }
numRangeInput <- function(
  inputId, label = NULL, start = "", end = "",
  min = NULL, max = NULL, format = "#,###",
  separator = "to", width = NULL) {

  restored <- restoreInput(id = inputId, default = list(start, end))
  start <- restored[[1]]
  end <- restored[[2]]

  div(class = "form-group shiny-input-container shiny-bound-input",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      tags$label(class="control-label", `for` = inputId, label),
      # input-daterange class is needed for dropdown behavior
      div(class = "input-group",
          tags$input(
            id = paste0(inputId, "_1"),
            class = "input-sm form-control",
            type = "text",
            `data-min` = min,
            `data-max` = max,
            `data-format` = format,
            value = formatNum(start)
          ),
          span(class = "input-group-addon", separator),
          tags$input(
            id = paste0(inputId, "_2"),
            class = "input-sm form-control",
            type = "text",
            `data-min` = min,
            `data-max` = max,
            `data-format` = format,
            value = formatNum(end)
          )
      )
  )

}

formatNum <- function(x, ...) {
  if (is.null(x)) return(NULL)
  format(as.numeric(x), scientific = FALSE, nsmall=0, big.mark=",", digits = 15)
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

updatenumRangeInput <- function(session, inputId, range = 1, label = NULL, value = NULL) {
  value <- formatNum(value)
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(paste0(inputId,"_",range), message)
}
