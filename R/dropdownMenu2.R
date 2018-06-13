
#' Create a custom dropdown menu to place in a dashboard header
#'
#' Create a custom dropdown menu to place in a dashboard header
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   library(shiny)
#'   library(shinydashboard)
#'
#'   # A dashboard header with 3 dropdown menus
#'   header <- dashboardHeader(
#'     title = "Dashboard Demo",
#'
#'     # Dropdown menu for messages
#'     dropdownMenu(
#'       type = "messages", badgeStatus = "success",
#'       messageItem("Support Team",
#'                   "This is the content of a message.",
#'                   time = "5 mins"
#'       ),
#'       messageItem("Support Team",
#'                   "This is the content of another message.",
#'                   time = "2 hours"
#'       ),
#'       messageItem("New User",
#'                   "Can I get some help?",
#'                   time = "Today"
#'       )
#'     ),
#'
#'     # Dropdown menu for notifications
#'     dropdownMenu(
#'       type = "notifications", badgeStatus = "warning",
#'       notificationItem(icon = icon("users"), status = "info",
#'                        "5 new members joined today"
#'       ),
#'       notificationItem(icon = icon("warning"), status = "danger",
#'                        "Resource usage near limit."
#'       ),
#'       notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
#'                        status = "success", "25 sales made"
#'       ),
#'       notificationItem(icon = icon("user", lib = "glyphicon"),
#'                        status = "danger", "You changed your username"
#'       )
#'     ),
#'
#'     # Dropdown menu for tasks, with progress bar
#'     dropdownMenu(
#'       type = "tasks", badgeStatus = "danger",
#'       taskItem(value = 20, color = "aqua",
#'                "Refactor code"
#'       ),
#'       taskItem(value = 40, color = "green",
#'                "Design new layout"
#'       ),
#'       taskItem(value = 60, color = "yellow",
#'                "Another task"
#'       ),
#'       taskItem(value = 80, color = "red",
#'                "Write documentation"
#'       )
#'     ),
#'     uiOutput("ddm", container = tags$li, class = "dropdown")
#'   )
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header,
#'       dashboardSidebar(),
#'       dashboardBody()
#'     ),
#'     server = function(input, output) {
#'       output$ddm <- renderUI({
#'         #' Dropdown menu for custom
#'         dropdownMenu2(
#'           taglist = TRUE,
#'           dropstyle = 'width:240px;',
#'           selectInput(
#'             inputId = "mtcars_vs", label = 'mtcars_vs', choices = unique(mtcars$vs)),
#'           selectInput(
#'             inputId = "mtcars_vs", label = "mtcars_am", choices = unique(mtcars$am))
#'         )
#'       })
#'     }
#'   )
#' }
#' @export
dropdownMenu2 <- function (..., status = "primary", btnstyle = NULL, dropstyle = NULL, icon = 'toggle-down', .list = NULL, taglist = FALSE) {
  status <- match.arg(
    arg = status,
    choices = c("default", "primary", "success", "info", "warning", "danger")
  )
  if (is.null(status)) {
    badge <- NULL
  }else {
    badge <- span(class = paste0("label label-", status))
  }

  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(dropstyle)) dropstyle,
    lapply(X = list(...), FUN = tags$li, style = "margin-top: 10px; margin-left: 10px; margin-right: 10px;")
  )

  icon <- icon(icon)
  html_a <- list(
    href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", icon, badge
  )

  if(taglist){
    tagList(
      do.call(tags$a, html_a),
      # a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", icon, badge),
      do.call(tags$ul, html_ul),
      tags$script("$('.dropdown-menu').click(function(e) {e.stopPropagation();});")
    )
  }else{
    tags$li(
      class = "dropdown",
      do.call(tags$a, html_a),
      # a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", icon, badge),
      do.call(tags$ul, html_ul),
      tags$script("$('.dropdown-menu').click(function(e) {e.stopPropagation();});"))
  }
}
