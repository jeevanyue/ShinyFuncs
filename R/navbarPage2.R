#' navbarPage add custom input
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   library(shiny)
#'   library(shinydashboard)
#'   navbar_search <- tags$ul(
#'     class = "nav navbar-nav navbar-left",
#'     tags$li(
#'       tags$form(
#'         class = "navbar-form",
#'         tagList(
#'           textInput("search", label = NULL))
#'         ))
#'   )
#'   navbar_user <- tags$ul(
#'     class = "nav navbar-nav navbar-right",
#'     tags$li(tags$form(class = "navbar-text", "User: Dean")),
#'     tags$li(
#'       tags$form(
#'         class = "navbar-form",
#'         tagList(
#'           actionButton(
#'             style = "background-color:rgba(0,0,0,0); border:0; border-radius:15px;",
#'             "help", label = NULL, icon = icon("question")),
#'           actionButton(
#'             style = "background-color:rgba(0,0,0,0); border:0;border-radius:15px;",
#'             "signout", label = NULL, icon = icon("sign-out")))
#'         ))
#'   )
#'   shinyApp(
#'     ui = navbarPage2(
#'       "Test app",
#'       tabPanel("tab1", "tab 1"),
#'       tabPanel("tab2", "tab 2"),
#'       tabPanel("tab3", "tab 3"),
#'       navbar2 = tagList(navbar_search, navbar_user)
#'     ),
#'     server = function(input, output, session) {
#'     }
#'     )
#' }
navbarPage2 <- function(..., navbar2 = NULL) {
  navbar <- navbarPage(...)
  if(!is.null(navbar2)){
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
      navbar[[3]][[1]]$children[[1]], navbar2)
  }
  navbar
}

