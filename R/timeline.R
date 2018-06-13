timeline_sub <- function(date = Sys.Date(), time = format(Sys.time(),'%H:%M'),
                         header = "Header", body = "Body", footer = "Footer"){
  date <- span(as.character(date), class = "bg-red")

  time <- span(as.character(time), class = "time")
  header <- h3(class = "timeline-header",
               a(href = "#", header))
  body <- div(class = "timeline-body", body)
  footer <- div(class = "timeline-footer",
                a(class = "btn btn-primary btn-xs", footer))
  text <- div(class = "timeline-item",
              time, header, body, footer)

  timelist_sub <- tagList(tags$li(class = "time-label", date),
                          tags$li(tags$i(class = "fa fa-calendar-check-o bg-blue"), text))
  return(timelist_sub)
}


#' Create an timeline information for the main body of a dashboard.
#'
#' An information timeline
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' library(shiny)
#' library(shinydashboard)
#'
#' df <- data.frame(
#'   date=c(Sys.Date()-1, Sys.Date()),
#'   time=c(format(Sys.time(),'%H:%M'), format(Sys.time()-180,'%H:%M')),
#'   header=c("V0.01", "V0.02"),
#'   body=c("Start first version 0.01","version 0.02"),
#'   footer=c("Footer", "Footer"))
#'
#' server <- function(input, output) {
#' }
#'
#' body <- dashboardBody(
#'   timeline(df)
#' )
#'
#' ui <- dashboardPage(
#'   dashboardHeader(disable = TRUE),
#'   dashboardSidebar(disable = TRUE),
#'   body
#' )
#'
#' shinyApp(ui, server)
#' }
#' @export
timeline <- function(timeline_df = NULL){
  if(!is.null(timeline_df)){
  timeline_l <- mapply(timeline_sub, timeline_df$date, timeline_df$time,
                       timeline_df$header, timeline_df$body, timeline_df$footer)
  withTags(
    ul(class = "timeline",
       do.call(tagList, timeline_l)
    ))
  }else{
    return(NULL)
  }
}



