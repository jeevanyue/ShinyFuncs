% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeline.R
\name{timeline}
\alias{timeline}
\title{Create an timeline information for the main body of a dashboard.}
\usage{
timeline(timeline_df = NULL)
}
\description{
An information timeline
}
\examples{
## Only run this example in interactive R sessions
if (interactive()) {
library(shiny)
library(shinydashboard)

df <- data.frame(
  date=c(Sys.Date()-1, Sys.Date()),
  time=c(format(Sys.time(),'\%H:\%M'), format(Sys.time()-180,'\%H:\%M')),
  header=c("V0.01", "V0.02"),
  body=c("Start first version 0.01","version 0.02"),
  footer=c("Footer", "Footer"))

server <- function(input, output) {
}

body <- dashboardBody(
  timeline(df)
)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body
)

shinyApp(ui, server)
}
}
