library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(magrittr)
library(DT)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    # tags$head(
    #   singleton(
    #     includeScript("www/readCookie.js")
    #   )),
    textInput("fileviewId",
              "Fileview containing .sf files and metadata:"),
    actionButton("getfv", "Get Fileview"),
    textOutput("fv_dl") %>% withSpinner(),
    conditionalPanel(
      condition = "output.fv_dl == true",
      selectizeInput("fileviewCols",
                  "", choices = NULL, multiple = T))
  ),
  dashboardBody(
    DTOutput("fileview") %>% withSpinner(color="#0dc5c1")
    )
)

