library(shiny)
shinyUI(fluidPage(
  tagList(
    tags$head(
      tags$title("Automated Clearing combination")), 
      h2("Automated Clearing combination", style = "color:#99CCFF;font-family:'times'")),
  sidebarLayout(
    sidebarPanel(
      textInput("cash", "cash:", "1000000"),
      selectInput("comb", "comb:",choices=c("1","2","3","4","5")),
      actionButton("Btn1", "Clear Data", width = "100%"),
      p(),
      actionButton("Btn2", "Paint plot", width = "100%")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", 
                 downloadButton('DD_QSR', 'Download Table'),
                 DT::dataTableOutput('QSRtable')
                 ),
        tabPanel("Plot",
                 plotOutput('plot')
                 )
      )
    )
   )
  )
)

