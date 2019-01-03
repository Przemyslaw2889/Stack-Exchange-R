library(shiny)
source("tags.R")

ui <- fluidPage(
  titlePanel("Stack Exchange Forums"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("forum", h3("Forum name"),
                   choices = list("beer"="beer", "coffee"="coffee", "gaming"="gaming"))
        
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)

server <- function(input, output) {
  output$barplot <- renderPlot({
    title <- "Most popular tags"
    tags_barplot_name(input$forum, k=5)
  })
}

shinyApp(ui=ui, server=server)
