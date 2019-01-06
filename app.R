library(shiny)
source("tags.R")

ui <- navbarPage("Stack Exchange Forums Analysis",
                 tabPanel("Component 1",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("forum", h3("Forum name"),
                                           choices = list("beer"="beer", "coffee"="coffee", "gaming"="gaming")),
                              sliderInput("n_tags", "Number of tags:",  
                                          min = 2, max = 20, value = 10)
                              
                            ),
                            
                            mainPanel(
                              plotOutput("barplot")
                            )
                          )),
                 tabPanel("Component 2"),
                 tabPanel("Component 2")
)


server <- function(input, output) {
  output$barplot <- renderPlot({
    title <- "Most popular tags"
    tags_barplot_name(input$forum, input$n_tags, main=title)
  })
}

shinyApp(ui=ui, server=server)
