library(shiny)
source("tags.R")
source("maps.R")

ui <- navbarPage("Stack Exchange Forums Analysis",
                 tabPanel("Tags",
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
                 
                 tabPanel("Maps",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("forum_maps", h3("Forum name"),
                                           choices = list("beer"="beer", "coffee"="coffee", "gaming"="gaming"))
                            ),
                            
                            mainPanel(
                              plotOutput("map")
                            )
                          )),
                 tabPanel("Component 2")
)


server <- function(input, output) {
  output$barplot <- renderPlot({
    title <- "Most popular tags"
    tags_barplot_name(input$forum, input$n_tags, main=title)
  })
  
  output$map <- renderPlot({
    print(input$fourm)
    mapa(input$forum_maps)
  })
}


shinyApp(ui=ui, server=server)

