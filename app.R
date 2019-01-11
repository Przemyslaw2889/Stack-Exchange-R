library(shiny)
source("tags.R")
source("maps.R")
source("boxplot_app_polarity.R")
source("polarity_function_app.R")
source("wordcloud_app.R")
source("emotion_barplot_app.R")

ui <- navbarPage("Stack Exchange Forums Analysis",
                 tabPanel("Tags",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("forum_barplot", h3("Forum name"),
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
                 
                 tabPanel("Wordclouds",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("forum_wordcloud", h3("Forum name"),
                                           choices = list("beer"="beer", "coffee"="coffee", "gaming"="gaming")),
                              sliderInput("n_words", "Number of words in wordcloud:",  
                                          min = 10, max = 100, value = 30)
                            ),
                            
                            mainPanel(
                              plotOutput("emotion_barplot")
                            )
                          )),
                 
                 tabPanel("Polarity",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("posts_or_comments", h3("Posts or Comments"),
                                           choices = list("posts"="posts", "comments"="comments")),
                              helpText("Polarity is a measure of emotion contained in a sentence.
                                       The bigger it is the more positive the sentence.
                                       Zero is neutral."),
                              textInput("text", "Text", "I hate this world"),
                              textOutput("polarity_text")
                            ),
                            
                            mainPanel(
                              plotOutput("emotion_barplot")
                              
                            )
                            
                          )),
                 
                 tabPanel("Emotions",
                          sidebarLayout(
                            
                            sidebarPanel(
                              radioButtons("emotion_post_or_comments", h3("Posts or Comments"),
                                           choices = list("posts"="Posts", "comments"="Comments")),
                              checkboxGroupInput("emotion_list", 
                                                 h3("Choose emotions"), 
                                                 choices = list("anger" = "anger", 
                                                                "anticipation" = "anticipation", 
                                                                "disgutst" = "disgust",
                                                                "fear" = "fear",
                                                                "joy" = "joy",
                                                                "negative" = "negative",
                                                                "positive" = "positive",
                                                                "sadness" = "sadness",
                                                                "surprise" = "surprise",
                                                                "trust" = "trust"),
                                                 selected = "anger")
                            ),
                            mainPanel(
                              plotOutput("wordcloud")
                            )
                          )),
                 
                 tabPanel("Component")
)


server <- function(input, output) {
  output$barplot <- renderPlot({
    title <- "Most popular tags"
    tags_barplot_name(input$forum_barplot, input$n_tags, main=title)
  })
  
  output$map <- renderPlot({
    mapa(input$forum_maps)
  })
  
  output$polarity_plot <- renderPlot({
    boxplot_app(input$posts_or_comments)
  })
  
  output$polarity_text <- renderText({
    polarity_text(input$text)
  })
  
  output$wordcloud <- renderPlot({
    wordcloud_app(input$forum_wordcloud, input$n_words)
  })
  
  output$emotion_barplot <- renderPlot({
    barplot_emotion(input$emotion_posts_or_comments, input$emotion_list)
    print(input$emotion_list)
  })
}


shinyApp(ui=ui, server=server)

barplot_emotion("Comments", c("anger", "negative"))
