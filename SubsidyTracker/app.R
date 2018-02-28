#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library('ggplot2')
library(shiny)

ui <- fluidPage(
  titlePanel("Subsidy Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create state maps with 
               information from GJF Subsidy Tracker"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Share of foreign investment", 
                              "Share of foreign mega deals"),
                  selected = "Share of foreign investment")
      ),
    
    mainPanel(plotOutput("map"))
  )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    
    data <- switch(input$var, 
                   "Share of foreign investment" = subsidy.summary$foreign.share, 
                   "Share of foreign mega deals" = subsidy.summary$mega.foreign.share)
    
    ggplot() + 
      geom_map(data = states, map = states, aes(x = long, y = lat, map_id = region),fill = "white", color = "black") +
      geom_map(data = subsidy.summary, map = states, aes(fill = data, map_id = ID)) +
      #  scale_fill_continuous(name = var,low='#deebf7', high='#08306b', guide='colorbar') + 
      labs(x=NULL, y=NULL) + 
      coord_map("albers", lat0 = 39, lat1 = 45) + 
      theme(panel.border = element_blank()) + 
      theme(panel.background = element_blank()) + 
      theme(axis.ticks = element_blank()) + 
      theme(axis.text = element_blank())
    
  })
}

# Run app ----
shinyApp(ui, server)



