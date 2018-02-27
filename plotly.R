

Sys.setenv("plotly_username"="fansi-sifan")
Sys.setenv("plotly_api_key"="rR48wbDvfdnqC3mLPz2X")

library(shiny)
library(plotly)
library(dplyr)
data <- read.csv('summary.csv')
xwalk <- read.csv("/Users/Fancy/OneDrive - The Brookings Institution/Classes/code/Xwalk/states.csv")

data$hover <- with(data, paste(State, '<br>', "Foreign Share:", paste(round(100*foreign.share, 2), "%", sep="")))
data <- left_join(data, xwalk, by = c("State" = "Name"))
# give state boundaries a white border


l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)



p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
api_create(p, filename = "subsidy_foreign_share_test", sharing = "public")


# Rshiny ------------------------------------------------------------------



ui <- fluidPage(
  titlePanel("Subsidy Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create state maps with 
               information from GJF Subsidy Tracker"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Share of foreign investment"="foreign.share", 
                              "Share of foreign mega deals"="mega.foreign.share"),
                  selected = "foreign.share")
      ),
    
    mainPanel(plotOutput("map"))
  )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    ggplot() +
      geom_map(data = test, map = test,
                        aes(x = long, y = lat, map_id = region, fill = input$var),
                        color = "#ffffff", size = 0.15)+ 
#      scale_fill_continuous(name = var,low ='#deebf7', high ='#08306b', guide ='colorbar') + 
      labs(x = NULL, y = NULL, title = title) + 
      coord_map("albers", lat0 = 39, lat1 = 45)  
      # theme(panel.border = element_blank()) + 
      # theme(panel.background = element_blank()) + 
      # theme(axis.ticks = element_blank()) + 
      # theme(axis.text = element_blank())
      # 
  })
}

# Run app ----
shinyApp(ui, server)



