

Sys.setenv("plotly_username"="fansi-sifan")
Sys.setenv("plotly_api_key"="rR48wbDvfdnqC3mLPz2X")

library(shiny)
library(plotly)
library(dplyr)
subsidy.summary <- read.csv('summary.csv')
#xwalk <- read.csv("/Users/Fancy/OneDrive - The Brookings Institution/Classes/code/Xwalk/states.csv")
xwalk <- read.csv("../R/Xwalk/state2abb.csv")

#data$hover <- with(data, paste(State, '<br>', "Foreign Share:", paste(round(100*foreign.share, 2), "%", sep="")))
data <- left_join(subsidy.summary, xwalk, by = c("State" = "Name"))
# give state boundaries a white border


l <- list(color = toRGB("black"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~foreign.share, text = ~State, locations = ~State.y,
    color = ~foreign.share, colors = 'Blues'
  ) %>%
  colorbar(title = "Share") %>%
  layout(
    title = 'Foreign subsidy share by State<br>(Hover for breakdown)',
    geo = g
  )

p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
api_create(p, filename = "subsidy_foreign_share_test", sharing = "public")

