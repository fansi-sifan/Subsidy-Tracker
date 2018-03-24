# Author:Sifan Liu
# Date: Fri Mar 23 20:55:10 2018
# --------------
pkgs <- c('dplyr', 'shiny', 'plotly')
check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}


# set up ------------------------------------------------------------------

Sys.setenv("plotly_username"="fansi-sifan")
Sys.setenv("plotly_api_key"="rR48wbDvfdnqC3mLPz2X")


load('summary.Rda')
xwalk <- read.csv("/Users/Fancy/OneDrive - The Brookings Institution/Classes/code/Xwalk/states.csv")
#xwalk <- read.csv("../R/Xwalk/state2abb.csv")

#data$hover <- with(data, paste(State, '<br>', "Foreign Share:", paste(round(100*foreign.share, 2), "%", sep="")))
data <- summary %>%
  filter(foreign ==1 & federal ==0) %>%
  left_join(xwalk, by = c("state" = "Name")) 

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
    z = ~industry_sum, text = ~Major.Industry.of.Parent, locations = ~State,
    color = ~industry_sum, colors = 'Blues'
  ) %>%
  colorbar(title = "Sum") %>%
  layout(
    title = 'Top industry by State<br>(Hover for breakdown)',
    geo = g
  )

p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
api_create(p, filename = "Top industry by state", sharing = "public")

