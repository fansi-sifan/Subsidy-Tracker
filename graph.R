# Author:Sifan Liu
# Date: Fri Mar 23 20:52:11 2018
# --------------
pkgs <- c('dplyr', 'maps', 'mapproj', 'ggplot2', 'scales', 'ggthemes', 'RColorBrewer', "plotly", 'fiftystater')
check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

# SET UP -----------------------------------------------------
load("summary.Rda")
states <- map_data("state")
summary$ID <- tolower(summary$state)

# MAP ---------------------------------------------------------------------
# state colour map =============================================
# map function #############
state_color <- function(dt,var,text){
  g <- ggplot() +
        geom_map(data = states, map = fifty_states, aes(x = long, y = lat, map_id = region),fill = "white", color = "black") +
        geom_map(data = dt, map = fifty_states, aes_string(fill = var), map_id = dt$ID) +
        scale_fill_continuous(name = var,low='#deebf7', high='#08306b', guide='colorbar') +
        labs(x=NULL, y=NULL) +
        coord_map("albers", lat0 = 39, lat1 = 45) +
        theme_map()
  return(g)
}

# map variables #############
test <- summary %>%
  filter(foreign==1 & federal ==0)
state_color(test, "subsidy.total", 'state')

# show text on map ##########
text_pos <- states %>% 
  group_by(region) %>% 
  summarise(long= median(long, na.rm=TRUE),
            lat = median(lat, na.rm=TRUE)) %>%
  right_join(test,by = c("region"="ID")) %>%
  mutate(industry = as.character(Major.Industry.of.Parent))

industry <- state_color(test, "industry_sum") +
  geom_text(data = text_pos,aes(x=long, y=lat, label= Major.Industry.of.Parent),size = 2)
industry

