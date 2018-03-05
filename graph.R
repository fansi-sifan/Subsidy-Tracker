#Graphics

# SET UP -----------------------------------------------------

library('dplyr')
library('maps')
library('mapproj')
library('ggplot2')
library('scales')
library('ggthemes')
library('RColorBrewer')
library('plotly')
library(fiftystater)

states <- map_data("state")
subsidy.summary <- read.csv('../summary.csv')
subsidy.summary$ID <- tolower(subsidy.summary$State)


# MAP ---------------------------------------------------------------------

# state colour map =============================================
# map function #############
state_color_d <- function(data, var, title){
    gg <- ggplot()
    gg <- gg + geom_map(data = data, map = data,
                        aes(x = long, y = lat, map_id = region, fill = data[,c(var)]),
                        color = "#ffffff", size = 0.15)
    gg <- gg + scale_fill_manual(name = var, values = colorRampPalette(brewer.pal(8, "Set1"))(17))
    gg <- gg + labs(x=NULL, y=NULL, title = title)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg <- gg + theme(panel.border = element_blank())
    gg <- gg + theme(panel.background = element_blank())
    gg <- gg + theme(axis.ticks = element_blank())
    gg <- gg + theme(axis.text = element_blank())
    
    return(gg)

}

state_color_c <- function(data, var, title){
  gg <- ggplot()
  gg <- gg + geom_map(data = data, map = data,
                      aes(x = long, y = lat, map_id = region, fill = data[,c(var)]),
                      color = "#ffffff", size = 0.15)
  gg <- gg + scale_fill_continuous(name = var,low='#deebf7', high='#08306b', guide='colorbar')
  gg <- gg + labs(x=NULL, y=NULL, title = title)
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(panel.background = element_blank())
  gg <- gg + theme(axis.ticks = element_blank())
  gg <- gg + theme(axis.text = element_blank())
  
  return(gg)
  
}

# raw script
g <- ggplot() + 
  geom_map(data = states, map = fifty_states, aes(x = long, y = lat, map_id = region),fill = "white", color = "black") +
  geom_map(data = subsidy.summary, map = fifty_states, aes(fill = foreign.share, map_id = ID)) +
#  scale_fill_continuous(name = var,low='#deebf7', high='#08306b', guide='colorbar') + 
  labs(x=NULL, y=NULL) + 
  coord_map("albers", lat0 = 39, lat1 = 45) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())+
  theme(panel.background = element_blank())

g 

# map variables #############
state_color_c(test, "foreign.share", "Share of subsidies to foreign companies by state")
state_color_d(test, "Industry.foreign", "Primary industry of the foreign company that receives the most subsidies in each state")

# show text on map ##########
textval <- test %>% 
  group_by(region, HQ.Country.of.Parent) %>% 
  summarise(subsidy = mean(subsidy),
            long= mean(long),
            lat = mean(lat))

gg <- gg + geom_text(aes(x=long, y=lat, label= HQ.Country.of.Parent), data=textval)

gg



# PIE CHART IN GRID -------------------------------------------------------

foreign_subsidy_combined <- dta2000 %>% 
  #  filter(Subsidy.Source != "federal") %>%
  filter(HQ.Country.of.Parent != "USA") %>%
  filter(nchar(state)>1)%>%
  group_by(state, HQ.Country.of.Parent) %>%
  summarise(subsidy= sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE)) %>%
  mutate(subsidy.total = sum(subsidy),
         subsidy.share = subsidy/subsidy.total)%>%
  top_n(3,subsidy) %>% arrange(-subsidy.share) %>% mutate(rank = order (-subsidy.share))

others <- foreign_subsidy_combined %>% select(-rank)
others$HQ.Country.of.Parent <- "Others"
others <- others %>%
  group_by(state) %>%
  mutate(subsidy = sum(subsidy, na.rm = TRUE),
         subsidy.share = 1-subsidy/subsidy.total) %>%
  distinct()

foreign_subsidy_combined$HQ.Country.of.Parent <- as.character(foreign_subsidy_combined$HQ.Country.of.Parent)
foreign_subsidy_combined  <- bind_rows(foreign_subsidy_combined, others)


#crazyPalette = scale_fill_manual(values = c('#7DADDD', '#EF1C22', '#BEC7D6', "#ffeda0"))

crazy = ggplot(foreign_subsidy_combined[1:3] , aes(x = factor(1), fill = HQ.Country.of.Parent))
crazy = crazy + geom_bar(width = 1) + coord_polar('y') + 
  facet_wrap(~ state) 

crazy

dat <- foreign_subsidy_combined %>% select(state, subsidy.share, HQ.Country.of.Parent)
dat <- dat %>% group_by(state) %>%
  arrange(state, subsidy.share) %>%
  mutate(ymax = cumsum(subsidy.share))
dat$ymin = c(0, head(dat$ymax, n=-1))
dat$ymin = ifelse(dat$ymin == 1, 0, dat$ymin)



p1 = ggplot(dat, aes(y = subsidy.share, fill=HQ.Country.of.Parent, ymax=ymax, ymin=ymin, xmax=4, xmin=2)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  facet_wrap( ~state) +
  labs(title="New")

p1

# Create test data.
dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))

# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))



# STAR CHART/RADAR --------------------------------------------------------

setwd("/Users/Fancy/OneDrive - The Brookings Institution/code/star chart")

race <- read.csv("output-race1.csv")
stars(race)
row.names(race) <- race$state
race <- race[,2:6]


# race[1:5] <- sapply(race[1:5], sqrt) ##not sure if I should square them


library(RColorBrewer)
brewer.pal(5,"Blues")
col <- brewer.pal(5,"Blues")

#stars(race, flip.labels=FALSE, key.loc = c(15, 1.5))
#stars(race, flip.labels=FALSE, key.loc = c(15, 1.5), full=FALSE)
stars(race, flip.labels=FALSE, key.loc = c(15, 1.5), draw.segments=TRUE, col.segments=col)


