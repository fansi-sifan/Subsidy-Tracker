#EDA

library('dplyr')
library('ggplot2')

dta <- read.csv("master.csv", header = TRUE)
dta$foreign <- ifelse(dta$HQ.Country.of.Parent == "USA", 0, 1)

dta.value <- select(dta, Year, Subsidy.Value, Megadeal.Contribution, Subsidy.Value.Adjusted.For.Megadeal,Loan.Value, foreign)

factor2num <- function(x)as.numeric(gsub(",", "", gsub("[$]", "", x)))

dta.value[2:5] <- lapply(dta.value[2:5], factor2num)

year_tot <- dta.value %>% group_by(Year, foreign) %>%
  summarise(Sub = sum(Subsidy.Value, na.rm = TRUE),
            Mega = sum(Megadeal.Contribution, na.rm = TRUE), 
            Sub_adj = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE),
            Loan = sum(Loan.Value, na.rm = TRUE),
            Count = n())
year_tot$foreign <- factor(year_tot$foreign, labels = c("USA","foreign"))

#summarize total subsidy values by year, US vs foreign deals
ggplot(data = year_tot, aes(x = Year, y = Sub, fill = foreign)) + 
  geom_bar(stat="identity")

ggplot(data = year_tot, aes(x = Year, y = Sub_adj, fill = foreign)) + 
  geom_bar(stat="identity")

#share of foreign receipints
year_tot <- year_tot %>% group_by(Year) %>%
  mutate(total = sum(Sub_adj, na.rm = TRUE), 
         share = Sub_adj/total)
ggplot(data = filter(year_tot, foreign == "foreign"), aes(x = Year, y = share)) + 
  geom_bar(stat="identity")

#China
China <- read.csv("GJF_subsidy tracker_China.csv")

#### country analysis ####
top_foreign_subsidy_federal <- dta2000 %>% 
  filter(Subsidy.Source == "federal") %>%
  filter(HQ.Country.of.Parent != "USA") %>%
  group_by(State.in.Which.Facility.Is.Located, HQ.Country.of.Parent) %>%
  summarise(subsidy = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE)) %>%
  slice(which.max(subsidy))

top_foreign_subsidy_state <- dta2000 %>% 
  filter(Subsidy.Source != "federal") %>%
  filter(HQ.Country.of.Parent != "USA") %>%
  group_by(Location, HQ.Country.of.Parent) %>%
  summarise(subsidy = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE)) %>%
  slice(which.max(subsidy))

top_foreign_subsidy <- left_join(top_foreign_subsidy_federal, top_foreign_subsidy_state, 
                                 by = c("State.in.Which.Facility.Is.Located" = "Location"))

