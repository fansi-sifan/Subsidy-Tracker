##############
## ANALYSIS ##
##############

# SET UP ------------------------------------------------------------------
source('Clean.R')

# share to foreign countries ==============================================
subsidy_amount <- dta.main %>% 
  group_by(state, foreign, federal) %>%
  summarise(subsidy.total = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE),
            subsidy.mega = sum(Megadeal.Contribution, na.rm = TRUE), 
            undisclosed.share = sum(undisclosed, na.rm = TRUE)/n())

# Top industry by source of funding (is federal?) and by receipients (is foreign?) ==============================
top_industry_subsidy <- dta.main %>%
  group_by(foreign, federal, state, Major.Industry.of.Parent)%>%
  summarise(industry_sum = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE)) %>%
  slice(which.max(industry_sum))


# WRITE TO FILE -----------------------------------------------------------
summary <- left_join(top_industry_subsidy,subsidy_amount, by = c("foreign","federal","state"))
save(summary, file = 'summary.Rda')
write.csv(summary,"summary.csv")
