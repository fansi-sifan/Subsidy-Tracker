## ANALYSIS ##


# SET UP ------------------------------------------------------------------
library('dplyr')
#dta <- read.csv("master.csv")
factor2num <- function(x)as.numeric(gsub(",", "", gsub("[$]", "", x)))

# create year group =====================================================

dta$Year.range <- "before 2000"
dta$Year.range <- ifelse(dta$Year > 2000, "2001 - 2008", dta$Year.range)
dta$Year.range <- ifelse(dta$Year > 2008, "2009 - 2016", dta$Year.range)
dta$Year.range <- ifelse(dta$Year > 2016, "2017 onwards", dta$Year.range)

dta$Year.range <- as.factor(dta$Year.range)
dta$Year.range <- relevel(dta$Year.range, "before 2000")
summary(dta$Year.range)

# DATA SLICE --------------------------------------------------------------
dta.main <- dta %>%
  select (Year, Year.range,Company, Parent.Company, Location,
          Subsidy.Value, Megadeal.Contribution, Subsidy.Value.Adjusted.For.Megadeal,Loan.Value, 
          Type.of.Subsidy,Subsidy.Source,State.in.Which.Facility.Is.Located,
          HQ.State.of.Parent, HQ.Country.of.Parent,Major.Industry.of.Parent)

# standardize value coding ###############################################

# text value to lower
dta.main[,c("Type.of.Subsidy","Subsidy.Source")] <- lapply(dta.main[,c("Type.of.Subsidy","Subsidy.Source")],
                                                           function(x)factor(tolower(x)))
summary(dta.main$Type.of.Subsidy)

# dollar value, convert from factor to number
dta.main$undisclosed <- ifelse(dta.main$Subsidy.Value =="undisclosed",1,0)
dta.main[,c("Subsidy.Value", "Megadeal.Contribution", "Subsidy.Value.Adjusted.For.Megadeal","Loan.Value")] <- 
  lapply(dta.main[,c("Subsidy.Value", "Megadeal.Contribution", "Subsidy.Value.Adjusted.For.Megadeal","Loan.Value")], 
         factor2num)

# take out white space from state names
dta.main[,c("Location", "State.in.Which.Facility.Is.Located", "HQ.Country.of.Parent", "HQ.State.of.Parent")] <- 
  lapply(dta.main[,c("Location", "State.in.Which.Facility.Is.Located", "HQ.Country.of.Parent", "HQ.State.of.Parent")], 
         function(x)(trimws(as.character(x))))

# generate a unified state name for federal subsidy projects (SIWFIL) and local projects (Location)
dta.main$state <- dta.main$Location
dta.main$state <- ifelse(dta.main$state == "United States",dta.main$State.in.Which.Facility.Is.Located, dta.main$state) 
dta.main$state <- ifelse(dta.main$state == "" & dta.main$HQ.Country.of.Parent == "USA",as.character(dta.main$HQ.State.of.Parent), dta.main$state) 

dta.main$STATE <- as.factor(dta.main$state)           
summary(dta.main$STATE)

# create dummy for receipients (foreign = 1) and source of funding (federal = 1)
dta.main$foreign <- ifelse(dta.main$HQ.Country.of.Parent == "USA", 0, 1)
dta.main$federal <- ifelse(grepl( "federal", dta.main$Type.of.Subsidy), 1,0)

#check for missing data or outliers
dta.main %>%
  filter(!is.na(Subsidy.Value)) %>%
  filter(is.na(state))

# ANALYSIS ----------------------------------------------------------------

# share to foreign countries ==============================================
# state and local subsidies only
foreign_subsidy_share_state <- dta.main %>% 
  filter(Subsidy.Source != "federal") %>%
  group_by(Year.range,Location, foreign) %>%
  summarise(subsidy.total = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE),
            subsidy.mega = sum(Megadeal.Contribution, na.rm = TRUE), 
            undisclosed.share = sum(undisclosed, na.rm = TRUE)/n())

library("reshape2")
library('data.table')

temp <- dcast(setDT(foreign_subsidy_share_state),Year.range + Location ~ foreign, value.var = c("subsidy.total","subsidy.mega", "undisclosed.share"))
temp$total.foreign.share <- temp$subsidy.total_1 /(temp$subsidy.total_1+temp$subsidy.total_0)
temp$mega.foreign.share <- temp$subsidy.mega_1 /(temp$subsidy.mega_1+temp$subsidy.mega_0)

foreign_share <- temp %>% 
  filter(!is.na(Year.range))

names(foreign_share) <- c("Year.range", "State", "Total.subsidy_USA", "Total.subsidy_foreign",
                          "Total.subsidy_mega.deal_USA", "Total.subsidy_mega.deal_foreign",
                          "Undisclosed.deal.share_USA", "Undisclosed.deal.share_foreign",
                          "% total.subsidy.to.foreign", "% total.subsidy_mega.to.foreign")

write.csv(foreign_share,"foreign_share.csv")

foreign_share_sum <- foreign_share %>% group_by(Year.range) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

write.csv(foreign_share_sum,"foreign_share_sum.csv")

# industry ================================================================

top_industry_subsidy_USA <- dta.main %>%
  filter(HQ.Country.of.Parent == "USA") %>%
  group_by(state, Major.Industry.of.Parent)%>%
  summarise(subsidy.USA = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE))%>%
  slice(which.max(subsidy.USA))

top_industry_subsidy_foreign <- dta.main %>%
  filter(HQ.Country.of.Parent != "USA") %>%
  group_by(state, Major.Industry.of.Parent)%>%
  summarise(subsidy.foreign = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE))%>%
  slice(which.max(subsidy.foreign))

top_industry_subsidy_F <- dta.main %>%
  #  filter(HQ.Country.of.Parent != "USA") %>%
  group_by(State.in.Which.Facility.Is.Located, Major.Industry.of.Parent)%>%
  summarise(subsidy.federal = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE))%>%
  slice(which.max(subsidy.federal))

top_industry_subsidy_L <- dta.main %>%
  #  filter(HQ.Country.of.Parent != "USA") %>%
  group_by(Location, Major.Industry.of.Parent)%>%
  summarise(subsidy.local = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE))%>%
  slice(which.max(subsidy.local))


# WRITE TO FILE -----------------------------------------------------------

# combine to summary table ================================================
industry <- left_join(top_industry_subsidy_F, top_industry_subsidy_L,
                      by = c('State.in.Which.Facility.Is.Located' = "Location"))

industry_compare <- left_join(top_industry_subsidy_USA, top_industry_subsidy_foreign, by = "state" )

summary <- right_join(industry, foreign_share,
                      by = c('State.in.Which.Facility.Is.Located' = "Location"))

summary <- left_join(summary, industry_compare, 
                     by = c("State.in.Which.Facility.Is.Located" = "state"))

names(summary) <- c("State", "Industry.federal","Subsidy.federal",
                    "Industry.local","Subsidy.local","Subsidy.domestic","Subsidy.foreign",
                    "Subsidy.mega.domestic","Subsidy.mega.foreign",
                    "foreign.share", "mega.foreign.share", 
                    "Industry.USA", "Subsidy.USA","Industry.foreign", "Subsidy.frg")

# Write result ============================================================

write.csv(summary,"summary.csv")

