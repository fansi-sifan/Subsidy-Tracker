
source('Func.R')
load('master.Rdata')

# create year group -------------------------------------------------------------

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

# standardize value coding ------------------------------------------------

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

# Create dummies for analysis ---------------------------------------------
# create dummy for receipients (foreign = 1) and source of funding (federal = 1)
dta.main$foreign <- ifelse(dta.main$HQ.Country.of.Parent == "USA", 0, 1)
dta.main$federal <- ifelse(grepl( "federal", dta.main$Type.of.Subsidy), 1,0)

# Check for missing data or outliers ---------------------------------------------
dta.main %>%
  filter(!is.na(Subsidy.Value)) %>%
  filter(is.na(state))

