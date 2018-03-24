####################################
###  Project: SUBSIDY TRACKER
###  Goal: find all subsidies that go to companies with HQ in foreign countries
###  Source: GOOD JOB FIRST
###################################
# Dec.3, 2017
# Sifan Liu

library('plyr')

# Raw data ----------------------------------------------------------------

# Read the country summary data from GoodJobFirst website
country <- read.csv(url("https://subsidytracker.goodjobsfirst.org/prog.php?&detail=toplist_hqcountry_csv"), header= TRUE)

# Take the country names, clean the formats for url read
countries <- country$HQ.Country

countries <- lapply(countries, function(x)gsub(" ","%20",x))
countries <- lapply(countries, function(x)gsub("(reincorporated%20for%20tax%20reasons)","tax",x))
countries <- lapply(countries, function(x)gsub("(reincorp.%20for%20tax%20reasons)","tax",x))

# Dowload data ===========================================================
# url composition 
path <- lapply(countries, function(x)paste0("https://subsidytracker.goodjobsfirst.org/prog.php?hq_id=",x,"&detail=export_csv"))

# Function to read.csv, continue the the next list item if errors occur
download <- function (x) {
  return(tryCatch(read.csv(url(x)), header = TRUE, error=function(e)NULL))
}
# read into data files
files <- lapply(path, download)
summary(files)

# merge all data ===========================================================
dta <- do.call(rbind.data.frame, files)
summary(dta)
rm(files)

# for some reason, Wyoming is missing from the main database, manually add
Wyoming <- download("https://subsidytracker.goodjobsfirst.org/prog.php?state=WY&detail=export_csv")
Wyoming$HQ.Country.of.Parent <- "USA"
# bind Wyoming data to the master data file
dta <- dplyr::bind_rows(dta, Wyoming)

# save data ===============================================================
save(dta, file = "master.Rdata")


