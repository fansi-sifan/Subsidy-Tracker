#GOOD JOB FIRST
#SUBSIDY TRACKER
#find all subsidies that go to companies with HQ in foreign countries

# Dec.3, 2017
# Sifan Liu

library('plyr')
#setwd("/Users/Fancy/Google Drive/Skyladder/Subsidy Tracker/Data")

#read the country summary data from GoodJobFirst website
country <- read.csv(url("https://subsidytracker.goodjobsfirst.org/prog.php?&detail=toplist_hqcountry_csv"), header= TRUE)

#take the country names, clean the formats for url read
countries <- country$HQ.Country
countries <- lapply(countries, function(x)gsub(" ","%20",x))
countries <- lapply(countries, function(x)gsub("(reincorporated%20for%20tax%20reasons)","tax",x))
countries <- lapply(countries, function(x)gsub("(reincorp.%20for%20tax%20reasons)","tax",x))

#url composition
path <- lapply(countries, function(x)paste0("https://subsidytracker.goodjobsfirst.org/prog.php?hq_id=",x,"&detail=export_csv"))

#read.csv, continue the the next list item if errors occur
download <- function (x) {
  return(tryCatch(read.csv(url(x)), header = TRUE, error=function(e)NULL))
}

#read data
files <- lapply(path, download)
summary(files)

#merge all data
dta <- do.call(rbind.data.frame, files)
summary(dta)
rm(files)

#for some reason, Wyoming is missing from the main database
Wyoming <- download("https://subsidytracker.goodjobsfirst.org/prog.php?state=WY&detail=export_csv")
Wyoming$HQ.Country.of.Parent <- "USA"

#bind Wyoming data to the master data file
dta <- dplyr::bind_rows(dta, Wyoming)

write.csv(dta, "/Users/Fancy/Google Drive/Skyladder/Subsidy Tracker/Data/master.csv")


