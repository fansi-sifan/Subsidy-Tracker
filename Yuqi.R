#GOOD JOB FIRST
#SUBSIDY TRACKER
#find all subsidies that go to companies with HQ in foreign countries
#Yuqi
dta <- read.csv("master.csv")

#EDA
#factor to number

dta$undisclosed <- ifelse(dta$Subsidy.Value =="undisclosed",1,0)

factor2num <- function(x)as.numeric(gsub(",", "", gsub("[$]", "", x)))

dta$Subsidy.Value <- factor2num(dta$Subsidy.Value)
dta$Megadeal.Contribution <- factor2num(dta$Megadeal.Contribution)
dta$Subsidy.Value.Adjusted.For.Megadeal <- factor2num(dta$Subsidy.Value.Adjusted.For.Megadeal)
dta$Loan.Value <- factor2num(dta$Loan.Value)

#turn into a tibble for better data manipulation
dta_tbl <- tbl_df(dta)
glimpse(dta_tbl)
summary(dta_tbl)
summary(dta_tbl$Type.of.Subsidy)

#creat a new variable indicating federal/non-federal
dta_tbl$Federal <- ifelse(((dta_tbl$Type.of.Subsidy == "federal grant") | 
                             (dta_tbl$Type.of.Subsidy == "federal insurance") |
                             (dta_tbl$Type.of.Subsidy == "federal loan or loan guarantee") |
                             (dta_tbl$Type.of.Subsidy == "federal tax-exempt bond") |
                             (dta_tbl$Type.of.Subsidy == "federal allocated tax credit")), 1, 0)

dta_tbl %>%
  summarise(pct = mean(Federal))

dta_tbl %>%
  summarise(pct = mean(Federal,na.rm = TRUE ))


# Table 1 - subsidy / loan by beneficiary's HQ country since 2000
table1 <- dta_tbl %>% 
  filter(Year >= 2000) %>%
  group_by(HQ.Country.of.Parent) %>% 
  summarise(Subsidy = sum(Subsidy.Value, na.rm = TRUE), 
            Subsidy_adjust = sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm = TRUE),
            Megadeal = sum(Megadeal.Contribution, na.rm = TRUE),
            Loan = sum(Loan.Value, na.rm = TRUE),
            Subsidy_adjust_federal = sum(Subsidy.Value.Adjusted.For.Megadeal[which(Federal == 1)]),
            Subsidy_adjust_nonfederal = sum(Subsidy.Value.Adjusted.For.Megadeal[which(Federal == 0)]), 
            Federal_pct_value = weighted.mean(Federal,Subsidy.Value.Adjusted.For.Megadeal,na.rm = TRUE),
            Federal_pct_count = mean(Federal,na.rm = TRUE),
            undisclosed = sum(undisclosed, na.rm = TRUE),
            Count = n()) %>%
  mutate(subsidy_per_count = Subsidy/Count,
         undisclosed_share = undisclosed/Count) %>%
  arrange(desc(subsidy_per_count)) 

View(table1)

# Kepp only the rows that has Subsidy_adjust > $1 billion (keeping the US for now)
table2 <- table1 %>%
  filter(Subsidy_adjust > 1000000000) %>%
  arrange(desc(Subsidy_adjust)) 

View(table2)

# Kepp only the rows that has Subsidy_adjust > $1 billion (getting rid of the US for now)
table3 <- table1 %>%
  filter(Subsidy_adjust > 1000000000 & HQ.Country.of.Parent != "USA") %>%
  arrange(desc(Subsidy_adjust)) 

View(table3)

table3 <- table3 %>%
  select(HQ.Country.of.Parent, Subsidy_adjust, Subsidy_adjust_federal, Subsidy_adjust_nonfederal) %>%
  gather(key = Federal_nonFederal, value = Federal_nonFederal_Subsidy, -HQ.Country.of.Parent, -Subsidy_adjust)

#rescale the numeric columns
table3$Subsidy_adjust <- table3$Subsidy_adjust / 1000000000
table3$Federal_nonFederal_Subsidy <- table3$Federal_nonFederal_Subsidy / 1000000000

# Visualize it!

library(ggplot2)

#with the US
# ggplot(data=table2, aes(x = reorder(HQ.Country.of.Parent,table2$Subsidy_adjust),y = Subsidy_adjust)) +
#   #scale_y_continuous(breaks=c(-20,0,20,40,60,80), limits=c(-20,80)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +                          
#   theme_bw()   


# Reorder the levels of Federal_nonFederal
table3$Federal_nonFederal <- factor(table3$Federal_nonFederal, levels = c("Subsidy_adjust_federal", "Subsidy_adjust_nonfederal"))

#without the US
ggplot(data = table3, aes(x = reorder(HQ.Country.of.Parent,table3$Subsidy_adjust),
                          y = Federal_nonFederal_Subsidy,
                          fill = Federal_nonFederal,
                          text = Subsidy_adjust)) +
  #scale_y_continuous(breaks=c(-20,0,20,40,60,80), limits=c(-20,80)) +
  #geom_hline(yintercept = 0, color = c("#646464")) +
  geom_bar(stat = "identity") +
  coord_flip() +            
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = wes_palette("GrandBudapest")) + 
  labs(title = "blablabla", y = "Subsidy amount (in billions)", x = "jurisdictions", fill = "Subisidy type")

#




# 
# 
# ggplot(data=timss_wide2, aes(x=Jurisdiction,y=average_change)) +
#   scale_y_continuous(breaks=c(-20,0,20,40,60,80), limits=c(-20,80)) +
#   geom_bar(stat="identity") +
#   coord_flip() +                          
#   theme_bw()   
# 
# 
# ggplot(data = filter(table1, HQ.Country.of.Parent != "USA"), aes(x = HQ.Country.of.Parent, y = Subsidy_adjust)) + 
#   geom_bar(stat = "identity")







