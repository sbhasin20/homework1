if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

library(scales)

source("data-code/1_Plan_Data.R")
source("data-code/2_Plan_Characteristics.R")
source("data-code/3_Service_Areas.R")
source("data-code/4_Penetration_Files.R")

#1 
tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))
tot.obs
#There are 19,126,783 observations in my current dataset of full.ma.data. 

#2 
plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
plan.type.table
#There are 27 different plan_types in the data. 

#3 

plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) 
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_")

knitr::kable(plan.type.year1, col.names=c("Plan Type", "2007","2008", "2009","2010","2011","2012","2013","2014","2015"),
             type="html", caption = "Plan Count by Year", booktabs = TRUE)
view(plan.type.year1)
#4 

final.plans <- full.ma.data %>%
  filter(snp== 'No' & eghp == "No" &
           (planid < 800 | planid >= 900))

plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year,-n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_")
view(plan.type.year2)

knitr::kable(plan.type.year2, col.names=c("Plan Type", "2007","2008", "2009","2010","2011","2012","2013","2014","2015"),
             type="html", caption = "Plan Count by Year", booktabs = TRUE)

#5 

final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))

fig.avg.enrollment <- final.data %>%
  group_by(fips, year) %>%
  select(fips, year, avg_enrollment) %>%
  ggplot(aes(x=factor(year), y=all_enroll)) +
  stat_summary(fun = "mean", geom="bar") +
  labs(
    x="Year",
    y="People", 
    title =""
  ) + scale_y_continuous(labels=comma) +
  theme_bw()





 


