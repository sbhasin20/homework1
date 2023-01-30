if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

library(scales)

tinytex::install_tinytex()

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
view(plan.type.table)
#There are 26 different plan_types in the data. 

#3 

plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) 
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "")

knitr::kable(plan.type.year1, col.names=c("Plan Type", "2007","2008", "2009","2010","2011","2012","2013","2014","2015"),
             type="html", caption = "Plan Count by Year", booktabs = TRUE)
view(plan.type.year1)

#4 

final.plans <- full.ma.data %>%
  filter(snp== 'No' & eghp == "No" &
           (planid < 800 | planid >= 900))

plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year,-n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "")
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
  filter(year !=2007) %>%
  select(fips, year, avg_enrollment) %>%
  summarize(all_enroll=sum(avg_enrollment)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=all_enroll)) +
  stat_summary(fun="mean", geom = "bar") +
  labs(
    x= "Year",
    y= "People",
    title =""
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

fig.avg.enrollment

#6 
final.data.pen <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))

final.state <- final.data.pen %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))

final.data <- final.data %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year")) 

final.data3 <- ggplot(final.data, aes(x=year, y=premium)) +
  stat_summary(fun="mean", geom = "line") +
  labs(
    x= "Year",
    y= "Average Premium Over",
    title =""
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

final.data3
 
#7

final_data2 <- final.data %>% 
  filter(!is.na(premium))%>%
  group_by(year)%>% 
  summarize(perc_0 = ((sum(premium == 0))/n())* 100)%>% 
  ggplot( aes(year, perc_0))+
  geom_line()+
  labs( title = "", x = 'Year', y = 'Percentage of $0 Premium Plans')+
  theme_minimal()

final_data2

save.image("Hw1_workspace.Rdata")


