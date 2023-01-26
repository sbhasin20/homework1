#########################################################################
## Read in enrollment data for january of each year
#########################################################################
install.packages("usethis")
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)



for (y in 2007:2015) {
  ## Basic contract/plan information
  ma.path=paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Contract_Info_",y,"_01.csv")
  contract.info=read_csv(ma.path,
                         skip=1,
                         col_names = c("contractid","planid","org_type","plan_type",
                                       "partd","snp","eghp","org_name","org_marketing_name",
                                       "plan_name","parent_org","contract_date"),
                         col_types = cols(
                           contractid = col_character(),
                           planid = col_double(),
                           org_type = col_character(),
                           plan_type = col_character(),
                           partd = col_character(),
                           snp = col_character(),
                           eghp = col_character(),
                           org_name = col_character(),
                           org_marketing_name = col_character(),
                           plan_name = col_character(),
                           parent_org = col_character(),
                           contract_date = col_character()
                         ))
  
  contract.info = contract.info %>%
    group_by(contractid, planid) %>%
    mutate(id_count=row_number())
  
  contract.info = contract.info %>%
    filter(id_count==1) %>%
    select(-id_count)
  
  ## Enrollments per plan
  ma.path=paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Enrollment_Info_",y,"_01.csv")
  enroll.info=read_csv(ma.path,
                       skip=1,
                       col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
                       col_types = cols(
                         contractid = col_character(),
                         planid = col_double(),
                         ssa = col_double(),
                         fips = col_double(),
                         state = col_character(),
                         county = col_character(),
                         enrollment = col_double()
                       ),na="*")
  
  
  ## Merge contract info with enrollment info
  plan.data = contract.info %>%
    left_join(enroll.info, by=c("contractid", "planid")) %>%
    mutate(year=y)
  
  ## Fill in missing fips codes (by state and county)
  plan.data = plan.data %>%
    group_by(state, county) %>%
    fill(fips)
  
  ## Fill in missing plan characteristics by contract and plan id
  plan.data = plan.data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan.data = plan.data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)
  
  ## Collapse from monthly data to yearly
  plan.year = plan.data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) %>%
    rename(avg_enrollment=enrollment)
  
  write_rds(plan.year,paste0("data/output/ma_data_",y,".rds"))
}

full.ma.data <- read_rds("data/output/ma_data_2007.rds")
for (y in 2008:2015) {
  full.ma.data <- rbind(full.ma.data,read_rds(paste0("data/output/ma_data_",y,".rds")))
}

write_rds(full.ma.data,"data/output/full_ma_data.rds")
sapply(paste0("ma_data_", 2007:2015, ".rds"), unlink)

#Homework 1 #make sure you have all the packages 

#Enrollment Data 

#1. 

tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))

#There are 19,126,783 observations in my current data set. 

#2 There are 27 different plan_type in the data

plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)

#3 

plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year,-n) %>% filter(plan_type, NA)
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_")

#view after 

final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))
  
#4

final.plans <- full.ma.data %>%
  filter(snp= 'No' & eghp == "No" &
  (planid < 800 | planid >= 900))

final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))

final.data.pen <- final.data %>% 
  left_join( ma.penetration.data %>% ungroup() %>%)
              rename(state_long=state, country_long)
              
              
plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year,-n) %>% filter(plan_type, NA)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_")
#view after 

#5 

final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
             select(contractid, fips, year), 
           by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))


#enrollment figure 

fig.avg.enrollment <- final.data %>%
  group_by(flips, year) %>%
  select(flips, year, avg_enrollment) %>%
  summarize(all_enroll=sum(avg_enrollment)) %>%
  ggplot2(aes(x=as, factor(year), y=all_enroll)) +
  stat_summary(fun.y="mean", geom = "bar") +
  labs(
    x= "Year"
    y= "People"
    title =""
  ) + scale_y_continous(labels=comma) +
  theme_bw()

rm(list=c("full.ma.data, "contract.info"")) # basically drop everything in inviorment except fig.avg. enroll + plan.type,table + year1

#save.image("Hw1_workspace.Rdata") #save it as an image 


#7 



#8 I think we dropped it since there were replciates when we merged the dataset

#9 The beneficiary is not making a profit since they are charging the expect value of the cost. 

#10 It has been very challenging to work with this data. I think getting the tables was a learnign experience since you had to make sure R knew exactly what data to put where. Also, it was aggervating when the errors would come, then I would try to fix them and they would keep occuring. 