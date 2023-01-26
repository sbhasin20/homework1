if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

source("data-code/1_Plan_Data.R")
source("data-code/3_Service_Areas.R")
source("data-code/Market_Penetration.R")
