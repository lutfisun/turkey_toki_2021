# lutfi sun June 21 2021

# housekeeping
rm(list=ls())

library(readr)
library(tidyverse)
library(zoo)

#-----------------------------------
# merge population and toki_yearly

popi <- read_csv("macro_and_population/population02_20.csv")  %>%
  mutate(pdy = paste(province_district, year, sep= "_"))  %>%
  select(-c(year_province_district))

gen_elec <- read_csv("elections/gen_elections.csv")  %>%
  mutate(pdy = paste(province_district, year, sep= "_"))  %>%
  select(-c(year_province_district))

popi_gen <- popi  %>% 
  left_join(gen_elec, by = c("pdy"="pdy", "year" = "year",
                             "province" = "province", "district" = "district"))

toki_yearly <- read_csv("temp/toki_yearly.csv") 
  
merged_yearly <- popi  %>% 
  left_join(toki_yearly, by = c("pdy"="pdy"))  %>%
  select(-c(year_province_district))

names(merged_yearly)

general_elections_district.csv
