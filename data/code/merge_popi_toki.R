# lutfi sun June 21 2021

# housekeeping
rm(list=ls())

library(readr)
library(tidyverse)
library(zoo)

#-------------------------------------------
# add in the new 2020 population figues

popi02 <- read_csv("macro_and_population/population02_20.csv")  %>%
  select(-c(year_province_district, province_district)) %>% 
  filter(year < 2020)

popi20 <- read_csv("macro_and_population/popi20.csv")

popi20$province <- tolower(popi20$province)
popi20$province <- gsub("ğ", "g", popi20$province)
popi20$province <- gsub("ç", "c", popi20$province)
popi20$province <- gsub('ı', 'i', popi20$province)
popi20$province <- gsub("ö", "o", popi20$province)
popi20$province <- gsub("ş", "s", popi20$province)
popi20$province <- gsub("ü", "u", popi20$province)

popi20$district <- tolower(popi20$district)
popi20$district <- gsub("ğ", "g", popi20$district)
popi20$district <- gsub("ç", "c", popi20$district)
popi20$district <- gsub('ı', 'i', popi20$district)
popi20$district <- gsub("ö", "o", popi20$district)
popi20$district <- gsub("ş", "s", popi20$district)
popi20$district <- gsub("ü", "u", popi20$district)

##

popi <- bind_rows(popi02, popi20)  %>%
  mutate(pd  = paste(province, district, sep= "_"),
         pdy = paste(pd, year, sep= "_")) %>%
  arrange(pdy)

#-------------------------------------------
# merge population, election, and toki_yearly

gen_elec <- read_csv("elections/gen_elections.csv")  %>%
  mutate(pdy = paste(province_district, year, sep= "_"))  %>%
  select(-c(year_province_district, province_district))

cabinet <- read_csv("elections/cabinet_districtchanges.csv")
  
popi_gen <- popi  %>% 
  left_join(gen_elec, by = c("pdy"="pdy", "year" = "year",
                             "province" = "province", "district" = "district"))  %>% 
  left_join(cabinet, by = c("year" = "year", "province" = "province", "district" = "district"))

toki_yearly <- read_csv("temp/toki_yearly.csv")   %>% 
  select(-c(imp_try_sum, imp_usd_sum))

merged_yearly <- popi_gen  %>% 
  left_join(toki_yearly,  
            by = c("pdy"="pdy"))

names(merged_yearly)

merged_yearly[is.na(merged_yearly)] <- 0

merged_yearly <- merged_yearly  %>% 
  mutate(try_pc = real_try_sum / population,
         usd_pc = dollars_sum / population,
         hou_pc = houses_sum / population)


write_csv(merged_yearly, 'merged_yearly02.csv')


partisan_try <- lm(try_pc ~ akp + population + turnout_rate + as.character(year),
                   data = merged_yearly)
partisan_usd <- lm(usd_pc ~ akp + population + turnout_rate + as.character(year),
                   data = merged_yearly)
partisan_hou <- lm(hou_pc ~ akp + population + turnout_rate + as.character(year),
                   data = merged_yearly)

summary(partisan_try)
summary(partisan_usd)
summary(partisan_hou)


