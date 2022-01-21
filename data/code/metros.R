#----------------------------------------------

library(tidyverse)
library(vroom)
library(purrr)
library(zoo)
library(tidyr)
library(janitor)
library(readxl)   

#----------------------------------------------

metro_04 <- read.csv("elections/metro/metro_2004.csv") %>% clean_names() %>% 
  rename("akp" = "ak_parti",
         "sp" = "saadet_partisi",
         "independent"  = "bagimsizlar")

names(metro_04)

metro_09 <- read_excel("elections/metro/metro_2009.xlsx", skip = 9) %>% 
  clean_names() %>% mutate(year = 2009)
metro_14 <- read_excel("elections/metro/metro_2014.xlsx", skip = 9) %>% 
  clean_names() %>% mutate(year = 2014)
metro_19 <- read_excel("elections/metro/metro_2019.xlsx", skip = 9) %>% 
  clean_names() %>% mutate(year = 2019)

metros <- bind_rows(metro_09, metro_14, metro_19) %>% 
  select(-c("sira_no", "itirazsiz_gecerli_oy_sayisi", 
            "itirazli_gecerli_oy_sayisi", "toplam_gecersiz_oy"))  %>% 
  rename("registered_voters" = "kayitli_secmen_sayisi",
         "attended_voters" = "oy_kullanan_secmen_sayisi",
         "valid_votes" = "toplam_gecerli_oy",
         "akp" = "ak_parti",
         "sp" = "saadet",
         "independent"  = "bagimsiz_toplam_oy",
         "province" = "il_adi")

metros <- bind_rows(metro_04, metros)
  
metros$province <- tolower(metros$province)
metros$province <- gsub("ğ", "g", metros$province)
metros$province <- gsub("ç", "c", metros$province)
metros$province <- gsub('ı', 'i', metros$province)
metros$province <- gsub("ö", "o", metros$province)
metros$province <- gsub("ş", "s", metros$province)
metros$province <- gsub("ü", "u", metros$province)

names(metros)

write_csv(metros, 'metros.csv')

