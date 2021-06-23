# Lutfi Sun | June 4 2021

# housekeeping
rm(list=ls())

# install and load packages
library(tidyverse)
library(readxl)
library(stringr)

library(readr)
library(writexl)
library(xlsx)

# read data

toki20 <- read_csv("toki/toki20_scrape.csv") %>% 
  janitor::clean_names() %>% 
  mutate(size_liras = replace(size_liras, size_liras == 0, NA),
         houses = replace(houses, houses == 0, NA))

toki21 <- read_csv("toki/toki21_edit1.csv") %>% 
  janitor::clean_names()%>% 
  mutate(liras = replace(liras, liras == 0, NA),
         konut_sayisi = replace(konut_sayisi, konut_sayisi == 0, NA))

names(toki20)
names(toki21)

#
toki20$province <- tolower(toki20$province)
toki20$province <- gsub("ğ", "g", toki20$province)
toki20$province <- gsub("ç", "c", toki20$province)
toki20$province <- gsub('ı', 'i', toki20$province)
toki20$province <- gsub("ö", "o", toki20$province)
toki20$province <- gsub("ş", "s", toki20$province)
toki20$province <- gsub("ü", "u", toki20$province)

#
toki21$province <- tolower(toki21$province)
toki21$province <- gsub("ğ", "g", toki21$province)
toki21$province <- gsub("ç", "c", toki21$province)
toki21$province <- gsub('ı', 'i', toki21$province)
toki21$province <- gsub("ö", "o", toki21$province)
toki21$province <- gsub("ş", "s", toki21$province)
toki21$province <- gsub("ü", "u", toki21$province)

#

joined21 <- toki21  %>% 
  left_join(toki20, by = c("year"="year", "liras" = "size_liras",
                           "category" = "category", "province"="province"))  %>% 
  select("sira_no", "category", "year", "month.x", "proje_adi_ve_bolgesi", "yuklenici_firma", 
         "fiziki_gercek", "liras", "konut_sayisi", "province", "district")
  
names(joined21)
####
joined21$district <- tolower(joined21$district)
joined21$district <- gsub("ğ", "g", joined21$district)
joined21$district <- gsub("ç", "c", joined21$district)
joined21$district <- gsub('ı', 'i', joined21$district)
joined21$district <- gsub("ö", "o", joined21$district)
joined21$district <- gsub("ş", "s", joined21$district)
joined21$district <- gsub("ü", "u", joined21$district)

#
joined21$proje_adi_ve_bolgesi <- tolower(joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub("ğ", "g", joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub("ç", "c", joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub('ı', 'i', joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub("ö", "o", joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub("ş", "s", joined21$proje_adi_ve_bolgesi)
joined21$proje_adi_ve_bolgesi <- gsub("ü", "u", joined21$proje_adi_ve_bolgesi)

#
joined21$yuklenici_firma <- tolower(joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub("ğ", "g", joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub("ç", "c", joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub('ı', 'i', joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub("ö", "o", joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub("ş", "s", joined21$yuklenici_firma)
joined21$yuklenici_firma <- gsub("ü", "u", joined21$yuklenici_firma)


####

joined21$first <- NA
joined21$second <- NA
joined21$third <- NA
joined21$fourth <- NA

for (i in 1:3404) {
  current_proj <- strsplit((joined21$proje_adi_ve_bolgesi[i]), " ")[[1]]
  joined21$first[i] <- current_proj[1]
  joined21$second[i] <- current_proj[2]
  joined21$third[i] <- current_proj[3]
  joined21$fourth[i] <- current_proj[4]
}

write_csv(joined21, 'temp/toki_names1234.csv')
