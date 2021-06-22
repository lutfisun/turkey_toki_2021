# lutfi sun June 11 2021

# housekeeping
rm(list=ls())

library(readr)
library(tidyverse)
library(zoo)

#-----------------------------------
# toki

tokidist_01 <- read_csv("data/toki_edit2.csv")

tokidist_01$pdy <- paste(tokidist_01$province, 
                         tokidist_01$district,
                         tokidist_01$year,
                         sep= "_")

# date was missing in 105 observations. year guessed as 2003-2005 in most of these 
# based on their order of entry in the data. i will keep them in the yearly table
# and discard these projects in the monthly table:
#   * toki_yearly     : includes the 105 projects where date entry was missing
#   * toki_monthly    : the 105 projects excluded
#   * toki_quarterly  : the 105 projects excluded

#-----------------------------------
# toki_02
# for the 105 incomplete observations, i am assigning 6 as month just to use 
# the second quarter's macro variables for conversion (real prices etc)
# this is for yearly table. for monthly table, we will disregard those 
# observations anyway

toki_02 <- tokidist_01 %>%
  mutate(na_date = is.na(month),                    # to mark the 105 obs
         month   = replace(month, is.na(month), 6), # for conversion purposes
         ym   = as.yearmon(paste(year, month), "%Y %m"),
         yq   = as.yearqtr(ym, format = "%Y-%m-%d")
         )

#-----------------------------------
## add macroeconomic variables
macroecon <- read_csv("data/evds_clean.csv") %>%
  select(date, usd_buy, cpi03_all, ppi_03, gov_rev, gdp)

macroecon$yq <- gsub("-", " ", macroecon$date)
macroecon$year <- as.numeric( str_split_fixed(macroecon$date, "-", 2)[,1])

# merge toki with macro
toki_02 <- toki_02 %>% 
  mutate(ch_yq = as.character(yq)) %>% 
  left_join(macroecon, by = c("ch_yq"="yq", "year" = "year"))  %>% 
  select(-c(date, ch_yq))

names(toki_02)
# make real and dollar values
toki_02 <- toki_02  %>% 
  mutate(real_try = liras * 100 / cpi03_all,
         dollars  = liras / usd_buy)
names(toki_02)

#-----------------------------------
# there are projects where the approximate cost (or the winning bid) is missing. 
sum(is.na(toki_02$liras)) # 204 projects.
sum(is.na(toki_02$liras)) / length(toki_02$liras) # 6.03 percent
# we don't know the monetary cost for 204 of 3381 projects.
# 87 of these are in 2021 and 47 others are in the final quarter of 2020.
# This suggests that either the bidding is not over for these projects or the 
# results have not been entered into the toki website at the time of scraping.
# I suggest that we omit the projects in 2021 and use the average winning bid
# in a given category as our proxy for the rest of them (204-87=117 projects)
#   1. omit 2021 entries
#   2. replace na liras with category average (real or usd average)

toki_03  <-  toki_02 %>% 
  filter(year < 2021) %>% 
  filter(category != "hizmet_alimi")
sum(is.na(toki_03$liras)) # 115 projects.
sum(is.na(toki_03$liras)) / length(toki_03$liras) # 3.5 percent

# there are two projects in category hizmet_alimi neither have any infor on 
# cost or number of houses. this category was added in late 2020

cat_avgs <- toki_03  %>% 
  group_by(category)  %>% 
  summarize_at(vars(real_try, dollars), funs(mean(., na.rm = TRUE)))

try_model <- lm(real_try ~ category, data=toki_03)
usd_model <- lm(dollars ~ category, data=toki_03)

toki_03$pred_try <- predict(try_model, toki_03)
toki_03$pred_usd <- predict(usd_model, toki_03)

toki_03 <- toki_03 %>%
  mutate(imp_try = real_try,                   
         imp_usd = dollars)

toki_03[ is.na(toki_03$imp_try) , "imp_try" ] <- 
  toki_03[ is.na(toki_03$imp_try), "pred_try" ]

toki_03[ is.na(toki_03$imp_usd) , "imp_usd" ] <- 
  toki_03[ is.na(toki_03$imp_usd), "pred_usd" ]

#-----------------------------------
# toki_yearly

names(toki_03)
toki_yearly <- toki_03 %>%
  group_by(pdy, category) %>%
  summarize_at(vars(imp_try, imp_usd, real_try, dollars, houses), 
               funs(sum, n()))

toki_yearly <- toki_yearly %>% 
  rename(projects = houses_n) %>% 
  select(-ends_with("_n"))

caty <- unique(toki_yearly$category)

for (i in caty) { print(i)}

toki_yearly$try_idare_konut    <- 0
toki_yearly$try_afet_konutu    <- 0
toki_yearly$try_alt_gelir      <- 0
toki_yearly$try_ikmal_ihalesi  <- 0
toki_yearly$try_protokol_meb   <- 0
toki_yearly$try_restorasyon    <- 0
toki_yearly$try_altyapi_sosyal <- 0
toki_yearly$try_danismanlik_hizmetleri <- 0
toki_yearly$try_stadyum                <- 0
toki_yearly$try_millet_bahcesi         <- 0
toki_yearly$try_gelir_paylasimli       <- 0
toki_yearly$try_konut_sosyal_donati    <- 0
toki_yearly$try_bakim_onarim           <- 0
toki_yearly$try_kamu_hizmet            <- 0
toki_yearly$try_tarimkoy               <- 0
toki_yearly$try_kentsel_donusum        <- 0
toki_yearly$try_diger                  <- 0
toki_yearly$try_talep_organizasyon     <- 0

toki_yearly$usd_idare_konut    <- 0
toki_yearly$usd_afet_konutu    <- 0
toki_yearly$usd_alt_gelir      <- 0
toki_yearly$usd_ikmal_ihalesi  <- 0
toki_yearly$usd_protokol_meb   <- 0
toki_yearly$usd_restorasyon    <- 0
toki_yearly$usd_altyapi_sosyal <- 0
toki_yearly$usd_danismanlik_hizmetleri <- 0
toki_yearly$usd_stadyum                <- 0
toki_yearly$usd_millet_bahcesi         <- 0
toki_yearly$usd_gelir_paylasimli       <- 0
toki_yearly$usd_konut_sosyal_donati    <- 0
toki_yearly$usd_bakim_onarim           <- 0
toki_yearly$usd_kamu_hizmet            <- 0
toki_yearly$usd_tarimkoy               <- 0
toki_yearly$usd_kentsel_donusum        <- 0
toki_yearly$usd_diger                  <- 0
toki_yearly$usd_talep_organizasyon     <- 0

names(toki_yearly)
for (i in 1:length(toki_yearly$pdy)) {
  if (toki_yearly$category[i] == 'idare_konut') {
    toki_yearly$try_idare_konut[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_idare_konut[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'restorasyon') {
    toki_yearly$try_restorasyon[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_restorasyon[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'gelir_paylasimli') {
    toki_yearly$try_gelir_paylasimli[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_gelir_paylasimli[i]   <- toki_yearly$imp_usd_sum[i]
  }  
  else if (toki_yearly$category[i] == 'tarimkoy') {
    toki_yearly$try_tarimkoy[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_tarimkoy[i]   <- toki_yearly$imp_usd_sum[i]
  }  
  else if (toki_yearly$category[i] == 'afet_konutu') {
    toki_yearly$try_afet_konutu[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_afet_konutu[i]   <- toki_yearly$imp_usd_sum[i]
  } 
  else if (toki_yearly$category[i] == 'danismanlik_hizmetleri') {
    toki_yearly$try_danismanlik_hizmetleri[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_danismanlik_hizmetleri[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'konut_sosyal_donati') {
    toki_yearly$try_konut_sosyal_donati[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_konut_sosyal_donati[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'kentsel_donusum') {
    toki_yearly$try_kentsel_donusum[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_kentsel_donusum[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'alt_gelir') {
    toki_yearly$try_alt_gelir[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_alt_gelir[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'altyapi_sosyal') {
    toki_yearly$try_altyapi_sosyal[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_altyapi_sosyal[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'bakim_onarim') {
    toki_yearly$try_bakim_onarim[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_bakim_onarim[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'diger') {
    toki_yearly$try_diger[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_diger[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'ikmal_ihalesi') {
    toki_yearly$try_ikmal_ihalesi[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_ikmal_ihalesi[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'stadyum') {
    toki_yearly$try_stadyum[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_stadyum[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'kamu_hizmet') {
    toki_yearly$try_kamu_hizmet[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_kamu_hizmet[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'talep_organizasyon') {
    toki_yearly$try_talep_organizasyon[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_talep_organizasyon[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'protokol_meb') {
    toki_yearly$try_protokol_meb[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_protokol_meb[i]   <- toki_yearly$imp_usd_sum[i]
  }
  else if (toki_yearly$category[i] == 'millet_bahcesi') {
    toki_yearly$try_millet_bahcesi[i]    <- toki_yearly$imp_try_sum[i]
    toki_yearly$usd_millet_bahcesi[i]   <- toki_yearly$imp_usd_sum[i]
  }
}

toki_yearly <- toki_yearly %>% 
  select(-c(category)) %>% 
  group_by(pdy) %>% 
  summarise_each(list(sum))

write_csv(toki_yearly, 'toki_yearly.csv')

#-----------------------------------
# toki_monthly

names(toki_03)
toki_monthly <- toki_03 %>%
  filter(!na_date) %>%
  mutate(pdym = paste(pdy, month, sep= "_")) %>%
  group_by(pdym, category) %>%
  summarize_at(vars(imp_try, imp_usd, real_try, dollars, houses), 
               funs(sum, n()))

toki_monthly <- toki_monthly %>% 
  rename(projects = houses_n) %>% 
  select(-ends_with("_n"))

caty <- unique(toki_monthly$category)

for (i in caty) { print(i)}

toki_monthly$try_idare_konut    <- 0
toki_monthly$try_afet_konutu    <- 0
toki_monthly$try_alt_gelir      <- 0
toki_monthly$try_ikmal_ihalesi  <- 0
toki_monthly$try_protokol_meb   <- 0
toki_monthly$try_restorasyon    <- 0
toki_monthly$try_altyapi_sosyal <- 0
toki_monthly$try_danismanlik_hizmetleri <- 0
toki_monthly$try_stadyum                <- 0
toki_monthly$try_millet_bahcesi         <- 0
toki_monthly$try_gelir_paylasimli       <- 0
toki_monthly$try_konut_sosyal_donati    <- 0
toki_monthly$try_bakim_onarim           <- 0
toki_monthly$try_kamu_hizmet            <- 0
toki_monthly$try_tarimkoy               <- 0
toki_monthly$try_kentsel_donusum        <- 0
toki_monthly$try_diger                  <- 0
toki_monthly$try_talep_organizasyon     <- 0

toki_monthly$usd_idare_konut    <- 0
toki_monthly$usd_afet_konutu    <- 0
toki_monthly$usd_alt_gelir      <- 0
toki_monthly$usd_ikmal_ihalesi  <- 0
toki_monthly$usd_protokol_meb   <- 0
toki_monthly$usd_restorasyon    <- 0
toki_monthly$usd_altyapi_sosyal <- 0
toki_monthly$usd_danismanlik_hizmetleri <- 0
toki_monthly$usd_stadyum                <- 0
toki_monthly$usd_millet_bahcesi         <- 0
toki_monthly$usd_gelir_paylasimli       <- 0
toki_monthly$usd_konut_sosyal_donati    <- 0
toki_monthly$usd_bakim_onarim           <- 0
toki_monthly$usd_kamu_hizmet            <- 0
toki_monthly$usd_tarimkoy               <- 0
toki_monthly$usd_kentsel_donusum        <- 0
toki_monthly$usd_diger                  <- 0
toki_monthly$usd_talep_organizasyon     <- 0

names(toki_monthly)
for (i in 1:length(toki_monthly$pdym)) {
  if (toki_monthly$category[i] == 'idare_konut') {
    toki_monthly$try_idare_konut[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_idare_konut[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'restorasyon') {
    toki_monthly$try_restorasyon[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_restorasyon[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'gelir_paylasimli') {
    toki_monthly$try_gelir_paylasimli[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_gelir_paylasimli[i]   <- toki_monthly$imp_usd_sum[i]
  }  
  else if (toki_monthly$category[i] == 'tarimkoy') {
    toki_monthly$try_tarimkoy[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_tarimkoy[i]   <- toki_monthly$imp_usd_sum[i]
  }  
  else if (toki_monthly$category[i] == 'afet_konutu') {
    toki_monthly$try_afet_konutu[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_afet_konutu[i]   <- toki_monthly$imp_usd_sum[i]
  } 
  else if (toki_monthly$category[i] == 'danismanlik_hizmetleri') {
    toki_monthly$try_danismanlik_hizmetleri[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_danismanlik_hizmetleri[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'konut_sosyal_donati') {
    toki_monthly$try_konut_sosyal_donati[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_konut_sosyal_donati[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'kentsel_donusum') {
    toki_monthly$try_kentsel_donusum[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_kentsel_donusum[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'alt_gelir') {
    toki_monthly$try_alt_gelir[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_alt_gelir[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'altyapi_sosyal') {
    toki_monthly$try_altyapi_sosyal[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_altyapi_sosyal[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'bakim_onarim') {
    toki_monthly$try_bakim_onarim[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_bakim_onarim[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'diger') {
    toki_monthly$try_diger[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_diger[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'ikmal_ihalesi') {
    toki_monthly$try_ikmal_ihalesi[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_ikmal_ihalesi[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'stadyum') {
    toki_monthly$try_stadyum[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_stadyum[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'kamu_hizmet') {
    toki_monthly$try_kamu_hizmet[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_kamu_hizmet[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'talep_organizasyon') {
    toki_monthly$try_talep_organizasyon[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_talep_organizasyon[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'protokol_meb') {
    toki_monthly$try_protokol_meb[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_protokol_meb[i]   <- toki_monthly$imp_usd_sum[i]
  }
  else if (toki_monthly$category[i] == 'millet_bahcesi') {
    toki_monthly$try_millet_bahcesi[i]    <- toki_monthly$imp_try_sum[i]
    toki_monthly$usd_millet_bahcesi[i]   <- toki_monthly$imp_usd_sum[i]
  }
}

toki_monthly <- toki_monthly %>% 
  select(-c(category)) %>% 
  group_by(pdym) %>% 
  summarise_each(list(sum))

write_csv(toki_monthly, 'toki_monthly.csv')




















