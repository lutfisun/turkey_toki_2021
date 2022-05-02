# lutfi sun - Jan 2022
# event study: mayor change on toki investment

# housekeeping
rm(list=ls())

library(tidyverse)

#------------------------------------------------------------------------------
# will use local elections for mayor
# load and transform data

locals_municipal <- read_csv("data/locals.csv") 

locals_district <- locals_municipal %>% 
  rowwise() %>% 
  mutate(
    max_vote = max(c_across(anavatan:shp)),
    mayor_akp = 1*(akp >= max_vote),
    mayor_chp = 1*(chp >= max_vote),
    mayor_mhp = 1*(mhp >= max_vote),
    mayor_hdp = 1*(hdp >= max_vote),
    n_district = 1
    ) %>% 
  select(-c(8:42), -municipality) %>% 
  group_by(province, district, year) %>%
  summarise_all(list(sum)) %>% 
  rowwise() %>% 
  mutate(
    akp_share = (mayor_akp / n_district),
    chp_share = (mayor_chp / n_district),
    is_akp = 1*(akp_share >= 0.5),
    is_chp = 1*(chp_share >= 0.5),
    is_oth = 1*(!is_akp)*(!is_chp)
    )
#------------------------------------------------------------------------------
# see how often and what kinds of party shifts happened

locals_district <- locals_district %>%
  group_by(province, district) %>%
  mutate(
    lag_isakp = dplyr::lag(is_akp, n = 1, default = NA),
    lag_ischp = dplyr::lag(is_chp, n = 1, default = NA),
    lag_isoth = dplyr::lag(is_oth, n = 1, default = NA),
    akp_akp   = lag_isakp * is_akp,
    akp_chp   = lag_isakp * is_chp,
    akp_oth   = lag_isakp * is_oth,
    chp_akp   = lag_ischp * is_akp,
    chp_chp   = lag_ischp * is_chp,
    chp_oth   = lag_ischp * is_oth,
    oth_akp   = lag_isoth * is_akp,
    oth_chp   = lag_isoth * is_chp,
    oth_oth   = lag_isoth * is_oth
  ) %>% select(province, district, year, c(15:29))

names(locals_district)
library(psych)
describe(locals_district[ , c(4:14)], fast=TRUE)

#------------------------------------------------------------------------------
# add toki info

toki_district <- read_csv("data/merged_yearly02.csv")[ -c(25:60) ] %>% 
  select("province","district", "pd","year", "population", 
         "try_pc","usd_pc","projects") %>% 
  left_join(locals_district, by=c("province", "district", "year")) %>% 
  arrange(pd,year) %>% 
  subset(year != 2002 & year != 2003)

# might be useful to add as controls:
# "turnout_rate", "cabinet_origin","cabinet_represents","gen_altered"

#------------------------------------------------------------------------------
# event study 2009

event_09 <- toki_district   %>% 
  subset( year > 2004 & year < 2014 ) %>% 
  select(-c(is_akp:lag_isoth)) %>% 
  group_by(province, district) %>% 
  mutate_at(
    vars(akp_akp:oth_oth),  ~replace(., is.na(.), mean(.,na.rm = TRUE))
    ) %>% 
  mutate(
    party = "other",
    party = replace(party, akp_akp == 1, "akp_akp"),
    party = replace(party, akp_chp == 1, "akp_chp"),
    party = replace(party, chp_akp == 1, "chp_akp"),
    party = replace(party, chp_chp == 1, "chp_chp")
    ) %>%
   group_by(year, party) %>% 
  summarise_at(c("try_pc", "usd_pc", "projects"), mean, na.rm = TRUE)

names(event_09)

gr_usd_09 <-ggplot(event_09, aes(x=year, y=usd_pc, group=party)) +
  geom_line(aes(color=party))+
  geom_point(aes(color=party))+
  geom_vline(xintercept=c(2009),linetype=4, colour="black") +
  ggtitle("USD per Capita TOKI Investments around 2009 Local Elections")

gr_usd_09

###

event_14 <- toki_district   %>% 
  subset( year > 2009 & year < 2019 ) %>% 
  select(-c(is_akp:lag_isoth)) %>% 
  group_by(province, district) %>% 
  mutate_at(
    vars(akp_akp:oth_oth),  ~replace(., is.na(.), mean(.,na.rm = TRUE))
  ) %>% 
  mutate(
    party = "other",
    party = replace(party, akp_akp == 1, "akp_akp"),
    party = replace(party, akp_chp == 1, "akp_chp"),
    party = replace(party, chp_akp == 1, "chp_akp"),
    party = replace(party, chp_chp == 1, "chp_chp")
  ) %>%
  group_by(year, party) %>% 
  summarise_at(c("try_pc", "usd_pc", "projects"), mean, na.rm = TRUE)

gr_usd_14 <-ggplot(event_14, aes(x=year, y=usd_pc, group=party)) +
  geom_line(aes(color=party))+
  geom_point(aes(color=party))+
  geom_vline(xintercept=c(2014),linetype=4, colour="black") +
  ggtitle("USD per Capita TOKI Investments around 2014 Local Elections")

gr_usd_14

####

event_19 <- toki_district   %>% 
  subset( year > 2015 & year < 2021 ) %>% 
  select(-c(is_akp:lag_isoth)) %>% 
  group_by(province, district) %>% 
  mutate_at(
    vars(akp_akp:oth_oth),  ~replace(., is.na(.), mean(.,na.rm = TRUE))
  ) %>% 
  mutate(
    party = "other",
    party = replace(party, akp_akp == 1, "akp_akp"),
    party = replace(party, akp_chp == 1, "akp_chp"),
    party = replace(party, chp_akp == 1, "chp_akp"),
    party = replace(party, chp_chp == 1, "chp_chp")
  ) %>%
  group_by(year, party) %>% 
  summarise_at(c("try_pc", "usd_pc", "projects"), mean, na.rm = TRUE)

gr_usd_19 <-ggplot(event_19, aes(x=year, y=usd_pc, group=party)) +
  geom_line(aes(color=party))+
  geom_point(aes(color=party))+
  geom_vline(xintercept=c(2019),linetype=4, colour="black") +
  ggtitle("USD per Capita TOKI Investments around 2019 Local Elections")

gr_usd_19

###


spec_09 <- toki_district   %>% 
  subset( year > 2004 & year < 2014 ) %>% 
  select(-c(is_akp:lag_isoth)) %>% 
  group_by(province, district) %>% 
  mutate_at(
    vars(akp_akp:oth_oth),  ~replace(., is.na(.), mean(.,na.rm = TRUE))
  ) %>% 
  mutate(
    party = "other",
    party = replace(party, akp_akp == 1, "akp_akp"),
    party = replace(party, akp_chp == 1, "akp_chp"),
    party = replace(party, chp_akp == 1, "chp_akp"),
    party = replace(party, chp_chp == 1, "chp_chp")
  )

spec_14 <- toki_district   %>% 
  subset( year > 2009 & year < 2019 ) %>% 
  select(-c(is_akp:lag_isoth)) %>% 
  group_by(province, district) %>% 
  mutate_at(
    vars(akp_akp:oth_oth),  ~replace(., is.na(.), mean(.,na.rm = TRUE))
  ) %>% 
  mutate(
    party = "other",
    party = replace(party, akp_akp == 1, "akp_akp"),
    party = replace(party, akp_chp == 1, "akp_chp"),
    party = replace(party, chp_akp == 1, "chp_akp"),
    party = replace(party, chp_chp == 1, "chp_chp")
  )  %>% 
  subset( year > 2009 & year < 2019 )

