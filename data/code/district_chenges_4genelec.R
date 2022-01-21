# Lutfi Sun - January 2021

#----------------------------------------------

library(tidyverse)
library(readr)

#----------------------------------------------

sdc <- read_csv("data/elections/some_district_changes.csv")

tr_to_eng <- function(tr_vector) {
  tr_vector <- tolower(tr_vector)
  tr_vector <- gsub("ğ", "g", tr_vector)
  tr_vector <- gsub("ç", "c", tr_vector)
  tr_vector <- gsub('ı', 'i', tr_vector)
  tr_vector <- gsub("ö", "o", tr_vector)
  tr_vector <- gsub("ş", "s", tr_vector)
  tr_vector <- gsub("ü", "u", tr_vector)
  return(tr_vector)
}

sdc$old <- tr_to_eng(sdc$old)
sdc$new <- tr_to_eng(sdc$new)
sdc$`Action taken on general elections` <- tr_to_eng(sdc$`Action taken on general elections`)

write_csv(sdc, "data/elections/district_changes_fin.csv")

