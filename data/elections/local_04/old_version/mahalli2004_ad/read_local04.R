library(tidyverse)
library(vroom)
library(purrr)
#----------------------------------------------
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mahalli_list <- read_excel_allsheets("local_04/mahal2004_dz.xlsx")
names(mahalli_list[[1]])

mahalli_dz = Reduce(function(...) merge(..., all=T), mahalli_list)
names(mahalli_dz) <- tolower(names(mahalli_dz))
names(mahalli_dz)  <- gsub("\n", " ", names(mahalli_dz) )

mahalli_dz2 <- mahalli_dz %>%
  select(c("province", "ilçe", "kayıtlı seçmen sayısı", "gecerli oy sayısı", 
         "ak parti", "chp", "mhp")) %>%
  rename("district" = "ilçe",
         "registered_voters" = "kayıtlı seçmen sayısı", 
         "valid_votes" = "gecerli oy sayısı",
         "akp" = "ak parti")

mahalli_dz2$province <- tolower(mahalli_dz2$province)
mahalli_dz2$province <- gsub("ğ", "g", mahalli_dz2$province)
mahalli_dz2$province <- gsub("ç", "c", mahalli_dz2$province)
mahalli_dz2$province <- gsub('ı', 'i', mahalli_dz2$province)
mahalli_dz2$province <- gsub("ö", "o", mahalli_dz2$province)
mahalli_dz2$province <- gsub("ş", "s", mahalli_dz2$province)
mahalli_dz2$province <- gsub("ü", "u", mahalli_dz2$province)

mahalli_dz2$district <- tolower(mahalli_dz2$district)
mahalli_dz2$district <- gsub("ğ", "g", mahalli_dz2$district)
mahalli_dz2$district <- gsub("ç", "c", mahalli_dz2$district)
mahalli_dz2$district <- gsub('ı', 'i', mahalli_dz2$district)
mahalli_dz2$district <- gsub("ö", "o", mahalli_dz2$district)
mahalli_dz2$district <- gsub("ş", "s", mahalli_dz2$district)
mahalli_dz2$district <- gsub("ü", "u", mahalli_dz2$district)

setwd("local_04/mahalli2004_ad")
mahalli_ad <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

names(mahalli_ad) <- tolower(names(mahalli_ad))
names(mahalli_ad)  <- gsub("\n", " ", names(mahalli_ad) )

mahalli_ad2 <- mahalli_ad %>%
  select(c("province", "ilçe", "kayıtlı seçmen sayısı", "gecerli oy sayısı", 
           "ak parti", "chp", "mhp")) %>%
  rename("district" = "ilçe",
         "registered_voters" = "kayıtlı seçmen sayısı", 
         "valid_votes" = "gecerli oy sayısı",
         "akp" = "ak parti")

mahalli_ad2$province <- tolower(mahalli_ad2$province)
mahalli_ad2$province <- gsub("ğ", "g", mahalli_ad2$province)
mahalli_ad2$province <- gsub("ç", "c", mahalli_ad2$province)
mahalli_ad2$province <- gsub('ı', 'i', mahalli_ad2$province)
mahalli_ad2$province <- gsub("ö", "o", mahalli_ad2$province)
mahalli_ad2$province <- gsub("ş", "s", mahalli_ad2$province)
mahalli_ad2$province <- gsub("ü", "u", mahalli_ad2$province)

mahalli_ad2$district <- tolower(mahalli_ad2$district)
mahalli_ad2$district <- gsub("ğ", "g", mahalli_ad2$district)
mahalli_ad2$district <- gsub("ç", "c", mahalli_ad2$district)
mahalli_ad2$district <- gsub('ı', 'i', mahalli_ad2$district)
mahalli_ad2$district <- gsub("ö", "o", mahalli_ad2$district)
mahalli_ad2$district <- gsub("ş", "s", mahalli_ad2$district)
mahalli_ad2$district <- gsub("ü", "u", mahalli_ad2$district)

mahalli_final <- merge(mahalli_ad2, mahalli_dz2, by = intersect(names(mahalli_ad2), names(mahalli_dz2)), all = TRUE)

mahalli_final$year <- 2004
mahalli_final$province_district <- paste0(mahalli_final$province, "_", mahalli_final$district)

write_csv(mahalli_final, "../mahalli2004_fin.csv")

#----------------------------------------------