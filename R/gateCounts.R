# Date: 2 Sept 2022

#### Set working directory ####
# AdminDrive Data
setwd("~/Library/CloudStorage/OneDrive-UniversityofToronto/UTorontoLibrary/DocumentsShared/Topics/Reporting/Association of ResearchLibraries(ARL)_Sept2021/ARL_Submissions/2021-2022/Q23")

#### Load needed packages  ####
# install.packages("reshape2")
# install.packages("gridExtra")
library("gridExtra")
library("stringr")
library("data.table")
library("EnvStats")
library("ggpubr")
library("reshape2")
library("tidyverse")
library("readxl")
library("magrittr")
library("lubridate")
library("ggplot2")
# install.packages("writexl")
library("writexl")
library("lubridate")
library("stringr")

##### Take initial circulations from Alma for 2022 ####

gateCountsFY2022 <- readxl::read_excel("UTL Daily Exit Counts WITH CALCS  Apr2020 with corrected calculations_AS_svd1Sept2022.xlsx",
                                       sheet = 2)
dim(gateCountsFY2022) #  3167   14
glimpse(gateCountsFY2022)

FYanalyzed <- c("2021", "2022")
yearMonthAnalyzed <- c("20215",
                       "20216",
                       "20217",
                       "20218",
                       "20219",
                       "202110",
                       "202111",
                       "202112",
                       "20221",
                       "20222",
                       "20223",
                       "20224")


rbs1FloorNORTH <- gateCountsFY2022 %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 1st floor NORTH") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  # replace values less 0 with 0
  dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x < 0, 0, x))) %>%
  sum(na.rm = TRUE) %>%
  round(digits = 0) # 48740



rbs1FloorSOUTH <- gateCountsFY2022 %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 1st floor SOUTH") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  # replace values less 0 with 0
  dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x < 0, 0, x))) %>%
  sum(na.rm = TRUE) %>%
  round(digits = 0) # 27658



rbsP4Ele <- gateCountsFY2022 %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 2nd floor P4 elevator") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  # replace values less 0 with 0
  dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x < 0, 0, x))) %>%
  sum(na.rm = TRUE) %>%
  round(digits = 0) # 1258


rbs2Main <- gateCountsFY2022 %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 2nd Floor - MAIN") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  # replace values less 0 with 0
  dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x < 0, 0, x))) %>%
  sum(na.rm = TRUE) %>%
  round(digits = 0) # 617060

# Gerstein
# March 10-11 is questionable
gersteinCounts <- gateCountsFY2022 %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Gerstein") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  # replace values less 0 with 0
  dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x < 0, 0, x))) %>%
  sum(na.rm = TRUE) %>%
  round(digits = 0) # 191482

##### Analyze Raw counts ####
# 9 Sept 2022

gateCountsFY2022raw <- readxl::read_excel("UTL Daily Exit Counts WITH CALCS  Apr2020 with corrected calculations_AS_svd1Sept2022.xlsx",
                                       sheet = 1)
dim(gateCountsFY2022raw) #  4507   14
glimpse(gateCountsFY2022raw)

gateType <- "Two-way" # then needs to divide by two
# "One-way"

FYanalyzed <- c("2021", "2022")
yearMonthAnalyzed <- c("20215",
                       "20216",
                       "20217",
                       "20218",
                       "20219",
                       "202110",
                       "202111",
                       "202112",
                       "20221",
                       "20222",
                       "20223",
                       "20224")

# Obtain data for year and month of FY2022
rbs1FloorNORTH <- gateCountsFY2022raw %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 1st floor NORTH") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble()

xValue <- 2:nrow(rbs1FloorNORTH)
collectValue <- vector(mode = "numeric", length = length(xValue))
# Initially:
# table(is.na(collectValue))
# FALSE  TRUE
# 32   332

# Later
# table(is.na(collectValue))
# FALSE  TRUE
# 39   325

for (i in seq(along = xValue)) {
  if(gateType == "Two-way") {
    collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                rbs1FloorNORTH[i, ]) / 2)

    # If an NA, then check if the i + 1 or i is NA
    if(is.na(collectValue[i]) == TRUE) {
      cat("\n", i, " Entered checking \n")
      # If i + 1 is NA = will not be addressed

      # If i is NA, and it is not the very first count of FY
      if((is.na(rbs1FloorNORTH[i, ]) == TRUE) && (i >= 2)) {
        # find the first number in the past that is not NA
        if((is.na(rbs1FloorNORTH[i - 1, ]) != TRUE) && (nrow(rbs1FloorNORTH[i - 1, ]) != 0)) {
          collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                      rbs1FloorNORTH[i - 1, ]) / 2)
        } else if ((is.na(rbs1FloorNORTH[i - 2, ]) != TRUE) && (nrow(rbs1FloorNORTH[i - 2, ]) != 0)) {
          collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                      rbs1FloorNORTH[i - 2, ]) / 2)
        } else if ((is.na(rbs1FloorNORTH[i - 3, ]) != TRUE) && (nrow(rbs1FloorNORTH[i - 3, ]) != 0)) {
          collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                      rbs1FloorNORTH[i - 3, ]) / 2)
        } else if ((is.na(rbs1FloorNORTH[i - 4, ]) != TRUE) && (nrow(rbs1FloorNORTH[i - 4, ]) != 0)) {
        collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                    rbs1FloorNORTH[i - 4, ]) / 2)
        } else if ((is.na(rbs1FloorNORTH[i - 5, ]) != TRUE) && (nrow(rbs1FloorNORTH[i - 5, ]) != 0)) {
          collectValue[i] <- ceiling((rbs1FloorNORTH[i + 1, ] -
                                      rbs1FloorNORTH[i - 5, ]) / 2)
        } else {
          collectValue[i] <- NA # i.e., if one of the first values with
        } # no preceding entry with a numeric value
      }
    }
  }

}

# check scenarios
# if value is negative, then counter reset or entry typo
# if NA values, can you add average


unlist(collectValue)
paste(is.na(collectValue), collapse=" ")
str_locate_all(string = paste(is.na(collectValue), collapse=" "),
           pattern = c("TRUE FALSE TRUE"))

str_locate_all(string = paste(is.na(collectValue), collapse=" "),
               pattern = c("TRUE FALSE FALSE TRUE"))

