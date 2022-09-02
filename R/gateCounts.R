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


test <- gateCountsFY2022 %>%
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
  sum(na.rm = TRUE)




