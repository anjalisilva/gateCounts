# Date: 2 Sept 2022

#### Set working directory ####
# ForAlma data
setwd("~/Library/CloudStorage/OneDrive-UniversityofToronto/UTorontoLibrary/DocumentsShared/Topics/Reporting/Association of ResearchLibraries(ARL)_Sept2021/ARL_Submissions/2021-2022/Q23")

#### Load needed packages  ####
# install.packages("reshape2")
# install.packages("gridExtra")
library("gridExtra")
library("stringr")
library("data.table")
library("EnvStats")
library("ggpubr")
library("car")
library("reshape2")

library("tidyverse")
library("readxl")
library("magrittr")
library("lubridate")
library("ggplot2")
# install.packages("writexl")
library("writexl")

##### Take initial circulations from Alma for 2022 ####

Circulations2022dwd5Aug2022 <- readxl::read_excel("copy of SB-KM- Fulfillment_ For UTL Annual report  Count of Initial Circulations, renewals and STL_ALL_forFY2022_dwd5Aug2022.xlsx")
dim(Circulations2022dwd5Aug2022) #  315   9
