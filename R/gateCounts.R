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

##### Write raw counts function####
# 9 Sept 2022

# 1. Gate type based calculation
# 2. Check counts for counter max value or typo (i.e., a negative count)
# 3. Check counts for NA values

gateCountAdjustment <- function(vectorCounts,
                                gateType = "One-way",
                                counterMaxValue = 999999) {
  # Check input arguments
  if(is.vector(vectorCounts) == FALSE &&
     tibble::is_tibble(vectorCounts) == FALSE) {
     stop("\n vectorCounts should be a numeric vector or tibble")
  }

  if(is.vector(vectorCounts) == TRUE) {
    tibbleCounts <- as_tibble(vectorCounts)
  } else {
    tibbleCounts <- vectorCounts
  }

  if(typeof(unlist(tibbleCounts)) != "double" &&
     typeof(unlist(tibbleCounts)) != "integer") {
    stop("\n vectorCounts should be a numeric vector or tibble")
  }


  # Begin calculations
  # Empty vector to capture visitor counts via calculation
  collectValue <- rep(NA, times = nrow(tibbleCounts))

  # Loop for obtaining visitor counts
  for (i in c(1:nrow(tibbleCounts))) {
      # 1. Gate type based calculation

        collectValue[i + 1] <- tibbleCounts[i + 1, ] - tibbleCounts[i, ]

        cat("\n Calc", i+1, "minus", i, "is:",
            unlist(tibbleCounts[i + 1, ]),
            "-", unlist(tibbleCounts[i, ]),
            " = ", unlist(collectValue[i + 1]), "\n")

        # 2. Check counts for counter max value or typo
        if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] < 0)) {
          # detecting if a counter max issue
          # If that is the case value/counterMax should be close to 1
          if((tibbleCounts[i, ] / counterMaxValue) >= 0.8) {
            collectValue[i + 1] <- (counterMaxValue - tibbleCounts[i, ]) +
                                    tibbleCounts[i + 1, ]

          } else if((tibbleCounts[i, ] / counterMaxValue) < 0.8) {
            # In this case, likely a typo from user entering data
            collectValue[i + 1] <- NA
            tibbleCounts[i + 1, ] <- NA
          }
        }

        # 3. Check counts for NA values
        # If an NA, then check if the i + 1 or i is NA
        if(is.na(collectValue[i + 1]) == TRUE) {

          # If tibbleCounts[i+1, ] is NA, will not be addressed

          # If i is NA, and it is not the very first entry in loop
          if((is.na(tibbleCounts[i, ]) == TRUE) && (i >= 2)) {

            # Check back on all past values to see if any numeric values
            # Otherwise no point in performing analysis
            # This would be i-c(1:(i-1))

            if(all(is.na(tibbleCounts[i-c(1:(i-1)), ])) == TRUE) {
              cat("\n Previous 10 values are NA \n")
            } else if(all(is.na(tibbleCounts[i-c(1:(i-1)), ])) == FALSE) {
              # See how many past counts have numeric values
              # Pick the most recent numeric count to subtract from
              recentCountPlace <- min(which(is.na(tibbleCounts[i-c(1:(i-1)), ]) == FALSE),
                                      na.rm = TRUE)
              collectValue[i + 1] <- (tibbleCounts[i + 1, ] -
                                      tibbleCounts[i - recentCountPlace, ])
              cat("\n Adjuted value to be", unlist(collectValue[i + 1]), "\n")
            } else {
              collectValue[i + 1] <- NA # i.e., if one of the first values with
              cat("\n NA option collectValue[i + 1] = ",  unlist(collectValue[i + 1]), "\n")
            }
          }
        }

      }


  # Calculations based on visitor counts
  sumValue <- sum(unlist(collectValue), na.rm = TRUE)

  if(gateType == "Two-way") {
    sumValue <- ceiling(sumValue / 2)
  }

  returnValues <- list(countSum = sumValue,
                       individualDailyCounts = unlist(collectValue),
                       gateType = gateType)
  class(returnValues) <- c("GateCounts")

  return(returnValues)
}
outPut<- gateCountAdjustment(vectorCounts = gersteinCounts,
                             gateType = "One-way")

outPut$countSum # 141371
#write.csv(outPut$individualDailyCounts, file = "gateCountsGerstein.csv")


##### Analyze using raw counts function ####

# 15 Sept 2022
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

# Obtain another set of data for FY2022
gersteinCounts <- gateCountsFY2022raw %>%
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
  as.numeric()
