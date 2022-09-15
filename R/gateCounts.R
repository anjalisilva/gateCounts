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

# Function
# 1. Gate type based calculation
# 2. Check counts for counter max value reset
# 3. Check counts for typo (i.e., a gate count less than previous day)
# 3. Check counts for NA values and adjust based on previous numeric count

# Arguments
# rawGateCounts: A numeric vector of length corresponding to days or a tibble
#                of dimensions days x 1, containing values of raw daily gate counts.
#                Here days is the number of days for which raw gate counts are present.
# gateType: A character string with options "Unidirectional" or "Bidirectional",
#           to indicate gate type. If the gate is one-way only, then enter
#           "Unidirectional". If the gate permits visitors in and out, then
#           enter "Bidirectional". Bidirectional selection will lead to count
#           sum being divided by two. The default value is "Unidirectional".
# gatecounterMaxValue: A numeric value greater than 0 indicating the gate counter
#                      max value, before it is reset. The default value is 999,999.

gateCountAdjustment <- function(rawGateCounts,
                                gateType = "Unidirectional",
                                gatecounterMaxValue = 999999) {
  # Check input arguments
  if(gateType != "Bidirectional" && gateType != "Unidirectional") {
    stop("\n gateType argument can only take on values Bidirectional
         or Unidirectional")
  }

  if (gatecounterMaxValue <= 0) {
    stop("\n gatecounterMaxValue argument should be greater than zero.")
  }

  if(all(is.na(rawGateCounts) == TRUE)) {
    stop("\n rawGateCounts doesn't contain numbers for calculation. ")
  }

  if(is.vector(rawGateCounts) == FALSE &&
     tibble::is_tibble(rawGateCounts) == FALSE) {
     stop("\n rawGateCounts should be a numeric vector or tibble")
  }

  # Converting to numeric vector or tibble
  if(is.vector(rawGateCounts) == TRUE) {
    tibbleCounts <- tibble::as_tibble(as.numeric(rawGateCounts))
  } else if (tibble::is_tibble(rawGateCounts) == TRUE) {
    tibbleCounts <- rawGateCounts %>%
      unlist() %>%
      as.numeric() %>%
      tibble::as_tibble()
  } else {
    stop("\n rawGateCounts should be a numeric vector or tibble")
  }

  if(typeof(unlist(tibbleCounts)) != "double" &&
     typeof(unlist(tibbleCounts)) != "integer") {
    stop("\n rawGateCounts should be a numeric vector or tibble")
  }


  # Begin calculations
  # Empty vector to capture visitor counts via calculation
  collectValue <- rep(NA, times = nrow(tibbleCounts))

  # Loop for obtaining visitor counts
   for (i in c(140:150)) { # for testing purposes
  # for (i in c(1:nrow(tibbleCounts))) {
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
          if((tibbleCounts[i, ] / gatecounterMaxValue) >= 0.8) {
            collectValue[i + 1] <- (gatecounterMaxValue - tibbleCounts[i, ]) +
                                    tibbleCounts[i + 1, ]

          } else if((tibbleCounts[i, ] / gatecounterMaxValue) < 0.8) {
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
              cat("\n No previous count with numeric value present. \n")
            } else if(all(is.na(tibbleCounts[i-c(1:(i-1)), ])) == FALSE) {
              # See how many past counts have numeric values
              # Pick the most recent numeric count to subtract from
              recentCountPlace <- min(which(is.na(tibbleCounts[i-c(1:(i-1)), ]) == FALSE),
                                      na.rm = TRUE)
              collectValue[i + 1] <- (tibbleCounts[i + 1, ] -
                                      tibbleCounts[i - recentCountPlace, ])
              # After adjustment check if a negative value, in case typo
              if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] < 0)) {
                cat("\n The value is negative, so tibbleCounts[i + 1, ] is set to NA \n")
                # resetCounter <- 0
                # while(! resetCounter) {
                # for(j in 1:length(which(is.na(tibbleCounts[i-c(1:(i-1)), ]) == FALSE))) {
                  # if negative subtract from previous value
                  tibbleCounts[i + 1, ] <- collectValue[i + 1] <- NA # adjust mistake to NA
                #  collectValue[i + 1] <- (tibbleCounts[i + 1, ] -
                #                            tibbleCounts[i - recentCountPlace - j, ])
                #  if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] >= 0)) {
                #    resetCounter <- 1
                #   }
                #  }
                #}
              }
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

  if(gateType == "Bidirectional") {
    sumValue <- ceiling(sumValue / 2)
  }

  returnValues <- list(countSum = sumValue,
                       individualDailyCounts = unlist(collectValue),
                       gateType = gateType)
  class(returnValues) <- c("GateCounts")

  return(returnValues)
}


##### Analyze using raw counts function ####
# 15 Sept 2022

gateCountsFY2022raw <- readxl::read_excel(
  "UTL Daily Exit Counts WITH CALCS  Apr2020 with corrected calculations_AS_svd1Sept2022.xlsx",
   sheet = 1)
dim(gateCountsFY2022raw) #  4507   14
glimpse(gateCountsFY2022raw)

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
dim(rbs1FloorNORTH) # 365   1
class(rbs1FloorNORTH)
is.vector(rbs1FloorNORTH) # TRUE

rbs1FloorNORTHcount <- gateCountAdjustment(
  rawGateCounts = rbs1FloorNORTH,
  gateType = "Bidirectional",
  gatecounterMaxValue = 999999)
rbs1FloorNORTHcount$countSum # 70686



rbs1FloorSOUTH <- gateCountsFY2022raw %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 1st floor SOUTH")
dim(rbs1FloorSOUTH)

rbs1FloorSOUTHcount <- gateCountAdjustment(
  rawGateCounts = rbs1FloorSOUTH,
  gateType = "Bidirectional",
  gatecounterMaxValue = 999999)
rbs1FloorSOUTHcount$countSum # 51240

rbsP4 <- gateCountsFY2022raw %>%
  dplyr::mutate(month = Date %>%
                  lubridate::ymd() %>%
                  lubridate::month()) %>%
  dplyr::mutate(year = Date %>%
                  lubridate::ymd() %>%
                  lubridate::year()) %>%
  dplyr::mutate(yearMonth = paste0(year,month)) %>%
  dplyr::filter(yearMonth %in% yearMonthAnalyzed) %>%
  dplyr::select("Robarts 2nd floor P4 elevator")
dim(rbsP4) # 365   1

rbsP4Counts <- gateCountAdjustment(
  rawGateCounts = rbsP4,
  gateType = "Bidirectional",
  gatecounterMaxValue = 999999)
rbsP4Counts$countSum # 2614
write.csv(ceiling(unlist(rbsP4Counts$individualDailyCounts)/2),
          file = "rbsP4Counts.csv")

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
length(gersteinCounts) #365
class(gersteinCounts)
is.vector(gersteinCounts) # TRUE

gersteinCountOput <- gateCountAdjustment(
  rawGateCounts = gersteinCounts,
  gateType = "Unidirectional",
  gatecounterMaxValue = 999999)
gersteinCountOput$countSum # 289917




rbs1FloorSOUTHcount <- gateCountAdjustment(
  rawGateCounts = c(NA, NA, NA))
