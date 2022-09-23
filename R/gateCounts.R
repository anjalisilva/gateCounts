#' Calculates Cumulative Gate Counts
#'
#' A function that calculates cumulative gate counts, provided a numeric
#' vector or a tibble containing values of raw daily gate counts. The
#' function adjusts for several factors outlined under details.
#'
#' @details The function requires directionality of the gates for which
#'    the daily counts are provided (called gate type). If unidirectional
#'    no adjustments are done. If bidirectional, the cumulative sum at
#'    the end will be divided by two. Testing has shown that this method
#'    of dividing at the end will help reduce issues with counts
#'    that result from division of daily counts by two and rounding up or
#'    down. Further, negative counts can result during calculation if
#'    the counter has reset or if a lower value has been entered compared
#'    to previous day. This function attempts to correct for both scenarios.
#'    In the case of counter reset, the 'gatecounterMaxValue' argument
#'    provided by the user will be used to calculate the corrected value.
#'    In the case of a lower value being entered compared to previous day,
#'    the negative value will be replaced by NA. For next new date, the
#'    count is calculated by the numeric value previously reported, that
#'    must be higher than current value. The function also ensure counts
#'    for empty cells (when the count was forgotten to be reported) are
#'    accounted for. All scenarios are explained with images on the tutorial.
#'
#' @param rawGateCounts A numeric vector of length corresponding to days or
#     a tibble of dimensions days x 1, containing values of raw daily gate
#     counts. Here days is the number of days for which raw gate counts
#'    are present.
#' @param gateType A character string with options "Unidirectional" or
#     "Bidirectional", to indicate gate type. If the gate is one-way only,
#     then enter "Unidirectional". If the gate permits visitors in and out,
#     then enter "Bidirectional". Bidirectional selection will lead to
#     count sum being divided by two. The default value is "Unidirectional".
#' @param gatecounterMaxValue A numeric value greater than 0 indicating the
#'    gate counter max value, before it is reset. The default value is
#'    999,999.
#'
#' @return Returns an S3 object of class InfCriteria with results.
#' \itemize{
#'   \item adjustedCountSum - Sum of daily gate counts for the period,
#'         adjusted for issues mentioned under details. If gateType was
#'         "Bidirectional", the final resulting number would be divided
#'         by two.
#'   \item unadjustedDailyCounts - A vector of daily counts, not adjusted
#'         for gate type. If "Bidirectional" gate type, then would need to
#'         sum this vector and divide by two at the end.
#'   \item gateType - Original gate type provided by the user, "Bidirectional"
#'         or "Unidirectional".
#' }
#'
#' @examples
rbs1FloorNORTHcount <- gateCountAdjustment(
  rawGateCounts = rbs1FloorNORTH,
  gateType = "Bidirectional",
  gatecounterMaxValue = 999999)
rbs1FloorNORTHcount$countSum # 70686


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
  # for (i in c(140:154)) { # for testing purposes
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

  returnValues <- list(adjustedCountSum = sumValue,
                       unadjustedDailyCounts = unlist(collectValue),
                       gateType = gateType)
  class(returnValues) <- c("GateCounts")

  # S4 object doesn't work
  # setClass("GateCounts", slots = c(adjustedCountSum = "numeric",
  #                                  unadjustedDailyCounts = "numeric",
  #                                  gateType = "character"))

  # returnValues <- new("GateCounts")

  # returnValues@adjustedCountSum <- sumValue
  # returnValues@unadjustedDailyCounts <- unlist(collectValue)
  # returnValues@gateType <- gateType
  # or
  # returnValues <- new("GateCounts", adjustedCountSum = sumValue,
  #                     unadjustedDailyCounts = unlist(collectValue),
  #                     gateType = gateType)
  return(returnValues)
}


