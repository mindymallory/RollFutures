# Test script for DTE-based rolling with corn futures
# This version uses Days To Expiration logic instead of volume

# --- Load required packages ---
library(plyr)
library(tidyr)
library(ggplot2)
library(xts)
library(zoo)

# --- User-Configurable Parameters ---
days_to_roll_before_expiry <- 5  # Roll 5 days before expiration
roll_method <- "dte"

# For this test, we'll use Corn ("C") with local data from 2013-2016
start <- 2013
end <- 2016

commodity_code <- "C"  # Corn
contracts <- c('H', 'K', 'N', 'U', 'Z')  # Corn contract months (Mar, May, Jul, Sep, Dec)

# --- Data Loading from Local CSV Files ---
years <- seq(start, end, by = 1)
data <- list()
k <- 1

# Loop through each year and each contract month code to load data from local CSV files
for (i in start:end) {
  for (j in 1:length(contracts)) {
    filename <- paste0("data-download/", contracts[j], i, ".csv")

    if (file.exists(filename)) {
      MyData <- read.csv(filename, stringsAsFactors = FALSE)
      MyData$Date <- as.Date(MyData$Date)

      # Handle different column names for Open Interest
      if ("Prev..Day.Open.Interest" %in% colnames(MyData)) {
        oi_data <- MyData$Prev..Day.Open.Interest
      } else if ("Open.Interest" %in% colnames(MyData)) {
        oi_data <- MyData$Open.Interest
      } else {
        warning(paste("Open Interest column not found in", filename))
        next
      }

      contract_id <- paste0(contracts[j], '-', i)
      MyData_subset <- data.frame(
        Date = MyData$Date,
        Settle = as.numeric(MyData$Settle),
        Volume = as.integer(MyData$Volume),
        OpenInterest = as.integer(oi_data),
        Contract.Settle = rep(paste0(contract_id, '-Settle'), nrow(MyData)),
        Contract.Volume = rep(paste0(contract_id, '-Volume'), nrow(MyData)),
        Contract.OpenInterest = rep(paste0(contract_id, '-OpenInterest'), nrow(MyData)),
        stringsAsFactors = FALSE
      )

      data[[k]] <- MyData_subset
      k <- k + 1
      cat("Loaded:", filename, "\n")
    } else {
      warning(paste("File not found:", filename))
    }
  }
}

# Combine data
DATA <- ldply(data, rbind)

# Reshape data
settle <- DATA[, c("Date", "Contract.Settle", "Settle")]
volume <- DATA[, c("Date", "Contract.Volume", "Volume")]
open_interest <- DATA[, c("Date", "Contract.OpenInterest", "OpenInterest")]
colnames(settle) <- c('Date', 'Contract', 'Value')
colnames(volume) <- c('Date', 'Contract', 'Value')
colnames(open_interest) <- c('Date', 'Contract', 'Value')

DATA <- rbind(settle, volume, open_interest)
colnames(DATA) <- c('Date', 'Contract', 'Value')

DATA <- spread(DATA, Contract, Value)
DATA <- as.xts(DATA[, -1], order.by = as.Date(DATA[, 1]))
DATA <- DATA[paste0(start,'/',end)]

cat("\nData loaded successfully. Dimensions:", dim(DATA), "\n\n")

# --- Get Approximate Expiration Date for Corn ---
# Corn futures expire on the business day prior to the 15th calendar day of the delivery month
get_corn_expiration_date <- function(contract_char, contract_year) {
  # Map contract codes to delivery months
  corn_month_codes <- list(H=3, K=5, N=7, U=9, Z=12)
  delivery_month <- corn_month_codes[[contract_char]]

  if (is.null(delivery_month)) {
    stop(paste("Invalid contract_char for Corn:", contract_char))
  }

  # Approximate expiration: 14th of the delivery month
  # (actual is business day prior to 15th, but 14th is close enough for this test)
  approx_exp_date <- as.Date(paste(contract_year, delivery_month, 14, sep="-"))
  return(approx_exp_date)
}

# --- Days to Expiration (DTE) Rolling Logic ---
Nearby_DTE <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Nearby_DTE) <- "Nearby_DTE_Values"

trading_days <- index(DATA)
settle_col_names <- grep("-Settle$", colnames(DATA), value = TRUE)
contract_SYs <- gsub("-Settle$", "", settle_col_names)

# Sort contracts chronologically
parsed_SYs <- strsplit(contract_SYs, "-")
valid_parse <- sapply(parsed_SYs, function(x) length(x) == 2)
parsed_SYs <- parsed_SYs[valid_parse]
contract_SYs <- contract_SYs[valid_parse]

years_SYs <- sapply(parsed_SYs, function(x) as.integer(x[2]))
month_codes_SYs <- sapply(parsed_SYs, function(x) x[1])

month_order_map <- setNames(seq_along(contracts), contracts)
numeric_months_SYs <- month_order_map[month_codes_SYs]

valid_months <- !is.na(numeric_months_SYs)
contract_SYs <- contract_SYs[valid_months]
years_SYs <- years_SYs[valid_months]
numeric_months_SYs <- numeric_months_SYs[valid_months]

ordered_indices <- order(years_SYs, numeric_months_SYs)
ordered_contract_SYs <- contract_SYs[ordered_indices]

if (length(ordered_contract_SYs) > 0) {
  current_contract_SY_index <- 1
  active_contract_SY <- ordered_contract_SYs[current_contract_SY_index]

  parsed_SY <- strsplit(active_contract_SY, "-")[[1]]
  active_contract_char <- parsed_SY[1]
  active_contract_year <- as.integer(parsed_SY[2])
  active_contract_exp_date <- get_corn_expiration_date(active_contract_char, active_contract_year)

  cat("Starting DTE-based roll with initial contract:", active_contract_SY, "\n")
  cat("Expiration date:", as.character(active_contract_exp_date), "\n")
  cat("Will roll", days_to_roll_before_expiry, "days before expiration\n\n")

  for (day_idx in seq_along(trading_days)) {
    current_date <- trading_days[day_idx]
    days_remaining <- as.numeric(active_contract_exp_date - current_date)

    # Check if roll is needed
    if (current_date > active_contract_exp_date || days_remaining <= days_to_roll_before_expiry) {
      # Roll to next contract
      old_contract <- active_contract_SY
      current_contract_SY_index <- current_contract_SY_index + 1

      if (current_contract_SY_index > length(ordered_contract_SYs)) {
        break
      }

      active_contract_SY <- ordered_contract_SYs[current_contract_SY_index]
      parsed_SY_new <- strsplit(active_contract_SY, "-")[[1]]
      active_contract_char <- parsed_SY_new[1]
      active_contract_year <- as.integer(parsed_SY_new[2])
      active_contract_exp_date <- get_corn_expiration_date(active_contract_char, active_contract_year)

      cat("Rolling from", old_contract, "to", active_contract_SY, "on", as.character(current_date),
          "(", days_remaining, "days before expiration)\n")
    }

    settle_col_name_for_active_contract <- paste0(active_contract_SY, "-Settle")

    if (settle_col_name_for_active_contract %in% colnames(DATA)) {
      price_value <- DATA[current_date, settle_col_name_for_active_contract]
      if (length(price_value) > 0 && !is.na(price_value)) {
        Nearby_DTE[current_date] <- price_value
      } else {
        Nearby_DTE[current_date] <- NA_real_
      }
    } else {
      Nearby_DTE[current_date] <- NA_real_
    }
  }

  DATA$Nearby_DTE <- Nearby_DTE
}

# Select final series
DATA$SelectedNearby <- DATA$Nearby_DTE

# --- Plotting and Exporting ---
if ("SelectedNearby" %in% names(DATA)) {
  plot_title <- paste("Nearby Series for", commodity_code, "rolled via", roll_method, "method")

  if(nrow(DATA$SelectedNearby) > 0 && !all(is.na(coredata(DATA$SelectedNearby)))){
    cat("\nGenerating plot...\n")
    g <- autoplot(DATA$SelectedNearby, main = plot_title)
    print(g)
  } else {
    warning("SelectedNearby is empty or contains all NA values. Skipping plot.")
  }
}

# Export to CSV
Series_To_Export <- DATA$SelectedNearby

if (!is.null(Series_To_Export) && is.xts(Series_To_Export) &&
    ncol(Series_To_Export) > 0 && nrow(Series_To_Export) > 0) {

  export_col_name <- paste0(commodity_code, "_Nearby_", roll_method)
  colnames(Series_To_Export) <- export_col_name
  export_filename <- paste0(commodity_code, "_Nearby_", roll_method, ".csv")

  write.zoo(Series_To_Export,
            file = export_filename,
            row.names = FALSE,
            na = "",
            col.names = TRUE,
            sep = ",")

  cat("\nExported data to:", export_filename, "\n")

  cat("\nSummary of nearby series:\n")
  print(summary(coredata(Series_To_Export)))
  cat("\nFirst few rows:\n")
  print(head(Series_To_Export))
  cat("\nLast few rows:\n")
  print(tail(Series_To_Export))
} else {
  warning("Series_To_Export is not valid. Skipping CSV export.")
}

cat("\nDTE Test complete!\n")
