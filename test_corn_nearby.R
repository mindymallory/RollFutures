# Test script to create a nearby series for corn futures using example data
# This version reads from local CSV files in data-download/ instead of Quandl API

# --- Load required packages ---
library(plyr)
library(tidyr)
library(ggplot2)
library(xts)
library(zoo) # for read.zoo and write.zoo

# --- User-Configurable Parameters ---

# days_to_roll_before_expiry: Specifies how many days before a contract's approximate expiration to roll.
# This parameter is only used if `roll_method` is set to "dte".
days_to_roll_before_expiry <- 5

# roll_method: Determines the logic for constructing the continuous nearby series.
# Options are:
#   "volume": Rolls based on which contract (current or next in sequence) has higher daily volume. (Default)
#   "dte": Rolls based on proximity to contract expiration, using `days_to_roll_before_expiry`.
#   "oi": Rolls based on which contract has the highest daily open interest.
roll_method <- "volume"  # Options: "volume", "dte", "oi"

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
    # Construct the filename for the local CSV file
    filename <- paste0("data-download/", contracts[j], i, ".csv")

    # Check if file exists
    if (file.exists(filename)) {
      # Read the CSV file
      MyData <- read.csv(filename, stringsAsFactors = FALSE)

      # The CSV has columns: X, Date, Open, High, Low, Last, Change, Settle, Volume, and either:
      # "Prev..Day.Open.Interest" (2013-2014) or "Open.Interest" (2015-2016)
      # We need: Date, Settle, Volume, OpenInterest
      # Convert Date to Date class
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

      # Select relevant columns and add contract identifiers
      # Create a data frame with Date, Settle, Volume, OpenInterest, and identifier columns
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

# Combine data for all contracts from the list 'data' into a single large data frame.
DATA <- ldply(data, rbind)

# Reshape data into a format suitable for analysis:
# Create separate data frames for Settle, Volume, and Open Interest
settle <- DATA[, c("Date", "Contract.Settle", "Settle")]
volume <- DATA[, c("Date", "Contract.Volume", "Volume")]
open_interest <- DATA[, c("Date", "Contract.OpenInterest", "OpenInterest")]
colnames(settle) <- c('Date', 'Contract', 'Value')
colnames(volume) <- c('Date', 'Contract', 'Value')
colnames(open_interest) <- c('Date', 'Contract', 'Value')

# Combine these three aspects into one long-format data frame
DATA <- rbind(settle, volume, open_interest)
colnames(DATA) <- c('Date', 'Contract', 'Value')

# Transform from long format to wide format using tidyr::spread
DATA <- spread(DATA, Contract, Value)

# Convert to xts (extensible time series) object
DATA <- as.xts(DATA[, -1], order.by = as.Date(DATA[, 1]))

# Filter to the specified date range
DATA <- DATA[paste0(start,'/',end)]

cat("\nData loaded successfully. Dimensions:", dim(DATA), "\n")
cat("Column names:", paste(colnames(DATA)[1:min(10, ncol(DATA))], collapse=", "), "...\n\n")

# --- Refined Volume Takeover Rolling Logic for DATA$Nearby ---
Refined_Nearby_Vol <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Refined_Nearby_Vol) <- "Refined_Nearby_Vol_Values"

trading_days <- index(DATA)

settle_cols <- grep("-Settle$", colnames(DATA), value = TRUE)

if (length(settle_cols) == 0) {
  warning("No Settle columns found. Cannot proceed with volume roll.")
  DATA$Nearby <- Refined_Nearby_Vol
} else {
  base_contract_names_from_settle <- gsub("-Settle$", "", settle_cols)
  unique_SYs <- unique(base_contract_names_from_settle)

  if (length(unique_SYs) == 0) {
    warning("No unique contract symbol-year combinations found.")
    DATA$Nearby <- Refined_Nearby_Vol
  } else {
    # Sort unique_SYs chronologically
    parsed_SYs <- strsplit(unique_SYs, "-")
    valid_parse <- sapply(parsed_SYs, function(x) length(x) == 2)

    if(!all(valid_parse)) {
      warning("Some contract names do not fit M-YYYY format. Filtering them out.")
      parsed_SYs <- parsed_SYs[valid_parse]
      unique_SYs <- unique_SYs[valid_parse]
    }

    years_SYs <- sapply(parsed_SYs, function(x) as.integer(x[2]))
    month_codes_SYs <- sapply(parsed_SYs, function(x) x[1])

    # Month order for corn contracts
    month_order_map <- setNames(seq_along(contracts), contracts)
    numeric_months_SYs <- month_order_map[month_codes_SYs]

    if(any(is.na(numeric_months_SYs))) {
      warning("Some contract month codes are not in the defined 'contracts' list.")
      valid_months <- !is.na(numeric_months_SYs)
      unique_SYs <- unique_SYs[valid_months]
      years_SYs <- years_SYs[valid_months]
      numeric_months_SYs <- numeric_months_SYs[valid_months]
    }

    if (length(unique_SYs) > 0) {
      ordered_indices <- order(years_SYs, numeric_months_SYs)
      ordered_contract_SYs <- unique_SYs[ordered_indices]

      current_contract_SY_idx <- 1
      active_contract_SY <- ordered_contract_SYs[current_contract_SY_idx]

      cat("Starting volume-based roll with initial contract:", active_contract_SY, "\n")

      for (day_idx in seq_along(trading_days)) {
        current_date <- trading_days[day_idx]

        active_settle_col <- paste0(active_contract_SY, "-Settle")
        active_volume_col <- paste0(active_contract_SY, "-Volume")

        chosen_settle_val <- NA_real_
        if (active_settle_col %in% colnames(DATA)) {
          price_val <- DATA[current_date, active_settle_col]
          if (length(price_val) == 1 && !is.na(coredata(price_val)[1])) {
            chosen_settle_val <- price_val
          }
        }

        # Roll Check
        if (current_contract_SY_idx < length(ordered_contract_SYs)) {
          next_contract_SY <- ordered_contract_SYs[current_contract_SY_idx + 1]
          next_volume_col <- paste0(next_contract_SY, "-Volume")
          next_settle_col <- paste0(next_contract_SY, "-Settle")

          if (next_volume_col %in% colnames(DATA) &&
              active_volume_col %in% colnames(DATA) &&
              next_settle_col %in% colnames(DATA)) {

            vol_active_val <- DATA[current_date, active_volume_col]
            vol_next_val <- DATA[current_date, next_volume_col]

            vol_active <- if(length(vol_active_val)==1) coredata(vol_active_val)[1] else NA_real_
            vol_next <- if(length(vol_next_val)==1) coredata(vol_next_val)[1] else NA_real_

            if (!is.na(vol_next) && !is.na(vol_active) && vol_next > vol_active) {
              # Roll to next contract
              cat("Rolling from", active_contract_SY, "to", next_contract_SY, "on", as.character(current_date), "\n")
              active_contract_SY <- next_contract_SY
              current_contract_SY_idx <- current_contract_SY_idx + 1

              price_val_rolled <- DATA[current_date, next_settle_col]
              if (length(price_val_rolled) == 1 && !is.na(coredata(price_val_rolled)[1])) {
                chosen_settle_val <- price_val_rolled
              } else {
                chosen_settle_val <- NA_real_
              }
            }
          }
        }
        Refined_Nearby_Vol[current_date] <- chosen_settle_val
      }
    } else {
      warning("After filtering, no valid contract symbol-year combinations remained.")
    }
  }
  DATA$Nearby <- Refined_Nearby_Vol
}

# --- Open Interest (OI) Takeover Rolling Logic ---
Nearby_OI <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Nearby_OI) <- "Nearby_OI_Values"

oi_column_names <- grep("-OpenInterest$", colnames(DATA), value = TRUE)

if (length(oi_column_names) == 0) {
  warning("No Open Interest columns found.")
  DATA$Nearby_OI <- Nearby_OI
} else {
  oi_data_block <- DATA[, oi_column_names, drop = FALSE]

  temp_which_max_in_oi_block <- apply(oi_data_block, 1, function(row_values) {
    if (all(is.na(row_values))) {
      return(NA_integer_)
    } else {
      return(which.max(row_values))
    }
  })

  for (i in seq_len(nrow(DATA))) {
    current_row_date <- index(DATA)[i]
    idx_in_block <- temp_which_max_in_oi_block[i]

    if (is.na(idx_in_block)) {
      Nearby_OI[current_row_date] <- NA_real_
      next
    }

    chosen_oi_col_name <- oi_column_names[idx_in_block]
    base_contract_name <- gsub("-OpenInterest$", "", chosen_oi_col_name)
    chosen_settle_col_name <- paste0(base_contract_name, "-Settle")

    if (chosen_settle_col_name %in% colnames(DATA)) {
      price_value <- DATA[current_row_date, chosen_settle_col_name]
      if (length(price_value) == 1 && !is.na(coredata(price_value)[1])) {
        Nearby_OI[current_row_date] <- price_value
      } else {
        Nearby_OI[current_row_date] <- NA_real_
      }
    } else {
      Nearby_OI[current_row_date] <- NA_real_
    }
  }
  DATA$Nearby_OI <- Nearby_OI
}

# --- Select Final Nearby Series based on chosen roll_method ---
if (roll_method == "volume") {
  if ("Nearby" %in% names(DATA)) {
    DATA$SelectedNearby <- DATA$Nearby
  } else {
    warning("Volume-based series not found.")
    DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
  }
} else if (roll_method == "oi") {
  if ("Nearby_OI" %in% names(DATA)) {
    DATA$SelectedNearby <- DATA$Nearby_OI
  } else {
    warning("OI-based series not found.")
    DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
  }
} else {
  warning(paste("Roll method", roll_method, "not fully implemented in this test script. Defaulting to volume."))
  if ("Nearby" %in% names(DATA)) {
    DATA$SelectedNearby <- DATA$Nearby
  } else {
    DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
  }
}

# --- Plotting and Exporting the Selected Nearby Series ---
if ("SelectedNearby" %in% names(DATA)) {
  plot_title <- paste("Nearby Series for", commodity_code, "rolled via", roll_method, "method")

  if(nrow(DATA$SelectedNearby) > 0 && !all(is.na(coredata(DATA$SelectedNearby)))){
    cat("\nGenerating plot...\n")
    g <- autoplot(DATA$SelectedNearby, main = plot_title)
    print(g)
  } else {
    warning("SelectedNearby is empty or contains all NA values. Skipping plot.")
  }
} else {
  warning("SelectedNearby not found. Skipping plot.")
}

# Export to CSV
Series_To_Export <- DATA$SelectedNearby

if (!is.null(Series_To_Export) &&
    is.xts(Series_To_Export) &&
    ncol(Series_To_Export) > 0 &&
    nrow(Series_To_Export) > 0) {

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

  # Show a summary of the exported data
  cat("\nSummary of nearby series:\n")
  print(summary(coredata(Series_To_Export)))
  cat("\nFirst few rows:\n")
  print(head(Series_To_Export))
  cat("\nLast few rows:\n")
  print(tail(Series_To_Export))

} else {
  warning("Series_To_Export is not valid. Skipping CSV export.")
}

cat("\nTest complete!\n")
