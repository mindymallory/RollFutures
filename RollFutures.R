# This script downloads historical futures contract data for a specified commodity using the Quandl API.
# It then constructs a continuous nearby futures series using one of several user-selectable rolling methodologies:
# 1. Volume-based roll (`roll_method = "volume"`): Rolls to the next contract when its volume surpasses the current contract's volume.
# 2. Days To Expiration (`roll_method = "dte"`): Rolls a fixed number of days before the approximate expiration of the current contract.
# 3. Open Interest-based roll (`roll_method = "oi"`): Selects the contract with the highest open interest each day.
# The resulting series is plotted and exported to a CSV file.

# --- Load required packages ---
# Ensure these packages are installed. If not, run install.packages("PackageName") in the R console.
# install.packages("Quandl") # For accessing historical futures data
# install.packages("plyr")   # For data manipulation (ldply)
# install.packages("tidyr")  # For data tidying (spread)
# install.packages("ggplot2")# For plotting
# install.packages("xts")    # Handled by Quandl/zoo but good to be aware for time series
library(Quandl)
library(plyr)
library(tidyr)
library(ggplot2)
library(xts) # Explicitly load for is.xts and xts object creation

# --- User-Configurable Parameters ---

# Quandl API Key: Replace "YourAPIKeyHere" with your actual Quandl API key.
# This is essential for downloading data from Quandl.
Quandl.api_key("YourAPIKeyHere")

# days_to_roll_before_expiry: Specifies how many days before a contract's approximate expiration to roll.
# This parameter is only used if `roll_method` is set to "dte".
days_to_roll_before_expiry <- 5

# roll_method: Determines the logic for constructing the continuous nearby series.
# Options are:
#   "volume": Rolls based on which contract (current or next in sequence) has higher daily volume. (Default)
#   "dte": Rolls based on proximity to contract expiration, using `days_to_roll_before_expiry`.
#   "oi": Rolls based on which contract has the highest daily open interest.
roll_method <- "volume"  # Options: "volume", "dte", "oi"

# start: The first year for which to download data (e.g., 2010).
start <- 2010
# end: The last year for which to download data (e.g., 2017).
end <- 2017

# c_code: An index into the `commodity_code` array/vector to select the target commodity for processing.
# For example, if commodity_code = c(..., "CL", ...), and "CL" is the 5th element, c_code should be 5.
# The default value of 7 originally pointed to "RB" in the example `commodity_code` list.
c_code <- 7

# commodity_code: A vector of commodity symbols. The script processes one commodity at a time,
# selected by the `c_code` index. This list might be specific to your Quandl subscription or data source.
# Example: c("C", "S", "W", "KW", "CL", "HO", "RB", "BO", "SM", "LN") for various futures.
commodity_code <- c("C", "S", "W", "KW", "CL", "HO", "RB", "BO", "SM", "LN")
# #C
# contracts <- c('H', 'K', 'N', 'U', 'Z')

# #S
# contracts <- c( 'F', 'H', 'K', 'N', 'Q', 'U', 'X')

# #BO
# contracts <- c('F', 'H', 'K', 'N', 'Q', 'U', 'Z')  # For meal and oil I made a decision to skip V and get Z to match crush production
                                                    #(can't sell SO and SM before you buy the soybeans).
##SM
#contracts <- c('F', 'H', 'K', 'N', 'Q', 'U', 'Z')

# #W
# contracts <- c( 'H', 'K', 'N', 'U', 'Z')
# #KW
# contracts <- c( 'H', 'K', 'N', 'U', 'Z')

#CL
# contracts: Defines the contract month codes for the currently selected commodity.
# This MUST be adjusted if `c_code` points to a commodity with different month codes
# or a different annual contract cycle.
# Example for "CL" (Crude Oil) or "RB" (RBOB Gasoline) which typically have 12 contracts a year:
contracts <- c( 'F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
# Example for Lean Hogs "LH" which has fewer contracts:
# contracts <- c('G', 'J', 'K', 'M', 'N', 'Q', 'V', 'Z')
# Ensure this variable is correctly set for the commodity selected by `c_code`.


# --- Data Fetching and Initial Processing ---
# Generate a sequence of years for data download based on `start` and `end` years.
years <- seq(start, end, by =1)
data <- list()
k <- 1

data <- list() # Initialize an empty list to store data for each contract month.
k <- 1 # Counter for the list.

# Loop through each year and each contract month code to download data.
for (i in start:end){
  for (j in 1:length(contracts)){
    # Construct the Quandl code for the specific contract month and year (e.g., "CME/RBJ2010").
    # The Quandl() function fetches data. Expected columns include Settle, Volume, and Prev. Day Open Interest.
    # cbind attaches three additional columns to the fetched data:
    #   - A unique name for the Settle series of this contract (e.g., "J-2010-Settle")
    #   - A unique name for the Volume series (e.g., "J-2010-Volume")
    #   - A unique name for the Open Interest series (e.g., "J-2010-OpenInterest")
    MyData = cbind(Quandl(paste0("CME/", commodity_code[c_code], contracts[j], years[i-start +1])),
                   paste0(contracts[j], '-', years[i-start+1], '-Settle'),
                   paste0(contracts[j], '-', years[i-start+1], '-Volume'),
                   paste0(contracts[j], '-', years[i-start+1], '-OpenInterest'))
    data[[k]] <- MyData # Add the data for the current contract to the list.
    k <- k+1
    # Optional: Uncomment the following line to save raw data for each individual contract to a CSV file.
    # write.csv(MyData, file = paste0("data-download/", contracts[j], years[i-start +1], ".csv"))
  }
}

# Combine data for all contracts from the list 'data' into a single large data frame.
DATA <- ldply(data, rbind)

# Select and rename relevant columns from the combined data.
# Assumed column positions from Quandl (these might vary):
#   1: Date
#   7: Settle price
#   8: Volume
#   9: Previous Day Open Interest
# Columns 10, 11, 12 are the descriptive names added during the cbind operation.
# IMPORTANT: Verify these numeric indices (7, 8, 9) if issues arise or if using different commodities/data sources.
DATA <- DATA[, c(1, 7, 8, 9, 10, 11, 12)]
DATA <- as.data.frame(DATA) # Ensure it's a data frame for consistent column operations.
colnames(DATA) <- c('Date', 'Settle', 'Volume', 'OpenInterest',
                    'Contract.Settle', 'Contract.Volume', 'Contract.OpenInterest')

# Reshape data into a format suitable for analysis:
# Create separate data frames for Settle, Volume, and Open Interest, each having 'Date', 'Contract', 'Value' columns.
settle <- DATA[, c("Date", "Contract.Settle", "Settle")]
volume <- DATA[, c("Date", "Contract.Volume", "Volume")]
open_interest <- DATA[, c("Date", "Contract.OpenInterest", "OpenInterest")]
colnames(settle) <- c('Date', 'Contract', 'Value') # Standardize column names
colnames(volume) <- c('Date', 'Contract', 'Value')
colnames(open_interest) <- c('Date', 'Contract', 'Value')

# Combine these three aspects (Settle, Volume, Open Interest) into one long-format data frame.
DATA <- rbind(settle, volume, open_interest)
colnames(DATA) <- c('Date', 'Contract', 'Value') # Final standard names before spreading

# Transform the data from long format to a wide format using tidyr::spread.
# This creates columns for each contract's Settle, Volume, and Open Interest
# (e.g., "F-2010-Settle", "F-2010-Volume", "F-2010-OpenInterest", etc.).
DATA <- spread(DATA, Contract, Value)

# Convert the wide data frame to an xts (extensible time series) object.
# The first column ('Date') is used for the time index. Dates are explicitly converted.
DATA <- as.xts(DATA[, -1], order.by = as.Date(DATA[, 1]))
# Filter the time series to include only the date range specified by `start` and `end` years.
DATA <- DATA[paste0(start,'/',end)]


# --- Refined Volume Takeover Rolling Logic for DATA$Nearby ---
# This section calculates the nearby series (DATA$Nearby) based on a volume takeover method.
# It iterates through each trading day, maintaining a currently "active" contract.
# It compares the volume of the active contract with the volume of the next chronological contract.
# If the next contract's volume is higher on a given day, a "roll" occurs,
# and the next contract becomes the active one for that day and subsequent days until another roll.
# The Settle price of the chosen active contract for each day forms the DATA$Nearby series.
Refined_Nearby_Vol <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Refined_Nearby_Vol) <- "Refined_Nearby_Vol_Values" # Temporary name

trading_days <- index(DATA)

settle_cols <- grep("-Settle$", colnames(DATA), value = TRUE)

if (length(settle_cols) == 0) {
  warning("No Settle columns found (e.g., ending in '-Settle'). Cannot proceed with refined volume roll. DATA$Nearby will be all NAs.")
  DATA$Nearby <- Refined_Nearby_Vol # Assign all NAs
} else {
  base_contract_names_from_settle <- gsub("-Settle$", "", settle_cols)
  unique_SYs <- unique(base_contract_names_from_settle)

  if (length(unique_SYs) == 0) {
    warning("No unique contract symbol-year combinations found from Settle columns. DATA$Nearby will be all NAs.")
    DATA$Nearby <- Refined_Nearby_Vol
  } else {
    # Sort unique_SYs chronologically
    # Assuming contract format "M-YYYY", e.g., "F-2010"
    parsed_SYs <- strsplit(unique_SYs, "-")

    # Check for parsing errors (e.g. SY not in M-YYYY format)
    valid_parse <- sapply(parsed_SYs, function(x) length(x) == 2)
    if(!all(valid_parse)) {
        warning("Some contract names do not fit M-YYYY format. Filtering them out.")
        parsed_SYs <- parsed_SYs[valid_parse]
        unique_SYs <- unique_SYs[valid_parse]
    }

    years_SYs <- sapply(parsed_SYs, function(x) as.integer(x[2]))
    month_codes_SYs <- sapply(parsed_SYs, function(x) x[1])

    # Define month order for CL-like contracts. This should align with 'contracts' var if commodity is CL.
    # For robustness, define it explicitly here if this logic is meant to be general or CL specific.
    # The 'contracts' variable ( F,G,H..Z for CL) is already available globally.
    # Using the global 'contracts' variable for month order:
    if (!exists("contracts") || length(contracts) == 0) {
        stop("Global variable 'contracts' (e.g., c('F', 'G', ...)) must be defined for month sorting.")
    }
    month_order_map <- setNames(seq_along(contracts), contracts)
    numeric_months_SYs <- month_order_map[month_codes_SYs]

    # Handle cases where a month code in a column name might not be in the global 'contracts' list
    if(any(is.na(numeric_months_SYs))) {
        warning("Some contract month codes are not in the defined 'contracts' list. These will be effectively ignored in sorting or may cause errors.")
        # Filter out those that didn't map:
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

            # Roll Check: only if there's a next contract to consider
            if (current_contract_SY_idx < length(ordered_contract_SYs)) {
                next_contract_SY <- ordered_contract_SYs[current_contract_SY_idx + 1]
                next_volume_col <- paste0(next_contract_SY, "-Volume")
                next_settle_col <- paste0(next_contract_SY, "-Settle") # Settle col for the next contract

                # Ensure all necessary columns exist for a valid comparison and roll
                if (next_volume_col %in% colnames(DATA) &&
                    active_volume_col %in% colnames(DATA) &&
                    next_settle_col %in% colnames(DATA)) { # Ensure next contract's Settle col also exists

                    vol_active_val <- DATA[current_date, active_volume_col]
                    vol_next_val <- DATA[current_date, next_volume_col]

                    # Ensure values are single numerics
                    vol_active <- if(length(vol_active_val)==1) coredata(vol_active_val)[1] else NA_real_
                    vol_next <- if(length(vol_next_val)==1) coredata(vol_next_val)[1] else NA_real_

                    if (!is.na(vol_next) && !is.na(vol_active) && vol_next > vol_active) {
                        # Roll to next contract
                        active_contract_SY <- next_contract_SY
                        current_contract_SY_idx <- current_contract_SY_idx + 1

                        # Update chosen_settle_val to the settle price of the new active contract
                        price_val_rolled <- DATA[current_date, next_settle_col] # Settle from the contract we rolled TO
                        if (length(price_val_rolled) == 1 && !is.na(coredata(price_val_rolled)[1])) {
                           chosen_settle_val <- price_val_rolled
                        } else {
                           chosen_settle_val <- NA_real_ # If new contract has NA settle, use NA
                        }
                    }
                }
            }
            Refined_Nearby_Vol[current_date] <- chosen_settle_val
        }
    } else {
         warning("After filtering, no valid contract symbol-year combinations remained for volume rolling.")
    }
  }
  DATA$Nearby <- Refined_Nearby_Vol
}
# --- End of Refined Volume Takeover Logic ---

# --- Days to Expiration (DTE) Rolling Logic ---

# --- Days to Expiration (DTE) Rolling Logic ---
# This section calculates a nearby series (DATA$Nearby_DTE) based on proximity to contract expiration.
# It uses the get_approx_expiration_date function to estimate when a contract expires.
# A roll to the next contract in the chronological sequence occurs if:
#   - The current date is past the active contract's approximate expiration date, OR
#   - The number of days remaining until expiration is less than or equal to `days_to_roll_before_expiry`.
# The Settle price of the active contract under this logic forms the DATA$Nearby_DTE series.

# get_approx_expiration_date: Estimates the expiration date of a futures contract.
# Arguments:
#   contract_char: Single character representing the contract month (e.g., 'F', 'G', ..., 'Z').
#   contract_year: Four-digit year of the contract (e.g., 2010).
#   commodity: String representing the commodity code (e.g., "CL"). Currently, logic is specific to "CL".
# Returns:
#   A Date object representing the approximate expiration date.
# Approximation Logic for "CL" (Crude Oil):
#   - Delivery month is determined from contract_char (F=Jan, G=Feb, etc.).
#   - The calculation month is one month prior to the delivery month.
#     (e.g., for an F contract (Jan delivery), calculation is based on December of the previous year).
#   - The target date is approximated as the 25th of this calculation month.
#   - The final approximate expiration date is 5 calendar days before this target date (proxy for ~3 business days).
# This is a simplified model; actual futures expiration rules can be complex and vary by contract.
get_approx_expiration_date <- function(contract_char, contract_year, commodity) {
  if (commodity == "CL") { # Specific logic for Crude Oil ("CL")
    cl_month_codes <- list(F=1, G=2, H=3, J=4, K=5, M=6, N=7, Q=8, U=9, V=10, X=11, Z=12) # Month codes for CL
    delivery_month <- cl_month_codes[[contract_char]]

    delivery_month <- cl_month_codes[[contract_char]]

    if (is.null(delivery_month)) {
      stop(paste("Invalid contract_char for CL:", contract_char, "- contract_char must be one of F,G,H,J,K,M,N,Q,U,V,X,Z for CL."))
    }

    expiration_calc_month <- delivery_month - 1 # Calculation is based on the month prior to delivery.
    expiration_calc_year <- contract_year

    if (expiration_calc_month == 0) { # If delivery month is January (F), expiration calc is for December of prior year.
      expiration_calc_month <- 12
      expiration_calc_year <- contract_year - 1
    }

    # Approximate target date as 25th of the calculation month.
    target_date_approx <- as.Date(paste(expiration_calc_year, expiration_calc_month, 25, sep="-"))
    # Roll back 5 calendar days from target as a proxy for ~3 business days before expiry.
    approx_exp_date <- target_date_approx - 5
    return(approx_exp_date)
  } else {
    # Placeholder for other commodities - this function would need expansion for them.
    stop(paste("Commodity", commodity, "not supported by get_approx_expiration_date. Add logic for this commodity."))
  }
}

# Initialize Nearby_DTE series (Days To Expiration based roll)
Nearby_DTE <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Nearby_DTE) <- "Nearby_DTE_Values" # Give it a temporary name to avoid issues if DATA already has Nearby_DTE

trading_days <- index(DATA)

# Determine the full list of specific contract instances available (e.g., "F-2010", "G-2010")
# These are derived from the Settle columns, assuming they exist for all relevant contracts
settle_col_names <- grep("-Settle$", colnames(DATA), value = TRUE)
contract_SYs <- gsub("-Settle$", "", settle_col_names)
ordered_contract_SYs <- sort(unique(contract_SYs)) # Sort chronologically

if (length(ordered_contract_SYs) == 0) {
  warning("No contract symbol-year combinations found. DTE Rolling will not execute.")
} else {
  current_contract_SY_index <- 1
  active_contract_SY <- ordered_contract_SYs[current_contract_SY_index]

  parsed_SY <- strsplit(active_contract_SY, "-")[[1]]
  active_contract_char <- parsed_SY[1]
  active_contract_year <- as.integer(parsed_SY[2])
  active_contract_exp_date <- get_approx_expiration_date(active_contract_char, active_contract_year, "CL") # Hardcoded "CL"

  for (day_idx in seq_along(trading_days)) {
    current_date <- trading_days[day_idx]
    days_remaining <- as.numeric(active_contract_exp_date - current_date)

    # Check if roll is needed
    # Roll if current_date is past expiration OR days_remaining is less than or equal to threshold
    if (current_date > active_contract_exp_date || days_remaining <= days_to_roll_before_expiry) {
      current_contract_SY_index <- current_contract_SY_index + 1
      if (current_contract_SY_index > length(ordered_contract_SYs)) {
        # No more contracts to roll into
        break
      }
      active_contract_SY <- ordered_contract_SYs[current_contract_SY_index]
      parsed_SY_new <- strsplit(active_contract_SY, "-")[[1]]
      active_contract_char <- parsed_SY_new[1]
      active_contract_year <- as.integer(parsed_SY_new[2])
      active_contract_exp_date <- get_approx_expiration_date(active_contract_char, active_contract_year, "CL") # Hardcoded "CL"
    }

    settle_col_name_for_active_contract <- paste0(active_contract_SY, "-Settle")

    if (settle_col_name_for_active_contract %in% colnames(DATA)) {
      price_value <- DATA[current_date, settle_col_name_for_active_contract]
      if (length(price_value) > 0 && !is.na(price_value)) {
         Nearby_DTE[current_date] <- price_value
      } else {
         # If data is NA for the active contract on this day, carry forward or handle as NA
         # For now, explicit NA if not found or NA. Could implement LOCF later if needed.
         Nearby_DTE[current_date] <- NA_real_
      }
    } else {
      # This case should ideally not happen if ordered_contract_SYs is derived from colnames(DATA)
      # and active_contract_SY is from this list.
      Nearby_DTE[current_date] <- NA_real_
    }
  }
  DATA$Nearby_DTE <- Nearby_DTE
}
# --- End of DTE Rolling Logic ---

# --- Open Interest (OI) Takeover Rolling Logic ---
# This section calculates a nearby series (DATA$Nearby_OI) by selecting the contract
# with the highest Open Interest on each trading day.
# The Settle price of this highest-OI contract for each day forms the DATA$Nearby_OI series.
Nearby_OI <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
colnames(Nearby_OI) <- "Nearby_OI_Values" # Temporary name to avoid conflict

oi_column_names <- grep("-OpenInterest$", colnames(DATA), value = TRUE)

if (length(oi_column_names) == 0) {
  warning("No Open Interest columns found (e.g., ending in '-OpenInterest'). Skipping Open Interest based roll. DATA$Nearby_OI will be all NAs.")
  # Ensure DATA$Nearby_OI exists and is all NAs if other series are consistently added
  DATA$Nearby_OI <- Nearby_OI
} else {
  oi_data_block <- DATA[, oi_column_names, drop = FALSE] # drop = FALSE to keep it xts if one column

  # Determine which contract has max OI for each day
  # which.max returns the index of the first max if multiple are identical.
  # If all values in a row are NA, which.max would error. Handle this.
  temp_which_max_in_oi_block <- apply(oi_data_block, 1, function(row_values) {
    if (all(is.na(row_values))) {
      return(NA_integer_) # Return NA if all OI values in the row are NA
    } else {
      # which.max ignores NAs by default if there's at least one non-NA value
      return(which.max(row_values))
    }
  })

  for (i in seq_len(nrow(DATA))) {
    current_row_date <- index(DATA)[i]

    idx_in_block <- temp_which_max_in_oi_block[i]

    if (is.na(idx_in_block)) {
      Nearby_OI[current_row_date] <- NA_real_
      next # Skip to next iteration
    }

    chosen_oi_col_name <- oi_column_names[idx_in_block]

    base_contract_name <- gsub("-OpenInterest$", "", chosen_oi_col_name)
    chosen_settle_col_name <- paste0(base_contract_name, "-Settle")

    if (chosen_settle_col_name %in% colnames(DATA)) {
      price_value <- DATA[current_row_date, chosen_settle_col_name]
      # Ensure price_value is a single scalar and not empty or a structure
      if (length(price_value) == 1 && !is.na(coredata(price_value)[1])) {
        Nearby_OI[current_row_date] <- price_value
      } else {
        Nearby_OI[current_row_date] <- NA_real_
      }
    } else {
      # This implies a Settle column is missing for a contract that has an OI column.
      Nearby_OI[current_row_date] <- NA_real_
      warning(paste("Settle column", chosen_settle_col_name, "not found for date", as.character(current_row_date), "when processing OI roll."))
    }
  }
  DATA$Nearby_OI <- Nearby_OI
}
# --- End of OI Takeover Rolling Logic ---


# --- Select Final Nearby Series based on chosen roll_method ---
# This section uses the `roll_method` parameter to choose which of the calculated
# nearby series (Volume-based, DTE-based, or OI-based) will be used as the final
# `DATA$SelectedNearby` series for plotting and export.
if (!is.xts(DATA)) {
  warning("DATA is not an xts object before roll_method selection. Cannot proceed with SelectedNearby assignment.")
  # If DATA is not xts, subsequent operations involving DATA$SelectedNearby might fail.
  # Consider adding stop() if DATA must be xts here.
} else {
  if (roll_method == "volume") {
    if ("Nearby" %in% names(DATA)) { # Check if the volume-based series exists
      DATA$SelectedNearby <- DATA$Nearby
    } else {
      warning("Roll method 'volume' selected, but DATA$Nearby (volume-based series) not found. DATA$SelectedNearby will be NAs.")
      DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA))) # Create an NA series
    }
  } else if (roll_method == "dte") {
    if ("Nearby_DTE" %in% names(DATA)) { # Check if the DTE-based series exists
      DATA$SelectedNearby <- DATA$Nearby_DTE
    } else {
      warning("Roll method 'dte' selected, but DATA$Nearby_DTE not found. DATA$SelectedNearby will be NAs.")
      DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
    }
  } else if (roll_method == "oi") {
    if ("Nearby_OI" %in% names(DATA)) { # Check if the OI-based series exists
      DATA$SelectedNearby <- DATA$Nearby_OI
    } else {
      warning("Roll method 'oi' selected, but DATA$Nearby_OI not found. DATA$SelectedNearby will be NAs.")
      DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
    }
  } else {
    # Fallback for invalid roll_method string
    warning(paste("Invalid roll_method '", roll_method, "' specified. Defaulting to 'volume' method if DATA$Nearby is available, otherwise NAs.", sep=""))
    if ("Nearby" %in% names(DATA)) {
      DATA$SelectedNearby <- DATA$Nearby
    } else {
      warning("Defaulting to 'volume' failed as DATA$Nearby not found. DATA$SelectedNearby will be NAs.")
      DATA$SelectedNearby <- xts(order.by = index(DATA), x = rep(NA_real_, nrow(DATA)))
    }
  }
}
# --- End of Select Final Nearby Series ---


# --- Plotting and Exporting the Selected Nearby Series ---

# Plot the selected nearby series (DATA$SelectedNearby) using ggplot2's autoplot.
# A dynamic title is created based on the selected commodity and rolling method.
if ("SelectedNearby" %in% names(DATA)) {
  plot_title <- paste("Nearby Series for", commodity_code[c_code], "rolled via", roll_method, "method")
  # Check if DATA$SelectedNearby is empty or contains only NA values before attempting to plot.
  if(nrow(DATA$SelectedNearby) > 0 && !all(is.na(coredata(DATA$SelectedNearby)))){
    g <- autoplot(DATA$SelectedNearby, main = plot_title)
    print(g) # Explicitly print the ggplot object.
  } else {
    warning(paste("DATA$SelectedNearby for", commodity_code[c_code], "is empty or contains all NA values. Skipping plot."))
  }
} else {
  warning("DATA$SelectedNearby not found in DATA object. Skipping plot.")
}

# Prepare the selected series for export. This assignment is somewhat redundant if DATA$SelectedNearby is used directly.
Series_To_Export <- DATA$SelectedNearby
 
# The following lines for merging multiple commodity exports are commented out,
# as the script currently focuses on processing and exporting a single commodity at a time.
# DATA_Export <- merge(DATA_Export_CL, DATA_Export_HO, DATA_Export_RB)
# colnames(DATA_Export) = c("CrudeOil", "ULSD", "RBOB")

# --- Dynamic Export of Selected Series to CSV ---
# This section exports the `Series_To_Export` (which is DATA$SelectedNearby) to a CSV file.
# The filename and column name within the CSV are generated dynamically based on the
# selected commodity and rolling method.

# Ensure Series_To_Export is valid and contains data before attempting to write.
if (!is.null(Series_To_Export) &&
    is.xts(Series_To_Export) &&    # Check if it's an xts object
    ncol(Series_To_Export) > 0 &&  # Check if it has at least one column
    nrow(Series_To_Export) > 0) {  # Check if it has at least one row

  current_commodity_name <- commodity_code[c_code] # Get the string name of the current commodity.
  # Create a dynamic column name for the CSV file, e.g., "CL_Nearby_volume".
  export_col_name <- paste0(current_commodity_name, "_Nearby_", roll_method)
  colnames(Series_To_Export) <- export_col_name # Set this as the column name in the data to be exported.

  # Create a dynamic filename, e.g., "CL_Nearby_volume.csv".
  export_filename <- paste0(current_commodity_name, "_Nearby_", roll_method, ".csv")

  # Write the time series data to the CSV file.
  write.zoo(Series_To_Export,
            file = export_filename,
            row.names = FALSE, # FALSE: Do not write R's internal row numbers; the time index will be the first column.
            na = "",           # Represent NA values as empty strings in the CSV.
            col.names = TRUE,  # TRUE: Include the header row with the column name.
            sep = ",")
  print(paste("Exported data to:", export_filename)) # Confirmation message.

} else {
  # Provide a more specific warning if the series is not suitable for export.
  current_commodity_name_msg <- "the current commodity" # Default message part
  # Try to get the actual commodity name for a more informative warning, if variables are set.
  if(exists("commodity_code") && exists("c_code") && c_code <= length(commodity_code) && c_code >= 1) {
      current_commodity_name_msg <- commodity_code[c_code]
  }
  warning(paste("Series_To_Export for", current_commodity_name_msg,
                "is not available, not a valid xts object, has no columns, or is empty. Skipping CSV export."))
}
# --- End of Dynamic Export ---
