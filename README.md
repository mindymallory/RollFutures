# RollFutures

Code to download futures contract data (primarily via Quandl) and construct a continuous nearby futures series. Supports multiple rolling methodologies.

## Features

*   Constructs a continuous nearby futures series from individual contracts.
*   Supports three primary rolling logic methods:
    *   **Volume-based (`roll_method = "volume"`)**: Rolls to the next contract in the series when its daily volume exceeds the volume of the currently active contract.
    *   **Days to Expiration (`roll_method = "dte"`)**: Rolls to the next contract a specified number of days before the approximate expiration of the current contract.
    *   **Open Interest-based (`roll_method = "oi"`)**: Selects the contract with the highest daily open interest as the nearby contract.
*   Downloads historical data using the Quandl API (specifically targeting CME data).
*   Generates a plot of the selected nearby series using `ggplot2`.
*   Exports the selected nearby series to a CSV file with a dynamic filename.

## Configuration

Open `RollFutures.R` and modify the parameters in the "User-Configurable Parameters" section at the top of the script:

*   `Quandl.api_key("YOUR_API_KEY_HERE")`: **Important!** Replace `"YOUR_API_KEY_HERE"` with your actual Quandl API key.
*   `days_to_roll_before_expiry`: Integer, e.g., `5`. Specifies how many days before a contract's approximate expiration to roll. This parameter is only used if `roll_method` is set to `"dte"`.
*   `roll_method`: Determines the logic for constructing the continuous nearby series. Options:
    *   `"volume"` (default): Uses the refined volume takeover logic.
    *   `"dte"`: Uses the days to expiration logic.
    *   `"oi"`: Uses the highest open interest logic.
*   `start`: The first year for which to download data (e.g., `2010`).
*   `end`: The last year for which to download data (e.g., `2017`).
*   `c_code`: An index into the `commodity_code` vector to select the target commodity for processing (e.g., `5` for `"CL"` if `"CL"` is the 5th element in `commodity_code`).
*   `commodity_code`: A vector of commodity symbols (e.g., `c("C", "S", "W", "KW", "CL", "HO", "RB", "BO", "SM", "LN")`). The script processes one commodity at a time, selected by `c_code`.
*   `contracts`: A vector of contract month codes specific to the selected commodity (e.g., `c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')` for Crude Oil). This **must** be reviewed and potentially adjusted if a different commodity is chosen via `c_code`.

## Output

*   **Plot**: A plot of the generated nearby series (using the selected `roll_method` and commodity) is displayed via `ggplot2::autoplot`. The plot title dynamically reflects the chosen commodity and roll method.
*   **CSV File**: The script saves the selected nearby series to a CSV file in the same directory where the script is run. The filename is dynamic, reflecting the commodity and roll method used (e.g., `CL_Nearby_volume.csv` if processing Crude Oil with the volume roll method).

## Dependencies
The script requires the following R packages:
* `Quandl`
* `plyr`
* `tidyr`
* `ggplot2`
* `xts`

You can install them using `install.packages("PackageName")` in your R console.
The script also includes commented-out `install.packages` lines as a reminder. It's generally better to manage package installation outside the main operational script.
