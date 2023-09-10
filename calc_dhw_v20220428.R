#' Calculate Degree Heating Weeks
#' @description Calculates degree heating weeks from raw temperature
#'   data. Degree heating weeks (DHW) illustrate heat stress at a location
#'   over the previous 3 months as a running sum of the devation over the
#'   bleaching threshold.
#'
#' @param path file pathway to where individual \code{.csv} files are located
#'   and to which the compiled file will be saved.
#' @param bl_th a data frame containing the bleaching thresholds (BT) and
#'   monthly maximum mean (MMM) for all locations (Location) with files
#'   in the pathway.
#' @param project project name to include in file names.
#'
#' @details All temperature files should be in the same file format
#'   and include 4 columns: Location, Date, Time, and Temperature.
#'
#' @import dplyr
#' @importFrom lubridate day week year
#' @importFrom magrittr "%>%"
#' @importFrom padr pad
#' @importFrom purrr map_df
#' @import readr
#' @importFrom tibble add_column
#'
#' @export calc_dhw
#'
#' @examples \dontrun{
#'
#' calc_dhw("C:/Users/User/Documents/TemperatureFiles",
#'          tcrmp_bl_th, "TCRMP")
#' }

calc_dhw <- function(path, bl_th, project) {

  # Bind variables locally to function
  Location <- Date <- Time <- Temperature <- Year <- YearWeek <- Day <- . <- MMMDev <- DHW <- WeekTemp <- MMM <- SampleDate <- NULL

  # List all files in the pathway
  raw_temp <- list.files(path, full.names = TRUE) %>%
    # Read all .csv files and define column types
    map_df(~read_csv(., col_types = cols(Location = col_character(), Date = col_date(), Time = col_character(), Temperature = col_double()))) |>
    # Add new columns for year, week of the year, and day
    mutate(Year = year(Date),
           YearWeek = week(Date),
           Day = day(Date))

  # Calculate temperature deviation
  mmm_dev <- raw_temp |>
    # Group temperature data by location, year, and week of the year
    group_by(Location, Year, YearWeek) |>
    # Calculate weekly temperature for each week of the year
    summarise(WeekTemp = mean(Temperature, na.rm = TRUE)) %>%
    # Join with bleaching thresholds
    left_join(., bl_th) %>%
    # Calculate deviation of weekly temperature from MMM
    mutate(MMMDev = WeekTemp - MMM) |>
    # Add new empty column for DHW
    add_column(DHW = NA, .after = "MMMDev")

  # Create vector of location names
  sites <- as.vector(unique(mmm_dev$Location))

  # Create empty data frame in which to save DHW calculation results
  raw_temp_dhw <- tibble()

  # Calculate DHW by location
  for (s in sites) {

    # Filter data for only the current location
    sub_temp <- filter(mmm_dev, Location == s)

    # Calculate DHW
    for (i in 1:nrow(sub_temp)) {

      if(i - 11 > 0) {

        # Temperature deviation of the previous 12 weeks
        past12 <- sub_temp$MMMDev[(i - 11):i]
        # Determine which weeks had temperature deviations greater than 1
        over <- past12[past12 >= 1]
        # Sum of temperature deviation for weeks that had a deviation greater than 1
        val <- sum(over)
        # Assign sum to DHW
        sub_temp$DHW[i] <- val

      } else {

        # Assign NA to newly created DHW column if DHW could not be calculated (i.e. missing data)
        sub_temp$DHW[i] <- NA

      }

    }

    # Combine DHW calculation by location with the running compilation of DHW calculations for all locations
    raw_temp_dhw <- rbind(raw_temp_dhw, sub_temp)

  }

  # Calculate mean daily temperature
  daily_temp <- raw_temp |>
    # Group values by location and date
    group_by(Location, Date) |>
    # Calculate mean daily temperature
    summarise(Temperature = mean(Temperature, na.rm = TRUE)) |>
    # Add columns for year and week of the year
    mutate(Year = year(Date),
           YearWeek = week(Date))

  # Combine mean daily temperature and DHW
  daily_temp_dhw <- left_join(daily_temp, raw_temp_dhw, by = c("Location", "Year", "YearWeek")) |>
    # Keep only relevant columns
    select(Location, Date, Temperature, DHW) |>
    # Rename "Date" to "SampleDate" (match with benthic sample dates if creating figures)
    rename(SampleDate = Date)

  # Create a sequence of dates for the full time series
  miss_dates <- pad(daily_temp_dhw, start_val = min(daily_temp_dhw$SampleDate, na.rm = TRUE), end_val = max(daily_temp_dhw$SampleDate, na.rm = TRUE), interval = "day") |>
    # Filter only dates where temperature data doesn't exist (missing from time series)
    filter(is.na(Temperature))
  # Join mean daily temperature temperature and missing dates
  daily_temp_dhw <- rbind(daily_temp_dhw, miss_dates) |>
    # Sort dates in correct time sequence
    arrange(Location, SampleDate)

  # Save mean daily temperature and DHW to the folder pathway
  write_csv(daily_temp_dhw, paste0(path, "/", project, "_temp_Daily_Temp_DHW.csv"))

}
