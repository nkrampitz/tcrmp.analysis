#' Temperature File Append
#' @description Appends multiple temperature \code{.csv} files from the same
#'   location into a single raw temperature file with missing dates inserted.
#'
#' @param path file pathway to folder directory where \code{.csv} files are
#'   located and to which the compiled files will be saved.
#' @param min_temp minimum temperature value (degrees C), below which data
#'   will be excluded.
#' @param project project name to include in file names.
#'
#' @details All temperature files should be in the same file format
#'   and include 3 columns: a Number/ID, a date-time stamp, and raw temperature.
#'
#' @import dplyr
#' @importFrom lubridate mdy
#' @importFrom magrittr "%>%"
#' @importFrom padr pad
#' @importFrom purrr map_df
#' @importFrom readr read_csv write_csv
#' @importFrom stringr str_replace str_split
#' @importFrom tidyr drop_na separate
#'
#' @examples \dontrun{
#'
#' temp_file_app("C:/Users/User/Documents/Temperature_Folders",
#'               "TCRMP")
#' }

temp_file_app <- function(path, min_temp, project) {

  # Bind variables locally to function
  Date <- Location <- Time <- Temperature <- . <- NULL

  # List full names of all folders in the file pathway
  dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)

  # For each folder in the file pathway, read all files and append
  for (d in seq_along(1:length(dirs))) {

    # List all files in the folder
    comb_file <- list.files(dirs[d], full.names = TRUE, pattern = "*.csv") %>%
      # Read all .csv files, skip the first row (old column names), and rename columns to "Number", "Date Time", and "Temperature"
      map_df(~read_csv(., col_names = c("Number", "Date Time", "Temperature"), col_types = "icd", skip = 1)) |>
      # Separate the second column ("Date Time") into individual "Date" and "Time" columns
      separate(2, into = c("Date", "Time"), sep = " ") |>
      # Set appropriate format for dates and add location name extracted from the current folder name
      mutate(Date = mdy(Date),
             Location = last(unlist(str_split(dirs[d], "_")))) |>
      # Keep relevant columns
      select(Location, Date, Time, Temperature) |>
      # Remove any dates that return as NA
      drop_na(Date) %>%
      # Remove duplicate time stamps, if present
      distinct(., Date, Time, .keep_all = TRUE)

    # Create a sequence of dates for the full time series
    miss_dates <- pad(comb_file, start_val = min(comb_file$Date, na.rm = TRUE), end_val = max(comb_file$Date, na.rm = TRUE), interval = "day") |>
      # Filter only dates where temperature data doesn't exist (missing from time series)
      filter(is.na(Temperature)) |>
      # Add location name extracted from the current folder name
      mutate(Location = last(unlist(str_split(dirs[d], "_"))))
    # Join combined temperature and missing dates
    comb_file <- rbind(comb_file, miss_dates) |>
      # Remove any dates that return as NA
      drop_na(Date) |>
      # Remove duplicate date-time stamps, if present
      distinct(Date, Time, .keep_all = TRUE) |>
      # Sort dates in correct time sequence
      arrange(Date) |>
      # Remove temperature values that fall below the defined minimum temperature
      filter(Temperature >= min_temp)

    # Save combined and processed temperature to the folder directory
    write_csv(comb_file, paste0(path, "/", project, "_temp_", str_replace_all(last(unlist(str_split(dirs[d], "_"))), pattern = " ", replacement = "_"), "_raw.csv"))

  }

}
