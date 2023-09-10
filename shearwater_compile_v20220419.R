#' Compile Shearwater Dive Log
#' @description Combines and cleans exported dive logs (\code{.csv}) from
#'   the Shearwater Desktop platform and saves the compiled data as a new
#'   file.
#'
#' @param path file pathway to folder where \code{.csv} files are located
#'   and to which the compiled file will be saved.
#' @param displ_year (optional) if supplied, summary statistics for the
#'   indicated year will be displayed.
#'
#' @details Optional displayed statistics include: dive year, dive mode (air,
#'   nitrox, CCR), dive depth range, total number of dives, and total dive
#'   time.
#'
#' @import dplyr
#' @importFrom lubridate mdy year
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_df
#' @importFrom readr write_csv
#' @importFrom stringr str_match str_split
#' @importFrom tools file_path_sans_ext
#'
#' @examples \dontrun{
#'
#' shearwater_compile("C:/Users/User/Documents/Logs")
#'
#' shearwater_compile("C:/Users/User/Documents/Logs", 2020)
#' }

shearwater_compile <- function(path, displ_year) {

  # Bind variables locally to function
  DiveDate <- Time <- Depth <- DiveDepth <- `Average PPO2` <- `Fraction O2` <- FirstStopDepth <- DiveNo <- DiveYear <- DiveTime <- DiveDepthRange <- DiveMode <- Deco <- . <- NULL

  # List individual .csv files in pathway
  files <- list.files(path, full.names = TRUE)
  # Create empty data frame in which to store dive data
  comb_dive_log <- tibble()

  # Extract dive information from individual .csv files and compile
  for (f in seq_along(1:length(files))) {
    # Extract individual file name
    file_name <- last(unlist(str_split(files[f], "/")))
    # Extract relevent dive information
    ind_dive_log <- files[f] %>%
      # Read individual Shearwater dive log .csv
      map_df(~read_csv(., skip = 2)) |>
      # Create new columns of relevant dive information
      mutate(DiveNo = last(unlist(str_match(file_name, "#(.*?)_")))[2],
             DiveDate = mdy(last(unlist(str_split(file_path_sans_ext(file_name), "_")))),
             DiveYear = year(DiveDate),
             DiveTime = max(Time, na.rm = TRUE) / 60,
             DiveDepth = max(Depth, na.rm = TRUE),
             DiveDepthRange = ifelse(DiveDepth <= 30, "0 - 30",
                                     ifelse(DiveDepth > 30 & DiveDepth <= 60, "30 - 60",
                                            ifelse(DiveDepth > 60 & DiveDepth <= 100, "60 - 100",
                                                   ifelse(DiveDepth > 100 & DiveDepth <= 130, "100 - 130",
                                                          ifelse(DiveDepth > 130 & DiveDepth <= 150, "130 - 150",
                                                                 ifelse(DiveDepth > 150 & DiveDepth <= 190, "150 - 190", "> 190")))))),
             DiveMode = ifelse(mean(`Average PPO2`, na.rm = TRUE) == 1.3 | mean(`Average PPO2`, na.rm = TRUE) == 0.7, "CCR", ifelse(`Fraction O2` > 0.21, "Nitrox", "Air")),
             Deco = ifelse(sum(FirstStopDepth, na.rm = TRUE) > 0, "Yes", "No")) |>
      # Keep only columns containing relevant dive information
      select(DiveNo, DiveDate, DiveYear, DiveTime, DiveDepth, DiveDepthRange, DiveMode, Deco) %>%
      # Reduce individual log to one row representing the dive
      distinct(., DiveNo, .keep_all = TRUE)
    # Add individual dive to compiled dive log
    comb_dive_log <- rbind(ind_dive_log, comb_dive_log)
  }

  # Sort dives in ascending order
  arrange(comb_dive_log, DiveNo) %>%
    # Save compiled dive log to file pathway
    write_csv(., paste0(path, "/CombinedDiveLog.csv"))

  # Display summary dive statistics
  if (!is.na(displ_year)) {
    # Summarize dive log for specified year
    dive_sum <- comb_dive_log |>
      # Filter full dive log for specified year
      filter(DiveYear == displ_year) |>
      # Convert depth ranges to ordered factors
      mutate(DiveDepthRange = factor(DiveDepthRange, levels = c("0 - 30", "30 - 60", "60 - 100", "100 - 130", "130 - 150", "150 - 190", "> 190"))) |>
      # Group dives by the specified year, dive mode (Air, Nitrox, CCR), and depth range
      group_by(DiveYear, DiveMode, DiveDepthRange, .drop = FALSE) |>
      # Summarize total dives and total time in minutes
      summarise(TotalDives = length(DiveNo),
                TotalTime = round(sum(DiveTime, na.rm = TRUE), 1))

    print(as.data.frame(dive_sum))
  }

}
