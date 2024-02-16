parse_hurdat <- function(x) {
  hurdat <- as.data.frame(x, stringsAsFactors = FALSE)
  
  header_rows <- grep(pattern = "^[[:alpha:]]{2}[[:digit:]]{6}.+", x)
  
  # Split header_rows into variables
  hurdat <- tidyr::extract(
    data = hurdat,
    col = "x",
    into = c("Key", "Name", "Lines"),
    regex = paste0(
      "([[:alpha:]]{2}[[:digit:]]{6}),\\s+", # Key
      "([[:upper:][:digit:]-]+)\\s*,\\s+", # Name
      "([[:digit:]]+)," # Number of lines that follow
    ),
    remove = FALSE,
    convert = TRUE
  )
  
  # Fill headers down
  hurdat <- tidyr::fill(data = hurdat, .data$Key, .data$Name, .data$Lines)
  
  # Remove original header rows
  hurdat <- hurdat[-header_rows, ]
  
  # Split storm details into variables
  hurdat <- tidyr::extract(
    data = hurdat,
    col = "x",
    into = c(
      "Year",
      "Month",
      "Date",
      "Hour",
      "Minute",
      "Record",
      "Status",
      "Lat",
      "LatHemi",
      "Lon",
      "LonHemi",
      "Wind",
      "Pressure",
      "NE34",
      "SE34",
      "SW34",
      "NW34",
      "NE50",
      "SE50",
      "SW50",
      "NW50",
      "NE64",
      "SE64",
      "SW64",
      "NW64",
      "RMW"
    ),
    regex = paste0(
      "^([[:digit:]]{4})", # Year
      "([[:digit:]]{2})", # Month
      "([[:digit:]]{2}),\\s+", # Date
      "([[:digit:]]{2})", # Hour
      "([[:digit:]]{2}),\\s+", # Minute
      "([[:alpha:]]*),\\s+", # Record
      "([[:alpha:]]{2}),\\s+", # Status
      "([[:digit:]]{1,2}\\.[[:digit:]]{1})", # Latitude
      "([[:alpha:]]{1}),\\s+", # Hemisphere - North or South
      "([[:digit:]]{1,3}\\.[[:digit:]]{1})", # Longitude
      "([[:alpha:]]{1}),\\s+", # Hemisphere - West or East
      "([[:digit:]-]+),\\s+", # Maximum sustained wind (in knots)
      "([[:digit:]-]+),\\s+", # Minimum Pressure (in millibars)
      "([[:digit:]-]+),\\s+", # 34 kt wind radii maximum extent in northeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 34 kt wind radii maximum extent in southeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 34 kt wind radii maximum extent in southwestern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 34 kt wind radii maximum extent in northwestern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 50 kt wind radii maximum extent in northeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 50 kt wind radii maximum extent in southeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 50 kt wind radii maximum extent in southwestern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 50 kt wind radii maximum extent in northwestern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 64 kt wind radii maximum extent in northeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 64 kt wind radii maximum extent in southeastern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 64 kt wind radii maximum extent in southwestern quadrant (in nautical miles)
      "([[:digit:]-]+),\\s+", # 64 kt wind radii maximum extent in northwestern quadrant (in nautical miles)
      "([[:digit:]-]+).*" # Radius of Maximum Wind (in nautical miles)
    ),
    remove = FALSE,
    convert = TRUE
  )
  
  hurdat <- dplyr::mutate(
    .data = hurdat,
    Lat = dplyr::if_else(
      .data$LatHemi == "N", .data$Lat * 1, .data$Lat * -1
    ),
    Lon = dplyr::if_else(
      .data$LonHemi == "E", .data$Lon * 1, .data$Lon * -1
    )
  )
  
  hurdat$DateTime <- paste(
    paste(hurdat$Year, hurdat$Month, hurdat$Date, sep = "-"),
    paste(hurdat$Hour, hurdat$Minute, "00", sep = ":"),
    sep = " "
  )
  
  hurdat <- dplyr::select(
    .data = hurdat,
    .data$Key, .data$Name, .data$DateTime, .data$Record:.data$Lat,
    .data$Lon, .data$Wind:.data$RMW
  )
  
  hurdat <- unique(hurdat)
  
  # Run audit and throw warning if any issues.
  if (nrow(audit_hurdat(hurdat)) > 0) {
    rlang::warn(
      message = paste0(
        "Observations received are not equal to those expected.",
        "Run `audit_hurdat()` for discrepancy table."
      )
    )
  }
  
  # Make certain values NA
  # I do this before converting `DateTime` because if that field has already
  # been converted then this cleaning will generate an error,
  # >  character string is not in a standard unambiguous format
  hurdat <-
    dplyr::mutate_at(
      .tbl = hurdat,
      .vars = dplyr::vars(
        .data$Key:.data$Status,
        .data$Wind:.data$NW64
      ),
      .funs = ~replace(
        x = .,
        list = . %in% c("", "0", -99, -999),
        values = NA
      )
    )
  
  hurdat$DateTime <- as.POSIXct(
    strptime(hurdat$DateTime, format = "%Y-%m-%d %H:%M:%S")
  )
  
  dplyr::arrange(hurdat, .data$DateTime, .data$Key)
}

audit_hurdat <- function(df) {
  problems <- dplyr::group_by(df, .data$Key, .data$DateTime)
  
  problems <- dplyr::count(problems)
  
  problems <- dplyr::filter(problems, .data$n > 1)
  
  dplyr::arrange(problems, .data$n, .data$Key, .data$DateTime)
}


get_hurdat <- function (basin = c("AL", "EP")) {
  basin <- toupper(basin)
  if (!all(basin %in% c("AL", "EP"))) {
    rlang::abort(message = "`basin` must be 'AL' and/or 'EP'.")
  }
  urls <- "https://www.aoml.noaa.gov/hrd/hurdat/hurdat2.html"
  txt <- purrr::map(urls, readr::read_lines)
  txt <- purrr::flatten_chr(txt)
  keep_lines <- grep(pattern = "^[[:space:]*[:alpha:]{2}[:digit:]{6}]|[[:digit:]]{8}", 
                     x = txt)
  txt <- txt[keep_lines]
  DF <- parse_hurdat(txt)
  DF$basin <- substr(DF$Key,1,2)
  DF <- DF[DF$basin %in% basin,]
  return(DF)
}