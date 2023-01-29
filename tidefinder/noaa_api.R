

# product <- "water_level"; begin_date <- "20220905"; end_date <- "20220906"; station = "9449424"
read.tides <- function(product, begin_date, end_date, station = "9447130", hiLow = FALSE) {
  urlBase <- "https://tidesandcurrents.noaa.gov/api/datagetter"
  options <- c(sprintf("product=%s", product),
               sprintf("begin_date=%s", begin_date),
               sprintf("end_date=%s", end_date),
               sprintf("station=%s", station),
               "application=web_services",
               "format=csv",
               "time_zone=lst_ldt",
               "units=english",
               "datum=MLLW")
  if(hiLow) {
    options <- c(options, "interval=hilo")
  }
  urlx <- paste(urlBase, paste(options, collapse = "&"), sep = "?")
  # print(urlx)
  apiCall <- httr::GET(urlx)
  dset <- read.csv(file = textConnection(httr::content(apiCall, "text")))
  if(nrow(dset) %in% c(0, 1)) {
    warning("No Data Found")
    return(NULL)
  }
  dset$Date.Time <- lubridate::ymd_hm(dset$Date.Time)
  if(hiLow) {
    return(dset)
  }
  if(product == "predictions") {
    lubridate::minute(dset$Date.Time) <- 0
    dset <- aggregate(. ~ Date.Time, FUN = mean, data = dset)
  }
  dset
}

# function(product, begin_date, end_date, station = "9447130", hiLow = FALSE) {
read_tides_nyr <- function(stationId, year, n = 1) {
  end_yr <- year + n - 1
  read.tides(
    product = "predictions",
    begin_date = sprintf("%s0101", year),
    end_date = sprintf("%s1231", end_yr),
    station = stationId,
    hiLow = TRUE
  )
}
# test <- read_tides_1yr(9449211, 2010)

# Get 18 years of tide predictions
read_tides <- function(stationId) {
  cur_yr <- year(today())
  tides1 <- read_tides_nyr(stationId, cur_yr, 6)
  tides2 <- read_tides_nyr(stationId, cur_yr + 6, 6)
  tides3 <- read_tides_nyr(stationId, cur_yr + 12, 6)
  tmp <- rbind(tides1, tides2, tides3)
  tmp$Id <- stationId
  tmp$year <- year(tmp$Date.Time)
  tmp
}



# cherry point 9449424
# test <- read.tides("water_level", "20220905", "20220906", 9449424)
# test2 <- read.tides("predictions", "20220905", "20220906", 9449424)
# head(test)
# plot(test$Date.Time, test$Water.Level)
# points(test2$Date.Time, test2$Prediction, col = "blue")
# test3 <- merge(test, test2)
# test3$diff <- test3$Water.Level - test3$Prediction
# plot(test3$Date.Time, test3$diff)
