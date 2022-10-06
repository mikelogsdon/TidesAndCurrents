library(lubridate)
library(dplyr)
library(ggplot2)
library(RSQLite)


setwd("~/Documents/Data Projects/TidesAndCurrents/")

source("./tidefinder/noaa_api.R")

# Read station metadata csv files
load("stations.rda")

# Just looking at Oregon, for now
# stations <- filter(stations, state == "Oregon")

# system.time(bli <- read_tides(9449211))

# tide_preds <- bli
# Calculate summary stats for a station with all tide predictions
calc_stats <- function(tide_preds) {
  tide_preds <- arrange(tide_preds, Date.Time)
  tidalRange <- diff(tide_preds$Prediction)
  data.frame(
    "Id" = tide_preds$Id[1],
    "aveTidalRange" = mean(tidalRange[tidalRange>0]),
    "maxTidalRange" = max(tidalRange[tidalRange > 0]),
    "lowestPredicted" = min(tide_preds$Prediction),
    "highestPredicted" = max(tide_preds$Prediction)
  )
}

# Only keep the top X tides per year to store in the app itself
keep_by_year <- function(tide_preds, ntides = 25) {
  low_tides <- tide_preds %>%
    filter(Type == "L") %>%
    group_by(year) %>%
    slice_min(Prediction, n = ntides)
  high_tides <- tide_preds %>%
    filter(Type == "H") %>%
    group_by(year) %>%
    slice_max(Prediction, n = ntides)
  rbind(low_tides, high_tides)
}

# Read the tides from NOAA, calc stats, discard non top 25 by year
read_and_process_one <- function(stationId) {
  tide_preds <- read_tides(stationId)
  tide_stats <- calc_stats(tide_preds)
  return(
    list(
      "metadata" = tide_stats,
      "preds" = keep_by_year(tide_preds, ntides = 50)
      # "preds" = tide_preds
    )
  )
}

system.time(test <- read_and_process_one(9449211))
system.time(read_and_process_one(9454755))

tideDataAll <- lapply(stations$Id, function(ix) {
  print(ix)
  tide_data1 <- try(read_and_process_one(ix))
  if(inherits(tide_data1, "try-error")) {
    return(NULL)
  } else {
    return(tide_data1)
  }
})




tide_metadata <- do.call('rbind', lapply(tideDataAll, function(x) x[[1]]))
tide_preds <- do.call('rbind', lapply(tideDataAll, function(x) x[[2]]))

rm(tideDataAll)
tide_preds2 <- tide_preds
save(file = "tide_preds.rda", tide_preds2)
load("tide_preds.rda")
tide_preds <- tide_preds2


tide_preds <- by(tide_preds, tide_preds$Id, keep_by_year)
tide_preds <- do.call('rbind', tide_preds)

tide_preds <- merge(tide_preds, stations[, c("Id", "group", "subgroup", "state", "stationName")])


conn <- dbConnect(SQLite(), dbname = "./tidefinder/tide_preds.db")
dbWriteTable(conn, "tide_preds", tide_preds, overwrite = TRUE)
dbSendQuery(conn, "CREATE INDEX stationid ON tide_preds(Id)")
# system.time(test <- dbGetQuery(conn, sprintf("SELECT * FROM tide_preds WHERE Id=%s", 9464881)))
# test <- dbGetQuery(conn, sprintf("SELECT * FROM tide_preds WHERE Id=%s", 9464881))



# merge the stats into stations
stations <- merge(stations, tide_metadata)
stations <- stations[!duplicated(stations$Id), ]
stations$Lon[stations$Lon > 0] <- stations$Lon[stations$Lon > 0] -360
stations <- stations[stations$Lat > 32.5, ]

save(file = "./tidefinder/stations.rda", stations)


# Store a sample
n_per_mo <- 150
tide_preds2$month <- month(tide_preds2$Date.Time)
tide_preds2$rand <- runif(n = nrow(tide_preds2), min = 0, max = 1)
patternx <- tide_preds2 %>%
  group_by(Id,month, Type) %>%
  slice_min(rand, n = n_per_mo)
  
dbWriteTable(conn, "pattern_data", patternx[, c("Date.Time", "Prediction", "Type", "Id")], overwrite = TRUE)
dbSendQuery(conn, "CREATE INDEX stationid2 ON pattern_data(Id)")

system.time(abc <- dbGetQuery(conn, "SELECT * FROM pattern_data WHERE Id == '9464881'"))

dbDisconnect(conn)

# 
# system.time(test <- tide_preds[tide_preds$Id == 9464881, ])
# save(file = "./tidefinder/tide_preds.rda", tide_preds)
# 
# 
# 
# or_metadata <- do.call('rbind', lapply(oregon, function(x) x[[1]]))
# or_preds <- do.call('rbind', lapply(oregon, function(x) x[[2]]))
# save(file = "or_preds.rda", or_preds)
# 
# system.time(tide_preds %>% filter(Id == 9434132))



