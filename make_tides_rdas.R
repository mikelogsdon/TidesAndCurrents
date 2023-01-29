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

# Read the tides from NOAA, calc stats
read_and_process_one <- function(stationId) {
  tide_preds <- read_tides(stationId)
  tide_stats <- calc_stats(tide_preds)
  return(
    list(
      "metadata" = tide_stats,
      # "preds" = keep_by_year(tide_preds, ntides = 50)
      "preds" = tide_preds
    )
  )
}

# system.time(test <- read_and_process_one(9450104))
# system.time(read_and_process_one(9454755))

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

# Save out all the preds, because we're going to monkey with it
saveRDS(file = "tide_preds.rds", tide_preds)
saveRDS(file = "tide_metadata.rds", tide_metadata)
tide_preds <- readRDS("tide_preds.rds")
tide_metadata <- readRDS("tide_metadata.rds")

rm(tideDataAll)

# Establish a database to write to
conn <- dbConnect(SQLite(), dbname = "./tidefinder/tide_preds.db")
tide_preds <- rename(tide_preds, Date_Time = Date.Time)

# Calculate stats by station by year for the yearly pattern plot
stats_by_year <- tide_preds %>%
  group_by(Id, year, Type) %>%
  summarise(
    "min" = min(Prediction),
    "q025" = as.numeric(quantile(Prediction, .025)),
    "q25" = as.numeric(quantile(Prediction, .25)),
    "q50" = as.numeric(quantile(Prediction, .50)),
    "q75" = as.numeric(quantile(Prediction, .75)),
    "q975" = as.numeric(quantile(Prediction, .975)),
    "max" = max(Prediction)
  )
dbWriteTable(conn, "stats_by_year", stats_by_year, overwrite = TRUE)
dbSendQuery(conn, "CREATE INDEX stats_id ON stats_by_year(Id)")
# system.time(test <- dbGetQuery(conn, "SELECT * FROM stats_by_year WHERE Id='9449211'"))



# Pull out a sample for the monthly pattern plot

# Store a sample for the monthly pattern plot
n_per_mo <- 150
tide_preds$month <- month(tide_preds$Date_Time)
tide_preds$rand <- runif(n = nrow(tide_preds), min = 0, max = 1)
pattern_data <- tide_preds %>%
  group_by(Id, month, Type) %>%
  slice_min(rand, n = n_per_mo)

dbWriteTable(conn, "pattern_data", pattern_data[, c("Date_Time", "Prediction", "Type", "Id")], overwrite = TRUE)
dbSendQuery(conn, "CREATE INDEX pattern_id ON pattern_data(Id)")

# system.time(abc <- dbGetQuery(conn, "SELECT * FROM pattern_data WHERE Id == '9464881'"))


# Calculate some more stats to the metadata (this let's us add things without spending 3 hours rerunning all the code)
lowstats <- tide_preds %>%
  filter(Type == 'L') %>%
  group_by(Id) %>%
  summarise(
    "q01" = as.numeric(quantile(Prediction, .01)),
    "q025" = as.numeric(quantile(Prediction, .025)),
    "q05" = as.numeric(quantile(Prediction, .05))
  )


# Keep top 50 H/L by year to save into the database
tide_preds <- by(tide_preds, tide_preds$Id, keep_by_year)
tide_preds <- do.call('rbind', tide_preds)
tide_preds <- tide_preds[, c("Date_Time", "Prediction", "Type", "Id", "year")]

# Merge in station info, write database
tide_preds <- merge(tide_preds, stations[, c("Id", "group", "subgroup", "state", "stationName")])


dbWriteTable(conn, "tide_preds", tide_preds, overwrite = TRUE)
dbSendQuery(conn, "CREATE INDEX stationid ON tide_preds(Id)")
# system.time(test <- dbGetQuery(conn, sprintf("SELECT * FROM tide_preds WHERE Id=%s", 9464881)))
# test <- dbGetQuery(conn, sprintf("SELECT * FROM tide_preds WHERE Id=%s", 9464881))


# Merge all the metadata together, then merge that with the station dataset
tide_metadata <- merge(tide_metadata, lowstats)
stations <- merge(stations, tide_metadata)
stations <- stations[!duplicated(stations$Id), ]
stations$Lon[stations$Lon > 0] <- stations$Lon[stations$Lon > 0] -360
stations <- stations[stations$Lat > 32.5, ]

save(file = "./tidefinder/stations.rda", stations)




dbDisconnect(conn)


