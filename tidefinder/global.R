library(ggplot2)
library(sp)
library(leaflet)
library(plyr)
library(dplyr)
library(RSQLite)
library(lubridate)


load("stations.rda")
# load("tideData.rda")
# load("tideDataSmall.rda")
source("noaa_api.R")



findStations <- function(state, region, station) {
  if(station != "") {
    selectedStations <- stations[stations$stationName == station & stations$state == state & stations$group == region, ]
  } else if(region != "") {
    selectedStations <- stations[stations$group == region, ]
  } else if(state != "") {
    selectedStations <- stations[stations$state == state, ]
  } else {
    selectedStations <- stations
  }
}

nearestStation <- function(click) {
  dists <- sapply(1:nrow(stations), function(i) {
    sqrt((click$lng - stations$Lon[i]) ^ 2 + (click$lat - stations$Lat[i]) ^ 2)
  })
  stations[which.min(dists), ]
}

returnBounds <- function(sx) {
  buf <- 20
  if(unique(sx$state) == "Alaska" & length(unique(sx$group)) > 1) {
    buf <- 100000
  }
  # Create latitude interval for map
  latInterval <- c(min(sx$Lat), max(sx$Lat))
  latSpan <- diff(latInterval)
  latInterval[1] <- latInterval[1] - latSpan / buf
  latInterval[2] <- latInterval[2] + latSpan / buf
  
  
  # Create longitude interval for map
  lonInterval <- c(min(sx$Lon), max(sx$Lon))
  lonSpan <- diff(lonInterval)
  lonInterval[1] <- lonInterval[1] - lonSpan / buf
  lonInterval[2] <- lonInterval[2] + lonSpan / buf
  
  # # Find the wider interval and make into a square
  # if(diff(latInterval) > diff(lonInterval)) {
  #   lonInterval <- c(mean(lonInterval) - diff(latInterval) / 2, mean(lonInterval) + diff(latInterval) / 2)
  # } else {
  #   latInterval <- c(mean(latInterval) - diff(lonInterval) / 2, mean(latInterval) + diff(lonInterval) / 2)
  # }
  # if(lonInterval[1] <= -180) lonInterval[1] <- -179.9
  if(latInterval[2] >= 72) latInterval[2] <- 72
  return(list("lng1" = lonInterval[1], "lat1" = latInterval[1], "lng2" = lonInterval[2], "lat2" = latInterval[2]))
}



makeTideSummary2 <- function(tides, nyears, limit = 100) {
  # print(dim(tides)); print(max(tides$Date_Time))
  currentYear <- lubridate::year(lubridate::today())
  tides <- tides[lubridate::year(tides$Date_Time) %in% seq(currentYear, currentYear + nyears - 1), ]
  if(tides$Type[1] == "H") tides <- arrange(tides, -Prediction)
  tides[1:limit, c("Prediction", "Date_Time", "stationName", "group")]
}

# state <- "Alaska"; ids <- stations$Id[stations$group == "Cook Inlet"]; type = "L"; maxYear <- 2020
makeTideSummary3 <- function(state, ids, type, nyears) {
  tides <- getTides(state, Id = ids, type = type, limit = NULL, nyears = nyears)
  # print(dim(tides)); print(max(tides$Date_Time))
  tides$top100 <- 1:nrow(tides) <= 100
  top100 <- aggregate(top100 ~ Id, data = tides, FUN = sum)
  byStation <- ddply(tides, .(Id), function(x) {
    x <- arrange(x, Date_Time)
    tidalRange <- diff(x$Prediction)
    data.frame(
      "aveTidalRange" = mean(tidalRange[tidalRange>0]),
      "lowestPredicted" = min(x$Prediction),
      "lowestDate" = x$Date_Time[which.min(x$Prediction)],
      "highestPredicted" = max(x$Prediction),
      "highestDate" = x$Date_Time[which.max(x$Prediction)]
    )
  })
  stations2 <- stations
  stations2 <- stations2[, !(names(stations2) %in% c("aveTidalRange", "lowestPredicted", "lowestDate", "highestPredicted", "highestDate"))]
  stations2 <- merge(stations2, byStation)
  stations2 <- merge(stations2, top100)
  if(type == "L") {
    vars <- c("lowestPredicted", "lowestDate", "top100Lowest", "stationName", "group")
    names(stations2)[names(stations2) == "top100"] <- "top100Lowest"
    stations2 <- arrange(stations2, lowestPredicted)
  } else if(type == "H") {
    names(stations2)[names(stations2) == "top100"] <- "top100Highest"
    vars <- c("highestPredicted", "highestDate", "top100Highest", "stationName", "group")
    stations2 <- arrange(stations2, -highestPredicted)
  }
  stations2[, vars]
}


get_tide_by_id <- function(idx) {
  conn <- dbConnect(SQLite(), dbname = "tide_preds.db")
  tmp <- dbGetQuery(
    conn,
    sprintf(
      "SELECT * FROM tide_preds WHERE Id IN ('%s')",
      paste(idx, sep = "'", collapse = "','")
    )
  )
  dbDisconnect(conn)
  tmp$Date_Time <- as.POSIXct(tmp$Date_Time, origin = "1970-01-01", tz = "UTC")
  return(tmp)
}

getTides <- function(state, Id = NULL, region = NULL, station = NULL, type = NULL, limit = NULL, nyears = NULL) {
  if(!is.null(Id)) {
    idx <- Id
  } else if(!is.null(station) & !is.null(region) & !is.null(state)) {
    idx <- stations$Id[stations$state == state & stations$group == region & stations$stationName == station]
  } else if(!is.null(state) & !is.null(region)) {
    idx <- stations$Id[stations$state == state & stations$group == region]
  } else if(!is.null(state)) {
    idx <- stations$Id[stations$state == state]
  } else{
    warning("Improper Specification to Read Tides")
    return(NULL)
  }
  # tmp <- tideDataSmall[tideDataSmall$Id %in% idx, ]
  tmp <- get_tide_by_id(idx)

  
  if(!is.null(nyears)) {
    maxYear <- lubridate::year(lubridate::today()) + nyears - 1
    tmp <- tmp[lubridate::year(tmp$Date_Time) <= maxYear, ]
  }
  
  # tmp <- merge(tmp, stations[, c("Id", "group", "subgroup", "state", "stationName")])
  
  if(!is.null(type)) {
    tmp <- tmp[tmp$Type == type, ]
    if(type == "L") {
      tmp <- arrange(tmp, Prediction)
    } else if(type == "H") {
      tmp <- arrange(tmp, -Prediction)
    }
  }
  
  if(!is.null(limit)) {
    tmp <- tmp[1:limit, ]
  }
  
  return(tmp)
}

getStationData <- function(stationId) {
  conn <- dbConnect(SQLite(), dbname = "tide_preds.db")
  pattern_data <- dbGetQuery(
    conn,
    sprintf(
      "SELECT * FROM pattern_data WHERE Id = '%s'",
      stationId
    )
  )
  stats_data <- dbGetQuery(
    conn,
    sprintf(
      "SELECT * FROM stats_by_year WHERE Id = '%s'",
      stationId
    )
  )
  lows_data <- dbGetQuery(
    conn,
    sprintf(
      "SELECT Id,Date_Time,Prediction,year FROM tide_preds WHERE Id = '%s' AND Prediction < %s",
      stationId, stations$q05[stations$Id == stationId]
    )
  )
  dbDisconnect(conn)
  pattern_data$Date_Time <- as.POSIXct(pattern_data$Date_Time, origin = "1970-01-01", tz = "UTC")
  lows_data$Date_Time <- as.POSIXct(lows_data$Date_Time, origin = "1970-01-01", tz = "UTC")
  return(list(pattern_data, stats_data, lows_data))
  # read_tides(stationId)
  
}

plotStationMonthly <- function(stationData, stationName) {
  pattern_data <- stationData[[1]]
  if(is.null(pattern_data)) {
    return(ggplot() + geom_blank())
  }
  # pattern_data <- getTides(Id = stationId)
  # pattern_data <- read_tides(stationId)
  
  pattern_data$hour <- lubridate::hour(pattern_data$Date_Time) + lubridate::minute(pattern_data$Date_Time) / 60
  pattern_data$month <- lubridate::month(pattern_data$Date_Time, label = TRUE, abbr = FALSE)
  pattern_data$year <- lubridate::year(pattern_data$Date_Time)
  pattern_data <- arrange(pattern_data, Prediction)
  # head(test)
  
  
  hourBreaks <- seq(0, 22, 2)
  hourLabels <- c("Midnight", "2AM", "4AM", "6AM", "8AM", "10AM", "Noon", "2PM", "4PM", "6PM", "8PM", "10PM")
  ggplot(pattern_data) + theme_bw() +
    geom_point(aes(hour, Prediction, col = Type)) +
    facet_wrap(~month) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_colour_discrete(labels = c("High Tide", "Low Tide"), name = "") +
    scale_x_continuous(breaks = hourBreaks, labels = hourLabels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Hour of Day") +
    ggtitle(sprintf("Tide Predictions for %s", stationName))
}

# stationId <- "9455912"
plotStationYearly <- function(stationData, stationName) {
  if(is.null(stationData)) {
    return(ggplot() + geom_blank())
  }
  stats_data <- stationData[[2]]
  # test <- read_tides(stationId)
  
  # test$hour <- lubridate::hour(test$Date_Time) + lubridate::minute(test$Date_Time) / 60
  # test$month <- lubridate::month(test$Date_Time, label = TRUE, abbr = FALSE)
  
  # cur_year <- year(today())
  # min_year_lab <- ceiling(cur_year / 2) * 2
  # max_year_lab <- min_year_lab + 18
  
  byYearLong <- reshape2::melt(stats_data, id.vars = c("Id", "year", "Type"))
  varsToUse <- c("max", "q975", "q75", "q50", "q25", "q025", "min")
  byYearLong <- byYearLong %>% filter(variable %in% varsToUse)
  byYearLong$variable <- factor(byYearLong$variable, levels = varsToUse, ordered = TRUE)
  byYearLong$Type <- factor(byYearLong$Type, levels = c("L", "H"), labels = c("Low Tide", "High Tide"), ordered = TRUE)
  stat_labs <- c("Max", "97.5%", "3rd Quartile", "Median", "1st Quartile", "2.5%", "Min")
  ggplot(byYearLong) + theme_bw() +
    geom_point(aes(year, value, col = variable, shape = variable), size = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # scale_x_continuous(breaks = seq(min_year_lab, max_year_lab, by = 2)) +
    xlab("") + ylab("Tide Prediction Height (MLLW)") +
    scale_colour_discrete("Summary Stat", labels = stat_labs) +
    scale_shape_manual(name = "Summary Stat", values = 1:7, labels = stat_labs) +
    facet_wrap(~Type, scales = "free_y") +
    ggtitle(sprintf("Yearly Tide Prediction Statistics for %s", stationName))

}


plotStationLows <- function(stationData, stationName) {
  lows_data <- stationData[[3]]
  lows_data$time2 <- lows_data$Date_Time
  year(lows_data$time2) <- 2020
  lows_data$catx <- "Good"
  lows_data$catx[lows_data$Prediction < stations$q025[stations$Id == lows_data$Id[1]]] <- "Great"
  lows_data$catx[lows_data$Prediction < stations$q01[stations$Id == lows_data$Id[1]]] <- "Outstanding"
  lows_data$catx <- factor(lows_data$catx, levels = c("Good", "Great", "Outstanding"), ordered = TRUE)
  
  minyr <- ceiling(min(lows_data$year) / 2) * 2
  maxyr <- minyr + 18
  lows_data <- arrange(lows_data, desc(catx))
  
  ggplot(lows_data) + theme_bw() +
    geom_point(aes(x = year, y = time2, col = catx, size = Prediction), alpha = .8) +
    scale_y_datetime(
      breaks = scales::date_breaks(width = "1 month"), 
      labels = scales::date_format("%B")) +
    scale_x_continuous(breaks = seq(minyr, maxyr, by = 2)) +
    scale_colour_viridis_d(name = "Low Tide") +
    scale_size_continuous(range = c(10, 1)) +
    # scale_size_discrete(name = "Low Tide") +#, range = c(2, 4, 8)) +
    ggtitle(sprintf("Distribution of Low Tides for %s", stationName)) +
    xlab("Year") + ylab("Month")
  
}


# state <- "Alaska"; stationids <- stations$Id[stations$group == "Cook Inlet"]
lowTidePlot <- function(state, stationids) {
  tidex <- getTides(state, stationids)
  tidex <- ddply(tidex, .(Id, year), function(x) {
    data.frame(
      "lowestTide" = min(x$Prediction)
    )
  })
  ggplot(tidex) + theme_bw() +
    geom_line(aes(year, lowestTide, col = Id))
}



makeNotes <- function() {
  c(
    # "<h3>Tide Finder Notes</h3>",
    "<h4>Alternate Title: It Was the Best of Tides, It Was the Worst of Tides</h5>",
    "<p>This tool uses the tidal predictions issued by the National Oceanic and Atmospheric Administration to facilitate exploration of interesting or unusual tides.",
    "You can learn more about NOAA tidal predictions <a href 'https://tidesandcurrents.noaa.gov/PageHelp.html'>here</a>.",
    "Current support is for Alaska, Washington, Oregon, and California on the west coast of the United States. This tool stores the 50 most extreme tides by station and by year, as well as a sample of all tidal predictions to make the tidal pattern graphics.</p>",
    "<p>The tool is meant to be useful for learning in general about tidal patterns, for example the extremes of Alaska's Cook Inlet versus the much milder tides of the Arctic Ocean versus the Bactrian Camel tides of Puget Sound.",
    "In addition, it is intended to be used for finding specific, interesting tides by location.",
    "For example, I live in the Puget Sound region of Washington, and I may be interested in the largest minus tide of the year at the nearest beach, or more broadly I might be interested in planning around the largest minus tide in the area in the next ten years.</p>",
    "<h5>Credits</h5>",
    "<p>The idea for this tool came from a conversation with Ruth Hulbert, a bookseller and artist in Palmer, Alaska, and Cynthia Hansen, a lab tech in Bellingham, Washington.",
    # "<p>This idea was pitched and first explored by Ruth Hulbert, a bookseller and artist in Palmer, Alaska, and Cynthia Hansen, a lab tech in Bellingham, Washington.",
    "The tool was built by Mike Logsdon, a data analyst in Seattle, Washington. All three are considerable beach enthusiasts.</p>",
    "<p>Code to build the datasets and create the app can be found at <a href 'https://github.com/mikelogsdon/TidesAndCurrents'>GitHub</a></p>",
    "<p>Contact me with questions or comments mikelogsdon87 at outlook dot com</p>"
  )
}


