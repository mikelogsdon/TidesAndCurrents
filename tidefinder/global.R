library(ggplot2)
library(sp)
library(leaflet)
library(plyr)
library(dplyr)


load("stations.rda")
# load("tideData.rda")
load("tideDataSmall.rda")

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
    buf <- 10000
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
  # print(dim(tides)); print(max(tides$Date.Time))
  currentYear <- lubridate::year(lubridate::today())
  tides <- tides[lubridate::year(tides$Date.Time) %in% seq(currentYear, currentYear + nyears - 1), ]
  if(tides$Type[1] == "H") tides <- arrange(tides, -Prediction)
  tides[1:limit, c("Prediction", "Date.Time", "stationName", "group")]
}

# state <- "Alaska"; ids <- stations$Id[stations$group == "Cook Inlet"]; type = "L"; maxYear <- 2020
makeTideSummary3 <- function(state, ids, type, nyears) {
  tides <- getTides(state, Id = ids, type = type, limit = NULL, nyears = nyears)
  # print(dim(tides)); print(max(tides$Date.Time))
  tides$top100 <- 1:nrow(tides) <= 100
  top100 <- aggregate(top100 ~ Id, data = tides, FUN = sum)
  byStation <- ddply(tides, .(Id), function(x) {
    x <- arrange(x, Date.Time)
    tidalRange <- diff(x$Prediction)
    data.frame(
      "aveTidalRange" = mean(tidalRange[tidalRange>0]),
      "lowestPredicted" = min(x$Prediction),
      "lowestDate" = x$Date.Time[which.min(x$Prediction)],
      "highestPredicted" = max(x$Prediction),
      "highestDate" = x$Date.Time[which.max(x$Prediction)]
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
  print(urlx)
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
  lubridate::minute(dset$Date.Time) <- 0
  dset <- aggregate(. ~ Date.Time, FUN = mean, data = dset)
  dset
}

readOne <- function(stationId) {
  tmp1 <- read.tides("predictions", "20180201", "20271231", station = stationId, hiLow = TRUE)
  tmp2 <- read.tides("predictions", "20280101", "20361231", station = stationId, hiLow = TRUE)
  rbind(tmp1, tmp2)
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
  tmp <- tideDataSmall[tideDataSmall$Id %in% idx, ]
  
  if(!is.null(nyears)) {
    maxYear <- lubridate::year(lubridate::today()) + nyears - 1
    tmp <- tmp[lubridate::year(tmp$Date.Time) <= maxYear, ]
  }
  
  tmp <- merge(tmp, stations[, c("Id", "group", "subgroup", "state", "stationName")])
  
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
  readOne(stationId)
}

plotStationMonthly <- function(test, stationName) {
  if(is.null(test)) {
    return(ggplot() + geom_blank())
  }
  # test <- getTides(Id = stationId)
  # test <- readOne(stationId)
  
  test$hour <- lubridate::hour(test$Date.Time) + lubridate::minute(test$Date.Time) / 60
  test$month <- lubridate::month(test$Date.Time, label = TRUE, abbr = FALSE)
  test$year <- lubridate::year(test$Date.Time)
  test <- arrange(test, Prediction)
  head(test)
  
  
  hourBreaks <- seq(0, 22, 2)
  hourLabels <- c("Midnight", "2AM", "4AM", "6AM", "8AM", "10AM", "Noon", "2PM", "4PM", "6PM", "8PM", "10PM")
  ggplot(test) + theme_bw() +
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
plotStationYearly <- function(test, stationName) {
  if(is.null(test)) {
    return(ggplot() + geom_blank())
  }
  # test <- readOne(stationId)
  
  # test$hour <- lubridate::hour(test$Date.Time) + lubridate::minute(test$Date.Time) / 60
  # test$month <- lubridate::month(test$Date.Time, label = TRUE, abbr = FALSE)
  test$year <- lubridate::year(test$Date.Time)
  
  byYear <- ddply(test, .(year, Type), function(x) {
    data.frame(
      "min" = min(x$Prediction),
      "q25" = as.numeric(quantile(x$Prediction, .25)),
      "q50" = as.numeric(quantile(x$Prediction, .50)),
      "q75" = as.numeric(quantile(x$Prediction, .75)),
      "max" = max(x$Prediction)
    )
  })
  byYearLong <- reshape2::melt(byYear, id.vars = c("year", "Type"))
  byYearLong$variable <- factor(byYearLong$variable, levels = c("max", "q75", "q50", "q25", "min"), ordered = TRUE)
  byYearLong$Type <- factor(byYearLong$Type, levels = c("L", "H"), labels = c("Low Tide", "High Tide"), ordered = TRUE)
  ggplot(byYearLong) + theme_bw() +
    geom_point(aes(year, value, col = variable, shape = variable), size = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(2018, 2036, by = 2)) +
    xlab("") + ylab("Tide Prediction Height (MLLW)") +
    scale_colour_discrete("Summary Stat", labels = c("Max", "3rd Quartile", "Median", "1st Quartile", "Min")) +
    scale_shape_discrete(name = "Summary Stat", labels = c("Max", "3rd Quartile", "Median", "1st Quartile", "Min")) +
    facet_wrap(~Type, scales = "free_y") +
    ggtitle(sprintf("Yearly Tide Prediction Statistics for %s", stationName))

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
    "Current support is for Alaska, Washington, and Oregon on the west coast of the United States. This tool stores the 25 most extreme tides by station and by year, and more detailed graphics are created by querying NOAA in real time.</p>",
    "<p>The tool is meant to be useful for learning in general about tidal patterns, for example the extremes of Alaska's Cook Inlet versus the much milder tides of the Arctic Ocean versus the Bactrian Camel tides of Puget Sound.",
    "In addition, it is intended to be used for finding specific, interesting tides by location.",
    "For example, I live in the Puget Sound region of Washington, and I may be interested in the largest minus tide of the year at the nearest beach, or more broadly I might be interested in planning around the largest minus tide in the area in the next ten years.</p>",
    "<h5>Credits</h5>",
    "<p>The idea for this tool came from a conversation with Ruth Hulbert, a bookseller and artist in Palmer, Alaska, and Cynthia Hansen, a lab tech in Bellingham, Washington.",
    # "<p>This idea was pitched and first explored by Ruth Hulbert, a bookseller and artist in Palmer, Alaska, and Cynthia Hansen, a lab tech in Bellingham, Washington.",
    "The tool was built by Mike Logsdon, a data analyst in Seattle, Washington. All three are considerable beach enthusiasts.</p>"
  )
}


