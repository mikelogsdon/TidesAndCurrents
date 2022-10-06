


setwd("~/Documents/Data Projects/TidesAndCurrents/")


sta_files <- dir(pattern = "stations_tides")
stations <- do.call('rbind', lapply(sta_files, read.csv))

# tmp <- merge(tmp, stations[, c("Id", "group", "subgroup", "state", "stationName")])

stupid_leading_space <- function(strx) {
  gsub("^[^A-Z]*(.+)$", "\\1", strx)
}

stations$group <- stupid_leading_space(stations$group)
stations$subgroup <- stupid_leading_space(stations$subgroup)
stations$state <- stations$state
stations$stationName <- stupid_leading_space(stations$Name)

stations <- stations[!duplicated(stations$Id), ]

save(file = "stations.rda", stations)

