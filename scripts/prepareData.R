rawData <- read.csv("raw_data/stadiumexplorer-latlong.csv", stringsAsFactors = F)
rawData <- rawData[1:30, ]
row.names(rawData) <- rawData$Stadium.Name
saveRDS(rawData, "Shiny_app/data/stadium_data.RDS")