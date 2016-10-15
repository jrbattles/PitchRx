mlbID <- "12345678"
qryDate <- Sys.Date()
fileURL <- "http://api-ant.fireants.io/mlb-api?mlbid="
fileURL <- paste0(fileURL,mlbID,"?date=",qryDate)
dfPitch <- fromJSON(fileURL)
