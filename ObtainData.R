## load libraries
library(pitchRx)
library(dplyr)

## Get MLB data for a day
dat <- scrape(start = "2016-07-02", end = "2017-07-02")

## 5 data frames are created
names(dat)

## take a look
dim(dat[["pitch"]])
str(dat[["pitch"]])

subTroutAtBats <- subset(dat[["atbat"]], batter_name == "Mike Trout")
subTroutCombo <- merge(subTroutAtBats, dat[["pitch"]], by= "play_guid")

# print some sample reports for comparisonswrite.csv(subTroutCombo, file="subTroutCombo.csv")
write.csv(subTroutCombo, file="subTroutCombo.csv")
write.csv(subTroutAtBats, file="subTroutAtBats.csv")
write.csv((dat[["atbat"]]), file = "atbat.csv")
write.csv((dat[["pitch"]]), file = "pitch.csv")


db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2016-07-02", end = "2017-07-02", connect = db$con)