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

## subset At Bats for Mike Trout At Bats
subTroutAtBats <- subset(dat[["atbat"]], batter_name == "Mike Trout")

## merge Mike Trout At Bats with all pitches for Pitch F/X variables?
subTroutCombo <- merge(subTroutAtBats, dat[["pitch"]], by= "play_guid")

## how can find add Batter ID to all pitches?

# print some sample reports for comparisonswrite.csv(subTroutCombo, file="subTroutCombo.csv")
write.csv(subTroutCombo, file="subTroutCombo.csv")
write.csv(subTroutAtBats, file="subTroutAtBats.csv")
write.csv((dat[["atbat"]]), file = "atbat.csv")
write.csv((dat[["pitch"]]), file = "pitch.csv")


db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2016-07-02", end = "2017-07-02", connect = db$con)