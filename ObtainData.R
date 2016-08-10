## load libraries
library(pitchRx)
library(dplyr)

## Get MLB data for a day
dat <- scrape(start = "2016-07-01", end = "2016-07-05")

## 5 data frames are created
names(dat)


# EDA: join pitch and at_bat data
pitch <- tbl_df(dat$pitch)
atbat <- tbl_df(dat$atbat)

pitch %>%
    select(gameday_link, num, id) %>%
    inner_join(x = ., 
               y = atbat %>%
                   select(gameday_link, num, pitcher, batter, pitcher_name, batter_name), 
               by = c('gameday_link', 'num'))



## take a look
dim(dat[["pitch"]])
dim(dat$atbat)
str(dat[["pitch"]])
str(dat$pitch)

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

#exploratory data analysis
# adding ggplot2 functions to customize animateFX() output won't work, but
# you can pass a list to the layer argument like this:
x <- list(
    facet_grid(pitcher_name ~ stand, labeller = label_both), 
    theme_bw(), 
    coord_equal()
)
animateFX(pitches, layer = x)

#more
animateFX(pitches, avg.by = "pitch_types", layer = x)

# even more
strikes <- subset(dat[['pitch']], des == "Called Strike")
strikeFX(strikes, geom = "tile") + 
    facet_grid(pitch_type ~ des) +
    coord_equal() +
    theme_bw() +
    viridis::scale_fill_viridis()




db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2016-07-02", end = "2017-07-02", connect = db$con)