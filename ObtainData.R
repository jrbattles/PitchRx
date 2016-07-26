## load libraries
library(pitchRx)
library(dplyr)

## Get MLB data for a day
dat <- scrape(start = "2016-07-02", end = "2016-07-02")

## 5 data frames are created
names(dat)

## If loaded properly, you should have 4478 obs. of 49 variables
dim(dat$pitch)

# EDA: join pitch and at_bat data. dplyr uses table data frames so convert anyway.
pitch <- tbl_df(dat$pitch)
atbat <- tbl_df(dat$atbat)

## Subset for testing purposes.
## atbat_sub <- atbat %>%
    ## filter(gameday_link == 'gid_2016_07_01_clemlb_tormlb_1') %>%
    ## select(gameday_link, num, pitcher, batter, pitcher_name, batter_name)

mypitch <- pitch %>%
    ## Restrict to specific fields within the pitch data frame.
<<<<<<< HEAD
    select(des, type, tfs, tfs_zulu, num, id, sz_top, sz_bot, px, pz, pitch_type, count) %>%  
    inner_join(x = ., 
               y = atbat %>%
                   select(num, pitcher, batter, pitcher_name, batter_name, stand, atbat_des, event, inning), 
=======
    # select(gameday_link, num, id) %>%  
    inner_join(x = ., 
               y = atbat %>%
                   select(gameday_link, num, pitcher, batter, pitcher_name, batter_name, stand, atbat_des, event, inning), 
>>>>>>> a98b6c17756263968ce0baefa49efc21b1a336a4
               by = c('gameday_link', 'num'))


## take a look
dim(dat$atbat)
str(dat$pitch)

## If loaded properly, you should have 1157 obs. of  30 variables
dim(dat$atbat)
str(dat$atbat)

## subset At Bats for Mike Trout At Bats
subTroutAtBats <- subset(dat$atbat, batter_name == "Mike Trout")

## merge Mike Trout At Bats with all matching Pitches for Pitch F/X variables?
subTroutCombo <- merge(subTroutAtBats, dat$pitch, by= "play_guid")

## struggling with the reverse.  Need to map Batter ID to all Pitches
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

## Write to database
db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2016-07-02", end = "2017-07-02", connect = db$con)

## Discussion with Frank Evans
### stringr - used for keyword searching.  perhaps use a subfunction to recurse.
### http://rawgit.com/jrbattles/fire-ants-mtgs/master/fire-ants-mtg02.html#1