## load libraries
library(pitchRx)
library(dplyr)
library(stringr)
library(ggplot2)


# load Quantitative and Qualitative Scoring Functions Functions
get_quant_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "Called Strike")) * -(1/3) +
        as.integer(str_detect(des, "Foul")) * -(1/3) +
        as.integer(str_detect(des, "In play, run")) * 1.0 +
        as.integer(str_detect(des, "In play, out")) * 0.0 +
        as.integer(str_detect(des, "In play, no out")) * 1.0 +
        as.integer(str_detect(des, "^Ball$")) * 0.25 +
        as.integer(str_detect(des, "Swinging Strike")) * -(1/3) +
        as.integer(str_detect(des, "Hit By Pitch")) * 1.0 +
        as.integer(str_detect(des, "Ball in Dirt")) * 0.25 +
        as.integer(str_detect(des, "Missed Bunt")) * -(1/3) +
        as.integer(str_detect(des, "Intent Ball")) * 0.25
    )
    return(score)
}
get_qual_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "homer")) * 2 +
        as.integer(str_detect(des, "line")) * 1 +
        as.integer(str_detect(des, "sharp")) * 1 +
        as.integer(str_detect(des, "grounds")) * -1 +
        as.integer(str_detect(des, "flies")) * -1 +
        as.integer(str_detect(des, "soft")) * -2 +
        as.integer(str_detect(des, "pop")) * -2 +
        as.integer(str_detect(des, "triples")) * 1.5 +
        as.integer(str_detect(des, "doubles")) * 1.0 +
        as.integer(str_detect(des, "error")) * 0.5
    )
    return(score)
}

## Get MLB data for a a week.
dat <- scrape(start = "2016-08-14", end = "2016-08-20")

### May need to scrape entire years by subsetting the individual teams.

## 5 data frames are created - atbat, action, pitch, po, runner
names(dat)

## If loaded properly, pitch data frame should have 28831 obs. of 49 variables
dim(dat$pitch)

## If loaded properly, atbat data frame should have 7475 obs. of 30 variables
dim(dat$atbat)

## take a look
str(dat$atbat)
str(dat$pitch)

# EDA: join pitch and at_bat data. dplyr uses table data frames so convert anyway.
pitch <- tbl_df(dat$pitch)
atbat <- tbl_df(dat$atbat)

# combine
joined <- pitch %>%
    select(gameday_link, num, des, type, tfs, tfs_zulu, 
           id, sz_top, sz_bot, px, pz, pitch_type, count) %>%
    inner_join(x = ., 
               y = atbat %>%
                   select(gameday_link, num, pitcher, batter, b_height, 
                          pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning), 
               by = c('gameday_link', 'num')) %>%
    mutate(quant_score = get_quant_score(des),
           qual_score = get_qual_score(atbat_des) * (type == 'X'),
           hitter_val = quant_score + qual_score)
    #%>% 
    #select(type, atbat_des, qual_score, des, quant_score) %>% 
    #filter(type == 'X', qual_score == 0) %>% 
    #View(.)


## hist(filter(joined, type == 'X')$qual_score)
## hist(joined$hitter_val)

## There should be 485 unique batters 
length(unique(joined$batter))

## subset At Bats for Mike Trout At Bats
subTrout <- subset(joined, batter == "545361")

# subset for all successful hits
subHits <- subset(subTrout, type == "X" & des == "In play, no out" | des =="In play, run(s)")



## Graphing experiments

## contour map with B, S, X types
strikeFX(subTrout, color = "type", point.alpha = 0.2, adjust = TRUE, contour = TRUE) + 
            facet_grid(. ~ p_throws) + 
            theme(legend.position = "right", legend.direction = "vertical") +
            coord_equal() + theme_bw()

## contour map
strikeFX(subTrout, color = "type", point.alpha = 0.1, adjust = TRUE, contour = TRUE) + 
    facet_grid(pitch_type ~ stand) + 
    theme(legend.position = "right", legend.direction = "vertical") +
    coord_equal() + theme_bw()

strikeFX(subTrout, geom = "tile") + 
    facet_grid(pitch_type ~ p_throws) +
    coord_equal() +
    theme_bw() +
    viridis::scale_fill_viridis()

joined %>%
    filter(pitch_type == 'CU') %>%
    select(stand, px, pz, sz_top, sz_bot, hitter_val) %>%
    ggplot(data = .) +
    geom_point(mapping = aes(x = px, y = pz, color = hitter_val), size = 3)
    
joined %>%
    filter(pitch_type == 'CU') %>%
    select(stand, px, pz, sz_top, sz_bot, hitter_val) %>%
    group_by() %>%
    mutate(sz_top_avg = mean(sz_top),
           sz_bot_avg = mean(sz_bot)) %>%
    ungroup() %>%
    ggplot(data = .) +
    # TODO: figure out how stat_ works
    #stat_density2d(mapping = ), agg hitter val instead of just count
    # TODO: filtering (hits, by batter)
    geom_hex(mapping = aes(x = px, y = pz, color = hitter_val)) +
    geom_rect(mapping = aes(xmin = -0.708, xmax = 0.708, 
                            ymin = sz_bot_avg, ymax = sz_top_avg), 
              alpha = 0, linetype = 'dotted', size = 0.5, color = 'orange')


joined %>%
    filter(pitch_type == 'FF', batter_name == 'Mike Trout') %>%
    select(stand, px, pz, sz_top, sz_bot, hitter_val) %>%
    mutate(pxr = round(px, digits = 0),
           pzr = round(pz, digits = 0)) %>%
    group_by(pxr, pzr) %>%
    summarise(hitter_val = mean(hitter_val),
              num_pitches = n()) %>%
    #filter(num_pitches > 10) %>%
    ggplot(data = .) +
    geom_tile(mapping = aes(x = pxr, y = pzr, fill = hitter_val))
    

joined %>%
    filter(pitch_type == 'FF', 
           batter_name == 'Mike Trout',
           type == 'X',
           quant_score > 0) %>%
    select(stand, px, pz, sz_top, sz_bot, hitter_val) %>%
    mutate(pxr = round(px, digits = 0),
           pzr = round(pz, digits = 0)) %>%
    group_by(pxr, pzr) %>%
    summarise(hitter_val = mean(hitter_val),
              num_pitches = n()) %>%
    ggplot(data = .) +
    stat_density2d(mapping = aes(x = px, y = pz))
    geom_tile(mapping = aes(x = pxr, y = pzr, fill = num_pitches))




persp(x = joined$px, y = joined$pz, z = joined$hitter_val)











## 5 data frames are created
names(dat)

## If loaded properly, you should have 4478 obs. of 49 variables
dim(dat$pitch)

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
strikes <- subset(joined, des == "Called Strike")
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









