## load libraries
library(pitchRx)    ## thank you Carson Sievert!!!
library(dplyr)      ## thank you Hadley Wickham
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
            as.integer(str_detect(des, "line")) * 1.5 +
            as.integer(str_detect(des, "sharp")) * 1.5 +
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
 
# Load data from final month of World Series
data.fin.month <- scrape(start = "2016-09-25", end = "2016-10-24", connect = my_db1$con)
#data.season 

pitch16 <- select(tbl(my_db1, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, sz_top, sz_bot, px, pz, pitch_type, count)
atbat16 <- select(tbl(my_db1, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)




