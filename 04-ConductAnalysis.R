## Conduct analysis

# Mike Trout
batsTrout <- filter(atbat16, batter == "545361")
#FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")

# inner join all pitches to specific batter
pitchesTrout <- collect(inner_join(pitch16, batsTrout))

# add new columns for HitterVal
joined <- pitchesTrout %>% mutate(quant_score = get_quant_score(des),
                                  qual_score = get_qual_score(atbat_des) * (type == 'X'),
                                  hitter_val = quant_score + qual_score)

# convert to factor variables
joined$pitch_type <- as.factor(joined$pitch_type) 
joined$des <- as.factor(joined$des) 
joined$type <- as.factor(joined$type)
joined$count <- as.factor(joined$count) 
joined$event <- as.factor(joined$event) 

levels(joined$pitch_type)[levels(joined$pitch_type)=="FS"] <- "SI"
levels(joined$pitch_type)[levels(joined$pitch_type)=="FT"] <- "SI"


# collect number of pitches observed
num.pitches <- nrow(joined)

# subset for all successful hits
subHits <- subset(joined, type == "X" & des == "In play, no out" | des =="In play, run(s)")
num.hits <- nrow(subHits)

# subset for all successful strikes
subStrikes <- subset(joined, type == "S")

#subset for All Hits and AllBallsInPlay 
#subAllHits <- subset(joined, type == "X" & des == "In play, no out" | des =="In play, run(s)")
subAllBallsInPlay <- subset(joined, type == "X")

pitch_label <- c(
    L = "LHP",
    R = "RHP"
)

# create Heat Map
strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(quant_score = 1), layer = facet_grid(pitch_type ~ p_throws, labeller = labeller(p_throws = pitch_label)))

strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(event = "Groundout"), layer = facet_grid(pitch_type ~ p_throws, labeller = labeller(p_throws = pitch_label)))

## Add Batter's strike zone
topKzone = mean(joined$sz_top)
botKzone = mean(joined$sz_bot) 
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)
