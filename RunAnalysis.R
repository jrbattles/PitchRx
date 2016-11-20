## Conduct analysis

# Mike Trout
batsTrout <- filter(atbat16, batter == "545361")
#FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")
pitchesTrout <- collect(inner_join(pitch16, batsTrout))

joined <- pitchesTrout %>% mutate(quant_score = get_quant_score(des),
                                  qual_score = get_qual_score(atbat_des) * (type == 'X'),
                                  hitter_val = quant_score + qual_score)
## subset At Bats for Mike Trout At Bats
subTrout <- subset(joined, batter == "545361")

# subset for all successful hits
subHits <- subset(subTrout, type == "X" & des == "In play, no out" | des =="In play, run(s)")

# subset for all successful strikes
subStrikes <- subset(subTrout, type == "S")

#subset for All Hits and AllBallsInPlay 
subAllHits <- subset(joined, type == "X" & des == "In play, no out" | des =="In play, run(s)")
subAllBallsInPlay <- subset(joined, type == "X")

pitch_label <- c(
    L = "LHP",
    R = "RHP"
)
strikeFX(subAllBallsInPlay, geom = "raster", density1 = list(type = "X"),
         density2 = list(quant_score = 1), layer = facet_grid(. ~ p_throws, labeller = labeller(p_throws = pitch_label)))
