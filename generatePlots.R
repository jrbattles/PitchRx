## Load Workspace | only in Dev Environment
setwd("C:/Users/cohend/fireant/PitchRx")  ## Fix path for your working directory
load("workspace_tables_with_pitch_type.RData")

## load libraries - Still required after loading workspace!
library(pitchRx)
library(dplyr)
library(stringr)
library(ggplot2)
require(akima)
library(wesanderson)

## Add Batter's strike zone
## not required if loading workspace workspace_tables_with_pitch_type
topKzone = mean(subTrout$sz_top)
botKzone = mean(subTrout$sz_bot) 
inKzone = -.95
outKzone = 0.95
kZone = data.frame(x = c(inKzone, inKzone, outKzone, outKzone, inKzone), y = c(botKzone, topKzone, topKzone, botKzone, botKzone))

## Filter out by Batter and Pitch Type
## not required if loading workspace workspace_tables_with_pitch_type
subTrout.FF <- joined %>% filter(batter=="545361",pitch_type=="FF")
subTrout.SL <- joined %>% filter(batter=="545361",pitch_type=="SL")
subTrout.CH <- joined %>% filter(batter=="545361",pitch_type=="CH")
subTrout.CU <- joined %>% filter(batter=="545361",pitch_type=="CU")
subTrout.FC <- joined %>% filter(batter=="545361",pitch_type=="FC")
subTrout.FT <- joined %>% filter(batter=="545361",pitch_type=="FT")
subTrout.SI <- joined %>% filter(batter=="545361",pitch_type=="SI")
subTrout.FS <- joined %>% filter(batter=="545361",pitch_type=="FS")

## FF Four-seam fastball
hv.FF <- data.frame(x = subTrout.FF$px, y = subTrout.FF$pz, z = subTrout.FF$hitter_val)
hv.FF.grid <- interp(hv.FF$x, hv.FF$y, hv.FF$z)
hv.FF.grid2 <- expand.grid(x=hv.FF.grid$x, y=hv.FF.grid$y)
hv.FF.grid2$z <- as.vector(hv.FF.grid$z)
ggplot(hv.FF.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout FF Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_FF.png", device="png", path="plots/")

## SL Slider
hv.SL <- data.frame(x = subTrout.SL$px, y = subTrout.SL$pz, z = subTrout.SL$hitter_val)
hv.SL.grid <- interp(hv.SL$x, hv.SL$y, hv.SL$z)
hv.SL.grid2 <- expand.grid(x=hv.SL.grid$x, y=hv.SL.grid$y)
hv.SL.grid2$z <- as.vector(hv.SL.grid$z)
ggplot(hv.SL.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout SL Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_SL.png", device="png", path="plots/")

## CH Change-up
hv.CH <- data.frame(x = subTrout.CH$px, y = subTrout.CH$pz, z = subTrout.CH$hitter_val)
hv.CH.grid <- interp(hv.CH$x, hv.CH$y, hv.CH$z)
hv.CH.grid2 <- expand.grid(x=hv.CH.grid$x, y=hv.CH.grid$y)
hv.CH.grid2$z <- as.vector(hv.CH.grid$z)
ggplot(hv.CH.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout CH Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_CH.png", device="png", path="plots/")

## CU Curveball
hv.CU <- data.frame(x = subTrout.CU$px, y = subTrout.CU$pz, z = subTrout.CU$hitter_val)
hv.CU.grid <- interp(hv.CU$x, hv.CU$y, hv.CU$z)
hv.CU.grid2 <- expand.grid(x=hv.CU.grid$x, y=hv.CU.grid$y)
hv.CU.grid2$z <- as.vector(hv.CU.grid$z)
ggplot(hv.CU.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout CU Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_CU.png", device="png", path="plots/")

## FC Cut fastball
hv.FC <- data.frame(x = subTrout.FC$px, y = subTrout.FC$pz, z = subTrout.FC$hitter_val)
hv.FC.grid <- interp(hv.FC$x, hv.FC$y, hv.FC$z)
hv.FC.grid2 <- expand.grid(x=hv.FC.grid$x, y=hv.FC.grid$y)
hv.FC.grid2$z <- as.vector(hv.FC.grid$z)
ggplot(hv.FC.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout FC Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_FC.png", device="png", path="plots/")

## FF Two-seam fastball
hv.FT <- data.frame(x = subTrout.FT$px, y = subTrout.FT$pz, z = subTrout.FT$hitter_val)
hv.FT.grid <- interp(hv.FT$x, hv.FT$y, hv.FT$z)
hv.FT.grid2 <- expand.grid(x=hv.FT.grid$x, y=hv.FT.grid$y)
hv.FT.grid2$z <- as.vector(hv.FT.grid$z)
ggplot(hv.FT.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout FT Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_FT.png", device="png", path="plots/")

## SI Sinker
hv.SI <- data.frame(x = subTrout.SI$px, y = subTrout.SI$pz, z = subTrout.SI$hitter_val)
hv.SI.grid <- interp(hv.SI$x, hv.SI$y, hv.SI$z)
hv.SI.grid2 <- expand.grid(x=hv.SI.grid$x, y=hv.SI.grid$y)
hv.SI.grid2$z <- as.vector(hv.SI.grid$z)
ggplot(hv.SI.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout SI Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_SI.png", device="png", path="plots/")

## FS Split-finger fastball
hv.FS <- data.frame(x = subTrout.FS$px, y = subTrout.FS$pz, z = subTrout.FS$hitter_val)
hv.FS.grid <- interp(hv.FS$x, hv.FS$y, hv.FS$z)
hv.FS.grid2 <- expand.grid(x=hv.FS.grid$x, y=hv.FS.grid$y)
hv.FS.grid2$z <- as.vector(hv.FS.grid$z)
ggplot(hv.FS.grid2) + labs(x="x pos",y="z pos") + ggtitle("Mike Trout FS Hitter Value") + geom_tile(aes(x = x, y = y, z = z, fill = z) ) + coord_equal() + geom_contour(aes(x = x, y = y, z = z, fill = z), color = "white", alpha = .3) + scale_fill_gradientn(name="Hitter\nValue",colors=pal, na.value="white", limits=c(min(subTrout$hitter_val),max(subTrout$hitter_val))) + geom_path(aes(x, y), data = kZone, linetype = 2) + coord_cartesian(xlim=c(-1.5,1.5),ylim=c(1,4)) + theme_bw()
## Save plot to working directory in the plots sub-folder
ggsave("545361_hv_FS.png", device="png", path="plots/")

