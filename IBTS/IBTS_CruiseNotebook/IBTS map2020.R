# IBTS Map 2020

#Libraries
library("tidyverse")
#library("RColorBrewer")
library("maps")
library("mapdata")
#library("sf")
#library("ggthemes", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#map
europe <- map_data("worldHires", c("Norway", "Sweden", "Denmark", "UK", "Germany")) 

#setting up grid
xvals <- seq(-4.5, 15.5, by=1)
yvals <- seq(55.25, 63, by=0.5)

ns_grid <- expand.grid(x=xvals,y=yvals)
write_csv2(ns_grid,"ns_grid.csv")

#editing the ns_grid.csv in excel adding square numbers and Norwegian Sampling scheme before exporting as a .csv file
ns_squares <- read.csv2("NSsquares.csv")


#adding grid and making map
gg <- ggplot(ns_squares, aes(x=Long, y=Lat, fill=as.factor(NOR_Squares))) + geom_tile(colour="gray40") + scale_fill_manual(values = c("NA", "lightskyblue2", "palevioletred2")) + guides(fill = guide_legend("Number of NOR \nstations pr square \n(1=1GOV+2MIK \n 2=2GOV+4MIK)")) +theme_minimal() + geom_map(data=europe, map=europe,  aes(x=long, y=lat, map_id=region), color="gray50", fill="gray60") + coord_map(xlim = c(-4, 12), ylim=c(55, 63)) + geom_rect(xmin=11,xmax=12,ymin=55,ymax=63, fill="white") + geom_rect(xmin=-4,xmax=12,ymin=62.5,ymax=63, fill="white") + annotate("text", label = c("39", "40", "41","42","43","44","45","46","47","48","49","50","51","52"), x = 11.5, y = seq(from = 55.25, to = 62, by = 0.5), size = 3) + annotate("text", label = c("E6", "E7", "E8","E9","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9", "G0"), x = seq(from = -3.5, to = 10.5, by = 1), y = 62.75, size = 3)

gg 
ggsave("IBTSmap2020.png")

#TODO

# - add grid cell row and column numbers to map (using text or similar)
