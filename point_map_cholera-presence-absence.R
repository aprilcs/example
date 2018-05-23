# Code to create a point map illustrating the presence and absence of cholera
#  by town in Ireland during the cholera outbreak of 1848 - 1850

#### Initial configuration ####
# R version 3.1.3 (2015-03-09)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 8 x64 (build 9200)

#### Working directory ####
# Clone or download the repository and set the working directory
#  with setwd to the folder where the repository is located.

#### Libraries ####
library(rgeos) # version 0.3-23
library(maptools) # version 0.8-34
library(rgdal) # version 1.2-7

#### Constants ####

data.file.path <- "/data/Analysis-data_Towns_Cholera-Ireland-1848-1850.txt"
shapefile.path <- "/data/Ireland-shapefile_Boundary/Ireland-shapefile_Boundary.shp"

legend.x <- 600000
legend.y <- 5750000
legend.text <- c("Cholera present","Cholera absent")
legend.colours <- c("red", "blue")
legend.title <- "Cholera presence or absence"

map.title <- "Figure 1. Presence and absence of reported cholera cases by Irish town (1848 - 1850)"

#### Point map ####
# Presence and absence of reported cholera cases in Ireland by town

pts <- read.table(data.file.path, header=T)

# pts with CASES == 0 are towns without reported cases of cholera
# pts with CASES >= 1 are towns with at least one reported case of cholera
ptsp <- pts[pts$CASES >= 1,]
ptsn <- pts[pts$CASES == 0,]

# Boundary is the boundary shapefile for Ireland

Boundary <- readOGR(shapefile.path)

dev.new()
png(filename="/results/cholera-presence-absence.png")
plot(Boundary, axes=T)

# ptsn$x and ptsn$y are the UTM coordinates of towns without reported cases of cholera
# ptsp$x and ptsp$y are the UTM coordinates of towns with reported cases of cholera
points(ptsn$x , ptsn$y, cex = 0.6, pch = 16, col = "blue")
points(ptsp$x , ptsp$y, cex = 0.6, pch = 16, col = "red")

legend(
  x = legend.x,
  y = legend.y, 
  pch = 16, 
  legend = legend.text[1:2], 
  cex = 0.8, 
  title = legend.title, 
  col = legend.colours[1:2]
)


title(main = NULL, sub = map.title)
dev.off()
