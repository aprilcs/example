# Code to create a model-based kriging risk map modelling the first day that 
#  cholera arrived by town in Ireland during the cholera outbreak of 1848 - 1850.
#  A model of the spatial dependence of the first day that cholera arrived in
#  each town is fitted, and a risk map is created by predicting the neighbouring
#  values through trend surface analysis. This models the temporal and 
#  spatial pattern of the disease spread.

#### Initial configuration ####
# R version 3.1.3 (2015-03-09)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 8 x64 (build 9200)

#### Working directory ####
# Clone or download the repository and set the working directory
#  with setwd to the folder where the repository is located.

#### Libraries ####

library(maptools) # version 0.8-34
library(geoR) # version 1.7-4.1
library(rgdal) # version 0.9-2
library(gstat) # version 1.0-22

#### Constants ####

data.file.path <- "/data/Analysis-data_Towns_Cholera-Ireland-1848-1850.txt"
shapefile.path <- "/data/Ireland-shapefile_Boundary-coords.txt"

semivariogram.max.distance <- 200000
sill <- 8000
range <- 150000
nugget <- 2000

map.width <- 6
map.height <- 3

map.title <- "Figure 2. Model of the epidemic day of first cholera case (1848 - 1850)"

#### Exploratory plots ####

pts <- read.table(data.file.path, header=T)
pts.matrix <- as.matrix(cbind(pts$x, pts$y, pts$FIRST_DAY))
pts.geo.first.day <- as.geodata(pts.matrix)

# plot the geodata object of the date of first cholera case per town

plot.geodata(pts.geo.first.day)
dev.new(width = map.width, height = map.height, pointsize = 12)

# plot semivariograms to show the spatial dependence structure
par (mfrow = c(1,2), pty = "s")
semivariogram.first.day <- variog(
  pts.geo.first.day, 
  estimator.type = "modulus", 
  op = "cloud"
)
plot(semivariogram.first.day)

emp.semiovariogram.first.day <- variog(pts.geo.first.day, estimator.type = "modulus")
plot(emp.semiovariogram.first.day)
dev.off()

#### Model the spatial dependence #####

semivariogram.model <- variog(
  pts.geo.first.day, 
  estimator.type = "modulus", 
  max.dist = semivariogram.max.distance
)
dev.new()
plot(semivariogram.model)

#### Fit the model using likfit function ####
semivariogram.model.fit <- likfit(
  pts.geo.first.day, 
  ini.cov.pars = c(sill, range), 
  trend = "cte", fix.nug=TRUE, 
  nugget = nugget, 
  cov.model = "exponential"
)

#### Plot the model over the semivariogram ####
lines.variomodel(
  semivariogram.model.fit, 
  lty = 1, 
  lwd = 2, 
  max.dist = semivariogram.max.distance, 
  col = "red"
) 

semivariogram.model.fit

#### Model-based kriging ####

### Create a grid the size of the country boundary for prediction locations ####
country.boundary <- read.table("/data/Ireland-shapefile_Boundary-coords.txt", header=T)
krige.grid <- expand.grid(
  seq(min(country.boundary$x), max(country.boundary$x), l = 100), 
  seq(min(country.boundary$y), max(country.boundary$y), l = 100)
)

boundary.matrix <- as.matrix(cbind(country.boundary$x, country.boundary$y))

#### Polynomial regression using ordinary kriging ####
# using the residuals from the trend surface analysis

first.day.kriging <- krige.conv(
  pts.geo.first.day, 
  krige = krige.control (type.krige = "ok", 
                         trend.d = "cte", 
                         obj.model = semivariogram.model.fit), 
  locations = krige.grid, 
  borders = boundary.matrix)

#### Map the model predictions ####
dev.new()
png(filename="/results/risk-map-first-day.png")

# first plot the negative predicted value to indicate high risk in heat colours
first.day.kriging$predict.neg <- -first.day.kriging$predict
image(first.day.kriging, krige.grid, col = heat.colors(10))

# second plot the predictive value of the epidemic first day of cholera by town
first.day.kriging$pred <- first.day.kriging$predict
contour(first.day.kriging, labcex=1.2, method = "edge", ad=T)

title(main=NULL, sub=map.title)
