#Calculate the area of map polygons
require(rgdal)
SacPop <- readOGR("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/CityData","Sac_2010CensusBlkPop",stringsAsFactors=F)
require(rgeos)
Area <- gArea(SacPop, byid=T) ## For subgeometries
Area <- gArea(SacPop) ## For entire object (byid=F)

#Merge polygons (dissolve)
require(rgeos)
SacOutline <- gUnaryUnion(SacPop)

##Intersection of two features
require(rgdal)
KinderVacc <- readOGR("../SchoolVaccination/Data", "KinderData1314", stringsAsFactors=F)
KinderVacc <- spTransform(KinderVacc, CRS=CRS(proj4string(SacPop)))
testover = over(KinderVacc, as(SacPop, "SpatialPolygons"))
KinderVaccSac <- KinderVacc[!is.na(testover),]
##OR (much quicker!)
KinderVaccSac2 <- KinderVacc[SacPop,]

##Aggregating attributes of one feature in another
testagg <- aggregate(KinderVacc[c("Total","PBETot")], SacPop, sum)
testagg$PBEPct <- testagg$PBETot / testagg$Total
testint <- classIntervals(testagg$PBEPct, n=4, style="fixed", fixedBreaks=c(0,.001,.03,.1,.325))
require(RColorBrewer)
pal <- brewer.pal(4, "Blues")
plot(testint, pal=pal)
aggColors <- findColours(testint,pal)
plot(testagg,col=aggColors, lwd=0.1, border="white")

##Sampling points
require(sp)
set.seed(1234)
randSamp <- spsample(SacPop, 1000, type="random")  #Random sample of points
plot(randSamp)
length(randSamp)
regSamp <- spsample(SacPop, 1000, type="regular")  #Grid sample of points
plot(regSamp)
length(regSamp)
##Type="stratified" is random point in each cell, "nonaligned" is similar

#Interesting functions in maptools()
#gzAzimuth - find direction to another point
#gcDestination - find coordinates of another point given direction and distance
#sunriset - find sunrise and sunset times for a given point