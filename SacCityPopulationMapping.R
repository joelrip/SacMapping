##Sacramento City population mapping by block 2010

##Read in shapefile
require(rgdal)
SacPop <- readOGR("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/CityData","Sac_2010CensusBlkPop",stringsAsFactors=F)

require(classInt)

##Calculate and map percent black (exported as PNG 2400 pixels wide)
SacPop$blackPct <- SacPop$BLACK / SacPop$TOTAL
b5 <- classIntervals(SacPop$blackPct, n=5, style="jenks")
pal <- brewer.pal(3, "Blues")
plot(b5, pal=pal)
b5Colours <- findColours(b5,pal)
plot(SacPop,col=b5Colours, lwd=0.1, border="white")
title("Percent Black")
labs <- c("< 6.7%", "6.7% - 18.1%", "18.1% - 32.9%", "32.9% - 58.8%", "> 58.8%")
legend("left", fill = attr(b5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Hispanic (exported as PNG 2400 pixels wide)
SacPop$hispPct <- SacPop$HISPANIC / SacPop$TOTAL
h5 <- classIntervals(SacPop$hispPct, n=5, style="jenks")
pal <- brewer.pal(3, "Greens")
plot(h5, pal=pal)
h5Colours <- findColours(h5,pal)
plot(SacPop,col=h5Colours, lwd=0.1, border="white")
title("Percent Hispanic")
labs <- c("< 11.9%", "11.9% - 25.6%", "25.6% - 41.3%", "41.3% - 61.1%", "> 61.1%")
legend("left", fill = attr(h5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Asian (exported as PNG 2400 pixels wide)
SacPop$asianPct <- SacPop$ASIAN / SacPop$TOTAL
a5 <- classIntervals(SacPop$asianPct, n=5, style="jenks")
pal <- brewer.pal(3, "Reds")
plot(a5, pal=pal)
a5Colours <- findColours(a5,pal)
plot(SacPop,col=a5Colours, lwd=0.1, border="white")
title("Percent Asian")
labs <- c("< 7.5%", "7.5% - 19.7%", "19.7% - 34.8%", "34.8% - 54.5%", "> 54.5%")
legend("left", fill = attr(a5Colours,"palette"), legend=labs, bty="n")

##Plot desktop wallpaper with gray background (exported as PNG 1920 pixels wide)
par(mfrow=c(1,3))
par(mar=c(0,0,0,0))
plot(SacPop,col=b5Colours, lwd=0.1, border="white", bg="gray30")
plot(SacPop,col=a5Colours, lwd=0.1, border="white", bg="gray30")
plot(SacPop,col=h5Colours, lwd=0.1, border="white", bg="gray30")

##Population density calculation
require(rgeos)
require(RColorBrewer)
require(classInt)
SacPop$area <- gArea(SacPop, byid=T)
SacPop$dens <- SacPop$TOTAL / (SacPop$area * 3.58701e-8)
dens10 <- classIntervals(SacPop$dens, n=10, style="jenks")
pal <- brewer.pal(8, "Reds")
plot(dens10, pal=pal)
dens10Colors <- findColours(dens10,pal)
plot(SacPop,col=dens10Colors, lwd=0.1, border="white")
title("Population Density per Square Mile")
labs <- c("< 2,651", "< 6,603", "< 9,865", "< 13,921", "< 20,305", "< 32,287", "< 57,690", "< 136,701", "< 249,440", "> 249,440")
legend("bottomleft", fill = attr(dens10Colors,"palette"), legend=labs, bty="n")
