##Mapping population race information by block group in SACOG region

##Read in and clean Census data
BGData <- read.csv("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/CensusData/DEC_10_SF1_QTP4/DEC_10_SF1_QTP4.csv", stringsAsFactors=F)
names(BGData)[2] <- "GEOID2"
names(BGData)[4] <- "TOTAL"
names(BGData)[6] <- "TOTNONH"
names(BGData)[14] <- "WHITE"
names(BGData)[18] <- "BLACK"
names(BGData)[26] <- "ASIAN"
names(BGData)[30] <- "PI"
names(BGData)[22] <- "NATIVE"
names(BGData)[34] <- "OTHER"
names(BGData)[38] <- "MULTI"
BGData2 <- BGData[,c(2,4,6,14,18,22,26,30,34,38)]
BGData2$HISP <- BGData2$TOTAL - BGData2$TOTNONH
BGData2$ASIANPI <- BGData2$ASIAN + BGData2$PI
BGData2$OTHMULTI <- BGData2$OTHER + BGData2$MULTI
BGData2$WhitePct <- BGData2$WHITE / BGData2$TOTAL
BGData2$BlackPct <- BGData2$BLACK / BGData2$TOTAL
BGData2$NativPct <- BGData2$NATIVE / BGData2$TOTAL
BGData2$AsianPct <- BGData2$ASIANPI / BGData2$TOTAL
BGData2$HispPct <- BGData2$HISP / BGData2$TOTAL
BGData2$OtherPct <- BGData2$OTHMULTI / BGData2$TOTAL

##Read in SACOG block group shapefile, merge with Census data
require(sp)
require(rgdal)
BlockGroups <- readOGR("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/SACOGData", "BlockGroups2010")
BlockGroups$GEOID2 <- as.numeric(as.character(BlockGroups$GEOID10))
ID_D <- match(BlockGroups$GEOID2, BGData2$GEOID2)
BGData2 <- BGData2[ID_D,]
row.names(BGData2) <- row.names(BlockGroups)
BlockGroupsPop <- spCbind(BlockGroups, BGData2)

##Calculate Theil entropy (diversity) measure
##First calculate total metro entropy (not actually needed for local diversity measure,
##but can be used to create an entropy index that rates segregation/integration of the metro
##area as a whole, for comparison to other metro areas)
WhitePctTot <- sum(BlockGroupsPop$WHITE)/sum(BlockGroupsPop$TOTAL)
entW <- WhitePctTot*log(1/WhitePctTot)
BlackPctTot <- sum(BlockGroupsPop$BLACK)/sum(BlockGroupsPop$TOTAL)
entB <- BlackPctTot*log(1/BlackPctTot)
HispPctTot <- sum(BlockGroupsPop$HISP)/sum(BlockGroupsPop$TOTAL)
entH <- HispPctTot*log(1/HispPctTot)
AsianPctTot <- sum(BlockGroupsPop$ASIANPI)/sum(BlockGroupsPop$TOTAL)
entA <- AsianPctTot*log(1/AsianPctTot)
NativPctTot <- sum(BlockGroupsPop$NATIVE)/sum(BlockGroupsPop$TOTAL)
entN <- NativPctTot*log(1/NativPctTot)
OtherPctTot <- sum(BlockGroupsPop$OTHMULTI)/sum(BlockGroupsPop$TOTAL)
entO <- OtherPctTot*log(1/OtherPctTot)
MetroEntropy <- entW+entB+entH+entA+entN+entO
##Then calculate each block group's entropy
#BlockGroupsPop$Diversity <- BlockGroupsPop$WhitePct*log(1/BlockGroupsPop$WhitePct) +
#  BlockGroupsPop$BlackPct*log(1/BlockGroupsPop$BlackPct) + 
#  BlockGroupsPop$HispPct*log(1/BlockGroupsPop$HispPct) +
#  BlockGroupsPop$AsianPct*log(1/BlockGroupsPop$AsianPct) +
#  BlockGroupsPop$NativPct*log(1/BlockGroupsPop$NativPct) +
#  BlockGroupsPop$OtherPct*log(1/BlockGroupsPop$OtherPct)
##Include option to set group entropy to zero if none in block group, rather than NA
for(i in 1:nrow(BlockGroupsPop)) {
  if(BlockGroupsPop$WhitePct[i]==0 | is.na(BlockGroupsPop$WhitePct[i])) {
    entW <- 0 
  } else {
    entW <- BlockGroupsPop$WhitePct[i]*log(1/BlockGroupsPop$WhitePct[i])
  }
  if(BlockGroupsPop$BlackPct[i]==0 | is.na(BlockGroupsPop$BlackPct[i])) {
    entB <- 0 
  } else {
    entB <- BlockGroupsPop$BlackPct[i]*log(1/BlockGroupsPop$BlackPct[i])
  }
  if(BlockGroupsPop$HispPct[i]==0 | is.na(BlockGroupsPop$HispPct[i])) {
    entH <- 0 
  } else {
    entH <- BlockGroupsPop$HispPct[i]*log(1/BlockGroupsPop$HispPct[i])
  }
  if(BlockGroupsPop$AsianPct[i]==0 | is.na(BlockGroupsPop$AsianPct[i])) {
    entA <- 0 
  } else {
    entA <- BlockGroupsPop$AsianPct[i]*log(1/BlockGroupsPop$AsianPct[i])
  }
  if(BlockGroupsPop$NativPct[i]==0 | is.na(BlockGroupsPop$NativPct[i])) {
    entN <- 0 
  } else {
    entN <- BlockGroupsPop$NativPct[i]*log(1/BlockGroupsPop$NativPct[i])
  }
  if(BlockGroupsPop$OtherPct[i]==0 | is.na(BlockGroupsPop$OtherPct[i])) {
    entO <- 0 
  } else {
    entO <- BlockGroupsPop$OtherPct[i]*log(1/BlockGroupsPop$OtherPct[i])
  }
  BlockGroupsPop$Diversity[i] <- entW+entB+entH+entA+entN+entO
}

##Calculate and map diversity measure (exported as PNG 2400 pixels wide)
require(classInt)
require(RColorBrewer)
require(rgeos)
d5 <- classIntervals(BlockGroupsPop$Diversity, n=5, style="jenks")
pal <- brewer.pal(3, "BuPu")
plot(d5, pal=pal)
d5Colours <- findColours(d5,pal)
plot(BlockGroupsPop,col=d5Colours, lwd=0.5, border="white")
title("Relative Diversity")
labs <- c("Low Diversity", "", "", "", "High Diversity")
legend("bottomright", fill = attr(d5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent black (exported as PNG 2400 pixels wide)
b5 <- classIntervals(BlockGroupsPop$BlackPct, n=5, style="jenks")
pal <- brewer.pal(3, "Blues")
plot(b5, pal=pal)
b5Colours <- findColours(b5,pal)
plot(BlockGroupsPop,col=b5Colours, lwd=0.5, border="white")
title("Percent Black")
labs <- c("< 4.0%", "4.0% - 9.4%", "9.4% - 16.3%", "16.3% - 27.4%", "> 27.4%")
legend("bottomright", fill = attr(b5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Hispanic (exported as PNG 2400 pixels wide)
h5 <- classIntervals(BlockGroupsPop$HispPct, n=5, style="jenks")
pal <- brewer.pal(3, "Greens")
plot(h5, pal=pal)
h5Colours <- findColours(h5,pal)
plot(BlockGroupsPop,col=h5Colours, lwd=0.5, border="white")
title("Percent Hispanic")
labs <- c("< 12.9%", "12.9% - 21.9%", "21.9% - 33.1%", "33.1% - 47.5%", "> 47.5%")
legend("bottomright", fill = attr(h5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Asian/Pacific Islander (exported as PNG 2400 pixels wide)
a5 <- classIntervals(BlockGroupsPop$AsianPct, n=5, style="jenks")
pal <- brewer.pal(3, "Reds")
plot(a5, pal=pal)
a5Colours <- findColours(a5,pal)
plot(BlockGroupsPop,col=a5Colours, lwd=0.5, border="white")
title("Percent Asian/Pacific Islander")
labs <- c("< 6.7%", "6.7% - 14.5%", "14.5% - 24.2%", "24.2% - 36.2%", "> 36.2%")
legend("bottomright", fill = attr(a5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Native American (exported as PNG 2400 pixels wide)
n5 <- classIntervals(BlockGroupsPop$NativPct, n=5, style="jenks")
pal <- brewer.pal(3, "Oranges")
plot(n5, pal=pal)
n5Colours <- findColours(n5,pal)
plot(BlockGroupsPop,col=n5Colours, lwd=0.5, border="white")
title("Percent Native American")
labs <- c("< 0.5%", "0.5% - 1.0%", "1.0% - 2.0%", "2.0% - 4.2%", "> 4.2%")
legend("bottomright", fill = attr(n5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent Other/Multi Race (exported as PNG 2400 pixels wide)
o5 <- classIntervals(BlockGroupsPop$OtherPct, n=5, style="jenks")
pal <- brewer.pal(3, "Purples")
plot(o5, pal=pal)
o5Colours <- findColours(o5,pal)
plot(BlockGroupsPop,col=o5Colours, lwd=0.5, border="white")
title("Percent Other/Multi Race")
labs <- c("< 2.4%", "2.4% - 3.6%", "3.6% - 4.7%", "4.7% - 6.1%", "> 6.1%")
legend("bottomright", fill = attr(o5Colours,"palette"), legend=labs, bty="n")

##Calculate and map percent White (exported as PNG 2400 pixels wide)
w5 <- classIntervals(BlockGroupsPop$WhitePct, n=5, style="jenks")
pal <- brewer.pal(3, "Greys")
plot(w5, pal=pal)
w5Colours <- findColours(w5,pal)
plot(BlockGroupsPop,col=w5Colours, lwd=0.5, border="white")
title("Percent White")
labs <- c("< 27.3%", "27.3% - 45.1%", "45.1% - 60.9%", "60.9% - 75.8%", "> 75.8%")
legend("bottomright", fill = attr(w5Colours,"palette"), legend=labs, bty="n")
