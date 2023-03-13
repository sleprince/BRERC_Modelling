setwd("C:/BRERC")
utm <- read.csv(file="BRERC.csv", header=TRUE, sep=",")
library(rgdal)

utm <- utm[complete.cases(utm),]
utm1 <- data.frame(y=utm$north,x=utm$east) 
coordinates(utm1) <- ~x+y 
class(utm1)
proj4string(utm1) <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +ellps=WGS84") 
brerc2 <- spTransform(utm1,CRS("+proj=longlat +datum=WGS84"))
head(brerc2)

xyData <- as.data.frame(sp::coordinates(brerc2))

utm$x = xyData$x
utm$y = xyData$y

write.csv(utm, "brercXY.csv")