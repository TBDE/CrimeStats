rm(list=ls())

source("library.R")

library(dendroextras)
library(datasets)
library(fields)

library(ff)

testing3 <- function(k=5)
{

    
}
    

testing2 <- function(k=5)
{
    crimeData <- initCrimeData(TRUE, "/tmp/temp.rData", 1)

    crimeData <- crimeData[1:10,]

    dumpObject(crimeData[1:3,])
    coors <- data.frame(crimeData$lon,crimeData$lat)

    rm(crimeData)
                                        #distance matrix
    dist.in.km.matrix <- rdist.earth(coors,miles = F,R=6371)

                                        #clustering
    fit <- hclust(as.dist(dist.in.km.matrix), method = "single")

    clusters <- cutree(fit,k=k)

    plot(coors, col = clusters, pch = 20)
}

testing <- function(k=2)
{
    ## https://stackoverflow.com/questions/28672399/spatial-clustering-in-r-simple-example    
    lon = c(31.621785, 31.641773, 31.617269, 31.583895, 31.603284)
    lat = c(30.901118, 31.245008, 31.163886, 30.25058, 30.262378)
    threshold.in.km <- 40
    coors <- data.frame(lon,lat)

                                        #distance matrix
    dist.in.km.matrix <- rdist.earth(coors,miles = F,R=6371)

                                        #clustering
    fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
    ## clusters <- cutree(fit,h = threshold.in.km)
    clusters <- cutree(fit,k=k)

    plot(lon, lat, col = clusters, pch = 20)
}

main <- function()
{
    data(USArrests)

    par(mar = c(2,10,2,10), cex = 0.6)
    clst1=colour_clusters(hclust(dist(USArrests), "ave"),5,groupLabels=as.roman)
    plot(clst1, main = "Dendrogram with 5 clusters", horiz = TRUE)
}

main()
