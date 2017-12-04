## load(file.path("..","Data","finalIterativeSolution-04.rdat")); par(mfrow=c(1,3)); geoPlotCrimeData (crimeData, truncatePrecinctData(precinctData)); plotCrimeData(crimeData, precinctData); boxPlots(crimeData, precinctData); closureRate(collectionOfDistances); round(distanceBetweenPrecincts  (crimeData, precinctData), 0)

rm(list=ls())

source("library.R")

plotMovement <- function()
{
    lat <- c()
    lon <- c()
lat <- c(lat, 36.65539); lon <- c(lon, -75.99312 )
lat <- c(lat, 36.65539); lon <- c(lon, -75.99312 )
lat <- c(lat, 36.66765); lon <- c(lon, -76.00551 )
lat <- c(lat, 36.68882); lon <- c(lon, -76.00411 )
lat <- c(lat, 36.70244); lon <- c(lon, -75.99374 )
lat <- c(lat, 36.71729); lon <- c(lon, -75.99125 )
lat <- c(lat, 36.74751); lon <- c(lon, -75.99114 )
lat <- c(lat, 36.76673); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77269); lon <- c(lon, -75.99986 )
lat <- c(lat, 36.77378); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77414); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77451); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77432); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77419); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77418); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77416); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77417); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77419); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77432); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77440); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77451); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.77505); lon <- c(lon, -75.99967 )
lat <- c(lat, 36.77595); lon <- c(lon, -76.00079 )
lat <- c(lat, 36.77687); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77669); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77660); lon <- c(lon, -76.00159 )
lat <- c(lat, 36.77669); lon <- c(lon, -76.00155 )
lat <- c(lat, 36.77669); lon <- c(lon, -76.00139 )
lat <- c(lat, 36.77716); lon <- c(lon, -76.00139 )
lat <- c(lat, 36.77832); lon <- c(lon, -76.00136 )
lat <- c(lat, 36.78088); lon <- c(lon, -75.99921 )
lat <- c(lat, 36.78574); lon <- c(lon, -75.99786 )
lat <- c(lat, 36.78787); lon <- c(lon, -75.99618 )
lat <- c(lat, 36.79044); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79138); lon <- c(lon, -75.99581 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79158); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79161); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99599 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )
lat <- c(lat, 36.79179); lon <- c(lon, -75.99532 )

    fileName <- "../Images/precinctMovement.png"
    png(filename=fileName, type="cairo")
    
    plot(lon, lat, type="b",
         ylim=c(36.65, 36.9),
         xlim=c(-76.2, -75.9),
         pch=16,
         ylab="Latitude",
         xlab="Longitude"
         )

    precinctData <- initPrecincitData()[1:4,]
    points(precinctData$lon, precinctData$lat, col="red", pch=15)
    dev.off()
    
    
    print(precinctData)
}


main <- function()
{
    files <- system("ls ../Data/finalIterativeSolution-*.rdat", intern=TRUE)

    counter <- 0
    totalDistances <- c()
    colors <- c()
    for (f in files)
    {
        print(sprintf("Processing %s", f))

        counter <- counter + 1
        load(f)
        totalDistances <- c(totalDistances, collectionOfDistances)
        colors <- c(colors, rep(counter, length(collectionOfDistances)))
        if (FALSE)
        {
            fileName <- sprintf("../Images/satellite-%02d.png", counter)
            png(filename=fileName, type="cairo")
            geoPlotCrimeData (crimeData, truncatePrecinctData(precinctData))
            dev.off()

            fileName <- sprintf("../Images/geoplot-%02d.png", counter)
            png(filename=fileName, type="cairo")
            plotCrimeData(crimeData, precinctData)
            dev.off()

            fileName <- sprintf("../Images/boxplot-%02d.png", counter)
            png(filename=fileName, type="cairo")
            boxPlots(crimeData, precinctData)
            dev.off()

            fileName <- sprintf("../Images/closure-%02d.png", counter)
            png(filename=fileName, type="cairo")
            closureRate(collectionOfDistances)
            dev.off()
        }
    }

    fileName <- "../Images/systemColsure.png"
    png(filename=fileName, type="cairo")
    plot(totalDistances, col=colors,
         type="b",
         ylab="Sum of distances between crimes and precincts",
         xlab="Solution iteration"
         )
    dev.off()
}

main()
