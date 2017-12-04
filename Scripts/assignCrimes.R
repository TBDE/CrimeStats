## load("../Data/finalSolution-04.rdat"); par(mfrow=c(1,3)); geoPlotCrimeData (crimeData, precinctData); plotCrimeData(crimeData, precinctData); boxPlots(crimeData, precinctData); closureRate(collectionOfDistances); round(distanceBetweenPrecincts  (crimeData, precinctData), 0)

rm(list=ls())

library(methods)
library(sp)
library(utils)
library(RColorBrewer)
library(NISTunits)
library(geosphere)

library(getopt)
library(jpeg)
library(ggmap)

distanceBetweenPrecincts <- function(crimeData, precinctData)
{
    assigned <- unique(crimeData$assignedPrecinct)

    count <- nrow(precinctData)

    distances <- matrix(rep(NA, count * count), nrow=count, ncol=count)

    for (i in 1:count)
    {
        if ((i %in% assigned) == TRUE)
        {
            if (i > 1)
            {
                for (j in 1:(i-1))
                {
                    if ((j %in% assigned) == TRUE)
                    {
                        
                        temp <- distance (precinctData$lat[i], precinctData$lon[i], precinctData$lat[j], precinctData$lon[j], type=4)

                        distances[i,j] <- distances[j,i] <- temp
                    }
                }
            }
        }
    }

    distances
}

getBaseMap <- function(lats, lons, useSaveExisting, saveFileName, zoom)
{
    doWork <- TRUE
    if (useSaveExisting == TRUE)
    {
        if (file.exists(saveFileName) == TRUE)
        {
            load(saveFileName)
            doWork <- FALSE
        }
    }

    if (doWork == TRUE)
    {
        map <- get_map(location = c(lon = mean(lons), lat = mean(lats)),
                       zoom = zoom,
                       maptype = "satellite",
                       source = "google")
        
        if (useSaveExisting == TRUE)
        {
            save(map, file=saveFileName)
        }
    }
    map
}

geoPlotCrimeData <- function(crimeData, precinctData)
{
    ## lon <- c(11.33, 11.36)       
    ## lat <- c(44.49, 44.5)
    lons <- range(crimeData$lon, precinctData$lon)
    lats <- range(crimeData$lat, precinctData$lat)

    zoom <- 10
    cols <- getColors()

    crimeData$color <- with(crimeData, cols[assignedPrecinct])
     
    ## map <- get_map(location = c(lon = mean(lon), lat = mean(lat)),
    ##                zoom = 11,
    ##                maptype = "satellite",
    ##                source = "google")

    saveFileName <- file.path("..", "Data", sprintf("%f-%f-%f-%f-%f", lons[1], lats[1], lons[2], lats[2], zoom))

    
    
    map <- getBaseMap (lats, lons, useSaveExisting=TRUE, saveFileName, zoom)
    
    foo <- ggmap(map) +
        scale_x_continuous(limits = lons, expand = c(0, 0)) +
        scale_y_continuous(limits = lats, expand = c(0, 0)) +
        geom_point(data=crimeData[,c("lon", "lat")], col=crimeData$color)

    assigned <- unique(crimeData$assignedPrecinct)

    for (i in 1:nrow(precinctData))
    {
        if ((i %in% assigned) == TRUE)
        {
            number <- ifelse(precinctData$fixed[i] == TRUE, 15, 17)

            foo <- foo + geom_point(data=precinctData[i,c("lon", "lat")], col="white", pch=number, cex=4)
        }
    }

    
    print(foo)
    ## plot(crimeData$lon, crimeData$lat, add=TRUE)## , col=crimeData$color,
}


plotCrimeData <- function(crimeData, precinctData)
{
    cols <- getColors()

    crimeData$color <- with(crimeData, cols[assignedPrecinct])

    print(unique(crimeData$color))
    
    plot(crimeData$lon, crimeData$lat, col=crimeData$color,
         xlab="Longitude",
         ylab="Latitude")

    assigned <- unique(crimeData$assignedPrecinct)

    size <- 0.5/60

    imageLocationM <- file.path("..", "Images", "policeBadge02.jpg")
    ## imgMobile <- readJPEG(imageLocation)

    imageLocationF <- file.path("..", "Images", "policeBadge01.jpg")
    ## imgFixed <- readJPEG(imageLocation)

    for (i in 1:nrow(precinctData))
    {
        if ((i %in% assigned) == TRUE)
        {
            imageLocation <- ifelse (precinctData$fixed[i] == TRUE, imageLocationF, imageLocationM)

            img <- readJPEG(imageLocation)

            rasterImage(img,
                        precinctData$lon[i] + size,
                        precinctData$lat[i] - size,
                        precinctData$lon[i] - size,
                        precinctData$lat[i] + size,
                        )
        }
    }
}

setValueBasedOnCommandLine <- function(input, default)
  {
    returnValue <- setValue(input, default)
  }

setValue <- function(inputValue, default)
{
  returnValue <- default
  if (typeof(default) == 'logical')
    {
      if (is.null(inputValue) == FALSE)
        {
          returnValue <- ! default          
        }
    }
  else
    {
      if (is.null(inputValue) == FALSE) {
        returnValue <- inputValue
      }
    }
  returnValue
}

getColors <- function(numberNeeded=12, fixed=TRUE)
{
    if (fixed == TRUE)
    {
        cols <- c("red", "green",
                  "blue", "black",
                  "purple", "brown",
                  "coral", "cyan",
                  "pink", "deeppink",
                  "turquoise", "greenyellow"
                  )
    }
    else
    {
        
        base <- 25
        cols <- colors()[base:(base+numberNeeded)]
    }

    cols <- c(cols, cols, cols)
    
    ## dumpObject(cols)

    cols
}

closureRate <- function(collectionOfDistances)
{
    rate <- c()

    for (i in 2:length(collectionOfDistances))
    {
        rate <- c(rate, ((collectionOfDistances[i-1]-collectionOfDistances[i])/collectionOfDistances[i]))
    }


    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    plot(collectionOfDistances,
         xlab="Precinct movement refinement",
         ylab="Total sum of miles to crimes",
         ylim=c(min(pretty(range(collectionOfDistances))), max(pretty(range(collectionOfDistances))))
         )

    par(new = TRUE)
    cols <- "red"
    plot(rate*100, type = "l", axes = FALSE,
         bty = "n", xlab = "", ylab = "",
         col=cols,
         ## log="y",
         )
    axis(side=4, at = pretty(range(rate*100)))
    mtext("Percent of change", side=4, line=3, col=cols)
    rate
}


boxPlots <- function(crimeData, precinctData)
{
    cols <- getColors()

    crimeData$color <- with(crimeData, cols[assignedPrecinct])

    dumpObject((table(crimeData$assignedPrecinct)), comment="Number of crimes assigned to each precinct")

    cols <- cols[as.numeric(names(table(crimeData$assignedPrecinct)))]
    
    dumpObject(unique(crimeData$color))
    dumpObject(cols)
    
    
    bp <- boxplot(crimeData$distance ~ crimeData$assignedPrecinct,
            col=cols,
            main="Boxplot of precincts and distance to crimes",
            xlab="Precinct number",
            ylab="Miles to crime")

    ## print(bp)
    usrData <- par("usr")
    for (i in 1:nrow(precinctData))
    {
        if (precinctData$fixed[i] == "TRUE")
        {
            rect(i-0.5, usrData[3], i+0.5, usrData[4], col = "gray")
        }
    }

    boxplot(crimeData$distance ~ crimeData$assignedPrecinct,
            col=cols,
            add=TRUE)

    for (i in 1:length(bp$names))
    {
        m <- median(crimeData$distance[which(crimeData$assignedPrecinct == as.numeric(bp$names[i]))])
        points(i, m, col="white", pch=16)
    }
}


testPrecincitData <- function()
{

    lats <- c( 31, 31.5, 32, 32.5)
    lons <- c( -75, -76, -75, -74)
    fixed <- c( TRUE, FALSE, FALSE, FALSE)

    n <- length(fixed)

    returnValue <- data.frame(
        n=n,
        lat=lats,
        lon=lons,
        fixed=fixed
    )
    returnValue
}

testCrimeData <- function(type=1)
{
    numberOfCrimes <- 10
    precinctData <- testPrecincitData()

    rangeLats <- range(precinctData$lat)
    rangeLons <- range(precinctData$lon)

    switch(as.character(type),
           "1"={
               lats <-c(rep(rangeLats[1], numberOfCrimes/2), rep(rangeLats[2], numberOfCrimes/2))
               lons <- seq(from=rangeLons[1], to=rangeLons[2], length.out=numberOfCrimes)

           },
           "2"={
               lats <-runif(numberOfCrimes, min=rangeLats[1], max=rangeLats[2])
               lons <- runif(numberOfCrimes, min=rangeLons[1], max=rangeLons[2])
           }
           )

    returnValue <- data.frame(lat = lats,
                              lon = lons,
                              assignedPrecinct = rep(1, numberOfCrimes),
                              distance = rep(.Machine$double.xmax, numberOfCrimes)
                              )

    returnValue
}



plotPrecinctCrimeData <- function(precinctData, crimeData, title="")
{
    lats <- range(c(precinctData$lat, crimeData$lat))
    lons <- range(c(precinctData$lon, crimeData$lon))
    ## cols<-brewer.pal(n=nrow(precinctData),name="Set3")

    cols <- getColors()

    dumpObject(lats)
    dumpObject(lons)

    dumpObject(head(crimeData))
    plot(x=lons, y=lats, type="n",
         xlab="Latitude", ylab="Longitude",
         main=title)

    for (i in 1:length(precinctData$lon))
    {
        points(x=precinctData$lon[i], y=precinctData$lat[i], pch=2, col=cols[i])
    }


    
    print(unique(sort(crimeData$assignedPrecinct)))
    ## points(x=crimeData$lon, y=crimeData$lat, pch=1, col=precinctData$assignedPrecinct)
    ## for (i in 1:length(crimeData$lon))
    ## {
    ##     points(x=crimeData$lon[i], y=crimeData$lat[i], pch=1, col=cols[crimeData$assignedPrecinct[i]])
    ## }

    crimeData$color <- with(crimeData, cols[assignedPrecinct])
    
    ## for (i in 1:nrow(crimeData))
    ## {
    ##     crimeData$color[i] <- cols[crimeData$assignedPrecinct[i]]
    ## }
    ## print(head(crimeData))

    points(crimeData$lat ~ crimeData$lon, col=crimeData$color, pch=1)
}

updatePrecincitLocations <- function(precinctData, crimeData)
{
    numberOfPrecincts <- nrow(precinctData)

    crimesPerPrecinct <- rep(0,numberOfPrecincts)

    dumpObject(head(precinctData), comment="Before changing location.")
    for (i in 1:numberOfPrecincts)
    {
        rows <- which(crimeData$assignedPrecinct == i)
        crimesPerPrecinct[i] <- length(rows)
        print(sprintf("%d crimes are assigned to precinct %d", crimesPerPrecinct[i], i))
        if (precinctData$fixed[i] == FALSE)
        {
            if (length(rows) > 0)
            {
                print(sprintf("Changing the location of precinct %d", i))
                precinctData$lat[i] = mean(crimeData$lat[rows])
                precinctData$lon[i] = mean(crimeData$lon[rows])
            }
        }
        else
        {
            print(sprintf("Location of precinct %d is fixed and will not be moved.", i))
        }
    }
    dumpObject(head(precinctData), comment="After changing location.")

    dumpObject(crimesPerPrecinct, comment="Crimes per precinct")
    dumpObject(sum(crimesPerPrecinct), comment="Number of crimes assigned to precincts")
    dumpObject(nrow(crimeData), comment="Should equal total number per precinct")

    precinctData
}


assignCrimesToPrecincits <- function(crimeData, numberOfPrecincits)
{
    precincts <- 1:length(crimeData$assignedPrecinct)

    precincts <- (precincts %% numberOfPrecincits) + 1

    crimeData$assignedPrecinct <- precincts

    crimeData
}

distance <- function(lat1, lon1, lat2, lon2, type=4)
{
    returnValue <- NULL

    deltaLat <- lat1 - lat2
    deltaLon <- lon1 - lon2

    switch(as.character(type),
           "1"={
               returnValue <- sqrt(deltaLat * deltaLat + deltaLon * deltaLon)
           },
           "2"={
               nmPerDegree <- 60
               returnValue <- nmPerDegree * sqrt(deltaLat * deltaLat +
                                                 cos(NISTdegTOradian((lat1 + lat2)/2)) * deltaLon * deltaLon)
           },
           "3"={
               print("Made it here.")
               ## https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
               R <- 6371 # Earth mean radius [km]
               ## R <- 3959 # Earth mean radius [mi]
               lat1 <- NISTdegTOradian(lat1)
               lat2 <- NISTdegTOradian(lat2)
               lon1 <- NISTdegTOradian(lon1)
               lon2 <- NISTdegTOradian(lon2)
               returnValue <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(lon2-lon1)) * R
           },
           "4"={
               returnValue <- distCosine(c(lon1, lat1), c(lon2, lat2), r=6371)#, r=3959)
           }

           )

    returnValue
}

dumpObject <- function(object, comment=" ", verbose=TRUE)
{
    if(verbose == TRUE)
    {
        print(sprintf("%s  -- Dumping the object: %s (of type: %s, class: %s)",
                      comment,
                      deparse(substitute(object)),
                      typeof(object),
                      class(object)
                      )
              )

        print(object)
    }
}

initPrecincitData <- function(numberOfKiosks=7)
{
    returnValue <- data.frame(
        n=as.numeric(),
        lat=as.numeric(),
        lon=as.numeric(),
        fixed=as.logical()
    )

    ## https://www.vbgov.com/government/departments/police/opsdiv/Pages/pct.aspx
    ## https://www.vbgov.com/government/departments/police/opsdiv/Documents/precincts.pdf
    ## https://stevemorse.org/jcal/latlon.php

    ## First precinct:
    counter <- 1
    temp <- data.frame(n=counter, lat=36.7536474, lon=-76.05830879999996, fixed=TRUE)
    returnValue <- rbind(returnValue, temp)

    ## Second precinct:
    counter <- counter + 1
    temp <- data.frame(n=counter, lat=36.843648, lon=-75.98615860000001, fixed=TRUE)
    returnValue <- rbind(returnValue, temp)

    ## Third precinct
    counter <- counter + 1
    temp <- data.frame(n=counter, lat=36.8702725, lon=-76.13274760000002, fixed=TRUE)
    returnValue <- rbind(returnValue, temp)

    ## Fourth precinct
    counter <- counter + 1
    temp <- data.frame(n=counter, lat=36.8433381, lon=-76.02455520000001, fixed=TRUE)
    returnValue <- rbind(returnValue, temp)

    ## counter <- counter + 1
    ## temp <- data.frame(n=counter, lat=37.5, lon=-66.5, fixed=FALSE)
    ## returnValue <- rbind(returnValue, temp)

    if (numberOfKiosks > 0)
    {
        for(i in 1:numberOfKiosks)
        {
            counter <- counter + 1
            temp <- data.frame(n=counter, lat=0, lon=0, fixed=FALSE)
            returnValue <- rbind(returnValue, temp)
        }
    }
    returnValue
}

initCrimeData <- function(useExisting=TRUE, savedFile=tempfile())
{
    dataFile <- file.path("..", "Data", "Police_Incident_Reports.csv")

    northernLat <- 37.5
    southernLat <- 36
    westernLon <- -76.5
    easternLon <- -75

    doWork <- TRUE

    if (useExisting == TRUE)
    {
        if (file.exists(savedFile) == TRUE)
        {
            load(savedFile)
            doWork <- FALSE
        }
    }

    if (doWork == TRUE)
    {
        d <- readLines(dataFile)
        numberOfRecords <- length(d)
        ## numberOfRecords <- 100
        dumpObject(numberOfRecords, comment="Number of crime related data records")

        lats <- c()
        lons <- c()
        for (i in seq(4, numberOfRecords, by=3))
        {
            ## temp <- unlist(regmatches(d[i], gregexpr('\\(?[0-9,.]+', d[i])))
            temp <- as.numeric(unlist(regmatches(d[i], gregexpr('-?[0-9.]+', d[i]))))
            if ((is.na(temp[1]) == FALSE) && (is.na(temp[2]) == FALSE))
            {
                if ((southernLat <= temp[1]) && (temp[1] <= northernLat) &&
                    (westernLon <= temp[2] && (temp[2] <= easternLon))
                    )
                {
                    lats <- c(lats, temp[1])
                    lons <- c(lons, temp[2])
                }
            }
        }

        returnValue <- data.frame(lat=lats, lon=lons,
                                  assignedPrecinct = rep(1, length(lats)),
                                  distance = rep(.Machine$double.xmax, length(lats))
                                  )

        save(returnValue, file=savedFile)
    }

    dumpObject(nrow(returnValue), comment="Number of crime locations")
    returnValue
}


main <- function(liveData=TRUE, numberOfKiosks=5)
{
    verbose <- FALSE

    plotIntermediateData <- FALSE

    maxIterations <- 100

    spec <- matrix(c(
        'verbose', 'v', 0, 'logical',
        'liveData', 'l', 0, 'logical',
        'plotIntermediateData', 'p', 0, 'logical',
        'maxIterations', 'm', 1, 'double',
        'numberOfKiosks', 'n', 1, 'double'
    ),
    ncol = 4, byrow = TRUE)

    opt <- getopt(spec)

    verbose <- setValueBasedOnCommandLine(opt$verbose, verbose)
    plotIntermediateData <- setValueBasedOnCommandLine(opt$plotIntermediateData, plotIntermediateData)
    maxIterations <- setValueBasedOnCommandLine(opt$maxIterations, maxIterations)
    liveData <- setValueBasedOnCommandLine(opt$liveData, liveData)
    numberOfKiosks <- setValueBasedOnCommandLine(opt$numberOfKiosks, numberOfKiosks)

    savedCrimeData <- file.path("..", "Data", "savedCrimeData.rdat")

    savedFinalSolution <- file.path("..", "Data", sprintf("finalSolution-%02d.rdat", numberOfKiosks))

    if(liveData == TRUE)
    {
        precinctData <- initPrecincitData(numberOfKiosks)
        crimeData <- initCrimeData(useExisting=TRUE, savedFile=savedCrimeData)
        ## crimeData <- crimeData[1:1000,]
    }
    else
    {
        set.seed(123)
        precinctData <- testPrecincitData()
        crimeData <- testCrimeData(type=2)
    }

    oldPar <- par()
    ## par(ask=TRUE)

    dumpObject(nrow(crimeData), verbose=verbose)
    dumpObject(head(crimeData), verbose=verbose, comment="Initial crime data")

    crimeData <- assignCrimesToPrecincits(crimeData, nrow(precinctData))

    if(plotIntermediateData == TRUE)
    {
        plotPrecinctCrimeData(precinctData, crimeData, title="Initial condition")
    }


    dumpObject(head(precinctData), verbose=verbose, comment="Initial precinct locations")
    precinctData <- updatePrecincitLocations(precinctData, crimeData)
    dumpObject(head(precinctData), verbose=verbose, comment="Updated precinct locations")

    iterationCounter <- 0
    doWork <- TRUE

    numberOfPrecincts <- nrow(precinctData)
    numberOfCrimes <- nrow(crimeData)

    sumDistances <- -1
    sumDistancesOld <- 0
    collectionOfDistances <- c()

    while((iterationCounter <= maxIterations) && (doWork == TRUE))
    {

        doWork <- FALSE
        iterationCounter <- iterationCounter + 1
        title <- sprintf("Movement loop %d of maximum %d.", iterationCounter, maxIterations)

        if(plotIntermediateData == TRUE)
        {
            plotPrecinctCrimeData(precinctData, crimeData, title=title)
        }

        print(title)

        for (i in 1:numberOfCrimes)
        {
            dumpObject(i, comment="Top of the outer loop.", verbose=verbose)

            ## crimeData$distance[i] <- .Machine$double.xmax

            for (j in 1:numberOfPrecincts)
            {
                dumpObject(j, verbose=verbose)

                dist <- distance(precinctData$lat[j],
                                 precinctData$lon[j],
                                 crimeData$lat[i],
                                 crimeData$lon[i])

                dumpObject(dist, verbose=verbose)

                ## if (i == 6){
                ##     dumpObject(dist, comment=sprintf("Tracking dist for crime %d",i))
                ## }

                if (dist < crimeData$distance[i])
                {
                    if(plotIntermediateData == TRUE)
                    {
                        print(sprintf("Changing precinct assignment of crime %d to precinct %d", i, j))
                    }
                    crimeData$distance[i] <- dist
                    crimeData$assignedPrecinct[i] <- j
                    ## doWork <- TRUE
                }
            }
        }

        sumDistances <- sum(crimeData$dist)
        collectionOfDistances <- c(collectionOfDistances, sumDistances)

        dumpObject(abs(sumDistances- sumDistancesOld), comment="Difference between current distances and previous distances.")

        if (sumDistances != sumDistancesOld)
        {
            doWork <- TRUE
            sumDistancesOld <- sumDistances
        }

        if (doWork == TRUE)
        {
            precinctData <- updatePrecincitLocations(precinctData, crimeData)
        }
    }

    dumpObject(precinctData)
    ## dumpObject(crimeData)
    par(mfrow=c(1,2))
    plotPrecinctCrimeData(precinctData, crimeData, title="Final solution")
    boxPlots(crimeData, precinctData)

    par(oldPar)

    geoPlotCrimeData (crimeData, precinctData)
    
    save(precinctData, crimeData, collectionOfDistances, file=savedFinalSolution)
    print(sprintf("Final solution data has been saved in %s", savedFinalSolution))

    print("The program has ended.")
}

system.time(main())
