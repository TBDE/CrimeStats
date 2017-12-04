## for (i in 1:10){load(file.path("..","Data",sprintf("finalIterativeSolution-%02d.rdat",i))); par(mfrow=c(1,3));  plotCrimeData(crimeData, precinctData); boxPlots(crimeData, precinctData); closureRate(collectionOfDistances); round(distanceBetweenPrecincts  (crimeData, precinctData), 0); readline("Press Enter for next plot")}
## load(file.path("..","Data","finalIterativeSolution-04.rdat")); par(mfrow=c(1,3)); geoPlotCrimeData (crimeData, truncatePrecinctData(precinctData)); plotCrimeData(crimeData, precinctData); boxPlots(crimeData, precinctData); closureRate(collectionOfDistances); round(distanceBetweenPrecincts  (crimeData, precinctData), 0)

rm(list=ls())

source("library.R")

addMissingPricinct <- function(precinctData, crimeData, kiosks)
{
    verbose <- FALSE
    precintsUsed <- unique(crimeData$assignedPrecinct)

    dumpObject(precintsUsed, verbose=verbose)
    greatestSD <- -1

    missingPrecinct <- -1
    worstPrecinct <- -1

    for (p in 1:kiosks)
    {
        if ( p %in% precintsUsed)
        {
            rows <- which(crimeData$assignedPrecinct == p)
            dumpObject(rows, verbose=verbose, comment=sprintf("Crime row indices assigned to precinct %d", p))

            temp <- ifelse(length(rows) == 1, 0, sd(crimeData$dist[rows]))
            dumpObject(temp, verbose=verbose, comment=sprintf("Standard deviation of distances of crimes assigned to precinct %d",p))

            if (temp > greatestSD)
            {
                greatestSD <- temp
                worstPrecinct <- p
            }
        }
        else
        {
            missingPrecinct <- p
        }
    }

    if (precinctData$fixed[missingPrecinct] == FALSE)
    {
        furthestDistance <- max(crimeData$dist[which(crimeData$assignedPrecinct == worstPrecinct)])[1]
        row <- crimeData[which((crimeData$assignedPrecinct == worstPrecinct) & (crimeData$distance == furthestDistance)),][1,]
        precinctData$lat[missingPrecinct] <- (precinctData$lat[worstPrecinct]  + row$lat)/2
        precinctData$lon[missingPrecinct] <- (precinctData$lon[worstPrecinct]  + row$lon)/2
        print(sprintf("Moving unused precinct %d to (%f, %f) to help precinct %d",
                      missingPrecinct,
                      precinctData$lat[missingPrecinct],
                      precinctData$lon[missingPrecinct],
                      worstPrecinct
                      ))
    }

    precinctData
}


setValueBasedOnCommandLine <- function(input, default)
{
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

    returnValue <- setValue(input, default)
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

    crimeData$color <- setCrimePrecinctColor(crimeData)

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
                ## precinctData$lat[i] = mean(crimeData$lat[rows])
                ## precinctData$lon[i] = mean(crimeData$lon[rows])
                precinctData$lat[i] = median(crimeData$lat[rows])
                precinctData$lon[i] = median(crimeData$lon[rows])
            }
        }
        else
        {
            print(sprintf("Location of precinct %d is fixed and will not be moved.", i))
        }
    }
    dumpObject(precinctData, comment="After changing location.")

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




main <- function(liveData=TRUE, numberOfKiosks=5)
{
    verbose <- FALSE

    plotIntermediateData <- FALSE

    maxIterations <- 100

    cityNumber <- 1
    
    spec <- matrix(c(
        'verbose', 'v', 0, 'logical',
        'liveData', 'l', 0, 'logical',
        'plotIntermediateData', 'p', 0, 'logical',
        'maxIterations', 'm', 1, 'double',
        'numberOfKiosks', 'n', 1, 'double',
        'cityNumber', 'c', 1, 'double'
    ),
    ncol = 4, byrow = TRUE)

    opt <- getopt(spec)

    verbose <- setValueBasedOnCommandLine(opt$verbose, verbose)
    plotIntermediateData <- setValueBasedOnCommandLine(opt$plotIntermediateData, plotIntermediateData)
    maxIterations <- setValueBasedOnCommandLine(opt$maxIterations, maxIterations)
    liveData <- setValueBasedOnCommandLine(opt$liveData, liveData)
    numberOfKiosks <- setValueBasedOnCommandLine(opt$numberOfKiosks, numberOfKiosks)
    cityNumber <- setValueBasedOnCommandLine(opt$cityNumber, cityNumber)

    savedCrimeData <- file.path("..", "Data", "savedCrimeData.rdat")

    if(liveData == TRUE)
    {
        precinctData <- initPrecincitData(numberOfKiosks, cityNumber=cityNumber)
        crimeData <- initCrimeData(useExisting=TRUE, savedFile=savedCrimeData, cityNumber=cityNumber)
        ## crimeData <- crimeData[1:1000,]
    }
    else
    {
        set.seed(123)
        precinctData <- testPrecincitData()
        crimeData <- testCrimeData(type=2)
    }


    crimeData <- assignCrimesToPrecincits(crimeData, 1)


    for (kiosks in 1:nrow(precinctData))
    {
        savedFinalSolution <- file.path("..", "Data", sprintf("finalIterativeSolution-%02d.rdat", kiosks))

        oldPar <- par()
        ## par(ask=TRUE)

        dumpObject(nrow(crimeData), verbose=verbose)
        dumpObject(head(crimeData), verbose=verbose, comment="Initial crime data")

        if(plotIntermediateData == TRUE)
        {
            ## plotPrecinctCrimeData(precinctData, crimeData, title="Initial condition")
            plotCrimeData(crimeData, precinctData, title="Initial condition")
        }


        dumpObject(head(precinctData), verbose=verbose, comment="Initial precinct locations")
        precinctData <- updatePrecincitLocations(precinctData, crimeData)
        dumpObject(head(precinctData), verbose=verbose, comment="Updated precinct locations")

        iterationCounter <- 0
        doWork <- TRUE

        numberOfPrecincts <- kiosks
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
                ## plotPrecinctCrimeData(precinctData, crimeData, title=title)
                plotCrimeData(crimeData, precinctData, title=title)
            }

            print(title)

            for (i in 1:numberOfCrimes)
            {
                dumpObject(i, comment="Top of the outer loop.", verbose=verbose)

                crimeData$distance[i] <- .Machine$double.xmax

                origPrecinct <- crimeData$assignedPrecinct[i]

                for (j in 1:numberOfPrecincts)
                {
                    dumpObject(j, verbose=verbose)

                    dist <- distance(precinctData$lat[j],
                                     precinctData$lon[j],
                                     crimeData$lat[i],
                                     crimeData$lon[i])

                    dumpObject(dist, verbose=verbose)

                    if (dist < crimeData$distance[i])
                    {
                            if (j != crimeData$assignedPrecinct[i])
                            {
                                ## doWork <- TRUE
                                if(plotIntermediateData == TRUE)
                                {
                                    print(sprintf("Changing precinct assignment of crime %d to precinct %d", i, j))
                                }
                            }
                        crimeData$distance[i] <- dist
                        crimeData$assignedPrecinct[i] <- j
                    }
                }

                if (origPrecinct != crimeData$assignedPrecinct[i])
                {
                    doWork <- TRUE
                }
            }

            sumDistances <- sum(crimeData$dist)
            collectionOfDistances <- c(collectionOfDistances, sumDistances)
            numberOfPrecintsUsed <- length(unique(crimeData$assignedPrecinct))

            dumpObject(abs(sumDistances- sumDistancesOld), comment="Difference between current distances and previous distances.")
            dumpObject(numberOfPrecintsUsed, comment="Number of used precincts (should hover around number of kiosks)")
            dumpObject(kiosks, comment="how many kiosks (or precincts) the system is trying to fit to the data")

            if ((sumDistances != sumDistancesOld) || (numberOfPrecintsUsed != kiosks))
            {
                doWork <- TRUE
                sumDistancesOld <- sumDistances
                if (numberOfPrecintsUsed != kiosks)
                {
                    precinctData <- addMissingPricinct(precinctData, crimeData, kiosks)
                }
            }

            if (doWork == TRUE)
            {
                precinctData <- updatePrecincitLocations(precinctData, crimeData)

            }
        }

        save(precinctData, crimeData, collectionOfDistances, file=savedFinalSolution)
    }

    par(mfrow=c(1,3))
    plotCrimeData(crimeData, precinctData)
    boxPlots(crimeData, precinctData)
    closureRate(collectionOfDistances)

    dumpObject(round(distanceBetweenPrecincts  (crimeData, precinctData), 0), comment="Distances between precincts")
    dumpObject(precinctData)
    readline("Press return to see the satellite image solution.")
    par(oldPar)

    geoPlotCrimeData (crimeData, precinctData)

    print(sprintf("Final solution data has been saved in %s", savedFinalSolution))

    print("The program has ended.")
}

system.time(main())
