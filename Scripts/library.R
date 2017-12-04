library(methods)
library(sp)
library(utils)
library(RColorBrewer)
library(NISTunits)
library(geosphere)

library(ggplot2)
library(getopt)
library(jpeg)
library(ggmap)

truncatePrecinctData <- function(precinctData)
{
    emptyRows <- which((precinctData$lat == 0) & (precinctData$lon == 0))

    returnValue <- precinctData

    if (length(emptyRows) > 0)
    {
        returnValue <- precinctData[-emptyRows, ]
    }
    returnValue
}

setCrimePrecinctColor <- function(crimeData)
{
    cols <- getColors()

    color <- with(crimeData, cols[assignedPrecinct])

    color
}

geoPlotCrimeData <- function(crimeData, precinctData, zoom = 10)
{
    ## lon <- c(11.33, 11.36)
    ## lat <- c(44.49, 44.5)
    lons <- range(crimeData$lon, precinctData$lon)
    lats <- range(crimeData$lat, precinctData$lat)

    crimeData$color <- setCrimePrecinctColor(crimeData)

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
}


plotCrimeData <- function(crimeData, precinctData, title="")
{
    cols <- getColors()

    crimeData$color <- setCrimePrecinctColor(crimeData)

    ## print(unique(crimeData$color))

    plot(crimeData$lon, crimeData$lat, col=crimeData$color,
         xlab="Longitude",
         ylab="Latitude",
         main=title)

    assigned <- unique(crimeData$assignedPrecinct)

    size <- 0.5/60

    imageLocationM <- file.path("..", "Images", "policeBadge02.jpg")

    imageLocationF <- file.path("..", "Images", "policeBadge01.jpg")

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

boxPlots <- function(crimeData, precinctData)
{
    cols <- getColors()
    
    crimeData$color <- setCrimePrecinctColor(crimeData)

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

closureRate <- function(collectionOfDistances)
{
    rate <- c(NA)

    for (i in 2:length(collectionOfDistances))
    {
        rate <- c(rate, ((collectionOfDistances[i-1]-collectionOfDistances[i])/collectionOfDistances[i]))
    }

    oldPar <- par()

    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    xLimits <- c(1, length(collectionOfDistances))

    plot(collectionOfDistances,
         xlab="Precinct movement refinement",
         ylab="Total sum of miles to crimes",
         ylim=c(min(pretty(range(collectionOfDistances))), max(pretty(range(collectionOfDistances)))),
         xlim=xLimits
         )

    par(new = TRUE)
    cols <- "red"
    plot(rate*100, type = "l", axes = FALSE,
         bty = "n", xlab = "", ylab = "",
         col=cols,
         xlim=xLimits
         ## log="y",
         )
    axis(side=4, at = pretty(range(rate[-1]*100)))
    mtext("Percent of change between distances", side=4, line=3, col=cols)
    par(oldPar)
    rate
}

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
               returnValue <- distHaversine(c(lon1, lat1), c(lon2, lat2), r=6371)#, r=3959)
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

initPrecincitData <- function(numberOfKiosks=7, cityNumber=1)
{
    returnValue <- data.frame(
        n=as.numeric(),
        lat=as.numeric(),
        lon=as.numeric(),
        fixed=as.logical()
    )

    counter <- 0
    switch(as.character(cityNumber),
           "1" = {
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
           }
           )

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

initCrimeData <- function(useExisting=TRUE, savedFile=tempfile(), cityNumber=1)
{
    dataFile <- file.path("..", "Data", "Police_Incident_Reports.csv")

    switch(as.character(cityNumber),
           "1" = {
               northernLat <- 37.5
               southernLat <- 36
               westernLon <- -76.5
               easternLon <- -75
           },
           "2" = {
               northernLat <- 90
               southernLat <- -90
               westernLon <- -180
               easternLon <- 180
           }
           )

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
