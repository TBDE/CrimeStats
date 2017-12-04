rm(list=ls())

library(NISTunits)
library(geosphere)

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

main <- function()
{

    dumpObject(distance(0,0,1,0), comment="Should be 60.")
    dumpObject(distance(0,0,30,0), comment="Should be 1800")
    dumpObject(distance(30,0,40,0), comment="Should be 600.")
    dumpObject(distance(30,0,30,10), comment="Should be 558.")
    dumpObject(distance(80,0,80,1), comment="Should be 104.")
    dumpObject(distance(58.97118,-62.05028, 65.27488, -170.4029), comment="Should be 4973 km")
    dumpObject(distance(0,0,1,0), comment="Should be 96 km")
}

main()
