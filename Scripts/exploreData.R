source("library.R")

main <- function()
{
    cityNumber <- 1

    zoomLimit <- 11

    tempFile <- tempfile()

    crimeData <- initCrimeData(useExisting=FALSE, savedFile=tempFile, cityNumber=cityNumber)

    precinctData <- initPrecincitData(cityNumber=cityNumber)
    
    plotCrimeData(crimeData, precinctData)

    par(ask=TRUE)
    
    zoom <- 4
    while(zoom <= zoomLimit)
    {
        geoPlotCrimeData(crimeData, precinctData, zoom = zoom)
        print(sprintf("Using zoom factor %d", zoom))

        if (zoom > zoomLimit)
        {
            break
        }
        readline(prompt="Press ENTER to see next zoom image.")
        zoom <- zoom + 1
    }
    print("The program has ended.")
}

main()
