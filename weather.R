## 
#  weather.R
#  Dave Varon 
#  Created 2013
#  Revised 
#    2016-02-25 generalized methods and comments

## CONSTANTS
url.base       <- "ftp://ftp.ncdc.noaa.gov"
url.path       <- "/pub/data/noaa/"
stations.index <- "isd-history.csv"
path.local     <- "data/"
path.raw       <- paste0(path.local,"raw/")
path.current   <- paste0(path.local,"current/")
path.csv       <- paste0(path.local,"csv/")
stations.index.local <- paste0(path.local,stations.index)
##

# the weather station index
getStationIndex <- function() {
  file <- paste0(url.base,url.path,stations.index)
  # get the file
  repeat {
    try(download.file(file, local.isd, quiet = TRUE))
    if (file.info(stations.index.local)$size > 0) {
      break
    }
  }
}

# boston = USAF=994971,WBAN=99999, default is norwood is USAF=725098,WBAN=54704
getStation <- function(CTRY="US",STATE="MA",STATION="NORWOOD MEMORIAL AIRPORT") {
  # read the index in
  stations <- read.csv(stations.index.local)
  # get the delired row
  stn <- stations[stations$CTRY == CTRY & stations$STATE == STATE & stations$STATION.NAME == STATION, ] 
  # isolate the year
  stn$BEGIN <- as.numeric(substr(stn$BEGIN, 1, 4))
  stn$END   <- as.numeric(substr(stn$END, 1, 4))
  # return the station row
  return(stn)
}

# retrive the files via curl/ftp
getDataFiles <- function(state="MA",start=2011,end,station,current=TRUE) {
  st      <- station
  path    <- if(current) path.current else path.raw
  outputs <- data.frame("FILE"=NA,"STATUS"=NA)
  for (year in start:end) {
    gz   <- paste0(st$USAF,"-",st$WBAN,"-",year,".gz")
    curl <- paste0("curl -o ",path,gz, " ",url.base,url.path,year, "/", gz)
    # make the curl call
    rbind(outputs,c(gz,try(system(curl, intern = FALSE,ignore.stderr = TRUE))))
  }
  system(paste0("gunzip -r ",path), intern = FALSE, ignore.stderr = TRUE)
}

# parse the data files and write out csv
conformFiles <- function(path=path.current) {
  # documentation:  http://www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf
  #                  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
  column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1, 3, 3, 2, 4, 1, 1)
  files <- list.files(path)
  for (i in 1:length(files)) {
    file <- files[i]
    data <- read.fwf(paste0(path,file), column.widths)
    data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33, 35:40)]
    names(data)    <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                     "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT", "ATM.PRES","ADD", "PRCP.ID", "PRCP.HRS", "PRCP.DPTH", "PRCP.COND", "PRCP.QC")
    data$LAT       <- data$LAT/1000
    data$LONG      <- data$LONG/1000
    data$WIND.SPD  <- data$WIND.SPD/10
    data$TEMP      <- data$TEMP/10
    data$DEW.POINT <- data$DEW.POINT/10
    data$ATM.PRES  <- data$ATM.PRES/10
    data$PRCP.DPTH <- data$PRCP.DPTH/10
    
    # this is a hack for when the ADD data is omitted
    data[data$ADD == "REM" | is.na(data$ADD),21] <- "U"
    data[data$ADD == "REM" | is.na(data$ADD),20] <- 9
    data[data$ADD == "REM" | is.na(data$ADD),19] <- 99.99
    data[data$ADD == "REM" | is.na(data$ADD),18] <- 99
    data[data$ADD == "REM" | is.na(data$ADD),17] <- "AA1"
    data[data$ADD == "REM" | is.na(data$ADD),16] <- "ADD"
    
    write.csv(data, file = paste(path.csv, file,".csv", sep = ""), row.names = FALSE)
  }
}

# clean up data dirs
deleteDataFiles <- function(path=path.current) {
  files   <- list.files(path)
  for (f in 1:length(files)) {
    unlink(paste0(path,files[f]))
  }
}

# create data dirs
createDirs <- function() {
  dir.create(path.csv, recursive = TRUE)
  dir.create(path.raw)
  dir.create(path.current)
}


## THE MEATY BIT:

## set up for boston, current year
station <- getStation(STATION="BOSTON")  
year    <- format(Sys.time(), "%Y")

## set your working directory
#setwd("~/")

## create dirs
#createDirs()

## reload station index (isd-history.csv)
#getStationIndex() 

## delete historical data files
#deleteDataFiles(path.raw)  

## delete current data files
#deleteDataFiles() 

## download historical files to raw dir
#getDataFiles(station=station,end=as.numeric(year)-1,current=FALSE)

## download current year data
#getDataFiles(start=year,end=year,station=station)                  

## create csv files with desired data (historical)
#conformFiles(path.raw)

## create csv files with desired data (historical)
#conformFiles()

# now you can load your csv's and go to town on the data
