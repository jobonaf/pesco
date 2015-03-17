# function: Italian to English dates
it2en.date <- function(date) {
  it.months=c("gen","feb","mar","apr","mag","giu","lug","ago","set","ott","nov","dic")
  en.months=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  for (i in 1:12) {
    if (xgrep(it.months[i],date)) {
      l <- nchar(date)
      p <- xgrep(it.months[i],date,where=TRUE)
      date <- paste(substr(date,1,(p-1)),en.months[i],substr(date,(p+3),l),sep="")
    }
  }
  return(date)
}

# function: English to Italian dates
en2it.date <- function(date) {
  it.months=c("gen","feb","mar","apr","mag","giu","lug","ago","set","ott","nov","dic")
  en.months=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  for (i in 1:12) {
    if (xgrep(en.months[i],date)) {
      l <- nchar(date)
      p <- xgrep(en.months[i],date,where=TRUE)
      date <- paste(substr(date,1,(p-1)),it.months[i],substr(date,(p+3),l),sep="")
    }
  }
  return(date)
}

# function: system dates are English or Italian?
date.lang <- function() {
  lang <- NA
  if (!is.na(as.Date("1gen2000",format="%d%b%Y"))) lang="it"
  if (!is.na(as.Date("1jan2000",format="%d%b%Y"))) lang="en"
  return(lang)
}

# function: from character YYYYMMDD to class Date    
date2Date <- function(date) {
  Date <- seq.Date(as.Date("1900-01-01"),
                   as.Date("1900-01-01")+length(date),
                   length.out=length(date))
  for (i in 1:length(Date)) {
    Date[i] <- as.Date(paste(substr(date[i],1,4),
                             substr(date[i],5,6),
                             substr(date[i],7,8),sep="-"),
                       format="%Y-%m-%d")
  }
  return(Date)
}

# function: from class Date to character YYYYMMDD
Date2date <- function(Date) {
  date <- vector(mode="character", length=length(Date))
  for (i in 1:length(Date)) {
    dum     <- as.character(as.Date(Date[i],format="%Y-%m-%d"))
    date[i] <- paste(substr(dum,1,4),substr(dum,6,7),substr(dum,9,10),sep="")
  }
  return(date)
}

# function: build sequence of dates as vector of character strings in format YYYYMMDD
seq.date <- function(from,to,by="1 day") {
  seq.date <- Date2date(seq.Date(from=date2Date(from),
                                 to  =date2Date(to),
                                 by  =by))
  return(seq.date)
}

## pretty dates
format.dates <- function(dates) {
  dates <- as.POSIXct(dates)
  hh <- format(dates,format="%H")
  dd <- format(dates,format="%d")
  mm <- format(dates,format="%m")
  mmm <- c("Jan","Feb","Mar","Apr",
           "May","Jun","Jul","Aug",
           "Sep","Oct","Nov","Dec")[as.numeric(mm)]
  yy <- format(dates,format="%Y")
  nt <- length(mm)
  im <- unique(c(1,which(mm[-1]!=mm[-nt])+1))
  if(length(unique(hh))>1) {
    id <- unique(c(1,which(dd[-1]!=dd[-nt])+1))
    out <- paste("h",hh,sep="")
    out[id] <- paste(dd[id],out[id])
  } else {
    out <- dd
    iy <- unique(c(1,which(yy[-1]!=yy[-nt])+1))
    out[iy] <- paste(yy[iy],out[iy]) 
  }
  out[im] <- paste(mmm[im],out[im])
  return(out)
}

## Italian holidays
ITholidays <- function(years) {
  library(timeDate)
  HO <- holiday(years, c("ITEpiphany", 
                         "ITLiberationDay", 
                         "ITAssumptionOfVirginMary", 
                         "ITAllSaints",   
                         "ITImmaculateConception",
                         "Easter",
                         "ChristmasDay",
                         "EasterMonday",
                         "BoxingDay",
                         "NewYearsDay"))
  YYYY <- as.numeric(format(HO,"%Y"))
  MM   <- as.numeric(format(HO,"%m"))
  DD   <- as.numeric(format(HO,"%d"))
  xMM <- c(5,6) # primo maggio e due giugno
  xDD <- c(1,2)
  nx <- length(xMM)
  ny <- length(years)
  YYYY <- c(YYYY,rep(years,each=nx))
  MM <- c(MM,rep(xMM,ny))
  DD <- c(DD,rep(xDD,ny))
  
  out <- data.frame(YYYY,MM,DD)
  out <- sort(as.Date(paste(out$YYYY,out$MM,out$DD,sep="-")))
  return(out)
}

