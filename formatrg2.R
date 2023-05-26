formatrg2 <- function(rgdatfr){
    library(lubridate)
    rmna <- is.na(rgdatfr[,2])
    rgdatfr <- rgdatfr[!rmna, ]
    time <- dmy_hms(rgdatfr$dateTime)
    mm <- c(1, diff(rgdatfr[, 2], 1))
    temp <- rgdatfr[,1]
    ans <- data.frame(name = rgdatfr[,3], time = time, mm = mm, temp = temp)
    ans <- ans[ans$mm > 0,]
    ans$time <- as.POSIXct(as.POSIXlt(ans$time, origin = "1970-01-01", tz = "Australia/Perth"))
    #I hate time zone stuff
    ans$time <- ans$time + hms("08:00:00")
    attr(ans$time, "tzone") <- "UTC"
    return(ans)
}