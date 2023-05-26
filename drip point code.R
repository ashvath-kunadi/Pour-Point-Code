rm(list = ls())

library(reshape2)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
library(strucchange)
library(ggallin)

#man rg
manrgdp <- read.csv("H:/My Documents/Project and related work/banksia at gingin/drippointdat.csv")
manrg <- read.csv("H:/My Documents/Project and related work/Rainfall-Throughfall/mangauge.csv")
manrgdp$Date <- dmy(manrgdp$Date)
manrg$Date <- dmy(manrg$Date)
names(manrgdp) <- names(manrg)
events <- sort(unique(manrgdp$Date[!is.na(manrgdp) & manrgdp$depth != 0]))
aldates <- sort(unique(manrgdp$Date))
tothdat <- rbind(manrgdp, manrg[manrg$Date>= min(manrgdp$Date),])
tothdat <- tothdat[order(tothdat$Date, tothdat$RG_id),]
top <- data.frame()
tothdat$Full <- F
tothdat$Full[tothdat$depth == "Full"] <- T
tothdat$depth[tothdat$depth == "Full"] <- 250
for(i in 1:length(unique(tothdat$RG_id))){
    sub <- tothdat[tothdat$RG_id == unique(tothdat$RG_id)[i],]
    sub$csdep[!(is.na(sub$depth))] <- cumsum(sub$depth[!(is.na(sub$depth))])
    top <- rbind(top, sub)
}
top2 <- data.frame()
topTF <- data.frame()
for(i in 1:length(unique(top$RG_id)[grepl("DP", unique(top$RG_id))])){
    sub <- top[top$RG_id == unique(top$RG_id)[grepl("DP", unique(top$RG_id))][i],]
    sub <- sub[!(is.na(sub$csdep)),]
    for(j in 2:length(unique(sub$Date))){
      evsub <- sub[sub$Date == unique(sub$Date)[j],]
      evdetsub <- top[top$Date == unique(sub$Date)[j] | top$Date == unique(sub$Date)[j-1],]
      evdetsub$cl <- "TF"
      evdetsub$cl[grepl("DP", evdetsub$RG_id)] <- "DP"
      evdetsub$cl[grepl("RG", evdetsub$RG_id)] <- "RF"
      evdetsub$cl[grepl("SDP[4-7]", evdetsub$RG_id) | evdetsub$RG_id == "NDP12"] <- "Ctrl"
      evRF <- 0 
      for(k in 1:length(unique(evdetsub$RG_id[evdetsub$cl == "RF"]))){
        evechrg <- evdetsub[evdetsub$RG_id == unique(evdetsub$RG_id[evdetsub$cl == "RF"])[k],]
        evRF[k] <- evechrg$csdep[evechrg$Date == max(evechrg$Date)]-evechrg$csdep[evechrg$Date == min(evechrg$Date)]
      }
      evTF <- 0
      for(k in 1:length(unique(evdetsub$RG_id[evdetsub$cl == "TF"]))){
        evechrg <- evdetsub[evdetsub$RG_id == unique(evdetsub$RG_id[evdetsub$cl == "TF"])[k],]
        evTF[k] <- evechrg$csdep[evechrg$Date == max(evechrg$Date)]-evechrg$csdep[evechrg$Date == min(evechrg$Date)]
      }
      evsub$mnRF <- mean(evRF, na.rm = T)
      evsub$nRF <- length(!is.na(evRF))
      evsub$mnTF <- mean(evTF, na.rm = T)
      evsub$nTF <- length(!is.na(evTF))
      top2 <- rbind(top2, evsub)
      topTF <- rbind(topTF, data.frame(rf = mean(evRF, na.rm = T),TF = evTF, RG_id = unique(evdetsub$RG_id[evdetsub$cl == "TF"])))
    }
}
top2$cl <- "Pour Point"
top2$cl[grepl("SDP[4-7]", top2$RG_id) | top2$RG_id == "NDP12"] <- "Ctrl"
top2$depth <- as.numeric(top2$depth)
top2$cl[top2$cl == "Ctrl"] <- "Control"
library(strucchange)
brkpnts <- 0
for(i in 1:length(unique(top2$RG_id))){
  sub <- top2[top2$RG_id == unique(top2$RG_id)[i],]
}
ggplot(top2, aes(mnRF, depth)) + theme_classic() + 
  geom_abline(slope = 1, intercept = 0, alpha =0.2) + geom_hline(yintercept= 250, col = "red", alpha = 0.2) + 
  geom_point(data = topTF, aes(rf, TF) , col = "grey") + 
  geom_point(aes(shape = cl), fill = "gold", size = 2) + scale_shape_manual(values = c(13,24)) + 
  xlim (0,200) + 
  ylim (0,255) +
  xlab("Rainfall (mm)") + 
  ylab("Pour point/Throughfall (mm)") +
  guides(shape = guide_legend(title = element_blank()))
  
# soil moisture from drip points
##needs the simulation work already run


##soil moisture sensors
vwccal1 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/vwcfrcal1.csv")
vwccal2 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/vwcfrcal2.csv")
tmrndcs650 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/cs650/timernd.csv")
cntmrndcs650 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/cs650/container rnd.csv")
try <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/cs650/CR1000_DatoutCS650.dat", header= FALSE)
calcs650 <- data.frame(time = try[5:dim(try)[1],1], ka1 = try[5:dim(try)[1],6],
                       ka2 = try[5:dim(try)[1],12], ka3 = try[5:dim(try)[1],18],
                       ka4 = try[5:dim(try)[1],24], ka5 = try[5:dim(try)[1],30],
                       ka6 = try[5:dim(try)[1],36], ka7 = try[5:dim(try)[1],42],
                       ka8 = try[5:dim(try)[1],48])
calcs650$time <- ymd_hms(calcs650$time)
calcs650$Round <- NA
rstr <- 4
for(i in tmrndcs650$Round){
  if(i != 1){
    rstr <- rstr+as.numeric(hm(tmrndcs650[i,2])-hm(tmrndcs650[i-1,3]))/60
  }
  dt <- as.numeric(hm(tmrndcs650[i,3])-hm(tmrndcs650[i,2]))/60
  calcs650$Round[rstr:(rstr+dt)] <- i
  rstr <- rstr + dt
}
mcalcs650 <- melt(calcs650, id = c("time", "Round"))
mcalcs650$value <- as.numeric(mcalcs650$value)
mcalcs650$Probe <- as.numeric(substr(as.character(mcalcs650$variable),3,3))
calgrpcs650 <- merge(mcalcs650, cntmrndcs650)
calgrpcs650 <- merge(calgrpcs650, vwccal2)
cubvalcs650 <- function(x,a3,a2,a1,a0){
  ans <- a3*x^3+a2*x^2+a1*x+a0
}
calcurcs650 <- data.frame(Probe = unique(calgrpcs650$Probe), cuba3 = NA, cuba2 = NA, cuba1 = NA, cuba0 = NA, cubrss = NA, cubsig = NA)
for(i in 1:length(unique(calgrpcs650$Probe))){
  sub <- calgrpcs650[calgrpcs650$Probe == unique(calgrpcs650$Probe)[i],]
  mod <- nls(VWC~a3*(value)^3+a2*(value)^2+a1*value+a0, data = sub, 
             start = list(a3 = 4.3*10^-6, a2 = -5.5*10^-4, a1 = 2.92*10^-2, a0 = -5.3*10^-2))#, algorithm = "port", lower = c(0.00004), upper = c(400)))
  calcurcs650[i,2] <- summary(mod)$coefficients[1,1]
  calcurcs650[i,3] <- summary(mod)$coefficients[2,1]
  calcurcs650[i,4] <- summary(mod)$coefficients[3,1]
  calcurcs650[i,5] <- summary(mod)$coefficients[4,1]
  calcurcs650[i,6] <- sum(summary(mod)$residuals^2)
  calcurcs650[i,7] <- summary(mod)$sigma
}
calgrpcs650 <- merge(calgrpcs650,calcurcs650)
calgrpcs650$cubfnvwc <- with(calgrpcs650, cubvalcs650(value,cuba3,cuba2,cuba1,cuba0))
ggplot(calgrpcs650, aes(VWC, cubfnvwc)) + geom_point(aes(col = time), alpha = 0.5) + facet_wrap(.~Probe) + theme_minimal() + 
  geom_abline(slope = 1, intercept = 0)


setwd("H:/My Documents/Project and related work/Drip points")
try <- read.csv("CR1000_DatoutCS650-17-03-2022.dat", header = FALSE)
df <- data.frame(time = try[5:dim(try)[1],1], ka1 = try[5:dim(try)[1],6],
                 ka2 = try[5:dim(try)[1],12], ka3 = try[5:dim(try)[1],18],
                 ka4 = try[5:dim(try)[1],24], ka5 = try[5:dim(try)[1],30],
                 ka6 = try[5:dim(try)[1],36], ka7 = try[5:dim(try)[1],42],
                 ka8 = try[5:dim(try)[1],48])
head(df)
df$time <- ymd_hms(df$time)
mdf <- melt(df, id = "time")
mdf$value <- as.numeric(mdf$value)
mdf$Probe <- as.numeric(substr(as.character(mdf$variable),3,3))
datgrpcs650 <- merge(mdf,calcurcs650)
datgrpcs650$cubfnvwc <- with(datgrpcs650, cubvalcs650(value,cuba3,cuba2,cuba1,cuba0))
ggplot(datgrpcs650, aes(time, cubfnvwc)) + facet_wrap(.~variable) + theme_minimal() + geom_line()


tmrndml1 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/timernd.csv")
cntmrndml1 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/container rnd.csv")
try <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/CR1000_Table1(try1).dat", header= FALSE)
calml1 <- data.frame(time = ymd_hms(try[5:dim(try)[1],1]), sm1 = as.numeric(try[5:dim(try)[1],3]),
                  sm2 = as.numeric(try[5:dim(try)[1],4]), sm3 = as.numeric(try[5:dim(try)[1],5]),
                  sm4 = as.numeric(try[5:dim(try)[1],6]), sm5 = as.numeric(try[5:dim(try)[1],7]),
                  sm6 = as.numeric(try[5:dim(try)[1],8]), sm7 = as.numeric(try[5:dim(try)[1],9]),
                  sm8 = as.numeric(try[5:dim(try)[1],10]), sm11 = as.numeric(try[5:dim(try)[1],13]),
                  sm12 = as.numeric(try[5:dim(try)[1],14]))
calml1$time <- ymd_hms(calml1$time)
calml1$Round <- NA
for(i in tmrndml1$Round){
  calml1$Round[(calml1$time>=ymd_hm(paste("2021-05-03", tmrndml1$Time.start[i]))) & (calml1$time<=ymd_hm(paste("2021-05-03", tmrndml1$Time.end[i])))] <- i
}
mcalml1 <- melt(calml1, id = c("time", "Round"))
mcalml1$value <- as.numeric(mcalml1$value)
mcalml1$Probe <- as.numeric(substr(as.character(mcalml1$variable),3,3))
calgrpml1 <- merge(mcalml1, cntmrndml1)
calgrpml1 <- merge(calgrpml1, vwccal1)
ggplot(calgrpml1, aes(VWC,value)) + geom_point(aes(col = time), alpha = 0.5) + facet_wrap(.~Probe) + theme_minimal()

tmrndml2 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/timernd2.csv")
cntmrndml2 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/container rnd2.csv")
try <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibration/mlx2/CR1000_Table1(try2).dat", header= FALSE)
calml2 <- data.frame(time = ymd_hms(try[5:dim(try)[1],1]), sm1 = as.numeric(try[5:dim(try)[1],3]),
                     sm2 = as.numeric(try[5:dim(try)[1],4]), sm3 = as.numeric(try[5:dim(try)[1],5]),
                     sm4 = as.numeric(try[5:dim(try)[1],6]), sm5 = as.numeric(try[5:dim(try)[1],7]),
                     sm6 = as.numeric(try[5:dim(try)[1],8]), sm7 = as.numeric(try[5:dim(try)[1],9]),
                     sm8 = as.numeric(try[5:dim(try)[1],10]), sm11 = as.numeric(try[5:dim(try)[1],13]),
                     sm12 = as.numeric(try[5:dim(try)[1],14]))
calml2$time <- ymd_hms(calml2$time)
calml2$Round <- NA
for(i in tmrndml2$Round){
  calml2$Round[(calml2$time>ymd_hm(paste("2021-05-18", tmrndml2$Time.start[i]))) & (calml2$time<=ymd_hm(paste("2021-05-18", tmrndml2$Time.end[i])))] <- i
}
mcalml2 <- melt(calml2, id = c("time", "Round"))
mcalml2$value <- as.numeric(mcalml2$value)
mcalml2$Probe <- as.numeric(substr(as.character(mcalml2$variable),3,4))
calgrpml2 <- merge(mcalml2, cntmrndml2)
calgrpml2 <- merge(calgrpml2, vwccal2)
calgrpml2$dielc1 <- with(calgrpml2, 1.07+(6.4*value/1000)-(6.4*(value/1000)^2)+(4.7*(value/1000)^3))
calgrpml2$dielc2 <- with(calgrpml2, 1.1+4.4*value/1000)
a0 <- with(calgrpml2, tapply(dielc2, list(Container, Probe), median)[6,])
cala0 <- data.frame(Probe = names(a0), a0 = a0)
calgrpml2 <- merge(calgrpml2, cala0)
calgrpml2$a1 <- with(calgrpml2, (dielc2-a0)/VWC)
a1 <- with(calgrpml2, tapply(a1, Probe, FUN = function(x)median(x, na.rm = T)))
cala1 <- data.frame(Probe = names(a1), meda1 = a1)
calgrpml2 <- merge(calgrpml2, cala1)
cubval <- function(x,a3,a2,a1,a0){
  ans <- a3*(x/1000)^3+a2*(x/1000)^2+a1*x/1000+a0
}
calcur <- data.frame(Probe = unique(calgrpml2$Probe), cuba3 = NA, cuba2 = NA, cuba1 = NA, cuba0 = NA, rss = NA, sig = NA)
for(i in 1:length(unique(calgrpml2$Probe))){
  sub <- calgrpml2[calgrpml2$Probe == unique(calgrpml2$Probe)[i],]
  mod <- nls(VWC~a3*(value/1000)^3+a2*(value/1000)^2+a1*value/1000+a0, data = sub, start = list(a3 = 1, a2 = 1, a1 = 1, a0 = 1))#, algorithm = "port", lower = c(0.00004), upper = c(400)))
  calcur[i,2] <- summary(mod)$coefficients[1,1]
  calcur[i,3] <- summary(mod)$coefficients[2,1]
  calcur[i,4] <- summary(mod)$coefficients[3,1]
  calcur[i,5] <- summary(mod)$coefficients[4,1]
  calcur[i,6] <- sum(summary(mod)$residuals^2)
  calcur[i,7] <- summary(mod)$sigma
}

calgrpml2 <- merge(calgrpml2, calcur)
calgrpml2$cubfnvwc <- with(calgrpml2, cubval(value,cuba3,cuba2,cuba1,cuba0))
ggplot(calgrpml2, aes(VWC, cubfnvwc)) + geom_point(aes(col = time), alpha = 0.5) + facet_wrap(.~Probe) + theme_minimal() + geom_abline(slope = 1, intercept = 0)


try2 <- read.csv("CR1000_Table1-17-03-2022.dat", header = FALSE)
df2 <- data.frame(time = ymd_hms(try2[5:dim(try2)[1],1]), sm1 = as.numeric(try2[5:dim(try2)[1],3]),
                  sm2 = as.numeric(try2[5:dim(try2)[1],4]), sm3 = as.numeric(try2[5:dim(try2)[1],5]),
                  sm4 = as.numeric(try2[5:dim(try2)[1],6]), sm5 = as.numeric(try2[5:dim(try2)[1],7]),
                  sm6 = as.numeric(try2[5:dim(try2)[1],8]), sm7 = as.numeric(try2[5:dim(try2)[1],9]),
                  sm8 = as.numeric(try2[5:dim(try2)[1],10]), sm11 = as.numeric(try2[5:dim(try2)[1],13]),
                  sm12 = as.numeric(try2[5:dim(try2)[1],14]))
mdf2 <- melt(df2, id = "time")
mdf2$Probe <- as.numeric(substr(as.character(mdf2$variable),3,4))
mdf2 <- merge(mdf2, calcur)
mdf2$VWC <- with(mdf2, cubval(value,cuba3,cuba2,cuba1,cuba0))
ggplot(mdf2, aes(time, value)) + facet_wrap(.~variable) + theme_minimal() + geom_line()

##gettting rainfall events and seperating the drip points from it
setwd("H:/My Documents/Project and related work/banksia at gingin/RG")

##reading the fuctions for this code
source("fileread2.R")
source("formatrg.R")
source("strmsepev.R")
source("formatrg2.R")
source("plotstroms.R")
source("sophis.R")
##reading in all the data
path2dat <- "H:/My Documents/Project and related work/banksia at gingin/RG"
dat <- fileread2(path2dat)

##making the data usable
thrfN <- data.frame()
thrfS <- data.frame()
rf <- data.frame()
incipp <- data.frame()
for(i in 1:length(dat)){
  if(grepl("9092", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE) | grepl("9080", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9047", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9093", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9078", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)){
    tempdat <- formatrg(dat[[i]])
    if(ncol(tempdat) == 3){
      thrfN <- rbind(thrfN, tempdat)
    }
  }
  if(grepl("9086", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE) | grepl("9079", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9082", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9077", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)){
    tempdat <- formatrg(dat[[i]])
    if(ncol(tempdat) == 3){
      thrfS <- rbind(thrfS, tempdat)
    }
  }
  if(grepl("9097", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE) | grepl("9098", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)| grepl("9099", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)){
    tempdat <- formatrg(dat[[i]])
    if(ncol(tempdat) == 3){
      rf <- rbind(rf, tempdat)
    }
  } 
  if(grepl("9094", substr(dat[[i]]$Name[1], 1, 4), ignore.case = TRUE)){
    tempdat <- formatrg(dat[[i]])
    if(ncol(tempdat) == 3){
      incipp <- rbind(incipp, tempdat)
    }
  }
}
thrfN$time <- as.POSIXct(as.POSIXlt(as.integer(as.character(thrfN$time)), origin = "1970-01-01", tz = "GMT"))
thrfS$time <- as.POSIXct(as.POSIXlt(as.integer(as.character(thrfS$time)), origin = "1970-01-01", tz = "GMT"))
rf$time <- as.POSIXct(as.POSIXlt(as.integer(as.character(rf$time)), origin = "1970-01-01", tz = "GMT"))
incipp$time <- as.POSIXct(as.POSIXlt(as.integer(as.character(incipp$time)), origin = "1970-01-01", tz = "GMT"))
thrfN$tips <- as.integer(as.character(thrfN$tips))*0.2
thrfS$tips <- as.integer(as.character(thrfS$tips))*0.2
rf$tips <- as.integer(as.character(rf$tips))*0.2
incipp$tips <- as.integer(as.character(incipp$tips))*0.2
thrfN$name <- substr(as.character(thrfN$name), 3,4)
thrfS$name <- substr(as.character(thrfS$name), 3,4)
rf$name <- substr(as.character(rf$name), 1,4)
incipp$name <- substr(as.character(incipp$name), 1,4)
names(rf) <- c("name", "time", "mm")
names(thrfN) <- c("name", "time", "mm")
names(thrfS) <- c("name", "time", "mm")
names(incipp) <- c("name", "time", "mm")
rm(dat, tempdat, path2dat, fileread2, formatrg, i)
mintime <- min(mdf2$time)
thrfN <- thrfN[thrfN$time > mintime,]
thrfS <- thrfS[thrfS$time > mintime,]
rf <- rf[rf$time>mintime,]
incipp <- incipp[incipp$time>mintime,]
maxtime <- max(c(thrfN$time, thrfS$time, rf$time, incipp$time), na.rm = T)

smsensdat <- read.csv("H:/My Documents/Project and related work/banksia at gingin/DPSMloc.CSV")

##automatic rain gauge under drip points
C9C3 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/ashvath.kunadi@research.uwa.edu.au_C9C3EB8176BD_1649733826694.csv")
C835 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/ashvath.kunadi@research.uwa.edu.au_C835D23D88AA_1649733815101.csv")
CA92 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/ashvath.kunadi@research.uwa.edu.au_CA9263744FA0_1649733841780.csv")
E770 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/ashvath.kunadi@research.uwa.edu.au_E770E42A57E5_1648018609990.csv")
F333 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/ashvath.kunadi@research.uwa.edu.au_F33345B90487_1649733853256.csv")
C9C3 <- formatrg2(C9C3)
C835 <- formatrg2(C835)
CA92 <- formatrg2(CA92)
E770 <- formatrg2(E770)
F333 <- formatrg2(F333)

C9C3$name <- "NDP8"
E770$name <- "NDP9"
F333$name <- "SF"
C835$name <- "PPCT"
CA92$name <- "PPFT"
incipp$temp <- NA
incipp$name <- "IncidentalPP"
evlst <- list()

##time to the next storm given as l 
l <- 2
evtim <- strmsepev(rf, l*60*60)
rf$events <- NA
thrfN$events <- NA
thrfS$events <- NA
mdf2$events  <- NA
datgrpcs650$events <- NA
C9C3$events <- NA
C835$events <- NA
CA92$events <- NA
E770$events <- NA
F333$events <- NA
incipp$events <- NA

for(i in 1:length(evtim[[1]])){
  rf$events[rf$time >= evtim[[1]][i] & rf$time <= evtim[[2]][i]] <- i
  thrfN$events[thrfN$time >= evtim[[1]][i] & thrfN$time <= evtim[[2]][i]] <- i
  thrfS$events[thrfS$time >= evtim[[1]][i] & thrfS$time <= evtim[[2]][i]] <- i
  mdf2$events[mdf2$time >= evtim[[1]][i] & mdf2$time <= evtim[[2]][i]] <- i
  datgrpcs650$events[datgrpcs650$time >= evtim[[1]][i] & datgrpcs650$time <= evtim[[2]][i]] <- i
  C9C3$events[C9C3$time >= evtim[[1]][i] & C9C3$time <= evtim[[2]][i]] <- i
  C835$events[C835$time >= evtim[[1]][i] & C835$time <= evtim[[2]][i]] <- i
  CA92$events[CA92$time >= evtim[[1]][i] & CA92$time <= evtim[[2]][i]] <- i
  E770$events[E770$time >= evtim[[1]][i] & E770$time <= evtim[[2]][i]] <- i
  F333$events[F333$time >= evtim[[1]][i] & F333$time <= evtim[[2]][i]] <- i
  incipp$events[incipp$time >= evtim[[1]][i] & incipp$time <= evtim[[2]][i]] <- i
}
names(datgrpcs650)[11] <- "VWC"
cs650 <- merge(datgrpcs650[,c(1:3,11,12)], smsensdat[smsensdat$SM.Type == "CS650",-6])
mlx2 <- merge(mdf2[,c(1:3,11,12)], smsensdat[smsensdat$SM.Type != "CS650",-6])
sm <- rbind(cs650, mlx2)
evrmv <- read.csv("H:/My Documents/Project and related work/banksia at gingin/evrmv.CSV")
evlst <- plotstorms(rf, thrf = rbind(thrfN, thrfS), dpsf = rbind(C9C3, E770, CA92, C835, F333,incipp),
                    sm,img = , prefix = paste(as.character(l), "-hour", evrmv))
gr1add <- data.frame(rf = evlst[[1]], evlst[[5]], NDP8 = evlst[[3]][,1], NDP9 = evlst[[3]][,2], 
                   PPFT = evlst[[3]][,3], PPCT = evlst[[3]][,4], SF = evlst[[3]][,5], IncidentalPP = evlst[[3]][,6])
gr1add <- melt(gr1add, "rf")
gr1add$variable <- as.character(gr1add$variable)
gr1add$variable[grep("X",gr1add$variable)] <- "TF"
maxrf <- max(gr1add$rf)
maxy <- max(gr1add$value, na.rm = T)
gr1add$col <- "darkorange"
gr1add$col[gr1add$variable == "TF"] <- "grey"
gr1add$col[gr1add$variable == "SF"] <- "brown"
gr1add$shp <- 17
gr1add$shp[gr1add$variable == "SF" | gr1add$variable == "TF"] <- 16
ggplot(gr1add, aes(rf, value)) + theme_classic() + 
  geom_point(aes(col = col, shape = shp)) + scale_color_identity() + scale_shape_identity() + 
  xlab("Rainfall (mm)") + geom_abline(slope = 1, intercept = 0) + 
  ylab("Pour point/Throughfall/Stemflow (mm)") + 
  guides(col = guide_legend(title = "Pour Point\n Behavior"), shape = guide_legend(title = "Pour Point\n Behavior")) 

gr2add <- data.frame(rf = evlst[[1]], evlst[[5]], NDP8 = evlst[[3]][,1], NDP9 = evlst[[3]][,2], 
                     PPFT = evlst[[3]][,3], PPCT = evlst[[3]][,4], SF = evlst[[3]][,5], IncidentalPP = evlst[[3]][,6])
gr2add$NDP8[evrmv$NDP8 != "OK" & evrmv$NDP8 != "Great"] <- NA
gr2add$NDP9[evrmv$NDP9 != "OK" & evrmv$NDP9 != "Great"] <- NA
gr2add$PPFT[evrmv$DPFT != "OK" & evrmv$DPFT != "Great"] <- NA
gr2add$PPCT[evrmv$DPCT != "OK" & evrmv$DPCT != "Great"] <- NA
gr2add$SF[evrmv$SF != "OK" & evrmv$SF != "Great"] <- NA
gr2add <- melt(gr2add, "rf")
gr2add$variable <- as.character(gr2add$variable)
gr2add$variable[grep("X",gr2add$variable)] <- "TF"

maxrf <- max(gr2add$rf)
maxy <- max(gr2add$value, na.rm = T)
# gr2add$col <- "gold"
# gr2add$col[gr2add$variable == "TF"] <- "grey"
# gr2add$col[gr2add$variable == "SF"] <- "brown"
# gr2add$shp <- 17
# gr2add$shp[gr2add$variable == "SF" | gr2add$variable == "TF"] <- 16
gr2add$variable[gr2add$variable != "TF" & gr2add$variable != "SF"] <- "Pour Point"
gr2add$variable[gr2add$variable == "SF"] <- "Stemflow"
subPP2 <- data.frame(rf = names(with(gr2add[gr2add$variable != "TF",], tapply(value, rf, FUN = function(x) mean(x,na.rm = T)))), 
                     PP = with(gr2add[gr2add$variable != "TF",], tapply(value, rf, FUN = function(x) mean(x,na.rm = T))))
subPP2$rf <- as.numeric(subPP2$rf)
pltln <- breakpoints(PP~rf, data = subPP2, h = 4)
pltln <- subPP2[pltln$breakpoints,]
ggplot(gr2add[gr2add$variable == "TF",], aes(rf, value)) + theme_classic() + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + geom_vline(data = pltln, aes(xintercept = rf), linetype = "dashed") +
  geom_point(col = "grey") + geom_point(data = gr2add[gr2add$variable != "TF",], aes(x = rf, y = value, fill = variable), shape = 24, size = 2) +
  scale_fill_manual(values = c("Pour Point" = "gold","Stemflow" = "brown")) + 
  xlab("Rainfall [mm]") + 
  ylab("Pour point/Throughfall/Stemflow [mm]") +   guides(fill = guide_legend(title = element_blank()))


tffa <- gr2add[gr2add$variable == "TF", -c(4,5)]
dpfa <- gr2add[gr2add$variable != "TF" & gr2add$variable != "SF", -c(4,5)]


thrf <- rbind(thrfN, thrfS)
dpsf <- rbind(C9C3, E770, CA92, C835, F333,incipp)
dpsf <- dpsf[,-4]
evchs <- 28
##get a couple of graphs of a chosen storm
subrf <- rf[rf$events == evchs, ]
top <- data.frame()
for(j in 1:length(unique(subrf$name))){
    subosubrf <- subrf[subrf$name == unique(subrf$name)[j],]
    if(nrow(subosubrf)!=0){
      subosubrf$value <- cumsum(subosubrf$mm)
      subosubrf$col <- "grey50"
      subosubrf$lab <- "RF Gauges"
      subosubrf$alpha <- 0.5
      top <- rbind(top, subosubrf[,c(1,2,5,6,7,8)])
    }
    if(j == length(unique(subrf$name))){
      subosubrf <- subrf[order(subrf$time),]
      subosubrf$mm <- subosubrf$mm/length(unique(subrf$name))
      rffrdiv <- subosubrf$mm/length(unique(subrf$name))
      subosubrf$value <- cumsum(subosubrf$mm)
      subosubrf$col <- "blue"
      subosubrf$name <- "Average rf"
      subosubrf$lab <- "Average RF"
      subosubrf$alpha <- 1
      top <- rbind(top, subosubrf[,c(1,2,5,6,7,8)])
    }
  }
subth <- thrf[thrf$events == evchs, ]
subth <- subth[complete.cases(subth),]
for(j in 1:length(unique(thrf$name))){
    subosubthrf <- subth[subth$name == unique(thrf$name)[j],]
    if(nrow(subosubthrf)!=0){
      subosubthrf$value <- cumsum(subosubthrf$mm)
      subosubthrf$col <- "grey30"
      subosubthrf$lab <- "TF Gauges"
      subosubthrf$alpha <- 0.5
      top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
    }
    if(j == length(unique(thrf$name))){
      if(nrow(subosubthrf)!=0){
        subosubthrf$value <- cumsum(subosubthrf$mm)
        subosubthrf$col <- "grey30"
        subosubthrf$lab <- "TF Gauges"
        subosubthrf$alpha <- 0.5
        top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
      }
      subosubthrf <- subth[order(subth$time),]
      if(nrow(subosubthrf)!=0){
        subosubthrf$mm <- subosubthrf$mm/length(unique(subth$name))
        subosubthrf$value <- cumsum(subosubthrf$mm)
        subosubthrf$col <- "forestgreen"
        subosubthrf$name <- "Average thrf"
        subosubthrf$lab <- "Average TF"
        subosubthrf$alpha <- 1
        top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
      }
    }
  }
subdpsf <- dpsf[dpsf$events == evchs, ]
subdpsf <- subdpsf[!is.na(subdpsf$name),]
if(nrow(subdpsf) != 0){
  for(j in 1:length(unique(dpsf$name))){
    subosubdpsf <- subdpsf[subdpsf$name == unique(dpsf$name)[j],]
    if(nrow(subosubdpsf) != 0){
      subosubdpsf$value <- cumsum(subosubdpsf$mm)
      subosubdpsf$col <- "gold"
      subosubdpsf$lab <- subosubdpsf$name[1]
      subosubdpsf$alpha <- 1
      top <- rbind(top, subosubdpsf[ , c(1,2,5,6,7,8)])
    }
  }
}
top <- top[!is.na(top$name),]
top$col[top$lab == "SF"] <- "brown"
g1 <- ggplot(top, aes(time, value, group = name, color = col, alpha = alpha)) + geom_line(aes(col = col), size = 1.3) + theme_classic() + 
      xlim(evtim[[1]][evchs],evtim[[2]][evchs]) + scale_color_identity(labels = c(gold = "Pour Point", 
                                                                          brown = "SF", grey30 = "TF", 
                                                                          grey50 = "RF", forestgreen = "Avg TF",
                                                                          blue = "Avg RF"), guide = "legend") + 
      ggtitle(paste("Storm on", as.character(as.Date(evtim[[1]][evchs])))) + ylab("Cumilative depth in [mm]") + 
      theme(legend.position = "bottom")
g1
try <- ggplot_build(g1)$data
ref <- try[[1]][try[[1]]$colour == "blue",3:4]
alt <- unique(try[[1]]$x)
ref <- rbind(ref, data.frame(x = alt, y = NA))
ref <- ref[order(ref$x),]
zrchck <- T
for(k in 1:nrow(ref)){
 if(is.na(ref$y[k])){
   if(zrchck){
     ref$y[k] <- 0 
   }
   else{
     ref$y[k] <- val
   }
 }
 else{
   zrchck <- F
   val <- ref$y[k]
 }
}
ref <- ref[!duplicated(ref),]
names(ref)[2] <- "nrml"
top2 <- try[[1]]
top2 <- merge(top2,ref)
top2$difffrmrf <- top2$y-top2$nrml
top2$x <- as.POSIXct(as.POSIXlt(top2$x, origin = "1970-01-01", tz = "GMT"))
rftop <- rf[rf$events == evchs,]
for(i in 1:length(unique(rftop$name))){
 if(i == 1){
   rftpoact <- rftop[rftop$name == unique(rftop$name)[i],]
 }else{
   rftpoact <- merge(rftpoact, rftop[rftop$name == unique(rftop$name)[i],], by = "time", all = TRUE)
 }
}
rftpoact <- rftpoact[,c(1,3,6,9)]
for(i in 1:ncol(rftpoact)){
 rftpoact[is.na(rftpoact[,i]),i] <- 0
}
finrftop <- data.frame()
for(i in 1:length(unique(hour(rftpoact$time)))){
 sub <- rftpoact[hour(rftpoact$time)==unique(hour(rftpoact$time))[i],]
 for(j in 1:length(unique(minute(sub$time)))){
   submin <- sub[minute(sub$time)==unique(minute(sub$time))[j],]
   dep <- mean(apply(submin[,2:4], 2, sum))
   finrftop <- rbind(finrftop, data.frame(time = (submin$time[1]-second(submin$time[1])+ 30), depth = dep))
 }
}
top2 <- top2[,c(1,2,3,5,6,11)]
names(top2)[6] <- "y"
top2$PANEL <- "Difference from Rainfall"
fintop <- rbind(top2, data.frame(x = finrftop$time, colour = "blue", alpha = 1, group = 21, PANEL = "Rainfall per minute", y = finrftop$depth))

ggplot(fintop, aes(x, y, group = group, col = colour, alpha = alpha)) +
  facet_grid(as.factor(PANEL)~., scales = "free") + geom_bar(data = fintop[fintop$PANEL == "Rainfall per minute",], aes(colour = colour), 
                                                  stat = "identity") +
  geom_line(data = fintop[fintop$PANEL == "Difference from Rainfall",], aes(colour = colour), size = 1.3) + theme_classic() +
  scale_color_identity(labels = c(darkorange3 = "Pour Point", brown = "SF",
  grey30 = "TF", grey50 = "RF", forestgreen = "Avg TF", blue = "Avg RF"), guide = "legend") +
  theme(legend.position = "bottom") +
  xlab("Time") + ylab("Depth [mm]") +
  ggtitle(paste("Storm on", as.character(as.Date(evtim[[1]][evchs]))))





pdf1 <- rbind(data.frame(type = "TF", origin = "Field-man", ratio = topTF$TF/topTF$rf),
              data.frame(type = "PP", origin = "Field-man", ratio = as.numeric(top2$depth)/top2$mnRF),
              data.frame(type = "TF", origin = "Field-aut", ratio = tffa$value/tffa$rf),
              data.frame(type = "PP", origin = "Field-aut", ratio = dpfa$value/dpfa$rf),
              data.frame(type = "TF", origin = "Lab", ratio = robustdf$rexcprf[!robustdf$outlier]),
              data.frame(type = "PP", origin = "Lab", ratio = robustdf$rexcprf[robustdf$outlier])
              )

ggplot(pdf1, aes(ratio)) + facet_grid(origin~., scales = "free_y") + geom_density(aes(fill = type), alpha = 0.2) + theme_classic() + 
  xlab("Ratio of Pour Point and Throughfall to Rainfall") + theme(strip.background = element_blank())  + 
  scale_fill_manual(values = c("darkorange", "grey")) + xlim(0,5)
write.csv(pdf1, "pdf1.csv")

for(i in 1:length(unique(pdf1$origin))){
  sub <- pdf1[pdf1$origin == unique(pdf1$origin)[i],]
  
}
##Stemflow comparison
try <- data.frame(evlst[[3]])
dpsf <- rbind(C9C3, E770, CA92, C835, F333, incipp)
names(try) <- unique(dpsf$name)
try$NDP8[evrmv$NDP8 != "OK" & evrmv$NDP8 != "Great"] <- NA
try$NDP9[evrmv$NDP9 != "OK" & evrmv$NDP9 != "Great"] <- NA
try$PPFT[evrmv$DPFT != "OK" & evrmv$DPFT != "Great"] <- NA
try$PPCT[evrmv$DPCT != "OK" & evrmv$DPCT != "Great"] <- NA
try$SF[evrmv$SF != "OK" & evrmv$SF != "Great"] <- NA
mtry <- melt(try, id = "SF")
mtry$ratio <- mtry$value/mtry$SF
library(ggridges)
library(viridis)
ggplot(mtry, aes(x = ratio, y = variable, fill = ..x..))  + geom_density_ridges_gradient(alpha = 0.2) + theme_ridges() + 
  scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100), labels = expression(10^-2, 10^-1, 1, 10^1, 10^2)) + scale_fill_viridis(trans = scales::pseudo_log_trans()) +
  theme_classic() + geom_vline(xintercept = 1) + xlab("Ratio of Pour Point to Stemflow") + labs(fill='Pour Point', col = 'Pour Point') + theme(legend.position = "none")       

with(mtry, tapply(ratio, variable, function(x) sum(x>1, na.rm = T)/sum(!is.na(x))))

##activation threshold gross amounts
gr2add$PPBehv <- F
gr2add$PPBehv[gr2add$value/gr2add$rf >= 1.5] <- T
top2$PPBehv <- F
top2$PPBehv[top2$depth/top2$mnRF >= 1.5] <- T
acthr <- data.frame(Rainfall = c(gr2add$rf[gr2add$variable != "TF"], top2$mnRF), 
                    PPBehv = c(gr2add$PPBehv[gr2add$variable != "TF"], top2$PPBehv))

x <- hist(acthr$Rainfall, 100)
x2 <- hist(acthr$Rainfall[acthr$PPBehv], breaks = x$breaks)
acthrtop <- data.frame(x = x$mids, y = x2$counts/x$counts)
ggplot(acthrtop, aes(x, y)) + geom_bar(stat= "identity") + theme_classic() + xlim (0,150) + 
  xlab("Rainfall (mm)") + ylab("Fraction of Pour Points Active") + geom_vline(xintercept = 10, col = "red", linetype =2)

##dynamic activation
wrk <- intanl(rf, thrf = rbind(thrfN, thrfS), dpsf = rbind(C9C3, C835, CA92, E770, F333, incipp),evtim[[1]],evtim[[2]])
wrk$gt <- "TF"
wrk$gt[nchar(wrk$gauge) == 4] <- "RF"
wrk$gt[is.na(as.integer(wrk$gauge))] <- "PP"
wrk$gt[wrk$gauge == "SF"] <- "SF"
plot1 <- data.frame(melt(with(wrk, tapply(intensity, list(tmint,gt), sd))))
ggplot(plot1, aes(Var1, value)) + geom_point(aes(col = Var2)) + theme_classic() + 
  xlab("Averaging time [mins]") + ylab("Standarad Deviation of Rainfall Intensity [mm/min]") + labs(col = "Gauge")
ansactdf <- data.frame()
for(t in 1:length(unique(wrk$tmint))){
  try10 <- wrk[wrk$tmint == unique(wrk$tmint)[t],]
  for(i in 1:length(unique(try10$event))){
    subev <- try10[try10$event == unique(try10$event)[i],]
    for(j in 1:length(unique(subev$tmid))){
      subtmid <- subev[subev$tmid == unique(subev$tmid)[j],]
      if(sum(subtmid$gt != "PP")>1){
        subtmid$mnint <- mean(subtmid$intensity[subtmid$gt != "PP" & subtmid$gt != "SF"], na.rm = T)
        subtmid$sdint <- sd(subtmid$intensity[subtmid$gt != "PP" & subtmid$gt != "SF"], na.rm = T)
        ansactdf <- rbind(ansactdf, subtmid)
      }else{
        subtmid$mnint <- NA
        subtmid$sdint <- NA
        ansactdf <- rbind(ansactdf, subtmid)
      }
    }
  }
}
##activation decieded to be nsd times the standard deviation
nsd <- 3
ansactdf$dev <- (ansactdf$intensity-ansactdf$mnint)/ansactdf$sdint
ansactdf$mrk <- F
ansactdf$mrk[ansactdf$dev >= nsd & ansactdf$dev != Inf] <- T
ggplot(ansactdf, aes(dev)) + geom_density(aes(fill = gt), alpha = 0.2) + facet_wrap(tmint~.,scales = "free") +
  geom_vline(xintercept = nsd) + theme_classic() + xlim(0,10)
actNDP8 <- ansactdf[(ansactdf$gauge == "NDP8" & (ansactdf$event %in% evrmv$Storm[evrmv$NDP8 == "OK" | evrmv$NDP8 == "Great"][!is.na(evrmv$Storm[evrmv$NDP8 == "OK" | evrmv$NDP8 == "Great"])])),]
actNDP9 <- ansactdf[(ansactdf$gauge == "NDP9" & (ansactdf$event %in% evrmv$Storm[evrmv$NDP9 == "OK" | evrmv$NDP9 == "Great"][!is.na(evrmv$Storm[evrmv$NDP9 == "OK" | evrmv$NDP9 == "Great"])])),]
actPPCT <- ansactdf[(ansactdf$gauge == "PPCT" & (ansactdf$event %in% evrmv$Storm[evrmv$DPCT == "OK" | evrmv$DPCT == "Great"][!is.na(evrmv$Storm[evrmv$DPCT == "OK" | evrmv$DPCT == "Great"])])),]
actPPFT <- ansactdf[(ansactdf$gauge == "PPFT" & (ansactdf$event %in% evrmv$Storm[evrmv$DPFT == "OK" | evrmv$DPFT == "Great"][!is.na(evrmv$Storm[evrmv$DPFT == "OK" | evrmv$DPFT == "Great"])])),]
actInci <- ansactdf[ansactdf$gauge == "IncidentalPP",]
actSF <- ansactdf[(ansactdf$gauge == "SF" & (ansactdf$event %in% evrmv$Storm[evrmv$SF == "OK" | evrmv$SF == "Great"][!is.na(evrmv$Storm[evrmv$SF == "OK" | evrmv$SF == "Great"])])),]
actRF <- ansactdf[ansactdf$gt == "RF",]
combactdf <- rbind(actNDP8, actNDP9, actPPCT, actPPFT, actInci, actSF, actRF)



actrfdf <- data.frame()
rfbfact <- data.frame()
evid <- 0
for(t in 1:length(unique(combactdf$tmint))){
  subt <- combactdf[combactdf$tmint == unique(combactdf$tmint)[t],]
  for(i in 1:length(unique(subt$gauge[subt$gt == "PP"]))){
    sub <- subt[(subt$gauge == unique(subt$gauge[subt$gt == "PP"])[i]),]
    timbrk <- sub$tmid[cumsum(rle(sub$mrk)$lengths)[2*(1:floor(length(rle(sub$mrk)$lengths)/2))]]
    evoftim <- sub$event[cumsum(rle(sub$mrk)$lengths)[2*(1:floor(length(rle(sub$mrk)$lengths)/2))]]
    for(j in 1:length(timbrk)){
      if(j ==1){
        rfsub <- subt[subt$gt == "RF" & subt$tmid < timbrk[j],]
        ppsub <- sub[sub$tmid < timbrk[j],]
      }else{
        rfsub <- subt[subt$gt == "RF" & subt$tmid < timbrk[j] & subt$tmid >= timbrk[j-1],]
        ppsub <- sub[sub$tmid < timbrk[j] & subt$tmid >= timbrk[j-1],]
      }
      rfsub <- rfsub[complete.cases(rfsub) & rfsub$event == evoftim[j],]
      ppsub <- ppsub[complete.cases(ppsub) & ppsub$event == evoftim[j],]
      actrf <- sum(rfsub$intensity*rfsub$tmint)/length(unique(rfsub$gauge))
      actrf <- sum(rfsub$intensity*rfsub$tmint)/length(unique(rfsub$gauge))
      if(nrow(rfsub)!=0){
        actrfdf <- rbind(actrfdf, data.frame(gauge = sub$gauge[1], tmint = rfsub$tmint[1], trec = timbrk[j], event = evoftim[j], actrf = actrf))
        evid <- evid+1
        combpprf <- rbind(rfsub, ppsub)
        combpprf$gg <- sub$gauge[1]
        combpprf$ttsub <- timbrk[j]
        combpprf$actev <- evid
        rfbfact <- rbind(rfbfact, combpprf)
      }
    }
  }
}
ggplot(ansactdf[ansactdf$gt == "PP",], aes(intensity, dev)) + geom_point(aes(col = gauge),alpha = 0.2) + facet_wrap(.~tmint, scales = "free") + theme_classic()

ggplot(actrfdf, aes(actrf, y = gauge)) + geom_density_ridges() + theme_ridges()
with(actrfdf, tapply(actrf, list(gauge,tmint), mean))
with(actrfdf, tapply(actrf, list(gauge,tmint), median))
with(actrfdf, tapply(actrf, list(gauge,tmint), sd))
firstact <- data.frame()
for(t in 1:length(unique(actrfdf$tmint))){
  subt <- actrfdf[actrfdf$tmint == unique(actrfdf$tmint)[t],]
  subt <- subt[complete.cases(subt),]
  for(i in 1:length(unique(subt$event))){
    sub <- subt[subt$event == unique(subt$event)[i],]
    for(j in 1:length(unique(sub$gauge))){
      subg <- sub[sub$gauge == unique(sub$gauge)[j],]
      firstact <- rbind(firstact, data.frame(subg[subg$trec == min(subg$trec, na.rm = T),]))
    }
  }
}
with(firstact, tapply(actrf, list(gauge,tmint), mean))
with(firstact, tapply(actrf, list(gauge,tmint), median))
with(firstact, tapply(actrf, list(gauge,tmint), sd))
firstactev <- as.integer(row.names(firstact))
actrfdf$first <- F
firstact$first <- T
combrfdf <- rbind(actrfdf, firstact)

ggplot(combrfdf[combrfdf$tmint == 3,], aes(gauge, actrf)) + scale_fill_brewer(palette = "Paired", name = "Activation", direction = -1, labels = c("All", "First")) + 
  geom_boxplot(aes(fill = factor(first))) + theme_classic() + ylab("Activation Rainfall [mm]") + xlab(element_blank())

library(tseries)
corr <- list()
key <- data.frame()
lmdat <- list()
findat <- data.frame()
for(t in 1:length(unique(rfbfact$tmint))){
  subt <- rfbfact[rfbfact$tmint == unique(rfbfact$tmint)[t],]
  subt <- subt[complete.cases(subt),]
  for(i in 1:length(unique(subt$gg))){
    sub <- subt[subt$gg == unique(subt$gg)[i],]
    for(j in 1:length(unique(sub$event))){
      subev <- sub[sub$event == unique(sub$event)[j],]
      if(sum(subev$gt == "PP") > 1 & sum(subev$gt == "RF") > 3){
        x <- data.frame(tmid = seq(from = min(subev$tmid), to = max(subev$tmid), by = (subev$tmint[1]*60)))
        try <- merge(x, subev[subev$gauge == "9097",c(3,5)], by = "tmid", all = TRUE)
        names(try)[2] <- "g97i"
        try[is.na(try[,2]),2] <- 0
        try <- merge(try, subev[subev$gauge == "9098",c(3,5)], by = "tmid", all = TRUE)
        names(try)[3] <- "g98i"
        try[is.na(try[,3]),3] <- 0
        try <- merge(try, subev[subev$gauge == "9099",c(3,5)], by = "tmid", all = TRUE)
        names(try)[4] <- "g99i"
        try[is.na(try[,4]),4] <- 0
        try$avgri <- apply(try[,c(2:4)], FUN = mean, 1) 
        try <- merge(try, subev[subev$gt == "PP",c(3,5,9,10,13)], by = "tmid", all = TRUE)
        names(try)[6] <- "PPri"
        try[is.na(try[,6]),6] <- 0
        key <- rbind(key,data.frame(tmint = subt$tmint[1], gauge = sub$gg[1], event = subev$event[1]))
        corr[[nrow(key)]] <- ccf(try$avgri, try$PPri, pl = T)
        lmdat[[nrow(key)]] <- lm(PPri~avgri,try)
        findat <- rbind(findat, data.frame(ky = nrow(key), key[nrow(key),],try))
      }
    }
  }
}

crplot <- findat[findat$avgri != 0 ,]
ggplot(crplot[crplot$tmint == 2,], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("2 Minute Averaging")
ggplot(crplot[crplot$tmint == 2 & crplot$mrk & complete.cases(crplot$mrk),], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("2 Minute Averaging")
ggplot(crplot[crplot$tmint == 3 ,], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("3 Minute Averaging")
ggplot(crplot[crplot$tmint == 3 & crplot$mrk & complete.cases(crplot$mrk),], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("3 Minute Averaging")
ggplot(crplot[crplot$tmint == 5 ,], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("5 Minute Averaging")
ggplot(crplot[crplot$tmint == 5 & crplot$mrk & complete.cases(crplot$mrk),], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("5 Minute Averaging")
ggplot(crplot[crplot$tmint == 7 ,], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("7 Minute Averaging")
ggplot(crplot[crplot$tmint == 7 & crplot$mrk & complete.cases(crplot$mrk) ,], aes(avgri, PPri)) + geom_point(aes(col = gauge), alpha = 0.1, size = 0.5)  + theme_classic() + geom_smooth(aes(col = gauge), method = "lm", formula = y~x+0) + xlab("Rain Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]") + geom_abline(intercept = 0, slope = 1) + ggtitle("7 Minute Averaging")
whtrel <- data.frame()
for(i in 1:length(unique(crplot$tmint))){
  sub <- crplot[crplot$tmint == unique(crplot$tmint)[i],]
  x <- lm(PPri~avgri, sub)
  whtrel <- rbind(whtrel, data.frame(tmint = unique(crplot$tmint)[i], adjr2 = summary(x)$adj.r.squared,
                                       r2 = summary(x)$r.squared, mx1 = summary(x)$coefficients[2],
                                       int = summary(x)$coefficients[1], all = TRUE))
  x <- lm(PPri~avgri, sub[sub$mrk,])
  whtrel <- rbind(whtrel, data.frame(tmint = unique(crplot$tmint)[i], adjr2 = summary(x)$adj.r.squared,
                                     r2 = summary(x)$r.squared, mx1 = summary(x)$coefficients[2],
                                     int = summary(x)$coefficients[1], all = FALSE))
  
}
topcoranl <- data.frame()
toplmanl <- data.frame()
for(i in seq(nrow(key))){
  nl <- length(corr[[i]]$acf)
  if(nl < 5){
    topcoranl <- rbind(topcoranl, 
                       data.frame(ky = i, key[i,],
                                  cor = corr[[i]]$acf[order(corr[[i]]$acf, decreasing = TRUE)][1:nl], 
                                  lg = corr[[i]]$lag[order(corr[[i]]$acf, decreasing = TRUE)][1:nl]))
  }else{
    topcoranl <- rbind(topcoranl, 
                       data.frame(ky = i, key[i,], 
                                  cor = corr[[i]]$acf[order(corr[[i]]$acf, decreasing = TRUE)][1], 
                                  lg = corr[[i]]$lag[order(corr[[i]]$acf, decreasing = TRUE)][1]))
  }
  toplmanl <- rbind(toplmanl, 
                    data.frame(ky = i, key[i,],
                               r2 = summary(lmdat[[i]])$r.squared, 
                               adr2 = summary(lmdat[[i]])$adj.r.squared,
                               slp = summary(lmdat[[i]])$coefficients[2,1],
                               rmse = summary(lmdat[[i]])$sigma))
}
write.csv(topcoranl, "mining.csv")
toplmanl <- melt(toplmanl, id = c("ky", "tmint", "gauge", "event"))
ggplot(toplmanl, aes(value)) + facet_wrap(.~variable, scale = "free") + geom_density(aes(fill = gauge), alpha = 0.2) + theme_classic()
ggplot(toplmanl, aes(value)) + facet_grid(tmint~variable, scale = "free") + geom_density(aes(fill = gauge), alpha = 0.2) + theme_classic()
ggplot(topcoranl, aes(lg, cor)) + theme_classic() + facet_grid(tmint~gauge) + geom_point() + geom_vline(xintercept = 0)

lgcrtbl <- data.frame()
for(i in 1:length(unique(topcoranl$gauge))){
  sub <- topcoranl[topcoranl$gauge == unique(topcoranl$gauge)[i],]
  sub <- sub[sub$lg != 0,]
  sub <- sub[sub$tmint <= 10,]
  ans <- 0
  for(j in 1:length(unique(sub$event))){
    subev <- sub[sub$event == unique(sub$event)[j],]
    ans[j] <-subev$tmint[subev$cor == max(subev$cor)]
  }
  lgcrtbl <- rbind(lgcrtbl, data.frame(gg = sub$gauge[1], data.frame(table(ans))))
}


finactdat <- data.frame()
for(i in 1:length(unique(findat$actev))){
  actid <- unique(findat$actev)[i]
  if(!is.na(actid)){
    subintdat <- findat[findat$actev == actid & complete.cases(findat),]
    incravgint <- cumsum(subintdat$avgri[order(subintdat$tmid, decreasing = T)])/(1:length(subintdat$avgri))
    frchck <- actid %in% firstactev
    finactdat <- rbind(finactdat, data.frame(actrfdf[actid,], first = frchck, ntsl = (1:length(subintdat$avgri)), rainint = incravgint, dev = subintdat$dev))
  }
}
ggplot(finactdat[finactdat$ntsl == 1,], aes(actrf, rainint)) + facet_wrap(.~first) + theme_classic() + geom_density_2d() + geom_point(alpha = 0.02) + xlab("Rain Volume [mm]") + ylab("Rain Intensity [mm/min]")

finactdat2 <- data.frame()
for(t in 1:length(unique(findat$tmint))){
  subt <- findat[findat$tmint == unique(findat$tmint)[t],]
  subt <- subt[complete.cases(subt),]
  for(i in 1:length(unique(subt$gauge))){
    sub <- subt[subt$gauge == unique(subt$gauge)[i],]
    for(j in 1:length(unique(sub$event))){
      subev <- sub[sub$event == unique(sub$event)[j],]
      subev$vol <- cumsum(subev$tmint*subev$avgri)
      finactdat2 <- rbind(finactdat2, subev)
    }
  }
}
topprobsal <- finactdat2[finactdat2$tmint == 3,]
topprobsal$volcat <- cut(topprobsal$vol, breaks = 25)
topprobsal$intcat <- cut(topprobsal$avgri, breaks = 15)
topprobsal$volcat2 <- apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", (topprobsal$volcat)))), 
                                  as.numeric(sub("]","",sub("\\(.*,", "", (topprobsal$volcat))))),1,mean)
topprobsal$intcat2 <- apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", (topprobsal$intcat)))), 
                                  as.numeric(sub("]","",sub("\\(.*,", "", (topprobsal$intcat))))),1,mean)
mxy <- quantile(topprobsal$intcat2, probs=c(.90))
mxx <- quantile(topprobsal$volcat2, probs=c(.90))
mtopprobsal <- data.frame(melt(with(topprobsal,tapply(mrk, list(volcat2,intcat2), FUN = function(x) (sum(x[!is.na(x)])/sum(!is.na(x)))))))
mtopprobsal$n <- data.frame(melt(with(topprobsal,tapply(mrk, list(volcat2,intcat2), FUN = function(x) (sum(!is.na(x)))))))[,3]
ggplot(mtopprobsal[!is.na(mtopprobsal$value),], aes(Var1, Var2, label = as.character(n))) + geom_tile(aes(fill = value)) + theme_classic() + 
  xlab("Rain volume accumulated [mm]") + scale_fill_continuous(name = expression(paste("Probability of\nActivation"))) + 
   ylab("Rain Intensity for the time step [mm/min]") + xlim(-1,mxx) + ylim(-0.01,0.5) 


topprobsal <- finactdat2[finactdat2$tmint == 3,]
topprobsal$volcat <- cut(topprobsal$vol, breaks = 20)
topprobsal$intcat <- cut(topprobsal$avgri, breaks = 30)
topprobsal$volcat2 <- apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", (topprobsal$volcat)))), 
                                  as.numeric(sub("]","",sub("\\(.*,", "", (topprobsal$volcat))))),1,mean)
topprobsal$intcat2 <- apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", (topprobsal$intcat)))), 
                                  as.numeric(sub("]","",sub("\\(.*,", "", (topprobsal$intcat))))),1,mean)
mxy <- quantile(topprobsal$intcat2, probs=c(.95))
mxx <- quantile(topprobsal$volcat2, probs=c(.95))
topprobsal <- data.frame(melt(with(topprobsal,tapply(mrk, list(volcat2,intcat2), FUN = function(x) (sum(x[!is.na(x)])/sum(!is.na(x)))))))
ggplot(topprobsal, aes(Var1, Var2)) + geom_tile(aes(fill = value)) + theme_classic() + xlab("Rain volume accumulated [mm]") + ylab("Rain Intensity for the time step [mm/min]") + xlim(0,mxx) + ylim(0,mxy)

ggplot(findat[findat$tmint == 3,], aes(avgri, PPri)) + geom_point(alpha = 0.2) + theme_classic() + facet_wrap(.~gauge) + geom_smooth(method = "lm") + xlab("Rainfall Intensity [mm/min]") + ylab("Pour Point Intensity [mm/min]")

tblwithintgraph <- data.frame()
for(t in 1:length(unique(findat$tmint))){
  subt <- findat[findat$tmint == unique(findat$tmint)[t],]
  subt <- subt[complete.cases(subt),]
  for(i in 1:length(unique(subt$gauge))){
    sub <- subt[subt$gauge == unique(subt$gauge)[i],]
    sublm <- lm(PPri~avgri, data = sub)
    tblwithintgraph <- rbind(tblwithintgraph, data.frame(tmint = sub$tmint[1], gauge = sub$gauge[1], r2 = summary(sublm)$r.squared, 
                                                         adr2 = summary(sublm)$adj.r.squared,
                                                         slp = summary(sublm)$coefficients[2,1],
                                                         rmse = summary(sublm)$sigma))
  }
}
##so these are events where the 3standard deviation was greater than the events that were taking place bitch
topanl <- rbind(rf,thrfN, thrfS,C9C3[,-4], C835[,-4], CA92[,-4], E770[,-4], F333[,-4])
topanl$gt <- "TF"
topanl$gt[nchar(topanl$gauge) == 4] <- "RF"
topanl$gt[nchar(topanl$gauge) == 12] <- "PP"
for(i in 1:length(unique(rfbfact$event))){
  sub <- topanl[topanl$events == unique(rfbfact$event)[i],]
  subtop <- data.frame()
  areag <- actrfdf[actrfdf$event == unique(rfbfact$event)[i]]
  for(j in 1:length(unique(sub$name))){
    subg <- sub[sub$name == unique(sub$name)[j],]
    subg$cmm <- cumsum(subg$mm)
    subtop <- rbind(subtop, subg)
  }
  subtop <- rbind(subtop, data.frame(name = "AvgTF", time = subtop$time[subtop$gt = "AvTF"], mm = subtop$mm[subtop$gt = "TF"],
                                     events = subtop$events[subtop$gt = "TF"], cmm = subtop$cmm[subtop$gt = "TF"][order(subtop$time[subtop$gt = "TF"])]))
  subtop <- rbind(subtop, data.frame(name = "AvgRF", time = subtop$time[subtop$gt = "AvRF"], mm = subtop$mm[subtop$gt = "RF"],
                                     events = subtop$events[subtop$gt = "RF"], cmm = subtop$cmm[subtop$gt = "RF"][order(subtop$time[subtop$gt = "RF"])]))
  g1 <- ggplot(subtop, aes(time, cmm)) + geom_line(aes(col = gt, alpha = gt)) + 
    scale_color_manual(values = c("RF"="grey", "TF"="grey","AvTF" = "green","AvRF" = "blue","PP"="brown")) +
    scale_alpha_manual(values = c("RF"= 0.5, "TF"=0.2,"AvTF" = 1,"AvRF" = 1,"PP"=1)) + 
    geom_area
  
}


with(actrfdf, tapply(actrf, list(tmint, gauge), sd))

for(i in 1:length(unique(rfbfact$event))){
  subrf <- rfbfact[rfbfact$event == unique(rfbfact$event)[i],]
  g1 <- ggplot(subrf, aes(timx, intensity)) + geom_line(aes(col = gauge)) + facet_wrap(.~tmint) + theme_classic()
  print(g1)
}


library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

smtry <- evlst[[4]]
evchs <- 5
smtop <- smtry[smtry$events == evchs,]
smtop$NDP[smtop$NDP == "4+5"] <- "4"
smtop$NDP <- paste("NDP",smtop$NDP)
smtop <- smtop[,c(1,4,5,8,9)]
rftop <- rf[rf$events == evchs,]
for(i in 1:length(unique(rftop$name))){
  if(i == 1){
    rftpoact <- rftop[rftop$name == unique(rftop$name)[i],]
  }else{
    rftpoact <- merge(rftpoact, rftop[rftop$name == unique(rftop$name)[i],], by = "time", all = TRUE)
  }
}
rftpoact <- rftpoact[,c(1,3,6,9)]
for(i in 1:ncol(rftpoact)){
  rftpoact[is.na(rftpoact[,i]),i] <- 0
}
finrftop <- data.frame()
for(i in 1:length(unique(hour(rftpoact$time)))){
  sub <- rftpoact[hour(rftpoact$time)==unique(hour(rftpoact$time))[i],]
  for(j in 1:length(unique(minute(sub$time)))){
    submin <- sub[minute(sub$time)==unique(minute(sub$time))[j],]
    dep <- mean(apply(submin[,2:4], 2, sum))
    finrftop <- rbind(finrftop, data.frame(time = (submin$time[1]-second(submin$time[1])+ 30), depth = dep))
  }
}
library(ggpubr)
theme_set(theme_pubr())
smplt <- ggplot(smtop[smtop$delVWC >= -0.4 & smtop$delVWC <= 0.4,],aes(time,Depth)) + geom_tile(aes(fill = delVWC)) + theme_classic() +
  scale_fill_gradient2(name = expression(paste(Delta, "VWC"))) + ylab("Depth [cm]") + xlab("Time") + 
  scale_y_continuous(trans=reverselog_trans(10)) + 
  facet_grid(NDP~.)
rfplt <- ggplot(finrftop, aes(time, depth)) + geom_bar(stat= "identity", col = "blue") + theme_classic() + xlim(min(smtop$time), 
                                                                                                                (max(smtop$time)+17800)) +
  ylab("Rain per minute [mm]") + xlab(element_blank()) +   ggtitle(paste("Storm on", as.character(as.Date(evtim[[1]][evchs]))))
figure <- ggarrange(rfplt, smplt, heights = c(1, 3),
                    ncol = 1, nrow = 2)
figure

ggplot(smtop[smtop$delVWC >= -0.4 & smtop$delVWC <= 0.4,],aes(time, -logdep)) + geom_tile(aes(fill = delVWC)) + theme_classic() +
scale_fill_gradient2(name = expression(paste(Delta, "VWC"))) + ylab(expression(paste(log[10], "(z)"))) +
facet_grid(NDP~.) + ggtitle(paste("Storm", "on", as.character(as.Date(evtim[[1]][evchs])))) +


ggplot(smtop[smtop$delVWC >= -0.4 & smtop$delVWC <= 0.4 & smtop$NDP == "NDP 2",],aes(time, -logdep)) + geom_tile(aes(fill = delVWC)) + theme_classic() + 
  scale_fill_gradient2(name = expression(paste(Delta, "VWC"))) + ylab(expression(paste(log[10], "(z)"))) + 
  ggtitle(paste("Storm", "on", as.character(as.Date(evtim[[1]][evchs])))) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab (element_blank())


cnrtl <- evlst[[6]]
theintdf <- data.frame()
for(i in 1:length(unique(smtry$NDP))){
  subNDP <- smtry[smtry$NDP == unique(smtry$NDP)[i],]
  trmvNDP <- evrmv[,c((2+((i-1)*3)):(1+((i)*3)))]
  for(j in 1:length(unique(subNDP$Depth))){
    subdep <- subNDP[subNDP$Depth == unique(subNDP$Depth)[j],]
    trmvev <- trmvNDP[,grep(unique(subNDP$Depth)[j], names(trmvNDP))[1]]
    change <- rep(F, length(trmvev))
    if(names(trmvNDP)[grep(unique(subNDP$Depth)[j], names(trmvNDP))[1]] == "NDP1_5"){
      change[trmvev == "NOK" & evrmv[,5] != "NOK" & evrmv[,8] != "NOK"] <- T
      trmvev[trmvev == "NOK" & evrmv[,5] != "NOK" & evrmv[,8] != "NOK"] <- "OK"
    }
    for(k in 1:length(unique(subdep$events))){
      if(trmvev[unique(subdep$events)[k]] == "OK" | trmvev[unique(subdep$events)[k]] == "Great"){
        subev <- subdep[subdep$events == unique(subdep$events)[unique(subdep$events)[k]],]
        subev <- subev[(l*60):nrow(subev),]
        subev <- subev[complete.cases(subev),]
        if(nrow(subev)!=0){
          if(change[subev$events[1]]){
            subcntrl <- cnrtl[cnrtl$events == subev$events[1],]
            subcntrl <- subcntrl[subcntrl$time >= min(subev$time) & subcntrl$time <= max(subev$time),]
            subev$delVWC <- subev$dpdelvwc-subcntrl[,4]
            subev$cntrldelvwc <- subcntrl[,4]
          }
          intg <- sum(diff(subev$delVWC),na.rm = T)
          sumpp <- sum(diff(subev$dpdelvwc),na.rm = T)
          sumcn <- sum(diff(subev$cntrldelvwc),na.rm = T)
          theintdf <- rbind(theintdf, data.frame(NDP = unique(smtry$NDP)[i],Depth = unique(subNDP$Depth)[j], 
                                                 events = unique(subdep$events)[k], rf = evlst[[1]][unique(subdep$events)[k]], 
                                                 ans = intg, infpp = sumpp, incntrl = sumcn))}
      }
    }
  }
}

theintdf <- theintdf %>%  mutate(multd1 = case_when(
    Depth == 5 ~ 13.75,
    Depth == 22.5 ~ 52.5,
    Depth == 100 ~ 38.75))
theintdf$depans1 <- theintdf$ans*theintdf$multd1
theintdf$infppcm <- theintdf$infpp*theintdf$multd1
theintdf$infcntrlcm <- theintdf$incntrl*theintdf$multd1

ggplot(theintdf, aes(x = Depth, y = depans1, group = Depth)) + geom_boxplot() + facet_wrap(.~NDP, scales = "free") +
  theme_classic() + scale_y_continuous(trans = pseudolog10_trans) + geom_hline(yintercept = 0) +
  ylab(expression(paste("Integration of ", Delta, "WC over storm interval")))
ggplot(theintdf, aes(rf, depans1*10)) + geom_hline(yintercept = 0, alpha = 0.2) + geom_point(shape = 21, size = 3, aes(fill = factor(Depth))) + theme_classic() + 
  xlab("Rainfall [mm]") + ylab("Excess Infiltration [mm]") + scale_fill_manual(values = c("5"= "skyblue3", "22.5" = "dodgerblue3", "100" = "blue4"), name = "Depth [cm]")
ggplot(theintdf, aes(rf, infcntrlcm*10)) + geom_hline(yintercept = 0, alpha = 0.2) + geom_point(shape = 21, size = 3, aes(fill = factor(Depth))) + theme_classic() + 
   xlab("Rainfall [mm]") + ylab("Control Infiltration [mm]") + scale_fill_manual(values = c("5"= "skyblue3", "22.5" = "dodgerblue3", "100" = "blue4"), name = "Depth [cm]")
ggplot(theintdf, aes(rf, infppcm*10)) + geom_hline(yintercept = 0, alpha = 0.2) + geom_point(shape = 21, size = 3, aes(fill = factor(Depth))) + theme_classic() + 
  xlab("Rainfall [mm]") + ylab("Pour Point Infiltration [mm]") + scale_fill_manual(values = c("5"= "skyblue3", "22.5" = "dodgerblue3", "100" = "blue4"), name = "Depth [cm]")


topsm <- data.frame(EWcm = with(theintdf, tapply( depans1, NDP, sum)), NDP = names(with(theintdf, tapply( depans1, NDP, sum))))

topsm$NDP[topsm$NDP=="4+5"] <- "4"
topsm$NDP <- paste("NDP",topsm$NDP)
ggplot(topsm, aes(NDP, EWcm)) + geom_bar(fill = "darkblue",stat = "identity") + theme_classic() + ylab("Total excess Water infiltrated for 32 cm rain [cm]")

nwimpsm <- melt(data.frame(NDP = c("NDP1","NDP2","NDP4"), Rainfall = with(theintdf,tapply( rf, NDP, sum)), Control = with(theintdf,tapply( infcntrlcm, NDP, sum))*10, PP = with(theintdf,tapply( infppcm, NDP, sum))*10), id = c("NDP", "Rainfall"))
nwimpsm$ratio <- nwimpsm$value/nwimpsm$Rainfall
ggplot(nwimpsm, aes(NDP, ratio)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity", color = "black") + 
  scale_fill_manual(name = element_blank(), labels = c("Control" = "Control", "PP" = "Pour Point"),
                    values = c("Control" = "forestgreen", "PP" = "gold")) + theme_classic() + xlab(element_blank()) + 
  ylab(expression(sum(frac("Infiltration", "Rainfall"))))

brgr1 <- rbind(C835, CA92, F333)
brgr1 <- brgr1 %>%
  mutate(obs = case_when(
    name == "C835D23D88AA" ~ "PPCT",
    name == "CA9263744FA0" ~ "PPFT",
    name == "F33345B90487" ~ "SF"
  ))
chsev <- c(18,19,22,26,28,29,31,33,36,37,38,45,49,60,64,95,96,110,129,131,139,140)

ansdf <- data.frame()
sub <- brgr1[brgr1$events %in% chsev,]
ansdf <- as.data.frame(tapply(sub$mm, list(sub$events, sub$obs), sum))
tousedf <- data.frame()
for(i in 1:length(chsev)){
    subev <- sub[sub$events == chsev[i],]
    wrkdf <- data.frame()
    for(j in 1:3){
      subg <- subev[sub$obs == unique(sub$obs)[j],]
      subg$csm <-  cumsum(subg$mm)
      wrkdf <- rbind(wrkdf, subg)
    }
    ref <- wrkdf[subev$obs == "SF",c(2,7)]
    alt <- unique(wrkdf$time[subev$obs != "SF"])
    ref <- rbind(ref, data.frame(time = alt, csm = NA))
    ref <- ref[order(ref$time),]
    zrchck <- T
    for(k in 1:nrow(ref)){
      if(is.na(ref$csm[k])){
        if(zrchck){
          ref$csm[k] <- 0 
        }
        else{
          ref$csm[k] <- val
        }
      }
      else{
        zrchck <- F
        val <- ref$csm[k]
      }
    }
    ref <- ref[!duplicated(ref),]
    names(ref)[2] <- "nrml"
    dyn1 <- merge(wrkdf,ref)
    dyn1$difffrmrf <- dyn1$csm-dyn1$nrml
    dyn1 <- dyn1[order(dyn1$obs),]
    g2 <- ggplot(dyn1, aes(time, difffrmrf, group = obs, col = obs)) + 
      geom_line(aes(colour = obs)) + theme_classic() + scale_color_discrete(c("brown", "orange", "darkorange")) +
      xlab("Time") + ylab("Difference from Stemflow")
    # jpeg(paste("SF Diff", as.character(chsev[i]), "Gauges.jpg"), width = 800, height = 800)
    print(g2)
    # dev.off()
    tousedf <- rbind(tousedf, dyn1)
}

##just a different analysis
smtoptry <- data.frame()
for(j in 1:length(unique(sm$NDP))){
    subsmNDP <- sm[sm$NDP == unique(sm$NDP)[j],]
    for(k in 1:length(unique(subsmNDP$Depth))){
      subsmNDPdep <- subsmNDP[subsmNDP$Depth == unique(subsmNDP$Depth)[k],]
      cntrl <- subsmNDPdep[subsmNDPdep$DP != "DP",]
      names(cntrl)[4] <- "cntrlVWC"
      dp <- subsmNDPdep[subsmNDPdep$DP == "DP",]
      names(dp)[4] <- "dpVWC"
      ans <- merge(cntrl[,c(2,4,5,6,8,9)], dp[,c(2,4,5,6,8,9)])
      smtoptry <- rbind(smtoptry, ans)
    }
}

for(i in 1:max(smtoptry$events, na.rm = T)){
   subev <- smtoptry[smtoptry$time >= (evtim[[1]][i]+(l*60*60)),]
   for(j in 1:length(unique(subev$NDP))){
     for(k in 1:length(unique(subsmNDP$Depth))){
       subsmNDPdep <- subsmNDP[subsmNDP$Depth == unique(subsmNDP$Depth)[k] & subsmNDP$NDP == unique(subsmNDP$NDP)[j],]
       if(nrow(cntrl) != 0){
         if(as.character(subsmNDPdep$variable[!is.na(subsmNDPdep$variable)][1]) == "sm11"){
           cntrl$cntrldelvwc <- -cntrl$VWC + cntrl$VWC[1]
         }else{
           cntrl$cntrldelvwc <- cntrl$VWC - cntrl$VWC[1]
         }
         if(cntrl[complete.cases(cntrl),][1,1] == 8){
           if(max(cntrl$cntrldelvwc, na.rm = T)>0.4){
             cntrl$cntrldelvwc <- NA
           }
         }
       }else{
         cntrl$cntrldelvwc <- cntrl$VWC - cntrl$VWC[1]
       }
       dp <- subsmNDPdep[subsmNDPdep$DP == "DP",]
       dp$dpdelvwc <- dp$VWC - dp$VWC[1]
       ans <- merge(cntrl[,c(2,5,6,8,9,10)], dp[,c(2,5,6,8,9,10)])
       ans$delVWC <- ans$dpdelvwc - ans$cntrldelvwc
       smtop <- rbind(smtop, ans)
     }
   }
}

