##Drip point graphs
library("ggpattern")
library(lubridate)
library(ggplot2)
library(reshape2)
library(moments)
library(robustbase)
library(vegan)
library(sp)
library(gstat)
library(aqp)
library(zoo)
library(strucchange)
##nik's request
top <- read.csv("H:/Downloads/plot-data.csv")
ggplot(top, aes(total.gauges, tfper.if.greater.gauges.are.missed)) + geom_point(size = 2) + theme_classic() + 
  xlab("Number of gauges sampled (lowest to highest)") + ylab(expression(paste("Sample Estimate ",frac("Throughfall", "Rainfall"),"%")))
top$try <- c(5,diff(top$tfper.if.greater.gauges.are.missed))
ggplot(top, aes(total.gauges, try/number.of.gauges)) + geom_point(size = 2) + theme_classic() + 
   xlab("Number of gauges sampled (lowest to highest)") + ylab(expression(paste("Additional ",frac("Throughfall", "Rainfall"),"% per gauge")))
argcal  <- read.table("H:/My Documents/Project and related work/banksia at gingin/FNL202203.PRN", quote="\"", comment.char="")
argcal$time <- ymd_hms(paste("2022-04-01",substr(argcal$V4, 1,2), substr(argcal$V4, 3,4), argcal$V5))
argcal$DP <-argcal$V7
argcal$TF <- argcal$V8*0.2
argcal$event <- NA
argcal$event[argcal$time >= ymd_hms("2022-04-01 15:19:00")  & argcal$time < ymd_hms("2022-04-01 15:24:00")] <- 1
argcal$event[argcal$time >= ymd_hms("2022-04-01 15:24:00")  & argcal$time < ymd_hms("2022-04-01 15:26:30")] <- 2
argcal$event[argcal$time >= ymd_hms("2022-04-01 15:26:30")  & argcal$time < ymd_hms("2022-04-01 15:29:00")] <- 3
argcal$event[argcal$time >= ymd_hms("2022-04-01 15:29:00")  & argcal$time < ymd_hms("2022-04-01 15:31:30")] <- 4
argcal$event[argcal$time >= ymd_hms("2022-04-01 15:31:30")  & argcal$time < ymd_hms("2022-04-01 15:34:00")] <- 5
for(i in 1:max(argcal$event, na.rm = T)){
  sub <- argcal[argcal$event == i,10:13]
  sub <- sub[complete.cases(sub),]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP) 
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(as.character(i))+ theme_classic()
  # print(g1)
}
dpcal <- tapply(argcal$DP, argcal$event, sum)
tfcal <- tapply(argcal$TF, argcal$event, sum)
tfcal[5] <- 1
sa <- pi*20.3^2/4
calh <- round(c(1000,500,250,124,64)/sa*10,1)
tfcal/calh
dpcal/calh

callc <- read.csv("H:/My Documents/Project and related work/banksia at gingin/callc.csv")
calmod <- lm(Water~Signal, callc)


dat <- read.csv("~/Project and related work/banksia at gingin/TRIAL BRANCH 20211215.PRN", header=FALSE)
dat$time <- ymd_hms(paste("2021-12-15",substr(dat$V4, 1,2), substr(dat$V4, 3,4), dat$V5))
dat$DP <-dat$V7*2
dat$TF <- dat$V8*0.4
dat$V6[dat$V6 == -6999] <- NA
dat$storage <- (dat$V6*-6.774)-(6.774*0.044)
dat$event <- NA
dat$event[dat$time < ymd_hms("2021-12-15 12:00:00")] <- "XYcal1"
dat$event[dat$time >= ymd_hms("2021-12-15 12:00:00") & dat$time < ymd_hms("2021-12-15 12:30:00")] <- "XYcal2"
dat$event[dat$time >= ymd_hms("2021-12-15 12:30:00") & dat$time < ymd_hms("2021-12-15 14:00:00")] <- "XYcal3"
dat$event[dat$time >= ymd_hms("2021-12-15 14:00:00") & dat$time < ymd_hms("2021-12-15 15:00:00")] <- "Br1Tr01"
dat$event[dat$time >= ymd_hms("2021-12-15 15:00:00") & dat$time < ymd_hms("2021-12-15 15:59:30")] <- "Br1Tr02"
dat$event[dat$time >= ymd_hms("2021-12-15 15:59:30") & dat$time < ymd_hms("2021-12-15 16:50:00")] <- "Br1Tr03"
dat$event[dat$time >= ymd_hms("2021-12-15 16:50:00")] <- "Br1Tr04"
for(i in 1:length(unique(dat$event))){
  sub <- dat[dat$event == unique(dat$event)[i],10:13]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP) 
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(unique(dat$event)[i])+ theme_classic()
  print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(unique(dat$event)[i])+ theme_classic() + ylim(6,8)
  print(g2)
}

dat2 <- read.table("~/Project and related work/banksia at gingin/TRIAL BRANCH 202112116.PRN", quote="\"", comment.char="")
dat2$time <-ymd_hms(paste("2021-12-16",substr(dat2$V4, 1,2), substr(dat2$V4, 3,4), dat2$V5))
dat2$DP <-dat2$V7*2
dat2$TF <- dat2$V8*0.4
dat2$V6[dat2$V6 == -6999] <- NA
dat2$storage <- (dat2$V6*-6.774)-(6.774*0.043)
dat2$event <- NA
dat2$event[dat2$time < ymd_hms("2021-12-16 11:54:00")] <- "Br1Tr05"
dat2$event[dat2$time >= ymd_hms("2021-12-16 11:54:00") & dat2$time < ymd_hms("2021-12-16 12:48:00")] <- "Br1Tr06"
dat2$event[dat2$time >= ymd_hms("2021-12-16 12:48:00") & dat2$time < ymd_hms("2021-12-16 13:44:29")] <- "Br1Tr07"
dat2$event[dat2$time >= ymd_hms("2021-12-16 13:44:30") & dat2$time < ymd_hms("2021-12-16 15:28:00")] <- "Br1Tr08"
dat2$event[dat2$time >= ymd_hms("2021-12-16 15:28:00") & dat2$time < ymd_hms("2021-12-16 16:14:00")] <- "Br1Tr09"
dat2$event[dat2$time >= ymd_hms("2021-12-16 16:14:00") & dat2$time < ymd_hms("2021-12-16 16:58:00")] <- "Br1Tr10"
dat2$event[dat2$time >= ymd_hms("2021-12-16 16:58:00")] <- "Br1Tr11"
for(i in 1:length(unique(dat2$event))){
  sub <- dat2[dat2$event == unique(dat2$event)[i],10:13]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP) 
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(unique(dat2$event)[i])+ theme_classic()
  print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(unique(dat2$event)[i])+ theme_classic() + ylim(5,7)
  print(g2)
}

dat3 <- read.table("~/Project and related work/banksia at gingin/TRIAL BRANCH 202112120.PRN", quote="\"", comment.char="")
dat3$time <-ymd_hms(paste("2021-12-20",substr(dat3$V4, 1,2), substr(dat3$V4, 3,4), dat3$V5))
dat3$DP <-dat3$V7*2
dat3$TF <- dat3$V8*0.4
dat3$V6[dat3$V6 == -6999] <- NA
dat3$storage <- (dat3$V6*6.774)-(6.774*0.043)
dat3$event <- NA
dat3$event[dat3$time < ymd_hms("2021-12-20 11:54:00")] <- "Br1Tr12"
dat3$event[dat3$time >= ymd_hms("2021-12-20 11:54:00") & dat3$time < ymd_hms("2021-12-20 12:38:00")] <- "Br1Tr13"
dat3$event[dat3$time >= ymd_hms("2021-12-20 12:38:00") & dat3$time < ymd_hms("2021-12-20 13:00:00")] <- "Br1Tr14"
dat3$event[dat3$time >= ymd_hms("2021-12-20 13:00:00") & dat3$time < ymd_hms("2021-12-20 13:44:00")] <- "Br1Tr15"
dat3$event[dat3$time >= ymd_hms("2021-12-20 13:44:00") & dat3$time < ymd_hms("2021-12-20 14:50:00")] <- "Br1Tr16"
dat3$event[dat3$time >= ymd_hms("2021-12-20 14:50:00") & dat3$time < ymd_hms("2021-12-20 15:28:00")] <- "Br1Tr17"
dat3$event[dat3$time >= ymd_hms("2021-12-20 15:28:00") & dat3$time < ymd_hms("2021-12-20 16:00:00")] <- "Br1Tr18"
dat3$event[dat3$time >= ymd_hms("2021-12-20 16:00:00") & dat3$time < ymd_hms("2021-12-20 17:30:00")] <- "Br1Tr19"
cal <- dat3[dat3$time >= ymd_hms("2021-12-20 17:30:00") & dat3$time < ymd_hms("2021-12-20 18:20:00"),]
dat3 <- dat3[dat3$time< ymd_hms("2021-12-20 17:30:00"),]
for(i in 1:length(unique(dat3$event))){
  sub <- dat3[dat3$event == unique(dat3$event)[i],10:13]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP) 
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(unique(dat3$event)[i])+ theme_classic()
  print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(unique(dat3$event)[i])+ theme_classic() + ylim(5,7)
  print(g2)
}
DPtuse <- c(tapply(dat$DP, dat$event, sum)[c(5:7,1:4)],tapply(dat2$DP, dat2$event, sum),tapply(dat3$DP, dat3$event, sum))
TFtuse <- c(tapply(dat$TF, dat$event, sum)[c(5:7,1:4)],tapply(dat2$TF, dat2$event, sum),tapply(dat3$TF, dat3$event, sum))


TFrd <- read.csv("H:/My Documents/Project and related work/banksia at gingin/TFGrid.csv")
TFrd <- TFrd[!is.na(TFrd$X),]
cal2use <- TFrd[TFrd$Trial == "XYcal3",]
cal2use$Vol <- cal2use$Vol/10.8/10.8/pi*40
cal2use$Vol[cal2use$Comments == "DP"] <- DPtuse[3]
cal2use$Vol[cal2use$Comments == "TF"] <- TFtuse[3]
cal2use$Vol <- cal2use$Vol/mean(cal2use$Vol[cal2use$X == 0])
cal2use <- cal2use[,1:3]
names(cal2use)[3] <- "Calratio"
TFmm <- data.frame()
TFnrm <- data.frame()
for(i in 1:length(unique(TFrd$Trial))){
  sub <- TFrd[TFrd$Trial == unique(TFrd$Trial)[i],1:7]
  sub$Vol <- sub$Vol/10.8/10.8/pi*40
  sub$Vol[sub$Comments == "DP"] <- DPtuse[i]
  sub$Vol[sub$Comments == "TF"] <- TFtuse[i]
  # g1 <- ggplot(sub, aes(X,Y,z = Vol)) + geom_contour_filled() + theme_classic() + ggtitle(unique(TFrd$Trial)[i]) + geom_point(aes(col = Vol), size = 1, alpha = 0.5)
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' Gross volume.jpg'))
  # print(g1)
  # dev.off()
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  subnrm <- merge(subnrm,cal2use)
  subnrm$rexcprf <- subnrm$Vol/subnrm$Calratio
  subnrm$X[subnrm$X != 0] <- (subnrm$X[subnrm$X != 0]+1)*30
  subnrm$X[subnrm$X == 0] <- 10
  subnrm$Y <- sub$Y*10
  g2 <- ggplot(subnrm, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle(unique(TFrd$Trial)[i]) 
  
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' normalized volume.jpg'))
  print(g2)
  # dev.off()
  TFmm <- rbind(TFmm, sub)
  TFnrm <- rbind(TFnrm, subnrm)
}
evdet <- read.csv("H:/My Documents/Project and related work/banksia at gingin/eventdets.csv")

##Tree 3
dattr2 <- read.table("H:/My Documents/Project and related work/banksia at gingin/TREE2202202.PRN", quote="\"", comment.char="")
dattr2$time <- with(dattr2, strptime(paste(V2, V3, V4, V5), format = "%Y %j %H%M %S"))
dattr2$DP <- dattr2$V7*2
dattr2$TF <- dattr2$V8*0.4
dattr2$V6[dattr2$V6 == -6999] <- NA
dattr2$storage <- (dattr2$V6*-6.774)-(6.774*0.044)
dattr2$event <- NA
tr3evdt <- grep("Br3",evdet$Trial)
tr3evdt <- c(tr3evdt,tr3evdt[length(tr3evdt)]+1:3)
for(i in 1:length(tr3evdt)){
  if(i == 1){
    dattr2$event[dattr2$time >= dmy_hm(evdet$Datetime[tr3evdt[i]], tz = "Australia/Perth") & 
                   dattr2$time <= dmy_hm(evdet$Datetime[tr3evdt[i+1]], tz = "Australia/Perth")] <- evdet$Trial[tr3evdt[i]]
  }else if(i == length(tr3evdt)){
    dattr2$event[dattr2$time >= dmy_hm(evdet$Datetime[tr3evdt[i]], tz = "Australia/Perth")] <- evdet$Trial[tr3evdt[i]]
  }else if(i == 15){
    dattr2$event[dattr2$time >= dmy_hm(evdet$Datetime[tr3evdt[i]], tz = "Australia/Perth") & 
                   dattr2$time <= dmy_hm("23/02/2022 15:30")] <- evdet$Trial[tr3evdt[i]]
  }
  else{
    d1 <- as.Date(dmy_hm(evdet$Datetime[tr3evdt[i]]))
    dattr2$event[(dattr2$time >= dmy_hm(evdet$Datetime[tr3evdt[i]], tz = "Australia/Perth") & 
                   dattr2$time <= dmy_hm(evdet$Datetime[tr3evdt[i+1]], tz = "Australia/Perth")) & as.Date(dattr2$time) == d1] <- evdet$Trial[tr3evdt[i]]
  }
}

for(i in 1:length(tr3evdt)){
  sub <- dattr2[dattr2$event == evdet$Trial[tr3evdt[i]],10:13]
  sub <- sub[!is.na(sub$TF),]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP)
  sub$time <- as.POSIXct(sub$time)
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(as.character(i))+ theme_classic()
  print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(evdet$Trial[tr3evdt[i]])+ theme_classic() + ylim(0,0.8)
  print(g2)
}

DPtusetr3 <- tapply(dattr2$DP, dattr2$event, sum)
TFtusetr3 <- tapply(dattr2$TF, dattr2$event, sum)

TFrdtr3 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/TFGrid_Tree3.csv")
TFrdtr3 <- TFrdtr3[!is.na(TFrdtr3$X),]
TFrdtr3$Vol <- TFrdtr3$Vol/10.8/10.8/pi*40
TFrdtr3$Vol[TFrdtr3$Comments == "DP"] <- DPtusetr3
TFrdtr3$Vol[TFrdtr3$Comments == "TF"] <- TFtusetr3
xycal5 <- TFrdtr3[grep(pattern = "XYcal", TFrdtr3$Trial),]
nrmxycal5 <- xycal5[grep(pattern = "XYcal4", xycal5$Trial),c(1,2,7)]
nrmxycal5$cal4 <- NA
nrmxycal5$cal5 <- NA
nrmxycal5$cal6 <- NA
for(i in 1:length(unique(xycal5$Trial))){
  sub <- xycal5[xycal5$Trial == unique(xycal5$Trial)[i],]
  nrmxycal5[,3+i] <- sub$Vol/mean(sub$Vol[sub$X == 0])
}
nrmxycal5$Vol <- apply(nrmxycal5[,4:6], 1, mean)
nrmxycal5$sd <- apply(nrmxycal5[,4:6], 1, sd)

##how much replication to be expected
xycalrg <- read.table("H:/My Documents/Project and related work/banksia at gingin/XYCAL202203.PRN", quote="\"", comment.char="")
xycalrg$time <- with(xycalrg, strptime(paste(V2, V3, V4, V5), format = "%Y %j %H%M %S"))
xycalrg$DP <- xycalrg$V7*2
xycalrg$TF <- xycalrg$V8*0.4
xycalrg$event <- NA
for(i in 100:112){
  xycalrg$event[xycalrg$time >= dmy_hm(evdet$Datetime[i], tz = "Australia/Perth") & 
                    xycalrg$time <= dmy_hm(evdet$Datetime[i+1], tz = "Australia/Perth")] <- evdet$Trial[i]
  if(i == 112){
    xycalrg$event[xycalrg$time >= dmy_hm(evdet$Datetime[i], tz = "Australia/Perth")] <- evdet$Trial[i]
  }
}

DPtusecal <- tapply(xycalrg$DP, xycalrg$event, sum)
TFtusecal <- tapply(xycalrg$TF, xycalrg$event, sum)

TFrdxyc <- read.csv("H:/My Documents/Project and related work/banksia at gingin/calibrationwrgloc.csv")
TFrdxyc <- TFrdxyc[!is.na(TFrdxyc$X),]
TFrdxyc$Vol <- TFrdxyc$Vol/10.8/10.8/pi*40
TFrdxyc$Vol[TFrdxyc$Comments == "DP"] <- DPtusecal
TFrdxyc$Vol[TFrdxyc$Comments == "TF"] <- TFtusecal
TFnrmxyc <- data.frame()
for(i in 1:length(unique(TFrdxyc$Trial))){
  sub <- TFrdxyc[TFrdxyc$Trial == unique(TFrdxyc$Trial)[i],1:7]
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  g2 <- ggplot(subnrm, aes(X,Y)) + geom_tile(aes(fill = Vol)) + theme_classic() + 
    ggtitle(unique(TFrdxyc$Trial)[i]) 
  print(g2)
  TFnrmxyc <- rbind(TFnrmxyc, subnrm)
}
nrmxycal5$Trial <- "XYcal456"
chckbk <- rbind(TFnrmxyc[,c(1,2,7,6,3)], nrmxycal5[,c(1,2,3,9,7)])
mncal <- tapply(chckbk$Vol, paste(chckbk$X,chckbk$Y), FUN = function(x)(mean(x, na.rm = T))) 
mncal <- data.frame(X = as.integer(substr(names(mncal), 1,1)), Y = as.integer(substr(names(mncal), 2,4)),
                    Comments = NA, Trial = "Mean_cal", Vol=mncal)
crc <- 0
for(i in 1:sum(is.na(chckbk$Vol))){
  sub <- chckbk[is.na(chckbk$Vol),1:2][i,]
  crc[i] <- mncal$Vol[mncal$X == sub$X & mncal$Y == sub$Y]
}
chckbk$Vol[is.na(chckbk$Vol)] <-  crc
chckbk <-  rbind(chckbk, data.frame(X = 8, Y = -3, Comments = "Taken from mean", Trial = "XYcal456", Vol = mncal$Vol[mncal$X == 8 & mncal$Y == -3]))
for(i in 1:length(unique(chckbk$Trial))){
  sub <- chckbk[unique(chckbk$Trial)[i] == chckbk$Trial,]
  if(sub$Y[sub$X == 1] == -1){
    chckbk <-  rbind(chckbk, data.frame(X = 1, Y = 0, Comments = "Taken from mean", Trial = sub$Trial[1], Vol = mncal$Vol[mncal$X == 1 & mncal$Y == 0]))
  }else{
    chckbk <-  rbind(chckbk, data.frame(X = 1, Y = -1, Comments = "Taken from mean", Trial = sub$Trial[1], Vol = mncal$Vol[mncal$X == 1 & mncal$Y == -1]))
  }
}

with(TFrdxyc,tapply(Vol*60/(Time_min+(Time_sec/60)), Trial, FUN = function(x) mean(x,na.rm = T)))

checkovrest <- matrix(data = NA, nrow= length(unique(TFnrmxyc$X)), ncol = length(unique(TFnrmxyc$Y)))
checkovrestTF <- matrix(data = NA, nrow= length(unique(TFnrmxyc$X)), ncol = length(unique(TFnrmxyc$Y)))
checkovrestDP <- matrix(data = NA, nrow= length(unique(TFnrmxyc$X)), ncol = length(unique(TFnrmxyc$Y)))
for(i in 1:length(unique(TFnrmxyc$X))){
  for(j in 1:length(unique(TFnrmxyc$Y))){
    checkovrest[i,j] <- mean(TFnrmxyc$Vol[TFnrmxyc$X == unique(TFnrmxyc$X)[i] & TFnrmxyc$Y == unique(TFnrmxyc$Y)[j] & TFnrmxyc$Comments == ""],na.rm=T)
    checkovrestTF[i,j] <- mean(TFnrmxyc$Vol[TFnrmxyc$X == unique(TFnrmxyc$X)[i] & TFnrmxyc$Y == unique(TFnrmxyc$Y)[j] & TFnrmxyc$Comments == "TF"],na.rm=T)
    checkovrestDP[i,j] <- mean(TFnrmxyc$Vol[TFnrmxyc$X == unique(TFnrmxyc$X)[i] & TFnrmxyc$Y == unique(TFnrmxyc$Y)[j] & TFnrmxyc$Comments == "DP"],na.rm=T)
  }
}

unicoef <- 0
for(i in 1:length(unique(TFrdxyc$Trial))){
  unicoef[i] <- 100*(1-(sum(abs(TFrdxyc$Vol[TFrdxyc$Trial == unique(TFrdxyc$Trial)[i]]-mean(TFrdxyc$Vol[TFrdxyc$Trial == unique(TFrdxyc$Trial)[i]], na.rm =T))/sum(!is.na(TFrdxyc$Vol[TFrdxyc$Trial == unique(TFrdxyc$Trial)[i]])))/mean(TFrdxyc$Vol[TFrdxyc$Trial == unique(TFrdxyc$Trial)[i]], na.rm =T)))
}
##treethrf
TFrdtr3 <- TFrdtr3[grep(pattern = "Br3Tr", TFrdtr3$Trial),]
TFmmtr3 <- data.frame()
TFnrmtr3 <- data.frame()
for(i in 1:length(unique(TFrdtr3$Trial))){
  sub <- TFrdtr3[TFrdtr3$Trial == unique(TFrdtr3$Trial)[i],1:7]
  sub$Vol[sub$Comments == "DP"] <- DPtusetr3[i]
  sub$Vol[sub$Comments == "TF"] <- TFtusetr3[i]
  # g1 <- ggplot(sub, aes(X,Y,z = Vol)) + geom_contour_filled() + theme_classic() + ggtitle(unique(TFrd$Trial)[i]) + geom_point(aes(col = Vol), size = 1, alpha = 0.5)
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' Gross volume.jpg'))
  # print(g1)
  # dev.off()
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  ggloc <- sub[sub$Comments == "DP" | sub$Comments == "TF",1:2]
  for(j in 1:length(unique(chckbk$Trial))){
    subchckbk <- chckbk[chckbk$Trial == unique(chckbk$Trial)[j],]
    calloc <- subchckbk[subchckbk$Comments == "DP" | subchckbk$Comments == "TF",1:2]
    if(sum(ggloc == calloc) == 4){
      cal2use <- subchckbk[,c(1,2,5)]
      names(cal2use)[3] <- "Calratio"
    }
  }
  subnrm <- merge(subnrm,cal2use)
  subnrm$rexcprf <- subnrm$Vol/subnrm$Calratio
  subnrm$X[subnrm$X != 0] <- (subnrm$X[subnrm$X != 0]+4)*20
  subnrm$X[subnrm$X == 0] <- 10
  subnrm$Y <- sub$Y*10
  g2 <- ggplot(subnrm, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle(unique(TFrdtr3$Trial)[i]) 
  
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' normalized volume.jpg'))
  print(g2)
  # dev.off()
  TFmmtr3 <- rbind(TFmmtr3, sub)
  TFnrmtr3 <- rbind(TFnrmtr3, subnrm)
}


##Tree 4
dattr4 <- read.table("H:/My Documents/Project and related work/banksia at gingin/TREE4202203.PRN", quote="\"", comment.char="")
dattr4$time <- with(dattr4, strptime(paste(V2, V3, V4, V5), format = "%Y %j %H%M %S"))
dattr4$DP <- dattr4$V7*2
dattr4$TF <- dattr4$V8*0.4
dattr4$V6[dattr4$V6 == -6999] <- NA
dattr4$storage <- (dattr4$V6*-6.774)-(6.774*0.043)

dattr4$event <- NA
tr4evdt <- grep("Br4",evdet$Trial)
for(i in 1:length(tr4evdt)){
  if(i == 1){
    dattr4$event[dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth") & 
                   dattr4$time <= dmy_hm(evdet$Datetime[tr4evdt[i+1]], tz = "Australia/Perth")] <- evdet$Trial[tr4evdt[i]]
  }else if(i == length(tr4evdt)){
    dattr4$event[dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth")] <- evdet$Trial[tr4evdt[i]]
  }else{
    d1 <- as.Date(dmy_hm(evdet$Datetime[tr4evdt[i]]))
    dattr4$event[(dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth") & 
                    dattr4$time <= dmy_hm(evdet$Datetime[tr4evdt[i+1]], tz = "Australia/Perth")) & as.Date(dattr4$time) == d1] <- evdet$Trial[tr4evdt[i]]
  }
}
for(i in 1:length(tr4evdt)){
  sub <- dattr4[dattr4$event == evdet$Trial[tr4evdt[i]],10:13]
  sub <- sub[!is.na(sub$TF),]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP)
  sub$time <- as.POSIXct(sub$time)
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(as.character(i))+ theme_classic()
  # print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(evdet$Trial[tr3evdt[i]])+ theme_classic() + ylim(0.2,0.4)
  # print(g2)
}

DPtusetr4 <- tapply(dattr4$DP, dattr4$event, sum)
TFtusetr4 <- tapply(dattr4$TF, dattr4$event, sum)

TFrdtr4 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/TFGrid_Tree4.csv")
TFrdtr4 <- TFrdtr4[!is.na(TFrdtr4$X),]
TFmmtr4 <- data.frame()
TFnrmtr4 <- data.frame()
for(i in 1:length(unique(TFrdtr4$Trial))){
  sub <- TFrdtr4[TFrdtr4$Trial == unique(TFrdtr4$Trial)[i],1:7]
  sub$Vol <- sub$Vol/10.8/10.8/pi*40
  sub$Vol[sub$Comments == "DP"] <- DPtusetr4[i]
  sub$Vol[sub$Comments == "TF"] <- TFtusetr4[i]
  # g1 <- ggplot(sub, aes(X,Y,z = Vol)) + geom_contour_filled() + theme_classic() + ggtitle(unique(TFrd$Trial)[i]) + geom_point(aes(col = Vol), size = 1, alpha = 0.5)
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' Gross volume.jpg'))
  # print(g1)
  # dev.off()
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  ggloc <- sub[sub$Comments == "DP" | sub$Comments == "TF",1:2]
  for(j in 1:length(unique(chckbk$Trial))){
    subchckbk <- chckbk[chckbk$Trial == unique(chckbk$Trial)[j],]
    calloc <- subchckbk[subchckbk$Comments == "DP" | subchckbk$Comments == "TF",1:2]
    if(sum(ggloc == calloc) == 4){
      cal2use <- subchckbk[,c(1,2,5)]
      names(cal2use)[3] <- "Calratio"
    }
  }
  subnrm <- merge(subnrm,cal2use)
  subnrm$rexcprf <- subnrm$Vol/subnrm$Calratio
  subnrm$X[subnrm$X != 0] <- (subnrm$X[subnrm$X != 0]+4)*20
  subnrm$X[subnrm$X == 0] <- 10
  subnrm$Y <- sub$Y*10
  g2 <- ggplot(subnrm, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle(unique(TFrdtr4$Trial)[i]) 
  
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' normalized volume.jpg'))
  print(g2)
  # dev.off()
  TFmmtr4 <- rbind(TFmmtr4, sub)
  TFnrmtr4 <- rbind(TFnrmtr4, subnrm)
}


##Tree 5-6
dattr5 <- read.table("H:/My Documents/Project and related work/banksia at gingin/TREE6202203.PRN", quote="\"", comment.char="")
dattr5$time <- with(dattr5, strptime(paste(V2, V3, V4, V5), format = "%Y %j %H%M %S"))
dattr5$DP <- dattr5$V7*2
dattr5$TF <- dattr5$V8*0.4
dattr5$V6[dattr5$V6 == -6999] <- NA
dattr5$storage <- (dattr5$V6*-6.774)-(6.774*0.044)

dattr5$event <- NA
tr5evdt <- grep("Br5",evdet$Trial)
for(i in 1:length(tr5evdt)){
  if(i == 1){
    dattr5$event[dattr5$time >= dmy_hm(evdet$Datetime[tr5evdt[i]], tz = "Australia/Perth") & 
                   dattr5$time <= dmy_hm(evdet$Datetime[tr5evdt[i+1]], tz = "Australia/Perth")] <- evdet$Trial[tr5evdt[i]]
  }else if(i == length(tr5evdt)){
    dattr5$event[dattr5$time >= dmy_hm(evdet$Datetime[tr5evdt[i]], tz = "Australia/Perth")] <- evdet$Trial[tr5evdt[i]]
  }else{
    d1 <- as.Date(dmy_hm(evdet$Datetime[tr5evdt[i]]))
    dattr5$event[(dattr5$time >= dmy_hm(evdet$Datetime[tr5evdt[i]], tz = "Australia/Perth") & 
                    dattr5$time <= dmy_hm(evdet$Datetime[tr5evdt[i+1]], tz = "Australia/Perth")) & 
                   as.Date(dattr5$time) == d1] <- evdet$Trial[tr5evdt[i]]
  }
}
for(i in 1:length(tr4evdt)){
  if(i == 1){
    dattr4$event[dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth") & 
                   dattr4$time <= dmy_hm(evdet$Datetime[tr4evdt[i+1]], tz = "Australia/Perth")] <- evdet$Trial[tr4evdt[i]]
  }else if(i == length(tr4evdt)){
    dattr4$event[dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth")] <- evdet$Trial[tr4evdt[i]]
  }else{
    d1 <- as.Date(dmy_hm(evdet$Datetime[tr4evdt[i]]))
    dattr4$event[(dattr4$time >= dmy_hm(evdet$Datetime[tr4evdt[i]], tz = "Australia/Perth") & 
                    dattr4$time <= dmy_hm(evdet$Datetime[tr4evdt[i+1]], tz = "Australia/Perth")) & as.Date(dattr4$time) == d1] <- evdet$Trial[tr4evdt[i]]
  }
}
for(i in 1:length(tr5evdt)){
  sub <- dattr5[dattr5$event == evdet$Trial[tr5evdt[i]],10:13]
  sub <- sub[!is.na(sub$TF),]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP)
  sub$time <- as.POSIXct(sub$time)
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(as.character(i))+ theme_classic()
  print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(evdet$Trial[tr5evdt[i]])+ theme_classic() + ylim(0.2,0.4)
   print(g2)
}

DPtusetr5 <- tapply(dattr5$DP, dattr5$event, sum)
TFtusetr5 <- tapply(dattr5$TF, dattr5$event, sum)

TFrdtr5 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/TFGrid_Tree5-6.csv")
TFrdtr5 <- TFrdtr5[!is.na(TFrdtr5$X),]
TFmmtr5 <- data.frame()
TFnrmtr5 <- data.frame()
for(i in 1:length(unique(TFrdtr5$Trial))){
  sub <- TFrdtr5[TFrdtr5$Trial == unique(TFrdtr5$Trial)[i],1:7]
  sub$Vol <- sub$Vol/10.8/10.8/pi*40
  sub$Vol[sub$Comments == "DP"] <- DPtusetr5[i]
  sub$Vol[sub$Comments == "TF"] <- TFtusetr5[i]
  # g1 <- ggplot(sub, aes(X,Y,z = Vol)) + geom_contour_filled() + theme_classic() + ggtitle(unique(TFrd$Trial)[i]) + geom_point(aes(col = Vol), size = 1, alpha = 0.5)
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' Gross volume.jpg'))
  # print(g1)
  # dev.off()
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  ggloc <- sub[sub$Comments == "DP" | sub$Comments == "TF",1:2]
  for(j in 1:length(unique(chckbk$Trial))){
    subchckbk <- chckbk[chckbk$Trial == unique(chckbk$Trial)[j],]
    calloc <- subchckbk[subchckbk$Comments == "DP" | subchckbk$Comments == "TF",1:2]
    if(sum(ggloc == calloc) == 4){
      cal2use <- subchckbk[,c(1,2,5)]
      names(cal2use)[3] <- "Calratio"
    }
  }
  subnrm <- merge(subnrm,cal2use)
  subnrm$rexcprf <- subnrm$Vol/subnrm$Calratio
  subnrm$X[subnrm$X != 0] <- (subnrm$X[subnrm$X != 0]+4)*20
  subnrm$X[subnrm$X == 0] <- 10
  subnrm$Y <- sub$Y*10
  g2 <- ggplot(subnrm, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle(unique(TFrdtr5$Trial)[i]) 
  
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' normalized volume.jpg'))
  print(g2)
  # dev.off()
  TFmmtr5 <- rbind(TFmmtr5, sub)
  TFnrmtr5 <- rbind(TFnrmtr5, subnrm)
}

##Tree 5-6
datGB <- read.table("H:/My Documents/Project and related work/banksia at gingin/GB202203.PRN", quote="\"", comment.char="")
datGB$time <- with(datGB, strptime(paste(V2, V3, V4, V5), format = "%Y %j %H%M %S"))
datGB$DP <- datGB$V7*2
datGB$TF <- datGB$V8*0.4
datGB$V6[datGB$V6 == -6999] <- NA
datGB$storage <- (datGB$V6*-6.774)-(6.774*0.044)
datGB$event <- NA
GBevdt <- grep("GB",evdet$Trial)
for(i in 1:length(GBevdt)){
  if(i == 1){
    datGB$event[datGB$time >= dmy_hm(evdet$Datetime[GBevdt[i]], tz = "Australia/Perth") & 
                  datGB$time <= dmy_hm(evdet$Datetime[GBevdt[i+1]], tz = "Australia/Perth")] <- evdet$Trial[GBevdt[i]]
  }else if(i == length(GBevdt)){
    datGB$event[datGB$time >= dmy_hm(evdet$Datetime[GBevdt[i]], tz = "Australia/Perth")] <- evdet$Trial[GBevdt[i]]
  }else{
    d1 <- as.Date(dmy_hm(evdet$Datetime[GBevdt[i]]))
    datGB$event[(datGB$time >= dmy_hm(evdet$Datetime[GBevdt[i]], tz = "Australia/Perth") & 
                   datGB$time <= dmy_hm(evdet$Datetime[GBevdt[i+1]], tz = "Australia/Perth")) & 
                   as.Date(datGB$time) == d1] <- evdet$Trial[GBevdt[i]]
  }
}
for(i in 1:length(GBevdt)){
  sub <- datGB[datGB$event == evdet$Trial[GBevdt[i]],10:13]
  sub <- sub[!is.na(sub$TF),]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP)
  sub$time <- as.POSIXct(sub$time)
  msub <- melt(sub[,c(1,5,6)], id = "time")
  g1 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) +ggtitle(as.character(i))+ theme_classic()
  # print(g1)
  g2 <- ggplot(sub, aes(time, storage)) + geom_point() +ggtitle(evdet$Trial[tr3evdt[i]])+ theme_classic() + ylim(0.2,0.4)
  # print(g2)
}

DPtuseGB <- tapply(datGB$DP, datGB$event, sum)
TFtuseGB <- tapply(datGB$TF, datGB$event, sum)

TFrdGB <- read.csv("H:/My Documents/Project and related work/banksia at gingin/Goldenbranch.csv")
TFrdGB <- TFrdGB[!is.na(TFrdGB$X),]
TFmmGB <- data.frame()
TFnrmGB <- data.frame()
for(i in 1:length(unique(TFrdGB$Trial))){
  sub <- TFrdGB[TFrdGB$Trial == unique(TFrdGB$Trial)[i],1:7]
  sub$Vol <- sub$Vol/10.8/10.8/pi*40
  sub$Vol[sub$Comments == "DP"] <- DPtuseGB[i]
  sub$Vol[sub$Comments == "TF"] <- TFtuseGB[i]
  # g1 <- ggplot(sub, aes(X,Y,z = Vol)) + geom_contour_filled() + theme_classic() + ggtitle(unique(TFrd$Trial)[i]) + geom_point(aes(col = Vol), size = 1, alpha = 0.5)
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' Gross volume.jpg'))
  # print(g1)
  # dev.off()
  subnrm <- sub
  subnrm$Vol <- subnrm$Vol/mean(sub$Vol[sub$X == 0],na.rm = T)
  ggloc <- sub[sub$Comments == "DP" | sub$Comments == "TF",1:2]
  for(j in 1:length(unique(chckbk$Trial))){
    subchckbk <- chckbk[chckbk$Trial == unique(chckbk$Trial)[j],]
    calloc <- subchckbk[subchckbk$Comments == "DP" | subchckbk$Comments == "TF",1:2]
    if(sum(ggloc == calloc) == 4){
      cal2use <- subchckbk[,c(1,2,5)]
      names(cal2use)[3] <- "Calratio"
    }
  }
  subnrm <- merge(subnrm,cal2use)
  subnrm$rexcprf <- subnrm$Vol/subnrm$Calratio
  subnrm$X[subnrm$X != 0] <- (subnrm$X[subnrm$X != 0]+4)*20
  subnrm$X[subnrm$X == 0] <- 10
  subnrm$Y <- sub$Y*10
  g2 <- ggplot(subnrm, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle(unique(TFrdGB$Trial)[i]) 
  # jpeg(paste(as.character(unique(TFrd$Trial)[i]), ' normalized volume.jpg'))
  print(g2)
  # dev.off()
  TFmmGB <- rbind(TFmmGB, sub)
  TFnrmGB <- rbind(TFnrmGB, subnrm)
}


##Combined analysis
combsim <- rbind(TFnrm, TFnrmtr3, TFnrmtr4, TFnrmtr5, TFnrmGB)
combmmsim <- rbind(TFmm, TFmmtr3, TFmmtr4, TFmmtr5, TFmmGB)
nocal <- combsim[-grep("XYcal", combsim$Trial),]
nocaldet <- evdet[-grep("XYcal", evdet$Trial),]
nocaldet$DPrat <- nocal$rexcprf[nocal$Comments == "DP"]
nocaldet$TFrat <- nocal$rexcprf[nocal$Comments == "TF"]
nocaldet$DP[nocaldet$DPm  == "DP"] <- nocaldet$DPrat[nocaldet$DPm  == "DP"]
nocaldet$DP[nocaldet$TFm  == "DP"] <- nocaldet$TFrat[nocaldet$TFm  == "DP"]
nocaldet$TF[nocaldet$TFm  == "TF"] <- nocaldet$TFrat[nocaldet$TFm  == "TF"]
nocaldet$SF[nocaldet$TFm  == "SF"] <- nocaldet$TFrat[nocaldet$TFm  == "SF"]
nocaldet$SF[nocaldet$DPm  == "SF"] <- nocaldet$DPrat[nocaldet$DPm  == "SF"]
nocaldet$BrNa <- substr(nocaldet$Trial, 1,3)
top <- nocaldet[,c(5,7,8,21,20,19,18)]
mtop <- melt(top, id = c("Foliation", "DW", "Angle.1", "BrNa"))
ggplot(mtop, aes(Foliation, value)) + geom_point(aes(col = variable)) + theme_classic() + 
  facet_wrap(.~BrNa, scales = "free") + scale_color_manual(values = c("darkgoldenrod4", "green3", "lightsalmon2"))
ggplot(mtop, aes(Angle.1, value)) + geom_point(aes(col = variable)) + theme_classic() + 
  facet_wrap(.~BrNa, scales = "free") + scale_color_manual(values = c("darkgoldenrod4", "green3", "lightsalmon2"))

robustdf <- data.frame()
for(i in 1:length(unique(nocal$Trial))){
  sub <- nocal[nocal$Trial == unique(nocal$Trial)[i],]
  sub$mdn <- median(sub$rexcprf, na.rm = T)
  sub$MAD <- with(sub, (1.4826*median(abs(rexcprf-median(rexcprf, na.rm = T)), na.rm = T)))
  sub$Qn <- Qn(sub$rexcprf, na.rm = T)
  mng <- nocaldet[nocaldet$Trial == unique(nocaldet$Trial)[i],]
  sub$Comments[sub$Comments == "DP"] <- mng$DPm
  sub$Comments[sub$Comments == "TF"] <- mng$TFm
  robustdf <- rbind(robustdf,sub)
}
robustdf$zscor <- with(robustdf, abs(Vol-mdn)/MAD)
robustdf$outlier <- F
robustdf$outlier[robustdf$zscor > 2.5] <- T

nocaldet$mnwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) mean(x, na.rm = T))
nocaldet$mnwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
       FUN = function(x) mean(x, na.rm = T))
nocaldet$mn <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) mean(x, na.rm = T))

nocaldet$sdwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) sd(x, na.rm = T))
nocaldet$sdwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                           FUN = function(x) sd(x, na.rm = T))
nocaldet$sd <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) sd(x, na.rm = T))

nocaldet$sdwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) sd(x, na.rm = T))
nocaldet$sdwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                           FUN = function(x) sd(x, na.rm = T))
nocaldet$sd <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) sd(x, na.rm = T))

nocaldet$krtwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) kurtosis(x, na.rm = T))
nocaldet$krtwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                           FUN = function(x) kurtosis(x, na.rm = T))
nocaldet$krt <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) kurtosis(x, na.rm = T))

nocaldet$skwwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) skewness(x, na.rm = T))
nocaldet$skwwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                            FUN = function(x) skewness(x, na.rm = T))
nocaldet$skw <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) skewness(x, na.rm = T))

shnchk <- 0
for(i in 1:10){
  shnchk <- c(shnchk, with(robustdf[!robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x)(shannonEntropy(table(round(x, i))/sum(table(round(x, i))))))))
  shnchk <- c(shnchk, with(robustdf[robustdf$Comments != "DP" & robustdf$Comments != "SF",], 
                           tapply(rexcprf, Trial, FUN = function(x)(shannonEntropy(table(round(x, i))/sum(table(round(x, i))))))))
  shnchk <- c(shnchk, with(robustdf, tapply(rexcprf, Trial, FUN = function(x)(shannonEntropy(table(round(x, i))/sum(table(round(x, i))))))))
}
shnchkdf <- data.frame(Trial = names(shnchk[-1]), excl = rep(c("Outlier", "DP & SF", "None"), each = length(unique(robustdf$Trial))), decpnt = rep(1:10, each = length(unique(robustdf$Trial))*3), shnetrp = shnchk[-1])
ggplot(shnchkdf, aes(decpnt, shnetrp, group = decpnt)) + geom_violin() + theme_classic() + facet_wrap(.~excl)

nocaldet$shnetrwoot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) (shannonEntropy(table(round(x, 2))/sum(table(round(x, 2))))))
nocaldet$shnetrwodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                            FUN = function(x) (shannonEntropy(table(round(x, 2))/sum(table(round(x, 2))))))
nocaldet$shnetr <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) (shannonEntropy(table(round(x, 2))/sum(table(round(x, 2))))))


skew8 <- function(x){
  o7 <- quantile(x, probs = .875, na.rm = T)
  o1 <- quantile(x, probs = .125, na.rm = T)
  med <- median(x,  na.rm = T)
  ans <- ((o7-med)-(med-o1))/(o7-o1)
  return(ans)
}

nocaldet$skw8woot <- tapply(robustdf$rexcprf[!robustdf$outlier], robustdf$Trial[!robustdf$outlier], FUN = function(x) skew8(x))
nocaldet$skw8wodpsf <-tapply(robustdf$rexcprf[robustdf$Comments != "DP" & robustdf$Comments != "SF"], robustdf$Trial[robustdf$Comments != "DP"& robustdf$Comments != "SF"], 
                              FUN = function(x) skew8(x))
nocaldet$skw8 <- tapply(robustdf$rexcprf, robustdf$Trial, FUN = function(x) skew8(x))


with(robustdf, tapply(rexcprf, Trial, FUN = function(x)(100*(1-(sum(abs(x-mean(x, na.rm = T)), na.rm = T)/length(x[!is.na(x)])/mean(x, na.rm = T))))))


nocaldet$smp5woot <- with(robustdf[!robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.05/mean(x, na.rm = T))^2))
nocaldet$smp5wodpsf <- with(robustdf[robustdf$Comments != "DP" & robustdf$Comments != "SF",], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.05/mean(x, na.rm = T))^2))
nocaldet$smp5 <- with(robustdf, tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.05/mean(x, na.rm = T))^2))

nocaldet$smp10woot <- with(robustdf[!robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.1/mean(x, na.rm = T))^2))
nocaldet$smp10wodpsf <- with(robustdf[robustdf$Comments != "DP" & robustdf$Comments != "SF",], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.1/mean(x, na.rm = T))^2))
nocaldet$sm10 <- with(robustdf, tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.1/mean(x, na.rm = T))^2))

nocaldet$smp20woot <- with(robustdf[!robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.2/mean(x, na.rm = T))^2))
nocaldet$smp20wodpsf <- with(robustdf[robustdf$Comments != "DP" & robustdf$Comments != "SF",], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.2/mean(x, na.rm = T))^2))
nocaldet$smp20 <- with(robustdf, tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/0.2/mean(x, na.rm = T))^2))

nocaldet$smpcnfwoot <- with(robustdf[!robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/sqrt(44)/mean(x, na.rm = T))))
nocaldet$smpcnfwodpsf <- with(robustdf[robustdf$Comments != "DP" & robustdf$Comments != "SF",], tapply(rexcprf, Trial, FUN = function(x)(2*sd(x, na.rm = T)/sqrt(44)/mean(x, na.rm = T))))
nocaldet$smpcnf <- with(robustdf, tapply(Vol, Trial, FUN = function(x)(2*sd(x, na.rm = T)/sqrt(44)/mean(x, na.rm = T))))


nocaldet$ov15 <- with(robustdf[robustdf$Comments != "SF",], tapply(rexcprf, Trial, FUN = function(x) sum(x >= 1.5, na.rm = T)))
nocaldet$totot <- with(robustdf[robustdf$Comments != "SF",], tapply(outlier, Trial, FUN = function(x) sum(x, na.rm = T)))
nocaldet$otav <- with(robustdf[robustdf$outlier,], tapply(rexcprf, Trial, FUN = function(x) mean(x, na.rm = T)))

df <- apply(nocaldet,2,as.character)
write.csv(df, "H:/My Documents/Project and related work/banksia at gingin/table1.csv")

ans <- data.frame()
for(i in 1:length(unique(robustdf$Trial))){
  TheData=na.omit(robustdf[robustdf$Trial == unique(robustdf$Trial)[i] & !robustdf$outlier,])
  if(nrow(TheData) > 10){
    coordinates(TheData)= ~ X+Y
    Vario_TF <- variogram(Vol~1, data = TheData)
    plot(Vario_TF)
  }
}

rglc <- rbind(dat, dat2, dat3, dattr2, dattr4, dattr5, datGB)
rglc <- rglc[-grep("XY", rglc$event),]
rglc <- rglc[!is.na(rglc$event),]

ansdftr1 <- data.frame()
ansdftr2 <- data.frame()
for(i in 1:length(unique(rglc$event))){
  sub <- rglc[rglc$event == unique(rglc$event)[i],]
  sub$CTF <- cumsum(sub$TF) 
  sub$CDP <- cumsum(sub$DP)
  avgtm <- 20
  rlavgld <- rollapply(sub$storage, avgtm, mean)
  rlavgTF <- rollapply(sub$TF, avgtm, mean)
  rlavgDP <- rollapply(sub$DP, avgtm, mean)
  nrmdiflc <- diff(rlavgld)/20*max(c(rlavgTF, rlavgDP), na.rm = T)/max((diff(rlavgld)/20), na.rm = T)
  try <- data.frame(time = sub$time[avgtm:dim(sub)[1]], rlavgld, rlavgTF, rlavgDP, nrmdiflc = c(NA,nrmdiflc), avgtm, event = sub$event[1])
  g1 <- ggplot(try, aes(time, nrmdiflc)) + geom_point(aes(time,rlavgTF), col = "forestgreen") + 
    geom_point(aes(time,rlavgDP), col = "darkorange") + geom_point(col = "darkblue") + 
    ggtitle(as.character(unique(rglc$event)[i])) + theme_classic()
  jpeg(paste(as.character(sub$event[1]), 'time to think.jpg'), width = 1000, height = 800)
  print(g1)
  dev.off()
  ansdftr1 <- rbind(ansdftr1, try)
  ansdftr2 <- rbind(ansdftr2, sub[,10:16])
}


for(i in 1:sum(nocaldet$DW == "Dry")){
  sub <- nocal[nocal$Trial == nocaldet$Trial[nocaldet$DW == "Dry"][i] | nocal$Trial == nocaldet$Trial[c(F,nocaldet$DW == "Dry"[-length(nocaldet$DW)])][i],]
  g1 <- ggplot(sub, aes(X,Y,z = rexcprf)) + geom_tile(aes(fill = rexcprf, col = rexcprf)) + theme_classic() + facet_wrap(.~Trial)
  jpeg(paste(as.character(sub$Trial[1]), 'ratio.jpg'), width = 1000, height = 800)
  print(g1)
  dev.off()
  sub <- rglc[rglc$event == nocaldet$Trial[nocaldet$DW == "Dry"][i] | rglc$event == nocaldet$Trial[c(F,nocaldet$DW == "Dry"[-length(nocaldet$DW)])][i],]
  sub <- sub[complete.cases(sub),]
  sub$CTF[sub$event == unique(sub$event)[1]] <- cumsum(sub$TF[sub$event == unique(sub$event)[1]]) 
  sub$CDP[sub$event == unique(sub$event)[1]] <- cumsum(sub$DP[sub$event == unique(sub$event)[1]])
  sub$CTF[sub$event == unique(sub$event)[2]] <- cumsum(sub$TF[sub$event == unique(sub$event)[2]]) 
  sub$CDP[sub$event == unique(sub$event)[2]] <- cumsum(sub$DP[sub$event == unique(sub$event)[2]])
  sub$time <- as.POSIXct(sub$time)
  msub <- melt(sub[,c(10,14,15,16)], id = c("time", "event"))
  g2 <- ggplot(msub, aes(time, value)) + geom_point(aes(col= variable)) + facet_wrap(.~event) + theme_classic()
  jpeg(paste(as.character(sub$event[1]), 'cumtfdf.jpg'), width = 1000, height = 800)
  print(g2)
  dev.off()
  g3 <- ggplot(sub, aes(time, storage)) + geom_point() + facet_wrap(.~event) + theme_classic() + ylim(quantile(sub$storage, .02),quantile(sub$storage, .98))
  jpeg(paste(as.character(sub$event[1]), 'strg.jpg'), width = 1000, height = 800)
  print(g3)
  dev.off()
}
ans <- NA
for(i in 1:length(unique(nocaldet$BrNa))){
  sub <- nocaldet$DP[nocaldet$BrNa == unique(nocaldet$BrNa)[i]]
  ans <- c(ans,sub-mean(sub))
}
nocaldet$salmet <- ans[-1]
userglc <- rglc[complete.cases(rglc),]
dynpl1 <- userglc[userglc$event == "Br4Tr06" |userglc$event == "Br4Tr07",]
dynpl1 <- dynpl1[(dynpl1$event == "Br4Tr06" & dynpl1$time < ymd_hms("2022-02-28 08:20:00"))| (dynpl1$event == "Br4Tr07" & dynpl1$time < ymd_hms("2022-02-28 08:50:00")),]
dynpl1$devSF[dynpl1$event == "Br4Tr06"] <- cumsum(dynpl1$TF[dynpl1$event == "Br4Tr06"])-cumsum(dynpl1$DP[dynpl1$event == "Br4Tr06"])
dynpl1$tstrt[dynpl1$event == "Br4Tr06"] <- dynpl1$time[dynpl1$event == "Br4Tr06"]-dynpl1$time[dynpl1$event == "Br4Tr06"][1]
dynpl1$devSF[dynpl1$event == "Br4Tr07"] <- cumsum(dynpl1$TF[dynpl1$event == "Br4Tr07"])-cumsum(dynpl1$DP[dynpl1$event == "Br4Tr07"])
dynpl1$tstrt[dynpl1$event == "Br4Tr07"] <- dynpl1$time[dynpl1$event == "Br4Tr07"]-dynpl1$time[dynpl1$event == "Br4Tr07"][1]
dynpl1$IC[dynpl1$event == "Br4Tr06"] <- "Dry"
dynpl1$IC[dynpl1$event == "Br4Tr07"] <- "Wet"

dynpl2 <- userglc[userglc$event == "Br3Tr12" |userglc$event == "Br3Tr13",]
dynpl2 <- dynpl2[(dynpl2$event == "Br3Tr12" & dynpl2$time < ymd_hms("2022-02-23 07:30:00"))| (dynpl2$event == "Br3Tr13" & dynpl2$time < ymd_hms("2022-02-23 08:00:00")),]
dynpl2$devSF[dynpl2$event == "Br3Tr12"] <- cumsum(dynpl2$TF[dynpl2$event == "Br3Tr12"])-cumsum(dynpl2$DP[dynpl2$event == "Br3Tr12"])
dynpl2$tstrt[dynpl2$event == "Br3Tr12"] <- dynpl2$time[dynpl2$event == "Br3Tr12"]-dynpl2$time[dynpl2$event == "Br3Tr12"][1]
dynpl2$devSF[dynpl2$event == "Br3Tr13"] <- cumsum(dynpl2$TF[dynpl2$event == "Br3Tr13"])-cumsum(dynpl2$DP[dynpl2$event == "Br3Tr13"])
dynpl2$tstrt[dynpl2$event == "Br3Tr13"] <- dynpl2$time[dynpl2$event == "Br3Tr13"]-dynpl2$time[dynpl2$event == "Br3Tr13"][1]
dynpl2$IC[dynpl2$event == "Br3Tr12"] <- "Dry"
dynpl2$IC[dynpl2$event == "Br3Tr13"] <- "Wet"

dynmcom <- rbind(dynpl1,dynpl2)
dynmcom$Branch <- paste("Branch", substr(dynmcom$event, 3,3))
ggplot(dynmcom, aes(tstrt/60, devSF)) + geom_line(aes(col = IC), size = 1.3) + theme_classic() + 
  facet_grid(Branch~., scales = "free") + xlab(expression(paste(Delta, "Minutes"))) + 
  ylab(expression(paste(sum("Pour Point","T"^"start","T"^"fin") - sum("Stem Flow","T"^"start","T"^"fin")))) + 
  scale_color_brewer(palette = "Paired", direction = 1)


dynfolang <- data.frame()
mod1 <- 0
mod2 <- 0
mod3 <- 0
for(i in 1:length(unique(nocaldet$BrNa))){
  sub <- nocaldet[nocaldet$BrNa == unique(nocaldet$BrNa)[i],c(21,18,5,8)]
  sub$deltDP <- sub$DP-mean(sub$DP, na.rm =T)
  sub$deltang <- sub$Angle.1-mean(sub$Angle.1, na.rm =T)
  x <- lm(deltDP ~ Foliation, data= sub)
  mod1[i] <- summary(x)$adj.r.squared
  x <- lm(deltDP ~ deltang , data= sub)
  mod2[i] <- summary(x)$adj.r.squared
  x <- lm(deltDP ~ Foliation + deltang + (deltang * Foliation), data= sub)
  mod3[i] <- summary(x)$adj.r.squared
  dynfolang <- rbind(dynfolang, sub)
}
dynfolang$Foliation[dynfolang$Foliation == 0.67 | dynfolang$Foliation == 0.666666667] <- 0.67
dynfolang$Foliation[dynfolang$Foliation == 0.333333333] <- 0.33
topfol <- dynfolang[dynfolang$BrNa == "Br1" & nocaldet$DW == "Wet",]
topfol$Foliation[is.na(topfol$Foliation)] <- "Branches removed"
topfol$Foliation <- factor(topfol$Foliation, levels = c("Branches removed","0","0.33","0.67","1"))

ggplot(topfol, aes(factor(Foliation), DP)) + xlab("Foliation") + ylab(expression(frac("Pour Point", "Rainfall"))) + 
  geom_bar(position = "dodge2", stat = "identity", aes(fill = Foliation), col = "black")  + theme_classic() + geom_hline(yintercept = 1, alpha = 0.3) + 
  scale_fill_brewer(palette = "BrGr")

dynfolang$DPSFrat <- nocaldet$DP/nocaldet$SF
ggplot(dynfolang[dynfolang$BrNa == "Br4" & nocaldet$DW == "Wet",], aes(deltang, DPSFrat))  + scale_fill_brewer(palette = "BrGr") + theme_classic()  +
  xlab(expression(paste(Delta, " Angle"))) + ylab(expression(frac("Pour Point", "Stemflow")))  + ggtitle("Branch 4") + 
  geom_smooth(data = dynfolang[dynfolang$BrNa == "Br4" & dynfolang$Foliation == 1,], aes(deltang, DPSFrat), method = "lm", col = "darkgreen") + 
  geom_smooth(data = dynfolang[dynfolang$BrNa == "Br4" & dynfolang$Foliation != 1,], aes(deltang, DPSFrat), method = "lm", col = "forestgreen") + 
  geom_jitter(aes(fill = as.factor(Foliation)), shape = 21, size = 3) 

ggplot(dynfolang[dynfolang$BrNa == "Br5" & nocaldet$DW == "Wet",], aes(deltang, DPSFrat))  + scale_fill_brewer(palette = "BrGr") + theme_classic()  +
  xlab(expression(paste(Delta, " Angle"))) + ylab(expression(frac("Pour Point", "Stemflow")))  + ggtitle("Branch 5") + 
  geom_jitter(aes(fill = as.factor(Foliation)), shape = 21, size = 3) 

ggplot(dynfolang[dynfolang$BrNa == "GBT" & nocaldet$DW == "Wet",], aes(deltang, DPSFrat))  + scale_fill_brewer(palette = "BrGr") + theme_classic()  +
  xlab(expression(paste(Delta, " Angle"))) + ylab(expression(frac("Pour Point", "Stemflow")))  + ggtitle("Golden branch") + 
  geom_jitter(aes(fill = as.factor(Foliation)), shape = 21, size = 3) 

dynfolang$control <- FALSE
dynfolang$control[dynfolang$BrNa == "Br3"] <- TRUE

ggplot(dynfolang[!is.na(dynfolang$DPSFrat),], aes(x = Foliation, y = DPSFrat))+ 
  geom_hline(yintercept = 1) + geom_boxplot_pattern(aes(fill = as.factor(Foliation), pattern = control)) + 
  scale_fill_brewer(palette = "Greens", name = "Control") + theme_classic() + 
  ylab(expression(frac("Pour Point", "Stemflow"))) + xlab("Foliation")


dynfolang$Foliation[is.na(dynfolang$Foliation)] <- "Branches removed"
dynfolang$Foliation <- factor(dynfolang$Foliation, levels = c("Branches removed","0","0.33","0.67","1"))
ggplot(dynfolang, aes(x = factor(Foliation), y = DP, group = Foliation)) + geom_hline(yintercept = 1) + geom_boxplot(aes(fill = factor(Foliation)))  + 
  scale_fill_brewer(palette = "BrGr", name = element_blank()) + theme_classic() + ylab(expression(frac("Pour Point", "Rainfall"))) + xlab("Foliation")
moreqs <- data.frame(Branch = unique(nocaldet$BrNa), Fol = mod1, delang = mod2, comb = mod3)
write.csv(moreqs,"morequestion.csv")

for(i in 1:length(unique(dynfolang$BrNa))){
  sub <- dynfolang[dynfolang$BrNa == unique(dynfolang$BrNa)[i],]
  if(sum(!is.na(sub$DPSFrat))){
    x <- lm(DPSFrat~deltang, data = sub)
    halmry[[i]] <- summary(x) 
    x <- lm(DPSFrat~Angle.1, data = sub)
    halmry2[[i]] <- summary(x) 
  }else{
    halmry[[i]] <- NA
    halmry2[[i]] <- NA
  }
}


evdt2 <- read.csv("H:/My Documents/Project and related work/banksia at gingin/eventdets - 2.csv")
evdt2$GStarttime <- dmy_hms(paste(substr(evdt2$Datetime, 1,10), evdt2$GStarttime))
evdt2$EquiLCtim  <- dmy_hms(paste(substr(evdt2$Datetime, 1,10), evdt2$EquiLCtim))
evdt2$Fintim <- dmy_hms(paste(substr(evdt2$Datetime, 1,10), evdt2$Fintim))
evdt2$DrainEndtime <- dmy_hms(paste(substr(evdt2$Datetime, 1,10), evdt2$DrainEndtime))
evdt2$Endtime <- dmy_hms(paste(substr(evdt2$Datetime, 1,10), evdt2$Endtime))


fitlc <- function(x, tsrt=10, tend=(60*15+10), tdend = (60*15+20), inwt = 2, mxwt = 3, finwt = 2.5, RF = 4, FF= 1000, EVF = 100){
  y <- NA
  for(j in 1:length(x)){
    if(x[j] < tsrt){
      y[j] <- inwt
    }else if(x[j] >= tsrt & x[j] < tend){
      y[j] <- inwt + mxwt*(1-exp(-(x[j]-tsrt)/RF))
    }else if(x[j] >= tend & x[j] < tdend){
      y[j] <- inwt + mxwt*(exp(-(x[j]-tend)/FF))
    }else{
      y[j] <- finwt+(y[tdend-1]-finwt)*(exp(-(x[j]-tdend)/EVF))
    }
  }
  return(y)
}
if(file.exists("H:/My Documents/Project and related work/banksia at gingin/mcsimres.csv")){
  mcsimres <- read.csv("H:/My Documents/Project and related work/banksia at gingin/mcsimres.csv")
  mcsimres <- mcsimres[,-1]
}else{
  try <- list()
  chktunit <- NA
  mcsimres <- data.frame()
  for(i in 69:length(unique(ansdftr2$event))){
    sub <- ansdftr2[ansdftr2$event == unique(ansdftr2$event)[i],]
    lindat <- evdt2[evdt2$Trial == unique(ansdftr2$event)[i],]
    if(!is.na(lindat$GStarttime)){
      sub <- sub[sub$time >= lindat$GStarttime,]
      
    }
    if(!is.na(lindat$DrainEndtime)){
      sub <- sub[sub$time <= lindat$DrainEndtime,]
    }
    sub <- sub[complete.cases(sub),]
    sub$ttu <- sub$time-sub$time[1]
    chktunit[i] <- units(sub$ttu)
    sub$ttu <- as.numeric(sub$ttu)
    b <- breakpoints(sub$storage ~ 1)
    te <- (sub$ttu[sub$time==lindat$Fintim])
    ltl <- (sub$ttu[sub$time==lindat$EquiLCtim])
    twt <- mean(sub$storage[sub$time>(lindat$EquiLCtim - 30) & sub$time<(lindat$EquiLCtim + 30)], na.rm = T)-(sub$storage[1])
    twtsd <- sd(sub$storage[sub$time>(lindat$EquiLCtim - 30) & sub$time<(lindat$EquiLCtim + 30)], na.rm = T)
    iwt <- sub$storage[1]
    finwt <- sub$storage[nrow(sub)]
    ulstt <- min(sub$ttu[sub$storage>min(c((iwt+twtsd), mean((twt+iwt),iwt), mean(iwt,finwt)),na.rm=T)],na.rm = T)
    for(sim in 1:10000){
      simtsrt <- as.integer(runif(1, 0, max(c(min(b$breakpoints), ulstt),na.rm=T)))
      simtend <- as.integer(runif(1, max(c(ltl, (te - 60)), na.rm=T), (60+te)))
      simtdend <- as.integer(runif(1, (simtend), (500+simtend)))
      siminwt <- rnorm(1, iwt, twtsd)
      simmxwt <- rnorm(1, twt, twtsd)
      simfnwt <- rnorm(1, finwt, twtsd)
      simRF <- runif(1,2,100)
      simFF <- runif(1,2,250)
      simEVF <- runif(1,500,10000)
      ansstrg <- fitlc(sub$ttu, tsrt=simtsrt, tend=simtend, tdend = simtdend, inwt = siminwt, 
                       mxwt = simmxwt, finwt = simfnwt, RF = simRF, FF= simFF, EVF = simEVF)
      absdiff <- sum(abs(sub$storage-ansstrg), na.rm = T)
      mcsimres <- rbind(mcsimres, data.frame(event = i, sim = sim, tsrt=simtsrt, tend=simtend, tdend = simtdend,
                                             inwt = siminwt, mxwt = simmxwt, finwt = simfnwt, RF = simRF, FF= simFF, EVF = simEVF, absdiff))
    }
    ans <- mcsimres[mcsimres$event == i,]
    ans <- ans[order(ans$absdiff)[1:3],]
    plot(sub$ttu, sub$storage)
    lines(sub$ttu, fitlc(sub$ttu, ans[1,3], ans[1,4], ans[1,5], ans[1,6], ans[1,7], ans[1,8],ans[1,9], ans[1,10], ans[1,11]), col = "red")
    lines(sub$ttu, fitlc(sub$ttu, ans[2,3], ans[2,4], ans[2,5], ans[2,6], ans[2,7], ans[2,8],ans[2,9], ans[2,10], ans[2,11]), col = "blue")
    lines(sub$ttu, fitlc(sub$ttu, ans[3,3], ans[3,4], ans[3,5], ans[3,6], ans[3,7], ans[3,8],ans[3,9], ans[3,10], ans[3,11]), col = "green")
    write.csv(mcsimres, "mcsimres.csv")
  }
}

dfparfin <- data.frame()
for(i in 1:length(unique(ansdftr2$event))){  
  subm <- mcsimres[mcsimres$event == unique(mcsimres$event)[i],]
  dfparfin <- rbind(dfparfin, subm[subm$absdiff == min(subm$absdiff),3:11])
}

tab2 <- as.data.frame(cbind(nocaldet, dfparfin))
tab2 <- tab2[,-39]
tab2$AGfl <- tab2$DP + ifelse(is.na(tab2$SF),0,tab2$SF)
sally1 <- list()
sally2 <- list()
sally3 <- list()
sally4 <- list()
sally5 <- list()
sally6 <- list()
sally7 <- list()
sally8 <- list()
me1 <- list()
me2 <- list()
me3 <- list()
finaldf <- data.frame()
for(i in 1:length(unique(tab2$BrNa))){
    subbr <- tab2[tab2$BrNa == unique(tab2$BrNa)[i],]
    subbr$chngang <- subbr$Angle.1-mean(subbr$Angle.1)
    sally1[[i]] <- lm(mxwt~chngang, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="maxwt", x1 = "angle", x2 = NA, adjr2 = summary(sally1[[i]])$adj.r.squared,
    r2 = summary(sally1[[i]])$r.squared, mx1 = summary(sally1[[i]])$coefficients[2],
    mx2 = NA, int = summary(sally1[[i]])$coefficients[1]))
    sally2[[i]] <- lm(RF~chngang, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="Risinglimb", x1 = "angle", x2 = NA, adjr2 = summary(sally2[[i]])$adj.r.squared,
    r2 = summary(sally2[[i]])$r.squared, mx1 = summary(sally2[[i]])$coefficients[2],
    mx2 = NA, int = summary(sally2[[i]])$coefficients[1]))
    sally3[[i]] <- lm(mxwt~Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="maxwt", x1 = "Foliation", x2 = NA, adjr2 = summary(sally3[[i]])$adj.r.squared,
    r2 = summary(sally3[[i]])$r.squared, mx1 = summary(sally3[[i]])$coefficients[2],
    mx2 = NA, int = summary(sally3[[i]])$coefficients[1]))
    sally4[[i]] <- lm(RF~Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="Risinglimb", x1 = "Foliation", x2 = NA, adjr2 = summary(sally4[[i]])$adj.r.squared,
    r2 = summary(sally4[[i]])$r.squared, mx1 = summary(sally4[[i]])$coefficients[2],
    mx2 = NA, int = summary(sally4[[i]])$coefficients[1]))
    sally5[[i]] <- lm(mxwt~chngang+Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="maxwt", x1 = "angle", x2 = "Foliation", adjr2 = summary(sally5[[i]])$adj.r.squared,
    r2 = summary(sally5[[i]])$r.squared, mx1 = summary(sally5[[i]])$coefficients[2],
    mx2 = summary(sally5[[i]])$coefficients[3], int = summary(sally5[[i]])$coefficients[1]))
    sally6[[i]] <- lm(RF~chngang+Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="Risinglimb", x1 = "angle", x2 = "Foliation", adjr2 = summary(sally6[[i]])$adj.r.squared,
    r2 = summary(sally6[[i]])$r.squared, mx1 = summary(sally6[[i]])$coefficients[2],
    mx2 = summary(sally6[[i]])$coefficients[3], int = summary(sally6[[i]])$coefficients[1]))
    sally7[[i]] <- lm(DP~mxwt, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="PPratio", x1 = "maxwt", x2 = NA, adjr2 = summary(sally7[[i]])$adj.r.squared,
    r2 = summary(sally7[[i]])$r.squared, mx1 = summary(sally7[[i]])$coefficients[2],
    mx2 = NA, int = summary(sally7[[i]])$coefficients[1]))
    sally8[[i]] <- lm(AGfl~Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="SF+PP", x1 = "Foliation", x2 = NA, adjr2 = summary(sally8[[i]])$adj.r.squared,
                                           r2 = summary(sally8[[i]])$r.squared, mx1 = summary(sally8[[i]])$coefficients[2],
                                           mx2 = NA, int = summary(sally8[[i]])$coefficients[1]))
  
    me1[[i]] <- lm(DP~Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="PPratio", x1 = "Foliation", x2 = NA, adjr2 = summary(me1[[i]])$adj.r.squared,
    r2 = summary(me1[[i]])$r.squared, mx1 = summary(me1[[i]])$coefficients[2],
    mx2 = NA, int = summary(me1[[i]])$coefficients[1]))
    me2[[i]] <- lm(DP~chngang, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="PPratio", x1 = "angle", x2 = NA, adjr2 = summary(me2[[i]])$adj.r.squared,
    r2 = summary(me2[[i]])$r.squared, mx1 = summary(me2[[i]])$coefficients[2],
    mx2 = NA, int = summary(me2[[i]])$coefficients[1]))
    me3[[i]] <- lm(DP~chngang+Foliation, subbr)
    finaldf <- rbind(finaldf, data.frame(Branch = unique(tab2$BrNa)[i],y="PPratio", x1 = "angle", x2 = "Foliation", adjr2 = summary(me3[[i]])$adj.r.squared,
    r2 = summary(me3[[i]])$r.squared, mx1 = summary(me3[[i]])$coefficients[2],
    mx2 = summary(me3[[i]])$coefficients[3], int = summary(me3[[i]])$coefficients[1]))
}
write.csv(finaldf, "pleasebeit.csv")

simactanl <- data.frame()
simactanlcomp <- data.frame()
for(i in 1:length(unique(ansdftr2$event))){
  sub <- ansdftr2[ansdftr2$event == unique(ansdftr2$event)[i],]
  lindat <- evdt2[evdt2$Trial == unique(ansdftr2$event)[i],]
  subpar <- dfparfin[i,]
  if(!is.na(lindat$GStarttime)){
    sub <- sub[sub$time >= lindat$GStarttime,]
  }
  if(!is.na(lindat$DrainEndtime)){
    sub <- sub[sub$time <= lindat$DrainEndtime,]
  }
  sub <- sub[complete.cases(sub),]
  sub$ttu <- sub$time-sub$time[1]
  sub$ttu <- as.numeric(sub$ttu)
  subact <- sub[sub$ttu < subpar$tend & sub$storage > subpar$inwt  & sub$storage < (subpar$inwt+subpar$mxwt),]
  subactcomp <- sub[sub$storage > subpar$inwt  & sub$storage < (subpar$inwt+subpar$mxwt),]
  if(nrow(subact) != 0){
    subact$strcls <- with(subact,cut(storage,10))
    subactcomp$strcls <- with(subactcomp,cut(storage,10))
    simactanl <- rbind(simactanl, data.frame(event = sub$event[1], mnclswt = rep(apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", levels(subact$strcls)))),
                                             as.numeric(sub("]","",sub("\\(.*,", "", levels(subact$strcls))))),1,mean),2),
                                             noofpnts = rep(with(subact,table(strcls)),2), prcoffnwt = rep(1:10,2),
                                             avgtrec = rep(with(subact,tapply(ttu, strcls, mean)),2),
                                             rgvals = with(subact,c(tapply(TF, strcls, mean),tapply(DP, strcls, mean))),
                                             rgloc = rep(c(lindat$TFm,lindat$DPm),each = 10)))
    simactanlcomp <- rbind(simactanlcomp, data.frame(event = sub$event[1], time = rep(c("Simulation", "Rapid Drain", "Evaporation"), each = 20),
                                                     mnclswt = rep(apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", levels(subactcomp$strcls)))),
                                                                               as.numeric(sub("]","",sub("\\(.*,", "", levels(subactcomp$strcls))))),1,mean),6),
                                                     noofpnts = c(rep(with(subactcomp[subactcomp$ttu < subpar$tend,],table(strcls),2)),
                                                                  rep(with(subactcomp[subactcomp$ttu < subpar$tdend & subactcomp$ttu > subpar$tend,],table(strcls),2)),
                                                                  rep(with(subactcomp[subactcomp$ttu > subpar$tdend,],table(strcls),2))),
                                                     prcoffnwt = rep(1:10,2),
                                                     avgtrec = c(rep(with(subactcomp[subactcomp$ttu < subpar$tend,],tapply(ttu, strcls, FUN = function(x) mean(x,nar.rm = T))),2),
                                                                 rep(with(subactcomp[subactcomp$ttu < subpar$tdend & subactcomp$ttu > subpar$tend,],tapply(ttu, strcls, FUN = function(x) mean(x,nar.rm = T))),2),
                                                                 rep(with(subactcomp[subactcomp$ttu > subpar$tdend,],tapply(ttu, strcls, FUN = function(x) mean(x,nar.rm = T))),2)),
                                                     rgvals = c(with(subactcomp[subactcomp$ttu < subpar$tend,],c(tapply(TF, strcls, FUN = function(x) mean(x,nar.rm = T)),tapply(DP, strcls, FUN = function(x) mean(x,nar.rm = T)))),
                                                                with(subactcomp[subactcomp$ttu < subpar$tdend & subactcomp$ttu > subpar$tend,],c(tapply(TF, strcls, FUN = function(x) mean(x,nar.rm = T)),tapply(DP, strcls, FUN = function(x) mean(x,nar.rm = T)))),
                                                                with(subactcomp[subactcomp$ttu > subpar$tdend,],c(tapply(TF, strcls, FUN = function(x) mean(x,nar.rm = T)),tapply(DP, strcls, FUN = function(x) mean(x,nar.rm = T))))),
                                                     rgloc = rep(rep(c(lindat$TFm,lindat$DPm),each = 10),3)))
  }
}

finsimactanl <- data.frame()
finsimactanlcomp <- data.frame()
for(i in 1:length(unique(simactanl$event))){
  sub <- simactanl[simactanl$event == unique(simactanl$event)[i],]
  subcomp <- simactanlcomp[simactanlcomp$event == unique(simactanlcomp$event)[i],]
  sub$ratrg[sub$rgloc != "DP"] <- sub$rgvals[sub$rgloc != "DP"]/max(sub$rgvals[sub$rgloc != "DP"], na.rm = T)
  sub$ratrg[sub$rgloc == "DP"] <- sub$rgvals[sub$rgloc == "DP"]/max(sub$rgvals[sub$rgloc == "DP"], na.rm = T)
  subcomp$ratrg[subcomp$rgloc != "DP"] <- subcomp$rgvals[subcomp$rgloc != "DP"]/max(subcomp$rgvals[subcomp$rgloc != "DP"], na.rm = T)
  subcomp$ratrg[subcomp$rgloc == "DP"] <- subcomp$rgvals[subcomp$rgloc == "DP"]/max(subcomp$rgvals[subcomp$rgloc == "DP"], na.rm = T)
  finsimactanl <- rbind(finsimactanl, sub)
  finsimactanlcomp <- rbind(finsimactanlcomp, subcomp)
}

finsimactanlcomp$time <- factor(finsimactanlcomp$time, levels = c("Simulation", "Rapid Drain", "Evaporation"))

ggplot(finsimactanlcomp, aes(as.factor(prcoffnwt*10), ratrg)) + geom_boxplot(aes(fill = rgloc), outlier.size = 0.5) + theme_classic() + facet_wrap(.~time) +
  xlab("% of Max Weight") + ylab("Ratio of max water recieved in RG") + 
  scale_fill_manual(values = c("DP" = "chocolate1", "SF" = "brown", "TF" = "forestgreen"), 
                    labels =  c("DP" = "Pour Point", "SF" = "Stemflow", "TF" = "Throughfall"), name = element_blank())




simactanlcomp <- data.frame()
for(i in 1:length(unique(ansdftr2$event))){
  sub <- ansdftr2[ansdftr2$event == unique(ansdftr2$event)[i],]
  lindat <- evdt2[evdt2$Trial == unique(ansdftr2$event)[i],]
  subpar <- dfparfin[i,]
  if(!is.na(lindat$GStarttime)){
    sub <- sub[sub$time >= lindat$GStarttime,]
    
  }
  if(!is.na(lindat$DrainEndtime)){
    sub <- sub[sub$time <= lindat$DrainEndtime,]
  }
  sub <- sub[complete.cases(sub),]
  sub$ttu <- sub$time-sub$time[1]
  sub$ttu <- as.numeric(sub$ttu)
  subactcomp <- sub[sub$storage > subpar$inwt  & sub$storage < (subpar$inwt+subpar$mxwt),]
  if(nrow(subact) != 0){
    subactcomp$strcls <- with(subactcomp,cut(storage,10))
    subactcomp$strcls <- factor(subactcomp$strcls, levels = levels(subactcomp$strcls)[order(levels(subactcomp$strcls))])
    subactcomp$mnclswt <- factor(subactcomp$strcls, levels = apply(cbind(as.numeric(sub("\\(","",sub(",.*]", "", levels(subactcomp$strcls)))), 
                                      as.numeric(sub("]","",sub("\\(.*,", "", levels(subactcomp$strcls))))),1,mean))
    subactcomp$mnclswt <- as.numeric(as.character(subactcomp$mnclswt))
    subactcomp$prcoffnwt <- as.numeric(subactcomp$strcls)*10
    subactcomp$catotim <- "Simulation"
    subactcomp$catotim[subactcomp$ttu < subpar$tdend & subactcomp$ttu > subpar$tend] <- "Rapid Drain"
    subactcomp$catotim[subactcomp$ttu > subpar$tdend] <- "Evaporation"
    msubactcomp <- melt(subactcomp[,-c(6:7)], id = names(subactcomp[,-c(6:7)])[-c(2:3)])
    msubactcomp$variable <- as.character(msubactcomp$variable)
    msubactcomp$ratrg[msubactcomp$variable == "DP"] <- msubactcomp$value[msubactcomp$variable == "DP"]/max(msubactcomp$value[msubactcomp$variable == "DP"])
    msubactcomp$ratrg[msubactcomp$variable == "TF"] <- msubactcomp$value[msubactcomp$variable == "TF"]/max(msubactcomp$value[msubactcomp$variable == "TF"])
    msubactcomp$variable[msubactcomp$variable == "DP"] <- lindat$DPm
    msubactcomp$variable[msubactcomp$variable == "TF"] <- lindat$TFm
    simactanlcomp <- rbind(simactanlcomp, msubactcomp)
  }
}
simactanlcomp$catotim <- factor(simactanlcomp$catotim, levels = c("Rainfall Simulation", "Rapid Drain", "Evaporation"))

ggplot(simactanlcomp, aes(as.factor(prcoffnwt), ratrg)) + geom_boxplot(aes(fill = variable), outlier.alpha = 0.2) + theme_classic() + 
  facet_wrap(.~catotim) +  xlab("% of Max Weight") + ylab("Ratio of max water recieved in RG") + 
  scale_fill_manual(values = c("DP" = "chocolate1", "SF" = "brown", "TF" = "forestgreen"), 
                    labels =  c("DP" = "Pour Point", "SF" = "Stemflow", "TF" = "Throughfall"), name = element_blank())


for(i in 1:length(unique(ansdftr2$event))){
  sub <- ansdftr2[ansdftr2$event == unique(ansdftr2$event)[i],]
  lindat <- evdt2[evdt2$Trial == unique(ansdftr2$event)[i],]
  subpar <- as.numeric(dfparfin[i,])
  subsimmm <- combmmsim[combmmsim$Trial == unique(ansdftr2$event)[i],]
  subsimnrm <- robustdf[robustdf$Trial == unique(ansdftr2$event)[i],]
  nrmval <- mean(subsimmm$Vol[subsimmm$X == 0])
  if(!is.na(lindat$GStarttime)){
    sub <- sub[sub$time >= lindat$GStarttime,]
    
  }
  if(!is.na(lindat$DrainEndtime)){
    sub <- sub[sub$time <= lindat$DrainEndtime,]
  }
  sub <- sub[complete.cases(sub),]
  sub$ttu <- sub$time-sub$time[1]
  sub$ttu <- as.numeric(sub$ttu)
  dfparfin$strbr[i] <- (sub$storage[sub$ttu == subpar[3]]-subpar[4])
  dfparfin$wtrblls[i] <- sum(sum((subsimnrm$Calratio[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP" | subsimnrm$Comments != "TF" ]-
                                subsimnrm$Vol[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP" | subsimnrm$Comments != "TF"])*
                               nrmval*pi*10.3*10.3/40, na.rm = T),
                               (sum(subsimnrm$Calratio[subsimnrm$Comments == "SF" | subsimnrm$Comments == "TF" | subsimnrm$Comments == "DP"]-
                                                          subsimnrm$Vol[subsimnrm$Comments == "TF" | subsimnrm$Comments == "DP"], na.rm = T)*
                                 nrmval*pi*20.3*20.3*2/40))*
                                ((max(subsimnrm$X)-min(subsimnrm$X))*(max(subsimnrm$Y)-min(subsimnrm$Y)))/
                                sum((sum(!is.na(subsimnrm$Vol[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP" | subsimnrm$Comments != "TF"]))*pi*10.3*10.3/4), (2*pi*20.3*20.3*2/4))/1000
  dfparfin$wtrbllsexcl[i] <- sum((subsimnrm$Calratio[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP"]-
                                      subsimnrm$Vol[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP"])*
                                     nrmval*pi*10.3*10.3/40, na.rm = T)*
                                    ((max(subsimnrm$X)-min(subsimnrm$X))*(max(subsimnrm$Y)-min(subsimnrm$Y)))/
                                    (sum(!is.na(subsimnrm$Vol[subsimnrm$Comments != "SF" | subsimnrm$Comments != "DP"]))*pi*10.3*10.3/4)/1000
  dfparfin$wtrbllsexclout[i] <- sum((subsimnrm$Calratio[!subsimnrm$outlier]-subsimnrm$Vol[!subsimnrm$outlier])*
                                   nrmval*pi*10.3*10.3/40, na.rm = T)*
                                  ((max(subsimnrm$X)-min(subsimnrm$X))*(max(subsimnrm$Y)-min(subsimnrm$Y)))/
                                  (sum(!is.na(subsimnrm$Vol[!subsimnrm$outlier]))*pi*10.3*10.3/4)/1000
  dfparfin$totrf[i] <- sum(sum(subsimnrm$Calratio[subsimnrm$Comments == "TF" | subsimnrm$Comments == "DP"], na.rm = T)*nrmval*pi*20.3*20.3/40 , sum(subsimnrm$Calratio[subsimnrm$Comments != "TF" | subsimnrm$Comments != "DP"], na.rm = T)*nrmval*pi*10.3*10.3/40)/1000
  dfparfin$totrfexcl[i]<- sum(subsimnrm$Calratio[subsimnrm$Comments != "TF" | subsimnrm$Comments != "DP"], na.rm = T)*nrmval*pi*10.3*10.3/40/1000
  dfparfin$totrfexclout[i]<- sum(subsimnrm$Vol[!subsimnrm$outlier], na.rm = T)*nrmval*pi*10.3*10.3/40/1000
  dfparfin$flitn[i] <- lindat$Foliation
  dfparfin$angl[i] <- lindat$Angle.1
  dfparfin$brch[i] <- substr(lindat$Trial,1,3)
}
dfparfin$flitn <- round(dfparfin$flitn,2)
ggplot(dfparfin, aes(strbr, wtrblls)) + geom_point() +  theme_classic() + xlab("Canopy storage [kgs]") + ylab("Interception loss [kgs]") + geom_abline(intercept = 0, slope = 1) + facet_wrap(.~brch, scales = "free")
ggplot(dfparfin, aes(strbr, wtrbllsexcl)) + geom_point() +  theme_classic() + xlab("Canopy storage [kgs]") + ylab("Interception loss [kgs]") + geom_abline(intercept = 0, slope = 1) + facet_wrap(.~brch, scales = "free")
ggplot(dfparfin, aes(strbr, wtrbllsexclout)) + geom_point() +  theme_classic() + xlab("Canopy storage [kgs]") + ylab("Interception loss [kgs]") + geom_abline(intercept = 0, slope = 1) + facet_wrap(.~brch, scales = "free")


wtrblnc <- data.frame(melt(dfparfin[,c(5,10,11,12,13,14,15,16,19)], id = names(dfparfin[,c(5,10,14,15,16,19)])))
topwtrblnc <- melt(with(wtrblnc, tapply((strbr-value), list(variable,brch), FUN = function(x) sum(x, na.rm = T))))
wtrblnc$PPrat <- nocaldet$DP
dfparfin$PPrat <- nocaldet$DP
topwtrblnc2 <- melt(with(wtrblnc[wtrblnc$variable == "wtrblls",], tapply((strbr-value), brch, FUN = function(x) sum(x, na.rm = T))))
topwtrblnc2rf <- melt(with(wtrblnc[wtrblnc$variable == "wtrblls",], tapply(totrf, brch, FUN = function(x) sum(x, na.rm = T))))
topwtrblnc3 <- melt(with(wtrblnc[wtrblnc$variable == "wtrbllsexcl",], tapply((strbr-value), brch, FUN = function(x) sum(x, na.rm = T))))
topwtrblnc3rf <- melt(with(wtrblnc[wtrblnc$variable == "wtrbllsexcl",], tapply(totrfexcl, brch, FUN = function(x) sum(x, na.rm = T))))

topwtrblnc <- melt(with(wtrblnc, tapply((strbr-value), list(variable,brch), FUN = function(x) sum(x, na.rm = T))))


ggplot(topwtrblnc, aes(Var2, value)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Var1)) + theme_classic() + 
  xlab("Branch Name") + ylab(expression(paste(sum("(Branch Storage" - "(Rainfall - Throughfall))"), " [kg]"))) + 
  scale_fill_brewer(palette =  "Blues", direction = -1,
                    labels = c("wtrblls" = "All Gauges", "wtrbllsexcl" = expression("Exclude Pour Point\nand Stemflow Gauges"), "wtrbllsexclout" = "Exclude Outlier Gauges"),
                    name = "Sum Over")

ggplot(topwtrblnc, aes(Var2, value)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Var1)) + theme_classic() + 
  xlab("Branch Name") + ylab(expression(paste(sum("(Branch Storage" - "(Rainfall - Throughfall))"), " [kg]"))) + 
  scale_fill_brewer(palette =  "Blues", direction = -1,
                    labels = c("wtrblls" = "All Gauges", "wtrbllsexcl" = "Exclude Pour Point and Stemflow Gauges", "wtrbllsexclout" = "Exclude Outlier Gauges"),
                    name = "Sum Over")

dfparfin$clsam <- (dfparfin$strbr-dfparfin$wtrblls)
ggplot(dfparfin, aes(PPrat, clsam)) + xlab(expression(frac("Pour Point", "Rainfall"))) + ylab("(Branch Storage - (Rainfall - Throughfall)) [kg]") + 
  geom_smooth(aes(col = brch), method = 'lm', se = FALSE) + 
  geom_point(aes(fill = brch), shape = 21, size = 3) + scale_color_brewer(palette = "Set1", name = "Branch")+
  scale_fill_brewer(palette = "Set1", name = "Branch")+ theme_classic()
  
wtrblncrat <- dfparfin
wtrblncrat$wtrblls <- wtrblncrat$wtrblls/wtrblncrat$totrf
wtrblncrat$wtrbllsexclout <- wtrblncrat$wtrbllsexclout/wtrblncrat$totrfexclout
wtrblncrat$wtrbllsexcl <- wtrblncrat$wtrbllsexcl/wtrblncrat$totrfexcl
mwtrblncrat <- data.frame(melt(wtrblncrat[,c(5,10,11,12,13,14,15,16)], id = names(wtrblncrat[,c(5,10,14,15,16)])))
topwtrblncrat <- melt(with(mwtrblncrat, tapply((strbr-value), list(variable,brch), FUN = function(x) sum(x, na.rm = T))))
wtrblncrat$PPrat <- nocaldet$DP
dfparfin$PPrat <- nocaldet$DP


ggplot(topwtrblncrat, aes(Var2, value)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Var1)) + theme_classic() + 
  xlab("Branch Name") + ylab(expression(sum(frac("(Branch Storage" - "(Rainfall - Throughfall))","Rainfall")))) + 
  scale_fill_brewer(palette =  "Blues", direction = -1,
                    labels = c("wtrblls" = "All Gauges", "wtrbllsexcl" = "Exclude Pour Point and Stemflow Gauges", "wtrbllsexclout" = "Exclude Outlier Gauges"),
                    name = "Sum Over")

ggplot(topwtrblncrat, aes(Var2, value)) + geom_bar(stat = "identity", position = "dodge", aes(fill = Var1)) + theme_classic() + 
  xlab("Branch Name") + ylab(expression(paste(sum("(Branch Storage" - "(Rainfall - Throughfall))"), " [kg]"))) + 
  scale_fill_brewer(palette =  "Blues", direction = -1,
                    labels = c("wtrblls" = "All Gauges", "wtrbllsexcl" = "Exclude Pour Point and Stemflow Gauges", "wtrbllsexclout" = "Exclude Outlier Gauges"),
                    name = "Sum Over")

dfparfin$clsam <- (dfparfin$strbr-dfparfin$wtrblls)
ggplot(dfparfin, aes(PPrat, clsam/totrf)) + xlab(expression(frac("Pour Point", "Rainfall"))) + ylab(expression(frac("(Branch Storage - (Rainfall - Throughfall))", "Rainfall"))) + 
  geom_smooth(aes(col = brch), method = 'lm', se = FALSE) + 
  geom_point(aes(fill = brch), shape = 21, size = 3) + scale_color_brewer(palette = "Set1", name = "Branch")+
  scale_fill_brewer(palette = "Set1", name = "Branch")+ theme_classic()

ansgr <- 0
ansrat <- 0
for(i in 1:length(unique(dfparfin$brch))){
  sub <- dfparfin[dfparfin$brch == unique(dfparfin$brch)[i],]
  ansgr[i] <- summary(lm(clsam~PPrat, sub))$adj.r.squared
  ansrat[i] <- summary(lm((clsam/totrf)~PPrat, sub))$adj.r.squared
}

ggplot(robustdf[robustdf$Trial == "Br1Tr03",], aes(X,Y)) + 
     geom_tile(aes(fill = rexcprf),col = "black") + theme_classic() + 
     scale_fill_gradient2(name = expression(frac(Throughfall,Rainfall)),   low = muted("red"),mid = "white",
                       high = muted("blue"), midpoint = 1) + xlab("X [cm]") + ylab("Y [cm]") + ggtitle("Branch 1") + 
     geom_point(data = robustdf[robustdf$Trial == "Br1Tr03" & robustdf$outlier,], shape = 4, size = 3, colour = "white", stroke = 3) + 
     geom_point(data = robustdf[robustdf$Trial == "Br1Tr03" & robustdf$outlier,], shape = 4, size = 3, colour = "black", stroke = 2)
  
ggplot(robustdf[robustdf$Trial == "GBTr04",], aes(X,Y)) + 
  geom_tile(aes(fill = rexcprf),col = "black") + theme_classic() + 
  scale_fill_gradient2(name = expression(frac(Throughfall,Rainfall)),   low = muted("red"),mid = "white",
                       high = muted("blue"), midpoint = 1) + xlab("X [cm]") + ylab("Y [cm]") + ggtitle("Gingin Branch") + 
  geom_point(data = robustdf[robustdf$Trial == "GBTr04" & robustdf$outlier,], shape = 4, size = 3, colour = "white", stroke = 3) +
  geom_point(data = robustdf[robustdf$Trial == "GBTr04" & robustdf$outlier,], shape = 4, size = 3, colour = "black", stroke = 2)




XYposgg <- TFrdxyc[TFrdxyc$Comments == "TF" |TFrdxyc$Comments == "DP",]
manggcalav <- melt(with(TFrdxyc[TFrdxyc$Comments != "TF" |TFrdxyc$Comments != "DP",], tapply(Vol, list(X, Y), FUN = function(x)mean(x, na.rm = T))))
manggcalsd <- melt(with(TFrdxyc[TFrdxyc$Comments != "TF" |TFrdxyc$Comments != "DP",], tapply(Vol, list(X, Y), FUN = function(x)sd(x, na.rm = T))))
names(manggcalav)[1:2] <- c("X","Y")
dif <- merge(XYposgg,manggcalav)

# 
# for(i in 1:length(unique(robustdf$Trial))){
#   sub <- robustdf[robustdf$Trial == unique(robustdf$Trial)[i],]
#   jpeg(paste("Simulation ", as.character(i), "TF distribution.jpg"), width = 800, height = 800)
#   g1 <- ggplot(sub, aes(X, Y)) + geom_tile(aes(fill = rexcprf)) + theme_classic()+ ggtitle((sub$Trial)[1]) 
#   print(g1)
#   dev.off()
# }



# for(i in 1:length(unique(ansdftr2$event))){
#   sub <- ansdftr2[ansdftr2$event == unique(ansdftr2$event)[i],]
#   lindat <- evdt2[evdt2$Trial == unique(ansdftr2$event)[i],]
#   subpar <- as.numeric(dfparfin[i,])
#   subsimmm <- combmmsim[combmmsim$Trial == unique(ansdftr2$event)[i],]
#   subsimnrm <- robustdf[robustdf$Trial == unique(ansdftr2$event)[i],]
#   nrmval <- mean(subsimmm$Vol[subsimmm$X == 0])
#   if(!is.na(lindat$GStarttime)){
#     sub <- sub[sub$time >= lindat$GStarttime,]
#     
#   }
#   if(!is.na(lindat$DrainEndtime)){
#     sub <- sub[sub$time <= lindat$DrainEndtime,]
#   }
#   sub <- sub[complete.cases(sub),]
#   sub$ttu <- sub$time-sub$time[1]
#   sub$ttu <- as.numeric(sub$ttu)
#   jpeg(paste("Simulation ", as.character(i), "storage and model.jpg"), width = 800, height = 800)
#   g1 <- ggplot(sub, aes(ttu, storage)) + geom_point() + stat_function(fun = fitlc, col = "red", size = 1, args = list(tsrt=subpar[1], tend=subpar[2], tdend = subpar[3], inwt = subpar[4], mxwt = subpar[5], finwt = subpar[6], RF = subpar[7], FF= subpar[8], EVF = subpar[9])) + 
#           theme_classic() + xlab("Time [secs]") + ylab("Storage [kg]")
#   print(g1)
#   dev.off()
# }

sampling <- nocaldet[,c(21,5,45,44,43,22,25,23,26,24,27)]
sampling$Foliation[sampling$Foliation > 0.2 & sampling$Foliation < 0.4] <- 0.33
sampling$Foliation[sampling$Foliation > 0.5 & sampling$Foliation < 0.8] <- 0.67
sampling$control <- F
sampling$control[sampling$BrNa == "Br3"] <- T
head(sampling)
with(sampling, tapply(sm10, list(control, Foliation), FUN = function(x)(mean(x, na.rm = T))))
