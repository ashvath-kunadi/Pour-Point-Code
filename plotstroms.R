plotstorms <- function(rf, thrf, dpsf, sm, img = F, prefix = NA){
  evrf <- 0
  evtf <- 0
  evdp <- matrix(NA, nrow = max(rf$events), ncol = length(unique(dpsf$name)))
  alltf <- matrix(NA, nrow = max(rf$events), ncol = length(unique(thrf$name)))
  smcntrldp <- data.frame()
  avgcntrl <- data.frame()
  dpsfcol <- c("darkorange3", "darkorange3", "darkorange3", "darkorange3","chocolate4", "darkorange3")
  thrf <- thrf[!is.na(thrf$events),]
  setwd("H:/My Documents/Project and related work/banksia at gingin/Alldpstrms")
    for(i in 1:max(rf$events)){
      subrf <- rf[rf$events == i, ]
      top <- data.frame()
      # top2 <- data.frame()
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
          evrf[i] <- max(subosubrf$value, na.rm = T)
          top <- rbind(top, subosubrf[,c(1,2,5,6,7,8)])
        }
      }
      subth <- thrf[thrf$events == i, ]
      subth <- subth[complete.cases(subth),]
      for(j in 1:length(unique(thrf$name))){
        subosubthrf <- subth[subth$name == unique(thrf$name)[j],]
        if(nrow(subosubthrf)!=0){
          subosubthrf$value <- cumsum(subosubthrf$mm)
          subosubthrf$col <- "grey30"
          subosubthrf$lab <- "TF Gauges"
          subosubthrf$alpha <- 0.5
          top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
          alltf[i,j] <- max(subosubthrf$value, na.rm = T)
        }
        if(j == length(unique(thrf$name))){
          if(nrow(subosubthrf)!=0){
            subosubthrf$value <- cumsum(subosubthrf$mm)
            subosubthrf$col <- "grey30"
            subosubthrf$lab <- "TF Gauges"
            subosubthrf$alpha <- 0.5
            top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
            alltf[i,j] <- max(subosubthrf$value, na.rm = T)
          }
          subosubthrf <- subth[order(subth$time),]
          if(nrow(subosubthrf)!=0){
            subosubthrf$mm <- subosubthrf$mm/length(unique(subth$name))
            subosubthrf$value <- cumsum(subosubthrf$mm)
            subosubthrf$col <- "forestgreen"
            subosubthrf$name <- "Average thrf"
            subosubthrf$lab <- "Average TF"
            subosubthrf$alpha <- 1
            evtf[i] <- max(subosubthrf$value, na.rm = T)
            top <- rbind(top, subosubthrf[,c(1,2,5,6,7,8)])
          }
        }
      }
      subdpsf <- dpsf[dpsf$events == i, ]
      subdpsf <- subdpsf[!is.na(subdpsf$name),]
      if(nrow(subdpsf) != 0){
        for(j in 1:length(unique(dpsf$name))){
          subosubdpsf <- subdpsf[subdpsf$name == unique(dpsf$name)[j],]
          if(nrow(subosubdpsf) != 0){
            subosubdpsf$value <- cumsum(subosubdpsf$mm)
            subosubdpsf$col <- dpsfcol[j]
            subosubdpsf$lab <- subosubdpsf$name[1]
            subosubdpsf$alpha <- 1
            evdp[i,j] <- max(subosubdpsf$value, na.rm = T)
            top <- rbind(top, subosubdpsf[,c(1,2,6,7,8,9)])
          }
        }
      }
      top <- top[!is.na(top$name),]
      if(img){
        jpeg(paste(prefix, "Storm", as.character(i), "Gauges.jpg"), width = 16, height = 10, units = "cm", res = 600)
        g1 <- ggplot(top, aes(time, value, group = name, color = col, alpha = alpha)) + geom_line(aes(col = col), size = 1.3) + theme_classic() + 
          xlim(evtim[[1]][i],evtim[[2]][i]) + scale_color_identity(labels = c(darkorange3 = "Pour Point", 
                                                                              chocolate4 = "SF", grey30 = "TF", 
                                                                              grey50 = "RF", forestgreen = "Avg TF",
                                                                              blue = "Avg RF"), guide = "legend") + 
          ggtitle(paste("Storm",as.character(i), "on", as.character(as.Date(evtim[[1]][i])))) + ylab("Cumilative depth in mm") + 
          theme(legend.position = "bottom", axis.text.x     = element_text(angle = 90))
        print(g1)
        dev.off()
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
        g2 <- ggplot(top2, aes(x, difffrmrf, group = group, col = colour, alpha = alpha)) + 
          geom_line(aes(colour = colour)) + theme_classic() + scale_color_identity() + theme(legend.position = "none") + 
          xlab("Time") + ylab("Difference from Average Rainfall") + 
          ggtitle(paste("Storm",as.character(i), "on", as.character(as.Date(evtim[[1]][i]))))
        jpeg(paste(prefix, "Storm Diff", as.character(i), "Gauges.jpg"), width = 16, height = 10, units = "cm", res = 600)
        print(g2)
        dev.off()
      }
      subsm <- sm[sm$events == i,]
      subsm <- subsm[complete.cases(subsm),]
      smtop <- data.frame()
      avgcntrlsub <- subsm[subsm$DP.Control == "Control" & subsm$NDP != "1" & subsm$Depth == 5.0,]
      cntrl <- data.frame()
      for(j in 1:length(unique(avgcntrlsub$NDP))){
        subsmNDP <- avgcntrlsub[avgcntrlsub$NDP == unique(avgcntrlsub$NDP)[j],]
        if(nrow(subsmNDP) != 0){
          if(j ==1){
            cntrl <- subsmNDP
            cntrl$Probe <- "Average"
            cntrl$variable <- "Average"
            cntrl$NDP <- "2 & 4"
          }
          if(as.character(subsmNDP$variable[!is.na(subsmNDP$variable)][1]) == "sm11"){
            cntrl$cntrldelvwc2 <- -subsmNDP$VWC + subsmNDP$VWC[1]
          }else{
            cntrl$cntrldelvwc4 <- subsmNDP$VWC - subsmNDP$VWC[1]
          }
          if(j == 2){
            cntrl$VWC <- apply(cbind(cntrl$cntrldelvwc4, cntrl$cntrldelvwc2), FUN = mean, 1)
            avgcntrl <- rbind(avgcntrl, cntrl)
          }
        }
      }

      for(j in 1:length(unique(subsm$NDP))){
        subsmNDP <- subsm[subsm$NDP == unique(subsm$NDP)[j],]
        for(k in 1:length(unique(subsmNDP$Depth))){
          subsmNDPdep <- subsmNDP[subsmNDP$Depth == unique(subsmNDP$Depth)[k],]
          cntrl <- subsmNDPdep[subsmNDPdep$DP != "DP",]
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
      smtop$logdep <- round(log10(smtop$Depth),2)
      smcntrldp <- rbind(smcntrldp, smtop)
      if(nrow(subsm)!= 0 & img){
        jpeg(paste(prefix, "Storm", as.character(i), "VWC.jpg"), width = 8, height = 13, units = "cm", res = 600)
        g2 <- ggplot(subsm, aes(time, VWC)) + geom_line(aes(col=DP.Control)) + facet_grid(Depth~NDP) + theme_classic() + 
          ggtitle(paste("Storm",as.character(i), "on", as.character(as.Date(evtim[[1]][i])))) + ylim(0,0.4)
        print(g2)
        dev.off()
        jpeg(paste(prefix, "Storm", as.character(i), "VWC tiled.jpg"), width = 8, height = 13, units = "cm", res = 600)
        g3 <- ggplot(smtop[smtop$delVWC >= -0.4 & smtop$delVWC <= 0.4,],aes(time,-logdep)) + geom_tile(aes(fill = delVWC)) + theme_classic() + 
          scale_fill_gradient2(name = expression(paste(Delta, "VWC"))) + ylab(expression(paste(log[10], "(z)"))) + 
          facet_grid(NDP~.) + ggtitle(paste("Storm",as.character(i), "on", as.character(as.Date(evtim[[1]][i]))))
        print(g3)
        dev.off()
      }
    }
  return(list(evrf, evtf, evdp, smcntrldp, alltf, avgcntrl))
}
