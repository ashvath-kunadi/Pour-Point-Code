intanl <- function(rf, thrf, dpsf, tstrt, tend, img = F, prefix = NA){
  evrf <- 0
  evtf <- 0
  ansdf <- data.frame()
  timint <- c(2, 3, 5, 7, 10, 15, 30, 60, 120)
  thrf <- thrf[!is.na(thrf$events),]
  for(i in 1:max(rf$events)){
      subrf <- rf[rf$events == i, ]
      subth <- thrf[thrf$events == i, ]
      subth <- subth[complete.cases(subth),]
      subdpsf <- dpsf[dpsf$events == i, -4]
      subdpsf <- subdpsf[!is.na(subdpsf$name),]
      if(nrow(subdpsf) != 0){
        for(t in 1:length(timint)){
          dt <- timint[t]
          top <- data.frame()
          nint <- ceiling(difftime(tend[i],tstrt[i], units = 'mins')/dt)
          comb <- rbind(subrf, subth, subdpsf)
          for(j in 1:nint){
            subevint <- comb[comb$time >= ((tstrt[i])+((j-1)*dt*60)) & comb$time < ((tstrt[i])+(j*dt*60)),]
            if(nrow(subevint)!=0){
              for(k in 1:length(unique(subevint$name))){
                subgaug <- subevint[subevint$name == unique(subevint$name)[k],]
                if(nrow(subgaug)!=0){
                  ans <- sum(subgaug$mm, na.rm = T)/dt
                  ansdf <- rbind(ansdf, data.frame(event = i, tmint = dt, tmid = ((tstrt[i])+((j-1)*dt*60)+(dt*30)), 
                                                   gauge = subgaug$name[1], intensity = ans))
                }
              }
            }
          }
        }
      }
    }
  return(ansdf)
}
