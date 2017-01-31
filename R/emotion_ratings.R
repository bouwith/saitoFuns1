emotion_ratings <- function(id)
{
  path1 <- "F:/motoki_saito_exp/stimuli_generation/exp3_emotion/"

  dat <- read.csv(paste(path1,"p",as.character(id),".csv",sep=""))

  #omit NA
  dat <- dat[!is.na(dat[,10]),c(1,10,12)]

  #subset emotion
  moji <- substring(as.character(dat[,1]),1,3)

  POdat <- dat[which(moji=="pos"),]
  NGdat <- dat[which(moji=="neg"),]
  NTdat <- dat[which(moji=="neu"),]

  POdat1 <- POdat[order(POdat[,2],decreasing=T),]
  NGdat1 <- NGdat[order(NGdat[,2],decreasing=T),]
  NTdat1 <- NTdat[order(NTdat[,2],decreasing=T),]

  res <- rbind(POdat1[1:16,],
               NGdat1[1:16,],
               NTdat1[1:16,])

  files <- list.files()


  cond <- c("ePO","eNG","enT")
  rcon <- c(0,16,32)

  for(i in 1:3)
  {
    for(j in 1:16)
    {
      num <- rcon[i]+j
      stiName <- as.character(res[num,1])
      file.copy(stiName,paste(c("p1/",cond[i],j,".jpg"),collapse=""))
    }
  }


  write.csv(res,paste(path1,"p",as.character(id),"_emotion.csv",sep=""))
}
