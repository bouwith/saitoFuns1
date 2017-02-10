emotion_ratings <- function(id)
{
  path1 <- "F:/motoki_saito_exp/stimuli_generation/exp3_emotion/"

  dat <- read.csv(paste(path1,"p",as.character(id),".csv",sep=""))
  dir.create(paste(path1,"p",id,sep=""))

  #omit NA
  dat <- dat[!is.na(dat[,10]),c(1,10,12)]

  #subset emotion
  moji <- substring(as.character(dat[,1]),1,3)

  POdat <- dat[which(moji=="pos"),]
  NGdat <- dat[which(moji=="neg"),]
  NTdat <- dat[which(moji=="neu"),]

  POdat1 <- POdat[order(POdat[,2],decreasing=T),]
  NGdat1 <- NGdat[order(NGdat[,2],decreasing=F),]
  NTdat1 <- NTdat[NTdat[,2]==4,]
  if(nrow(NTdat1)<16)
  {
  	NTdat1 <- rbind(NTdat1,NTdat[NTdat[,2]==5,])
  }
  if(nrow(NTdat1)<16)
  {
  	NTdat1 <- rbind(NTdat1,NTdat[NTdat[,2]==3,])
  }

  res <- rbind(POdat1[sample(1:16),],
               NGdat1[sample(1:16),],
               NTdat1[sample(1:16),])

  files <- list.files(path1)


  cond <- c("ePO","eNG","enT")
  rcon <- c(0,16,32)
  plu16 <- c(0,16)
  for(h in 1:2)
  {
	for(i in 1:3)
	{
		for(j in 1:16)
		{
		num <- rcon[i]+j
		stiName <- as.character(res[num,1])
		file.copy(paste(path1,stiName,sep=""),paste(c(path1,"p",as.character(id),"/",cond[i],j+plu16[h],".jpg"),collapse=""))
		}
	}
  }

  write.csv(res,paste(path1,"p",as.character(id),"_emotion.csv",sep=""))

}

