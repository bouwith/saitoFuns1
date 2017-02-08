face_ratings <- function(id)
{
  #vector to pair
  path1 <- "F:/motoki_saito_exp/stimuli_generation/exp3_face/"
  Pairs <- function(vec)
  {
    Anum <- length(vec)

    if(Anum%%2==1)
    {
      Anum <- Anum-1
    }

    orderNum <- 1:Anum
    leftNum <- sample(orderNum,Anum/2)
    RightNum <- orderNum[!is.element(orderNum,leftNum)]
    res <- cbind(vec[leftNum],vec[RightNum])
    return(res)
  }

  #make pairs matrix
PairDat <- function(devdat,sex)
{
	count <- 0
	for(i in 1:5)
	{	
		Ratings <- c(1:5)
		subRatings <- unique(devdat[,2])
		if(is.element(Ratings[i],subRatings)==FALSE)
		{
			next
		}
		
		ddat <- subset(devdat,devdat[,2]==i)
		if(nrow(ddat)<2)
		{
			next
		}
		
		res <- as.data.frame(Pairs(as.character(ddat[,1])))
		res1 <- cbind(i,res,sex)
		colnames(res1) <- c("rating","u_name","b_name","gender")
		
		if(count==0)
		{
			res2 <- res1
			count <- count+1
		}
		else
		{
			res2 <- rbind(res2,res1)
		}
	}
	return(res2)

#devdat
#sex
}

  PairDevSam <- function(dat)
  {
    #NA omit
    sdat <- dat[!is.na(dat[,10]),c(1,10)]

    itemNumber <- as.integer(gsub(".jpg","",sdat[,1]))
    nM <- which(itemNumber>=500)
    nF <- which(!itemNumber>=500)
    #Data divided gender
    Mdat <- sdat[nM,]
    Fdat <- sdat[nF,]

    conditions <- rep(c("posi","nega","neutSame"),each=16)
    fdat <- PairDat(Fdat,"f")
    mdat <- PairDat(Mdat,"m")

    ramOrderF <- sample(1:nrow(fdat))
    ramOrderM <- sample(1:nrow(mdat))
    fdat1 <- cbind(fdat[ramOrderF[1:48],],conditions)
    mdat1 <- cbind(mdat[ramOrderM[1:48],],conditions)
    Res <- rbind(fdat1,mdat1)
    return(Res)

  }
  
  ratingDev <- function(rdat,sex)
  {
    ramOrder <- sample(1:nrow(rdat))
    rdat1 <- rdat[ramOrder,]

    rn <- nrow(rdat1)
    if(rn%%2==1)
    {
      rn <- rn-1
    }
    dev1 <- rdat1[1:(rn/2),]
    dev2 <- rdat1[((rn/2)+1):rn,]

    rating <- dev1[,2]-dev2[,2]
    mdat <- as.data.frame(cbind(as.character(dev1[,1]),as.character(dev2[,1])))
    mdat1 <- cbind(rating,mdat,sex)
    d2 <- subset(mdat1,mdat1[,1]==2)[1:4,]
    d1 <- subset(mdat1,mdat1[,1]==1)[1:4,]
    dn1 <- subset(mdat1,mdat1[,1]==-1)[1:4,]
    dn2 <- subset(mdat1,mdat1[,1]==-2)[1:4,]
    mdat2 <- rbind(d2,d1,dn1,dn2)
    colnames(mdat2) <- c("rating","u_name","b_name","gender")
    return(mdat2)
  }



  PairDevDif <- function(dat)
  {
    #NA omit
    sdat <- dat[!is.na(dat[,10]),c(1,10)]

    itemNumber <- as.integer(gsub(".jpg","",sdat[,1]))
    nM <- which(itemNumber>=500)
    nF <- which(!itemNumber>=500)
    #Data divided by gender
    Mdat <- sdat[nM,]
    Fdat <- sdat[nF,]

    fDat <- ratingDev(Fdat,"f")
    mDat <- ratingDev(Mdat,"m")
    Res2 <- rbind(fDat,mDat)
    conditions <- "NeutDif"
    Res3 <- cbind(Res2,conditions)
    return(Res3)
  }


  #Exection
  dat <- read.csv(paste(path1,"p",id,".csv",sep=""))
  Res5 <- rbind(PairDevSam(dat),PairDevDif(dat))
  write.csv(Res5,paste(path1,"p",id,"_face.csv",sep=""))
}