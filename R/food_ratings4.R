food_ratings4 <- function(id)
{
  path1 <- "F:/motoki_saito_exp/stimuli_generation/exp4_saccade/"
  path2 <- "F:/motoki_saito_exp/stimuli_generation/exp5_house/"

  #food selection 
  DivideGroup <- function(dat)
  {
    dat1 <- dat[,c(1,2,3,4)]
    tMed <- 2
    hMed <- 2
    group <- numeric(length(dat1[,1]))

    group[(dat1[,2]>=tMed)&(dat1[,3]>=hMed)] <- "A"
    group[(dat1[,2]>=tMed)&(dat1[,3]<hMed)] <- "B"
    group[(dat1[,2]<tMed)&(dat1[,3]>=hMed)] <- "C"
    group[(dat1[,2]<tMed)&(dat1[,3]<hMed)] <- "D"

    if(sum(group=="A")<20)
    {
      dn <- 20-sum(group=="A")
      group[which((group=="B")|(group=="C")|(group=="D"))[1:dn]] <- "A"
    }else if(sum(group=="B")<20)
    {
      dn <- 20-sum(group=="B")
      group[which((group=="A")|(group=="C")|(group=="D"))[1:dn]] <- "B"
    }else if(sum(group=="C")<20)
    {
      dn <- 20-sum(group=="C")
      group[which((group=="A")|(group=="B")|(group=="D"))[1:dn]] <- "C"
    }else if(sum(group=="D")<20)
    {
      dn <- 20-sum(group=="D")
      group[which((group=="A")|(group=="C")|(group=="B"))[1:dn]] <- "D"
    }

    dat2 <- cbind(dat1,group)
    dat3 <- dat2[order(dat2[,5]),]
    return(dat3)

    #variable#
    # dat - food rating data

    #return#
    # dat3 - rating data with rating category
  }

  SamGet <- function(dat,n=20)
  {
    rdat <- matrix(NA,4*n,4)
    group <- rep(c("a","b","c","d"),each=n)
    pro_name <- paste(group,rep(c(1:n),4),sep="")
    rdat2 <- as.data.frame(rdat)
    rdat2[,1] <- group
    rdat2[,2] <- pro_name
    g1 <- dat[sample(which(dat[,5] == "A"),n),c(1,4)]
    g2 <- dat[sample(which(dat[,5] == "B"),n),c(1,4)]
    g3 <- dat[sample(which(dat[,5] == "C"),n),c(1,4)]
    g4 <- dat[sample(which(dat[,5] == "D"),n),c(1,4)]

    rdat2[,3:4] <- rbind(g1,g2,g3,g4)
    colnames(rdat2) <- c("group","pro_name","pic_name","pref_rating")
    return(rdat2)
    #引数#
    # dat - data returned by DivideGroup
    # n - number in each group, defalt=20

    #返却値#
    # rdat2 - stimuli names
  }


  # Execution
  Tdat <- read.csv(paste(path1,"p",as.character(id),"t.csv",sep=""))
  Hdat <- read.csv(paste(path1,"p",as.character(id),"h.csv",sep=""))
  Pdat <- read.csv(paste(path1,"p",as.character(id),"p.csv",sep=""))

  Tdat1 <- Tdat[!is.na(Tdat[,10]),c(1,10)]
  Hdat1 <- Hdat[!is.na(Hdat[,10]),c(1,10)]
  Pdat1 <- Pdat[!is.na(Pdat[,10]),c(1,10)]

  mdat <- merge(Tdat1,Hdat1,by="food")
  mdat1 <- as.data.frame(merge(mdat,Pdat1,by="food"))

  ddat <- DivideGroup(mdat1)
  
  res <- SamGet(ddat)
  
  write.csv(res,paste(path1,"p",as.character(id),"_saccade.csv",sep=""),)
  write.csv(res,paste(path2,"p",as.character(id),"_saccade.csv",sep=""),)
}
