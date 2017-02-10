Face_TobiiDirName <- function(id)
{
	path1 <- "F:/motoki_saito_exp/stimuli_generation/Tobii_matrix/"
	
	path2 <- "F:/motoki_saito_exp/stimuli_generation/exp3_face/"
	path3 <- paste(path2,"p",id,"/",sep="")
	path4 <- paste(path3,"tobii_dir",sep="")
	dir.create(paste(path4,1,sep="")) #1
	dir.create(paste(path4,2,sep="")) #2
	dir.create(paste(path4,3,sep="")) #3
	dir.create(paste(path4,4,sep="")) #4
	
	path5 <- paste(path3,"combine",sep="")
	dir.create(path5)
	
	path6 <- "F:/motoki_saito_exp/stimuli_generation/exp3_emotion/"
	path7 <- paste(path6,"p",id,"/",sep="")
	emoFiles <- list.files(path7)
	
	for(i in 1:96)
	{
		file.copy(paste(path7,emoFiles[i],sep=""),paste(path5,"/",emoFiles[i],sep=""))
	}
	
	faceFiles <- list.files(path3)
	face.files <- faceFiles[grep("\\.jpg$", faceFiles)]
	for(i in 1:128)
	{
		file.copy(paste(path3,face.files[i],sep=""),paste(path5,"/",face.files[i],sep=""))
	}
	
	file1 <- "Face1.csv"
	dat <- read.csv(paste(path1,file1,sep=""))
	dirNames <- paste(dat[,1],".jpg",sep="")
	stimuliNames <- paste(dat[,2],".jpg",sep="")
	
	for(i in 1:64)
	{
		file.copy(paste(path5,"/",stimuliNames[i],sep=""),paste(path4,1,"/",dirNames[i],sep=""))
	}
	
	file1 <- "Face2.csv"
	dat <- read.csv(paste(path1,file1,sep=""))
	dirNames <- paste(dat[,1],".jpg",sep="")
	stimuliNames <- paste(dat[,2],".jpg",sep="")
	
	for(i in 1:64)
	{
		file.copy(paste(path5,"/",stimuliNames[i],sep=""),paste(path4,2,"/",dirNames[i],sep=""))
	}
	
	file1 <- "Face3.csv"
	dat <- read.csv(paste(path1,file1,sep=""))
	dirNames <- paste(dat[,1],".jpg",sep="")
	stimuliNames <- paste(dat[,2],".jpg",sep="")
	
	for(i in 1:64)
	{
		file.copy(paste(path5,"/",stimuliNames[i],sep=""),paste(path4,3,"/",dirNames[i],sep=""))
	}
	
	file1 <- "Face4.csv"
	dat <- read.csv(paste(path1,file1,sep=""))
	dirNames <- paste(dat[,1],".jpg",sep="")
	stimuliNames <- paste(dat[,2],".jpg",sep="")
	
	for(i in 1:64)
	{
		file.copy(paste(path5,"/",stimuliNames[i],sep=""),paste(path4,4,"/",dirNames[i],sep=""))
	}
}

for(i in 1:40)
{
	Face_TobiiDirName(i)
}



