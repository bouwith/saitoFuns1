House_TobiiDirName <- function(id)
{
	path1 <- "F:/motoki_saito_exp/stimuli_generation/Tobii_matrix/"
	file1 <- "House.csv"
	dat <- read.csv(paste(path1,file1,sep=""))
	dirNames <- paste(dat[,1],".jpg",sep="")
	stimuliNames <- paste(dat[,2],".jpg",sep="")
	
	path2 <- "F:/motoki_saito_exp/stimuli_generation/exp5_house/"
	path3 <- paste(path2,"p",id,"/",sep="")
	path4 <- paste(path3,"tobii_dir",sep="")
	dir.create(path4)
	
	for(i in 1:80)
	{
		file.copy(paste(path3,stimuliNames[i],sep=""),paste(path4,"/",dirNames[i],sep=""))
	}
}
for(i in 1:40)
{
	House_TobiiDirName(i)
}