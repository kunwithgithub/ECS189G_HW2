ZIP_extractor<-function(ZIP){
	zip_code <- as.integer(ZIP)
	while(zip_code>=100){#truncating the number into two digits
		zip_code <- zip_code/10
	}
	as.integer(zip_code)
}

lmFinalModel<-function(u.big.tst){
	ZIP <- u.big.tst$ZIP #parse it from u.big.tst
	u.big.tst$ZIP <- as.factor(sapply(ZIP,ZIP_extractor)) #reassign after factorizing the vector
	hardwired_coef<-c() #lm outside of this function
	p_rating<-rep(0,nrows(u.big.tst)) #create a vector of 0s
	ans<-as.dataframe(cbind(u.big.tst$userId
							,u.big.tst$movieId
							,u.big.tst$age
							,u.big.tst$gender
							,p_rating)%*%hardwired_coef)
	
	require(regtools) # a library that TA recommended
	MAPE(u.big.tst$rating,ans$p_rating)	#MAPE 
}

# form the database we are gonna use for Hwk2
ratings <- read.table('ml-100k/u.data')
names(ratings) <- c('usernum', 'movienum', 'rating', 'transID')
demog <- read.table('ml-100k/u.user', sep='|')
names(demog) <- c('usernum', 'age', 'gender', 'occ', 'ZIP')
u.big <- merge(ratings,demog,by.x=1,by.y=1)
movies <- read.csv("ml-100k/u.item", sep="|", check.names=FALSE, header=FALSE, fill=TRUE)
genres <- read.csv("ml-100k/u.genre", sep="|", check.names=FALSE, header=FALSE)
movies <- movies[,c(-2:-5)]
names(movies) <- c('movienum', as.character(genres[,1]))
u.big <- merge(u.big, movies, by = "movienum", all.x = TRUE)
u.big <- u.big[order(u.big$usernum, u.big$movienum),]


tstRows <- function(seed=99){
  set.seed(seed)
  tstRows <- sample(1:100000, 5000)
  return(tstRows)
}

roundToNearestInt <- function(vec){
  a <- vec %% 1
  vec[a >= 0.5] = ceiling(vec[a >= 0.5])
  vec[a < 0.5] = floor(vec[a < 0.5])
  return(vec)
}

indexToStr <- function(u.big, index){
  
}


