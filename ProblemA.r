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
	
	require("MLmetrics") # a library that TA recommended
	MAPE(u.big.tst$rating,ans$p_rating)	#MAPE 
}







