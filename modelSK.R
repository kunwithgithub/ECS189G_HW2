#predictor: age, gender, ZIP, genre
sk_lm1 <-function(u.big){
	require(regtools)
	newAge <- u.big$age
	newAge[newAge<21]<-1
	newAge[newAge>=21]<-0
	u.big[,u.big$age]<-newAge
	
	tst <- tstRows()
	u.big.tst <- u.big[tst,]
	u.big.trn <- u.big[-tst,]
	u.big.tstX <- u.big.tst[,c(5,6,8,9:29)]
	u.big.tstY <- u.big.tst[,3] #rating
  
	lmout<-lm(ratings~transID+age+ZIP+gender+indexToStr(u.big,c(9:29)),u.big)
	lmout<-$coefficients
	res<-roundToNearestInt(predict(lmout,u.big.tstX))
	print(MAPE(res,u,big.tstY))
}

#predictor: age, gender, ZIP, transID, occ, genre
sk_lm2 <-function(u.big){
	require(regtools)
	
	tst <- tstRows()
	u.big.tst <- u.big[tst,]
	u.big.trn <- u.big[-tst,]
	u.big.tstX <- u.big.tst[,c(4:8,9:29)]
	u.big.tstY <- u.big.tst[,3] #rating
  
	lmout<-lm(ratings~age*transID+ZIP+gender*occ+indexToStr(u.big,c(9:29)),u.big)
	lmout<-$coefficients
	res<-roundToNearestInt(predict(lmout,u.big.tstX))
	print(MAPE(res,u,big.tstY))
}











