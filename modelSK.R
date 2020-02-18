source("./HwkII.R")

#predictor: age, gender, ZIP, genre
sk_lm1 <-function(u.big){
	require('regtools')
	newAge <- u.big$age
	newAge[newAge<21]<-1
	newAge[newAge>=21]<-0
	u.big$age<-newAge
	
	tst <- tstRows()
	u.big.tst <- u.big[tst,]
	u.big.trn <- u.big[-tst,]
	u.big.tstX <- u.big.tst[,c(5,6,8,9:27)]
	u.big.tstY <- u.big.tst[,3] #rating
  
	lmout<-lm(indexToStr(u.big,c(5,6,8,9:27)),u.big)
	res<-roundToNearestInt(predict(lmout,u.big.tstX))
	print(MAPE(res,u.big.tstY))
}

#predictor: age, gender, ZIP, transID, occ, genre
sk_lm2 <-function(u.big){
	require('regtools')
	
	tst <- tstRows()
	u.big.tst <- u.big[tst,]
	u.big.trn <- u.big[-tst,]
	u.big.tstX <- u.big.tst[,c(4:8,9:27)]
	u.big.tstY <- u.big.tst[,3] #rating
  
	lmout<-lm(indexToStr(u.big,c(4:8,9:27)),u.big)
	res<-roundToNearestInt(predict(lmout,u.big.tstX))
	print(MAPE(res,u.big.tstY))
}

u.big <- initialization()
colnames(u.big)[13]<-'Children'
colnames(u.big)[19]<-'FilmNoir'
colnames(u.big)[24]<-'SciFi'
sk_lm1(u.big)
sk_lm2(u.big)











