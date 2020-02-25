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
	u.big.tstX <- u.big.tst[,c(1,2,4:8,9:27)]
	u.big.tstY <- u.big.tst[,3] #rating
  
	lmout<-lm(paste(indexToStr(u.big,c(1,2,9:27)),"+age*transID"),u.big)
	summary(lmout)
	res<-roundToNearestInt(predict(lmout,u.big.tstX))
	print(MAPE(res,u.big.tstY))
}


sk_nmf <- function(u.big) {
  require(recosystem)
  r <- Reco()
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
  trnst <- data_memory(u.big.trn$usernum, u.big.trn$movienum, u.big.trn$rating)
  tststX <- data_memory(u.big.tst$usernum, u.big.tst$movienum, NULL)
  tststY <- u.big.tst[,3]
  res = r$tune(
		trnst,
		opts = list(dim = c(10, 20, 30,40,50),
		costp_l1 = c(0,0.1,0.2), costq_l1 = c(0,0.1,0.2),
		lrate = c(0.05, 0.1, 0.2), nthread = 2)
	)


  r$train(trnst, opts = res$min)
  res <- r$predict(tststX, out_memory())
  print(MAPE(res, tststY))
}

u.big <- initialization()
colnames(u.big)[13]<-'Children'
colnames(u.big)[19]<-'FilmNoir'
colnames(u.big)[24]<-'SciFi'
sk_lm2(u.big)












