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
  
	lmout<-lm(paste(indexToStr(u.big,c(9:27)),"+age*transID"),u.big)
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
  opts = r$tune(trnst, opts = list(dim = c(10, 20, 30), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))
  r$train(trnst, opts = list(dim = 20, nmf = TRUE))
  res <- r$predict(tststX, out_memory())
  print(MAPE(res, tststY))
}

u.big <- initialization()
colnames(u.big)[13]<-'Children'
colnames(u.big)[19]<-'FilmNoir'
colnames(u.big)[24]<-'SciFi'
sk_lm1(u.big)
sk_lm2(u.big)
sk_nmf(u.big)











