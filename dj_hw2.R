dj_lm1 <- function(u.big) {
  require(regtools)
  dum_gen <- factorToDummies(u.big$gender, "gen")
  u.big <- cbind(u.big, dum_gen)
  dum_occ <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, dum_occ)
  genderage <- u.big$dum_gen * u.big$age
  u.big <- c.bind(u.big, genderage)
  
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
  u.big.tstX <- u.big.tst[,c(5,9:49)]
  u.big.tstY <- u.big.tst[,3]
  
  lmout <- lm(as.formula(indexToStr(u.big,c(5,9:49))), data=u.big.trn)
  res <- predict(lmout, u.big.tstX)
  MAPE(res, u.big.tstY)
}

dj_lm2 <- function(u.big) {
 require(regtools)
 dum_gen <- factorToDummies(u.big$gender, "gen")
 u.big <- cbind(u.big, dum_gen)
 u.big$ZIP <- twodizip(u.big$ZIP)
 dum_zip <- factorToDummies(u.big$ZIP, "zip")
 u.big <- cbind(u.big, dum_zip)
  
 tst <- tstRows()
 u.big.tst <- u.big[tst,]
 u.big.trn <- u.big[-tst,]
 u.big.tstX <- u.big.tst[,c(5,9:123)]
 u.big.tstY <- u.big.tst[,3]
  
 lmout <- lm(as.formula(indexToStr(u.big,c(5,9:123))), data=u.big.trn)
 res <- predict(lmout, u.big.tstX)
 MAPE(res, u.big.tstY)
}

dj_nmf <- function(u.big) {
  require(recosystem)
  ty <- Reco()

  trnst <- data_memory(u.big.trn$usernum, u.big.trn$movienum, rating = u.big.trn$rating)
  tststX <- data_memory(u.big.tst$usernum, u.big.tst$movienum, rating = NULL)
  tststY <- u.big.tst[,3]

  ty$train(trnst, opts = list(dim = 20, nmf = TRUE))
  res <- ty$predict(tststX, out_memory())
  MAPE(res, tststY)
}
