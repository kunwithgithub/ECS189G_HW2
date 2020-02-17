dj_lm1 <- function(u.big) {
  dum_gen <- factorToDummies(u.big$gender, "gen")
  u.big <- cbind(u.big, dum_gen)
  dum_occ <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, dum_occ)
  
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
  u.big.tstX <- u.big.tst[,c(5,9:48)]
  u.big.tstY <- u.big.tst[,3]
  
  lmout <- lm(indexToStr(u.big,c(5,9:48)), data=u.big.trn)
  res <- predict(lmout, u.big.tstX)
  MAPE(res, u.big.tstY)
}
