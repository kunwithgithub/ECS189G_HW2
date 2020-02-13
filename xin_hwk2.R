# Example
# predictor: age, gender, genre, age * gender
xin_lm1 <- function(u.big){
  # require(regtools)
  # preprocess the dataset
  Female <- factorToDummies(u.big$gender, "G")
  u.big <- cbind(u.big, Female)
  AgeGender <- u.big$G.F * u.big$age
  u.big <- cbind(u.big, AgeGender)
  
  # divide u.big into training set(95,000) and test set(5,000)
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
  u.big.tstX <- u.big.tst[,c(5,9:29)]
  u.big.tstY <- u.big.tst[,3]
  
  # test
  lmout <- lm(indexToStr(u.big,c(5,9:29)), data=u.big.trn)
  print(lmout)
  res <- predict(lmout,u.big.tstX)
  res <- roundToNearestInt(res)
  print(res)
  print(MAPE(res, u.big.tstY))
}

xin_lm2 <- function(u.big){
  require(regtools)
  # preprocess
  
}
xin_nmf1 <- function(u.big.trn, u.big.tst){
  
}