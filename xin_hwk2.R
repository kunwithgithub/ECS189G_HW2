genres <- read.csv("ml-100k/u.genre", sep="|", check.names=FALSE, header=FALSE)
GenresName <- as.character(genres[,1])
# Example
# predictor: age, gender, genre, age * gender
xin_lm1 <- function(u.big){
  #require(regtools)
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
  lmout <- lm(rating ~ age + G.F + AgeGender + unknown + Action + Adventure + Animation + Comedy + 
                Crime + Documentary + Drama + Fantasy + Horror + Musical + Mystery + Romance +
                Thriller + War + Western, data=u.big)
  print(lmout)
  res <- predict(lmout,u.big.tstX)
  res <- roundToNearestInt(res)
  print(res)
  print(MAPE(res, u.big.tstY))
}

xin_nmf1 <- function(u.big.trn, u.big.tst){
  
}