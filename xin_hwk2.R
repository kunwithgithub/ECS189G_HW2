# Example
# predictor: age, gender, genre
xin_lm1 <- function(u.big){
  require(regtools)
  # preprocess the dataset
  
  
  # divide u.big into training set(95,000) and test set(5,000)
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
}

xin_nmf1 <- function(u.big.trn, u.big.tst){
  
}