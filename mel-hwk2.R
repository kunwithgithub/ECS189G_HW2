" movienum usernum rating   transID age gender        occ   ZIP unknown
1          1       1      5 874965758  24      M technician 85711       0
510        2       1      3 876893171  24      M technician 85711       0
637        3       1      4 878542960  24      M technician 85711       0

    Action Adventure Animation Children's Comedy Crime Documentary Drama
1        0         0         1          1      1     0           0     0
510      1         1         0          0      0     0           0     0
637      0         0         0          0      0     0           0     0

    Fantasy Film-Noir Horror Musical Mystery Romance Sci-Fi Thriller War
1         0         0      0       0       0       0      0        0   0
510       0         0      0       0       0       0      0        1   0
637       0         0      0       0       0       0      0        1   0

    Western
1         0
510       0
637       0"

source("./HwkII.R")

u.big <- initialization()
tst <- tstRows()
tstSet <- u.big[tst,]
trnSet <- u.big[-tst,]

# Predict: rating
# Features: age x gender x usernum
# PE: 0.92
# Features: age x occ
# PE: 0.85
# Features: age x gender + usernum + movienum
# PE: 0.87


lmout1 <- lm(rating ~ age*occ, data=trnSet)
summary(lmout1)
res <- predict(lmout1, tstSet)
res <- roundToNearestInt(res)
indx <- which(abs(res - tstSet[["rating"]]) > 2)
subset <- tstSet[indx,]
plot(indx, subset$occ)
# errvec <- abs(res[-indx] - tstSet[["rating"]][-indx])
# err <- MAPE(res[-indx], tstSet[["rating"]][-indx])
errvec <- abs(res - tstSet[["rating"]])
err <- MAPE(res, tstSet[["rating"]])
err
plot(errvec)


# NMF starts here

library(recosystem)
r <- Reco()
train_set <- data_memory(trnSet$usernum, trnSet$movienum, rating=trnSet$rating) 
test_set <- data_memory(tstSet$usernum, tstSet$movienum, rating=NULL)
opts <- r$tune(train_set, opts=list(dim = c(20,30)))
opts
r$train(train_set, opts=c(niter=10))
pred <- r$predict(test_set, out_memory())
pred <- roundToNearestInt(pred)
err <- MAPE(pred, tstSet$rating)
err