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
tstSet <- u.big[tst,c(5:7,3)]
trnSet <- u.big[-tst,c(5:7,3)]

# Predict: movie rating
# Features: age, gender, occ
lmout1 <- lm(rating ~ age + gender + occ, data=trnSet)
res <- predict(lmout1, tstSet)
res <- roundToNearestInt(res)

err<-MAPE(res, tstSet[,ncol(tstSet)])