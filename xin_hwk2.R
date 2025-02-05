names(u.big)[13] <- "Children"
names(u.big)[19] <- "FilmNoir"
names(u.big)[24] <- "SciFi"

test <-function(u.big, predictors){
  # divide u.big into training set(95,000) and test set(5,000)
  tst <- tstRows()
  u.big.tst <- u.big[tst,]
  u.big.trn <- u.big[-tst,]
  u.big.tstX <- u.big.tst[,predictors]
  u.big.tstY <- u.big.tst[,3]
  
  # test
  lmout <- lm(as.formula(indexToStr(u.big,predictors)), data=u.big.trn)
  print(summary(lmout))
  res <- predict(lmout,u.big.tstX)
  res <- roundToNearestInt(res)
  print(MAPE(res, u.big.tstY))
}

# predictor: age, gender, genre, age * gender
# MAPE: 0.8792
# 
# (Intercept)          age      unknown       Action    Adventure    Animation     Children       Comedy        Crime  
# 3.241536     0.004220    -0.248046    -0.079992     0.079968     0.360551    -0.216660    -0.061846     0.090597  
# Documentary        Drama      Fantasy     FilmNoir       Horror      Musical      Mystery      Romance        SciFi  
# 0.245841     0.237842    -0.205075     0.382203    -0.126794     0.062688     0.109086     0.113067     0.098887  
# Thriller          War      Western          G.F    AgeGender  
# 0.022001     0.252465     0.186695     0.038649    -0.001054  
xin_lm1 <- function(u.big){
  require(regtools)
  # preprocess the dataset
  Female <- factorToDummies(u.big$gender, "G")
  u.big <- cbind(u.big, Female)
  AgeGender <- u.big$G.F * u.big$age
  u.big <- cbind(u.big, AgeGender)
  test(u.big, c(5,9:29))
  test(u.big, c(5,28:29))
  test(u.big, c(9:27))
}

# delete predictors: age/Action/Adventure/Comedy/Crime/Musical/Thriller/G.F/AgeGender
# add predictors: occ
#
# result:
# only delete predictors: MAPE == 0.8778
# add occ: MAPE == 0.8668
# (Intercept)            unknown          Animation           Children        Documentary              Drama  
# 3.20505           -0.25837            0.37287           -0.18515            0.30252            0.28059  
# Fantasy           FilmNoir             Horror            Mystery            Romance              SciFi  
# -0.20125            0.45654           -0.11228            0.14261            0.10097            0.09381  
# War            Western  occ.administrator         occ.artist         occ.doctor       occ.educator  
# 0.24045            0.16261            0.24508            0.24361            0.27235            0.26263  
# occ.engineer  occ.entertainment      occ.executive     occ.healthcare      occ.homemaker         occ.lawyer  
# 0.15755            0.06088           -0.04387           -0.50255           -0.08685            0.34594  
# occ.librarian      occ.marketing           occ.none          occ.other     occ.programmer        occ.retired  
# 0.14998            0.08114            0.44275            0.16314            0.19714            0.07647  
# occ.salesman      occ.scientist        occ.student     occ.technician  
# 0.21574            0.19770            0.14909            0.15146  
xin_lm2 <- function(u.big){
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, occs)
  cols <- ncol(u.big)
  test(u.big, c(9,12,13,16:20,22:24, 26:27))
  test(u.big, c(9,12,13,16:20,22:24,26:cols))
  test(u.big, c(28:cols))
  test(u.big, c(9,12,13,16:20,22:24,35,40))
}

# delete predictors: only keep occ.healthcare/occ.none
# add predictors: zips
#
# result:
# only keep occ.healthcare/occ.none: MAPE == 0.8678
# add zips: MAPE == 0.856
# Call:
#   lm(formula = as.formula(indexToStr(u.big, predictors)), data = u.big.trn)
# 
# Coefficients:
#   (Intercept)         unknown       Animation        Children     Documentary           Drama         Fantasy  
# 3.4334136      -0.1653752       0.3637532      -0.1941555       0.3218682       0.2856661      -0.2057935  
# FilmNoir          Horror         Mystery         Romance           SciFi             War         Western  
# 0.4559685      -0.1148742       0.1406393       0.0976434       0.0898271       0.2391770       0.1575646  
# occ.healthcare        occ.none          zip.00          zip.01          zip.02          zip.03          zip.04  
# -0.5843794       0.2642759       0.0981637       0.0643968      -0.0196617       0.1944078       0.5453723  
# zip.05          zip.06          zip.07          zip.08          zip.09          zip.10          zip.11  
# -0.1110604       0.1183995      -0.4741312      -0.0004584      -0.2130463      -0.3756738      -0.0675529  
# zip.12          zip.13          zip.14          zip.15          zip.16          zip.17          zip.18  
# -0.2502170       0.6503191      -0.1435134      -0.0608546       0.4046498       0.0450792      -0.0834124  
# zip.19          zip.20          zip.21          zip.22          zip.23          zip.24          zip.25  
# -0.2431859      -0.0282871      -0.7666451      -0.2112118      -0.0846085       0.1875271       1.2448882  
# zip.26          zip.27          zip.28          zip.29          zip.30          zip.31          zip.32  
# 0.8175737      -0.0240821      -0.1290899      -0.2364760      -0.0865278      -0.0626168      -0.1689044  
# zip.33          zip.34          zip.35          zip.36          zip.37          zip.38          zip.39  
# 0.0402286       0.0465475       0.4681876       0.0678235       0.1124735      -0.1945453       0.1559864  
# zip.40          zip.41          zip.42          zip.43          zip.44          zip.45          zip.46  
# 0.0152342       0.0065820      -0.4473553      -0.2955599      -0.0554294       0.2921177       0.0561294  
# zip.47          zip.48          zip.49          zip.50          zip.51          zip.52          zip.53  
# 0.0355642      -0.1108194      -0.0569878       0.2142628      -0.2479227       0.0772916       0.1307237  
# zip.54          zip.55          zip.56          zip.57          zip.58          zip.59          zip.60  
# 0.1849818      -0.1055791       0.0581740       0.6858939      -0.4487803       0.3480298      -0.1905355 
# zip.61          zip.62          zip.63          zip.64          zip.65          zip.66          zip.67  
# 0.0422954       0.0446770       0.1197793      -0.0973069      -0.2159072       0.3558455       0.3304363  
# zip.68          zip.70          zip.71          zip.73          zip.74          zip.75          zip.76  
# 0.1468447      -0.1406021       0.0498962       0.1675364      -0.3421216       0.0124640      -0.3178048  
# zip.77          zip.78          zip.79          zip.80          zip.81          zip.82          zip.83  
# -0.1521017      -0.0662654      -0.2078539      -0.1448695       0.2954237      -0.1490245      -0.1271796  
# zip.84          zip.85          zip.87          zip.89          zip.90          zip.91          zip.92  
# 0.1214577      -0.1079798       0.0386832       0.0984276      -0.0328096      -0.0901813      -0.0774546  
# zip.93          zip.94          zip.95          zip.96          zip.97          zip.98          zip.99  
# 0.1839383      -0.0848608       0.0354223      -0.1655208      -0.1572516       0.0140553      -0.0633585  
# zip.E2          zip.K7          zip.L1          zip.L9          zip.M4          zip.M7          zip.N2  
# -0.4208023       0.3358271       0.3186562      -0.2146814      -0.3049974      -0.9562872      -0.2465766  
# zip.N4          zip.R3          zip.T8          zip.V0          zip.V1          zip.V3          zip.V5  
# -0.0109369       0.2208700      -0.2186889      -0.3259007      -0.3258229      -0.2073788      -0.8368489  
xin_lm3 <- function(u.big){
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40))
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,48:cols))
  test(u.big, c(48:cols))
}

# delete predictors:
# only keep zip.10/zip.19/zip.21/zip.22/zip.29/zip.43/zip.50/zip.55/zip.60
xin_lm4 <- function(u.big){
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40))
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,48:cols))
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,58,69))
}

# first assumption: people of different ages will have different preference for movies
# input a column of genres, plot average rating of that genre versus age
# Comedy(14)?
age_genres <- function(u.big, genre_col) {
  require(ggplot2)
  age_factor <- as.factor(u.big$age)
  levels <- levels(age_factor)
  res <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    ageX <- u.big[age_factor == levels[i],]
    ageX <- ageX[ageX[,genre_col] == 1, ]
    average_rate <- mean(ageX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    res[i,1] <- as.numeric(levels[i])
    res[i,2] <- average_rate
    res[i,3] <- nrow(ageX)
  }
  res <- as.data.frame(res)
  names(res) <- c("age","rating","sample_size")
  print(res)
  p <- ggplot(data = res, aes(x=age, y=rating)) + geom_point()
  print(p)
}
# test interaction term:age*Drama
xin_lm5 <- function(u.big) {
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ")
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  age_fantasy <- u.big$age * u.big$Fantasy
  #age_western <- u.big$age * u.big$Western
  #age_war <- u.big$age * u.big$War
  #age_thriller <- u.big$age * u.big$Thriller
  #age_mystery <- u.big$age * u.big$Mystery
  #age_horror <- u.big$age * u.big$Horror
  u.big <- cbind(u.big, age_fantasy)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,48:ncol(u.big)))
}

# second assumption: people of different occupations will have different preference for movies
occ_genres <- function(u.big, genre_col) {
  require(ggplot2)
  occ_factor <- as.factor(u.big$occ)
  levels <- levels(occ_factor)
  res <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    occX <- u.big[occ_factor == levels[i],]
    occX <- occX[occX[,genre_col] == 1, ]
    average_rate <- mean(occX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    res[i,1] <- levels[i]
    res[i,2] <- average_rate
    res[i,3] <- nrow(occX)
  }
  res <- as.data.frame(res)
  names(res) <- c("occ","rating","sample_size")
  print(res)
  p <- ggplot(data = res, aes(x=occ, y=rating)) + geom_bar(stat = "identity") + ylab("Average Rating for Comeday")
  print(p)
}

xin_lm6 <- function(u.big) {
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ", FALSE)
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  age_fantasy <- u.big$age * u.big$Fantasy
  first_four <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator
  executive_comedy <- u.big$occ.executive * u.big$Comedy
  occ_mystrery <- first_four*u.big$Mystery
  artist_SciFi <- u.big$occ.artist * u.big$SciFi
  writer_SciFi <- u.big$occ.writer * u.big$SciFi
  writer_action <- u.big$occ.writer * u.big$Action
  high_degree_occ <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator + u.big$occ.lawyer
  #occ_drama <- first_four * u.big$Drama
  #executive_documentary <- u.big$occ.executive * u.big$Documentary
  #age_western <- u.big$age * u.big$Western
  #age_war <- u.big$age * u.big$War
  #age_thriller <- u.big$age * u.big$Thriller
  #age_mystery <- u.big$age * u.big$Mystery
  #age_horror <- u.big$age * u.big$Horror
  u.big <- cbind(u.big, executive_comedy, occ_mystrery, artist_SciFi, writer_SciFi, writer_action, high_degree_occ)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,49:ncol(u.big)))
}

# third assumption: movienum
# some movie just better than others!
rating_movienum <- function(u.big){
  require(ggplot2)
  mov_factor <- as.factor(u.big$movienum)
  levels <- levels(mov_factor)
  res <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    movX <- u.big[mov_factor == levels[i],]
    average_rate <- mean(movX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    res[i,1] <- as.numeric(levels[i])
    res[i,2] <- average_rate
    res[i,3] <- nrow(movX)
  }
  res <- as.data.frame(res)
  names(res) <- c("mov","rating","sample_size")
  print(res)
  p <- ggplot(data = res, aes(x=mov, y=rating)) + geom_point()
  print(p)
}

# define average rate >= 4.0, bestMovies
# define average rate <= 3.5, worstMovies
xin_lm7 <- function(u.big) {
  mov_factor <- as.factor(u.big$movienum)
  levels <- levels(mov_factor)
  res <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    movX <- u.big[mov_factor == levels[i],]
    average_rate <- mean(movX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    res[i,1] <- as.numeric(levels[i])
    if(average_rate >= 4.0)
      res[i,2] <- 1
    if(average_rate <= 3.5)
      res[i,3] <- 1
  }
  res <- as.data.frame(res)
  names(res) <- c("movienum","best_movie","worst_movie")
  
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ", FALSE)
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  age_fantasy <- u.big$age * u.big$Fantasy
  first_four <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator
  executive_comedy <- u.big$occ.executive * u.big$Comedy
  occ_mystrery <- first_four*u.big$Mystery
  artist_SciFi <- u.big$occ.artist * u.big$SciFi
  writer_SciFi <- u.big$occ.writer * u.big$SciFi
  writer_action <- u.big$occ.writer * u.big$Action
  high_degree_occ <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator + u.big$occ.lawyer
  u.big <- cbind(u.big, executive_comedy, occ_mystrery, artist_SciFi, writer_SciFi, writer_action, high_degree_occ)
  u.big <- merge(u.big, res, by = "movienum", all.x = TRUE)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,49:ncol(u.big)))
}

# usernum?
# might be some user just tend to be tougher than others?
# define average rate >= 4.0: nice_person
# define average rate <= 3.0: tough_person
xin_lm8 <- function(u.big){
  mov_factor <- as.factor(u.big$movienum)
  levels <- levels(mov_factor)
  mov <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    movX <- u.big[mov_factor == levels[i],]
    average_rate <- mean(movX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    mov[i,1] <- as.numeric(levels[i])
    if(average_rate >= 4.0)
      mov[i,2] <- 1
    if(average_rate <= 3.5)
      mov[i,3] <- 1
  }
  mov <- as.data.frame(mov)
  names(mov) <- c("movienum","best_movie","worst_movie")
  
  usr_factor <- as.factor(u.big$usernum)
  levels <- levels(usr_factor)
  usr <- matrix(0,nrow=length(levels),ncol = 3)
  for (i in 1:length(levels)) {
    usrX <- u.big[usr_factor == levels[i],]
    average_rate <- mean(usrX$rating)
    if(is.na(average_rate))
      average_rate <- 0
    usr[i,1] <- as.numeric(levels[i])
    if(average_rate >= 4.0)
      usr[i,2] <- 1
    if(average_rate <= 3.0)
      usr[i,3] <- 1
  }
  usr <- as.data.frame(usr)
  names(usr) <- c("usernum","nice_person","tough_person")
  
  require(regtools)
  # preprocess
  occs <- factorToDummies(u.big$occ, "occ", FALSE)
  u.big <- cbind(u.big, occs)
  zips <- factorToDummies(u.big$ZIP, "zip")
  u.big <- cbind(u.big, zips)
  cols <- ncol(u.big)
  age_fantasy <- u.big$age * u.big$Fantasy
  first_four <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator
  executive_comedy <- u.big$occ.executive * u.big$Comedy
  occ_mystrery <- first_four*u.big$Mystery
  student_mystrery <- u.big$occ.student * u.big$Mystery
  student_action <- u.big$occ.student * u.big$Action
  artist_SciFi <- u.big$occ.artist * u.big$SciFi
  writer_SciFi <- u.big$occ.writer * u.big$SciFi
  writer_action <- u.big$occ.writer * u.big$Action
  high_degree_occ <- u.big$occ.administrator + u.big$occ.artist + u.big$occ.doctor + u.big$occ.educator + u.big$occ.lawyer
  u.big <- cbind(u.big, executive_comedy, student_mystrery, artist_SciFi, writer_SciFi, writer_action)
  u.big <- merge(u.big, mov, by = "movienum", all.x = TRUE)
  u.big <- merge(u.big, usr, by = "usernum", all.x = TRUE)
  test(u.big, c(9,12,13,16:20,22:24,26:27,35,40,49:ncol(u.big)))
}

xin_nmf1 <- function(u.big.trn, u.big.tst) {
  
}
