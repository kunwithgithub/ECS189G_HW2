ZIP_extractor<-function(ZIP){
	zip_code <- as.integer(ZIP)
	while(zip_code>=100){#truncating the number into two digits
		zip_code <- zip_code/10
	}
	as.integer(zip_code)
}

lmFinalModel<-function(u.big.tst){
  require(regtools)
  # preprocess the u.big.tst dataset
  #zips <- as.character(u.big.tst$ZIP)
  #zips <- substr(zips,1,2)
  #u.big.tst$ZIP <- as.factor(zips)
  names(u.big.tst)[13] <- "Children"
  names(u.big.tst)[19] <- "FilmNoir"
  names(u.big.tst)[24] <- "SciFi"
  # movienum
  best_movies <- c(12,22,23,45,48,50,56,57,59,60,64,79,83,89,96,98,100,113,
                   114,119,124,127,132,134,136,165,166,168,169,170,172,173,
                   174,178,180,181,183,185,187,189,190,191,192,194,197,198,
                   199,223,251,272,275,285,302,313,315,316,318,320,357,408,
                   427,430,474,478,479,480,483,484,487,488,489,490,493,494,
                   496,498,505,511,512,513,515,519,520,524,525,527,528,530,
                   589,601,603,604,606,611,613,615,633,641,647,648,651,654,
                   656,657,659,661,694,701,709,711,814,850,867,923,963,966,
                   1007,1039,1064,1080,1122,1125,1130,1131,1142,1143,1158,
                   1169,1189,1191,1194,1201,1203,1235,1251,1293,1367,1396,
                   1398,1449,1450,1452,1458,1466,1467,1482,1492,1498,1500,
                   1505,1512,1515,1516,1524,1525,1526,1533,1536,1537,1593,
                   1594,1599,1604,1613,1616,1623,1625,1629,1636,1639,1642,
                   1645,1650,1651,1653,1674)
  
  worst_movies <- c(2,3,5,13,16,17,18,20,21,24,25,26,27,29,33,34,35,36,37,38,39,40,41,43,44,49,51,
                    53,54,62,63,67,68,72,73,74,75,76,77,78,80,84,85,90,94,101,102,103,104,105,106,
                    107,108,109,110,111,112,118,120,121,122,123,130,138,139,140,141,142,145,146,
                    147,148,149,155,158,159,160,161,167,184,206,212,214,217,218,219,220,225,226,
                    227,229,230,231,232,233,235,240,243,244,245,247,249,250,252,254,255,259,260,
                    261,263,264,266,267,271,274,277,278,279,280,281,284,288,289,290,291,294,295,
                    296,299,301,308,309,312,314,319,321,322,323,324,325,326,327,328,329,330,332,
                    334,335,336,337,338,339,341,342,343,345,348,349,350,351,352,353,354,355,356,
                    358,359,360,361,362,363,364,365,366,367,368,369,370,371,373,374,375,376,377,
                    379,380,383,384,386,387,388,389,390,391,393,394,395,396,397,398,399,400,401,
                    402,403,405,406,407,409,410,411,412,413,415,416,417,422,424,426,437,438,439,
                    440,441,442,444,445,446,448,449,450,451,452,453,454,455,456,457,458,460,468,
                    472,473,476,477,501,532,534,535,536,538,539,540,541,542,544,545,546,547,548,
                    550,551,552,553,554,555,556,557,559,560,561,562,563,564,565,567,569,570,571,
                    572,573,574,575,576,577,578,579,580,581,583,585,586,590,592,593,594,595,596,
                    597,598,599,600,605,616,619,620,621,622,623,624,625,626,627,629,630,635,636,
                    637,640,642,643,649,658,662,665,666,667,668,669,670,672,674,676,677,678,679,
                    680,681,682,683,685,687,688,689,690,691,695,696,697,698,700,702,704,706,708,
                    712,714,715,716,717,719,720,721,722,723,724,725,726,727,728,729,730,731,733,
                    734,737,738,739,740,741,743,744,747,748,749,751,752,754,755,756,757,758,759,
                    760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,779,
                    780,781,782,783,784,785,786,787,788,790,791,793,795,796,797,798,799,800,801,
                    802,803,804,806,807,808,809,810,812,815,816,817,818,819,820,821,822,823,824,
                    825,826,827,828,829,830,831,832,833,834,838,839,840,841,842,843,844,845,846,
                    849,852,854,857,858,859,860,861,862,864,865,866,869,870,871,872,873,874,875,
                    876,877,878,879,880,881,882,883,884,885,886,888,889,890,891,892,893,894,895,
                    897,898,899,901,903,904,905,906,907,908,910,911,912,913,914,916,917,918,920,
                    924,925,926,927,928,929,930,931,932,933,934,935,937,938,940,941,942,943,944,
                    946,947,948,949,950,951,952,953,955,957,959,960,962,964,967,970,972,973,974,
                    975,976,977,978,979,980,981,982,983,984,985,986,987,988,989,990,991,992,994,
                    995,996,997,998,999,1000,1001,1002,1003,1004,1006,1008,1009,1010,1011,1013,
                    1014,1015,1016,1017,1018,1022,1023,1025,1026,1027,1028,1029,1030,1031,1032,
                    1033,1034,1035,1036,1037,1038,1040,1041,1042,1043,1044,1045,1046,1047,1048,
                    1049,1051,1052,1053,1054,1055,1056,1057,1058,1059,1060,1061,1065,1066,1067,
                    1068,1069,1071,1072,1074,1075,1076,1077,1078,1079,1081,1082,1083,1085,1086,
                    1087,1088,1089,1090,1091,1092,1093,1094,1095,1096,1097,1099,1100,1102,1104,
                    1105,1106,1107,1108,1109,1110,1111,1112,1113,1114,1115,1117,1118,1120,1126,
                    1127,1128,1132,1133,1134,1135,1136,1138,1139,1140,1141,1145,1146,1148,1150,
                    1151,1152,1153,1154,1155,1156,1157,1161,1162,1163,1164,1165,1166,1168,1170,
                    1171,1173,1174,1175,1176,1177,1178,1179,1180,1181,1182,1183,1184,1185,1186,
                    1187,1188,1190,1192,1195,1196,1198,1199,1200,1202,1205,1206,1207,1208,1209,
                    1210,1211,1212,1213,1214,1215,1216,1217,1218,1219,1220,1221,1222,1223,1224,
                    1225,1226,1227,1228,1229,1230,1231,1232,1233,1234,1236,1237,1238,1239,1241,
                    1242,1244,1245,1246,1247,1248,1249,1250,1252,1253,1254,1255,1256,1257,1258,
                    1259,1260,1261,1263,1264,1265,1266,1267,1268,1270,1271,1272,1273,1274,1275,
                    1276,1277,1279,1280,1281,1282,1283,1284,1285,1287,1288,1289,1290,1291,1292,
                    1294,1295,1296,1297,1299,1300,1302,1303,1304,1305,1306,1307,1308,1309,1310,
                    1311,1312,1313,1314,1315,1316,1317,1318,1319,1320,1321,1322,1323,1324,1325,
                    1326,1327,1328,1329,1330,1331,1332,1333,1334,1335,1336,1337,1338,1339,1340,
                    1341,1342,1343,1344,1345,1346,1347,1348,1349,1350,1351,1352,1353,1354,1355,
                    1356,1357,1358,1359,1360,1361,1362,1363,1364,1365,1366,1369,1370,1371,1372,
                    1373,1374,1376,1377,1378,1379,1380,1381,1382,1383,1384,1385,1386,1387,1388,
                    1389,1390,1391,1392,1393,1394,1395,1397,1399,1400,1401,1402,1403,1404,1405,
                    1406,1407,1408,1409,1410,1411,1412,1413,1414,1415,1416,1417,1419,1420,1421,
                    1422,1423,1424,1425,1426,1427,1429,1430,1431,1432,1433,1434,1435,1436,1437,
                    1438,1439,1440,1441,1442,1444,1445,1446,1447,1453,1454,1455,1457,1460,1461,
                    1463,1464,1465,1468,1469,1470,1471,1472,1474,1475,1476,1477,1478,1479,1480,
                    1481,1483,1484,1485,1486,1487,1488,1489,1490,1491,1493,1494,1496,1497,1499,
                    1501,1502,1503,1507,1508,1509,1510,1511,1513,1514,1517,1518,1519,1520,1521,
                    1522,1523,1527,1528,1529,1530,1531,1532,1534,1535,1538,1539,1540,1541,1542,
                    1543,1544,1545,1546,1547,1548,1549,1550,1551,1552,1553,1554,1555,1556,1557,
                    1559,1560,1561,1562,1563,1564,1565,1566,1567,1568,1569,1570,1571,1572,1573,
                    1574,1575,1576,1577,1578,1579,1580,1581,1582,1583,1584,1585,1586,1587,1588,
                    1589,1590,1591,1595,1596,1597,1598,1601,1602,1603,1605,1606,1607,1608,1609,
                    1610,1611,1614,1615,1617,1618,1619,1620,1621,1622,1624,1626,1627,1630,1631,
                    1632,1633,1634,1635,1637,1638,1640,1641,1644,1646,1647,1648,1649,1652,1654,
                    1655,1656,1657,1658,1659,1660,1661,1662,1663,1664,1665,1666,1667,1668,1669,
                    1670,1671,1672,1673,1675,1676,1677,1678,1679,1680,1681,1682)
  best_movie <- u.big.tst$movienum %in% best_movies
  worst_movie <- u.big.tst$movienum %in% worst_movies
  # usernum
  nice_persons <- c(4,9,10,12,14,16,24,25,34,46,52,60,79,89,90,96,97,118,127,131,136,137,138,
                    139,147,148,152,164,165,173,185,187,200,210,213,225,233,237,239,242,249,
                    252,257,260,261,263,264,270,272,274,278,283,287,292,295,296,298,312,315,
                    322,324,330,332,338,339,340,350,351,355,357,366,367,369,371,372,383,384,
                    388,392,415,419,420,427,438,440,457,462,464,469,472,474,477,503,507,512,
                    513,516,519,522,523,532,534,553,555,556,558,565,581,583,613,628,629,636,
                    640,641,644,680,686,688,691,694,701,747,759,765,767,770,772,777,794,799,
                    801,808,810,811,819,821,823,835,838,840,848,849,850,861,862,867,875,876,
                    882,888,891,892,907,909,923,928,939,941,942)
  tough_persons <- c(3,5,15,21,26,35,40,49,61,101,102,104,107,129,149,153,155,161,181,194,199,
                     202,205,206,217,224,229,246,255,268,289,302,336,399,405,418,425,445,446,
                     451,454,461,463,490,509,510,515,537,544,570,578,587,609,617,626,637,639,
                     646,653,655,656,660,685,698,702,713,724,726,729,736,761,774,778,782,797,
                     813,814,820,824,832,843,853,865,866,868,869,873,896,900,930,933)
  nice_person <- u.big.tst$usernum %in% nice_persons
  tough_person <- u.big.tst$usernum %in% tough_persons
  
  occs <- factorToDummies(u.big.tst$occ, "occ", FALSE)
  u.big.tst <- cbind(u.big.tst, occs)
  executive_comedy <- u.big.tst$occ.executive * u.big.tst$Comedy
  student_mystrery <- u.big.tst$occ.student * u.big.tst$Mystery
  artist_SciFi <- u.big.tst$occ.artist * u.big.tst$SciFi
  writer_SciFi <- u.big.tst$occ.writer * u.big.tst$SciFi
  writer_action <- u.big.tst$occ.writer * u.big.tst$Action
  u.big.tst <- cbind(u.big.tst, executive_comedy, student_mystrery, artist_SciFi, writer_SciFi, writer_action, best_movie, worst_movie, nice_person, tough_person)
  
  # calculation
  coefs <- c(3.7036323897362,0.504357257683445,0.171447190810902,-0.127708005409211,0.0707131587962175,0.0621790873648607,0.000759825341930069,0.0254993077560577,
             -0.029068935961735,0.0556986087134643,0.0373975035127715,0.0294848052499214,0.0294361604487492,0.0940044183404767,-0.24504860614798,0.408787617045819,
             -0.255876273711258,0.0224078605086261,0.0346920411119023,-0.0525256955495214,-0.0963853666448685,0.373796830175329,-0.626449431323673,0.561267381936944,
             -0.734638981282193)
  
  cols <- ncol(u.big.tst)
  tst <- u.big.tst[,c(9,12,13,16:20,22:24,26:27,35,40,49:cols)]
  tst <- cbind(1, tst)
  
  y <- as.matrix(tst) %*% coefs
  y <- roundToNearestInt(y)
  MAPE(u.big.tst$rating,y)
}

nmfFinalModel <- function(u.big.tst) {
  require(regtools)
  load("WH.RData")
  w <- wh$P[-1,]
  h <- wh$Q[-1,]
  a <- w %*% t(h)
  nmf.tst <- u.big.tst
  for (i in 1:nrow(nmf.tst)){
    nmf.tst$p2_rating[i] <- a[nmf.tst$usernum[i],nmf.tst$movienum[i]]
  }
  MAPE(nmf.tst$rating, nmf.tst$p2_rating)
}

nmf_gen <- function(u.big) {
	require(regtools)
	require(recosystem)
	ty <- Reco()

	tst <- tstRows()
	u.big.tst <- u.big[tst,]
	u.big.trn <- u.big[-tst,]
	trnst <- data_memory(u.big$usernum, u.big$movienum, rating = u.big$rating)
	tststX <- data_memory(u.big.tst$usernum, u.big.tst$movienum, rating = NULL)
	tststY <- u.big.tst[,3]

	ty$train(trnst, opts = list(dim = 25, niter = 80, lrate = 0.13, nmf = TRUE))
	res <- ty$predict(tststX, out_memory())

	wh <- ty$output(out_memory(),out_memory())
	save(wh, file = "WH.RData")
}

# form the database we are gonna use for Hwk2
initialization <- function() {
	library(regtools)
	ratings <- read.table('ml-100k/u.data')
	names(ratings) <- c('usernum', 'movienum', 'rating', 'transID')
	demog <- read.table('ml-100k/u.user', sep='|')
	names(demog) <- c('usernum', 'age', 'gender', 'occ', 'ZIP')
	u.big <- merge(ratings,demog,by.x=1,by.y=1)
	movies <- read.csv("ml-100k/u.item", sep="|", check.names=FALSE, header=FALSE, fill=TRUE)
	genres <- read.csv("ml-100k/u.genre", sep="|", check.names=FALSE, header=FALSE)
	movies <- movies[,c(-2:-5)]
	names(movies) <- c('movienum', as.character(genres[,1]))
	u.big <- merge(u.big, movies, by = "movienum", all.x = TRUE)
	u.big <- u.big[order(u.big$usernum, u.big$movienum),]
	# print(sum(is.na(u.big$ZIP)))
	# how to deal with ZIP
	zips <- as.character(u.big$ZIP)
	zips <- substr(zips,1,2)
	u.big$ZIP <- as.factor(zips)
	# print(sum(is.na(u.big$ZIP)))
	return(u.big)
}

avgOccRating <- function(df, occname) {
	ret <- list()
	for (i in 1:length(occname)) {
		index <- which(df$occ == occname[i])
		avgOccname <- mean(df$rating[index])
		ret[i] <- avgOccname
	}
	ret
}

tstRows <- function(seed=99){
  set.seed(seed)
  tstRows <- sample(1:100000, 5000)
  return(tstRows)
}

roundToNearestInt <- function(vec){
  a <- vec %% 1
  vec[a >= 0.5] = ceiling(vec[a >= 0.5])
  vec[a < 0.5] = floor(vec[a < 0.5])
  return(vec)
}

indexToStr <- function(u.big, index){
  str <- paste("rating ", "~",
        paste(' ', colnames(u.big)[index], ' ', sep = "", collapse = "+"),
        sep = "")
  return(str)
}


