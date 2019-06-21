##### Ultra SVS Data #####

Ultra.Sag <- read.csv(file = "Ultra_Sag_raw.csv", header = TRUE, as.is = TRUE)

View(head(Ultra.Sag))

colnames(Ultra.Sag[1]) <- "LotID"

#### substr Lot ID ####

Ultra.Sag$ï..LOTID<- substr(Ultra.Sag$ï..LOTID,7,9)

Ultra.Sag$ï..LOTID <- paste0("R", Ultra.Sag$ï..LOTID)

class(Ultra.Sag) 


Ultra.Sag$Mean <- "NA"
Ultra.Sag$STDEV <- "NA"  # now it has 91 variables


#Random.f <- function(Data=data){
set.seed(1234)
x <- seq(from=1, to=1729, by=1)
#Data <- as.data.frame(Data)
Data=Ultra.Sag
for (i in x){
  if (is.na(Data[i,89])==FALSE){ 
    index = sample(1:80, 35, replace=FALSE)
    index <- as.numeric(index)
    Data[i,90] <- round(mean(as.numeric(unlist(as.data.frame(Data[i,10:89])[,c(index)])), trim = 0, na.rm = TRUE) , digits = 2)
    Data[i,91]=round(sd(as.numeric(unlist(as.data.frame(Data[i,10:89])[,c(index)])), na.rm = TRUE), digits = 2)
  }else if (is.na(Data[i,59])==FALSE & is.na(Data[i,60])==TRUE){
    index = sample(1:50, 25, replace=FALSE)
    index <- as.numeric(index)
    Data[i,90]= round(mean(as.numeric(unlist(as.data.frame(Data[i,10:59])[,c(index)])), trim=0, na.rm=TRUE) , digits = 2)
    Data[i,91]=round(sd(as.numeric(unlist(as.data.frame(Data[i,10:59])[,c(index)])), na.rm = TRUE), digits = 2)
  } else if (is.na(Data[i,41])==FALSE & is.na(Data[i,42])==TRUE){
    index = sample(1:32, 20, replace=FALSE)
    index <- as.numeric(index)
    Data[i,90]= round(mean(as.numeric(unlist(as.data.frame(Data[i,10:41])[,c(index)])), trim=0, na.rm=TRUE),digits = 2)
    Data[i,91]=round(sd(as.numeric(unlist(as.data.frame(Data[i,10:41])[,c(index)])), na.rm = TRUE), digits = 2)
  } else if (is.na(Data[i,29])==FALSE & is.na(Data[i,30])==TRUE){
    index = sample(1:20, 15, replace=FALSE)
    index <- as.numeric(index)
    Data[i,90]= round(mean(as.numeric(unlist(as.data.frame(Data[i,10:29])[,c(index)])), trim=0, na.rm=TRUE),digits = 2)
    Data[i,91]=round(sd(as.numeric(unlist(as.data.frame(Data[i,10:29])[,c(index)])), na.rm = TRUE), digits = 2)
  }
  Data
  #  return(data=Data)
}
#}

View(head(Data))

Ultra.Sag=Data


## make a copy

Ultra.Sag2 <- Ultra.Sag

View(head(Ultra.Sag2))


TableB6 <- read.table(header = TRUE, as.is = FALSE,
                      text = "SampleSize Fvalue
                      3 0.436
                      4 0.374
                      5 0.346
                      7 0.318
                      10 0.298
                      15 0.284
                      20 0.277
                      25 0.273
                      30 0.271
                      35 0.267
                      50 0.261
                      75 0.256
                      100 0.253
                      150 0.25
                      200 0.249")  ## AQL=2.5

TableB6.2 <- read.table(header = TRUE, as.is = FALSE,
                        text = "SampleSize Fvalue
                        
                        4 0.353
                        5 0.323
                        7 0.295
                        10 0.275
                        15 0.262
                        20 0.256
                        25 0.252
                        30 0.249
                        35 0.246
                        50 0.241
                        75 0.236
                        100 0.233
                        150 0.23
                        200 0.23")  ## AQL=1.5


Ultra.Sag <- Ultra.Sag2

View(head(Ultra.Sag))

Ultra.Sag$Z4size <- NA
Ultra.Sag$Z9size <- NA


#Sample.f <- function(Data){
Data=Ultra.Sag
for (i in 1:nrow(Data)) {
  if (is.na(Data[i,89])==FALSE){
    Data[i,92]=80
    Data[i,93]=35}
  else if (is.na(Data[i,59])==FALSE & is.na(Data[i,60]==TRUE)){
    Data[i,92]=50
    Data[i,93]=25}
  else if (is.na(Data[i,41])==FALSE & is.na(Data[i,42])==TRUE){
    Data[i,92]=32
    Data[i,93]=20}
  else if (is.na(Data[i,29])==FALSE & is.na(Data[i,30])==TRUE){
    Data[i,92]=20
    Data[i,93]=15}
}
#}

Ultra.Sag=Data

View(head(Ultra.Sag))

Ultra.Sag$SampleSize <-NA

Ultra.Sag$SampleSize <- Ultra.Sag$Z9size

View(head(Ultra.Sag))


require(plyr)

plyr1 <- join(Ultra.Sag, TableB6, by = "SampleSize")

View(head(plyr1))

Ultra.Sag <- plyr1


Ultra.Sag$MSD <- NA

#plyr1$MSD <- 
for (i in 1:nrow(Ultra.Sag)) {
  Ultra.Sag[i,96]=Ultra.Sag[i,95]*(Ultra.Sag[i,8]-Ultra.Sag[i,6])
}

View(head(Ultra.Sag))


TableB3 <- read.table(header = TRUE, as.is = FALSE,
                      text = "SampleSize Mvalue
                      3 7.59
                      4 10.88
                      5 9.8
                      7 8.40
                      10 7.26
                      15 6.55
                      20 6.18
                      25 5.98
                      35 5.58
                      50 5.21
                      75 4.83
                      100 4.67
                      150 4.42
                      200 4.39")  ## AQL=2.5

TableB3.2 <- read.table(header = TRUE, as.is = FALSE,
                        text = "SampleSize Mvalue
                        
                        4 5.46
                        5 5.82
                        7 5.34
                        10 4.72
                        15 4.32
                        20 4.10
                        25 3.97
                        35 3.7
                        50 3.44
                        75 3.17
                        100 3.06
                        150 2.88
                        200 2.86")  ## AQL=1.5


plyr2 <- join(Ultra.Sag, TableB3, by = "SampleSize")

View(head(plyr2))

Ultra.Sag <- plyr2

View(head(Ultra.Sag))

### read in table B5 ###


TableB5 <- read.csv(file = "Table_B5_sub_2.csv", header = TRUE, as.is = FALSE)

View(TableB5)



View(head(Ultra.Sag))


### calculate the QL and QU ###

Ultra.Sag$QL <- NA
Ultra.Sag$QU <- NA


grep("Mean", colnames(Ultra.Sag)) ## 90
grep("MINIMUM", colnames(Ultra.Sag)) ## 6
grep("STDEV", colnames(Ultra.Sag)) ## 91
grep("MAXIMUM", colnames(Ultra.Sag)) ## 8


for (i in 1:nrow(Ultra.Sag)) {
  Ultra.Sag[i,98] <- round((as.numeric(Ultra.Sag[i,90])-as.numeric(Ultra.Sag[i,6]))/as.numeric(Ultra.Sag[i,91]), digits=2) # QL
  Ultra.Sag[i,99] <- round((as.numeric(Ultra.Sag[i,8])-as.numeric(Ultra.Sag[i,90]))/as.numeric(Ultra.Sag[i,91]), digits=2) # QU
}

View(head(Ultra.Sag))


Ultra.Sag[is.na(Ultra.Sag[,90:ncol(Ultra.Sag)])==TRUE] ## check if there are missing values in the calculations



Ultra.Sag$PL <- NA
Ultra.Sag$PU <- NA

View(head(TableB5))

TableB5.1 <- TableB5[,1:2]

View(TableB5.1)

class(TableB5.1)

names(TableB5.1) <- c("QL", "PL")
TableB5.2 <- TableB5[,3:4]
names(TableB5.2) <- c("QU", "PU")

View(TableB5.2)


plyr3 <- join(Ultra.Sag, TableB5.1, by = "QL")
plyr4 <- join(Ultra.Sag, TableB5.2, by = "QU")


View(head(plyr3)) ## want "PL"
View(head(plyr4)) ## want "PU"


str(plyr3)

View(plyr3)

nrow(plyr3)

View(head(Ultra.Sag))


Ultra.Sag$PL <- NA
Ultra.Sag$PU <- NA


Ultra.Sag2 <- Ultra.Sag  ## make a copy

PL <- plyr3[,ncol(plyr3)]
PU <- plyr4[,ncol(plyr4)]

PL.df <- as.data.frame(PL)
PU.df <- as.data.frame(PU)

PL.df[is.na(PL.df)==TRUE,] <- 0
PU.df[is.na(PU.df)==TRUE,] <- 0

str(PL.df)
str(plyr3)
str(PU.df)


Ultra.Sag2$PL <- PL.df
Ultra.Sag2$PU <- PU.df

View(head(Ultra.Sag2))

names(Ultra.Sag2[,100:101]) <- c("PL","PU")

Ultra.Sag2$P <- NA

Ultra.Sag2$P <- Ultra.Sag2$PL+Ultra.Sag2$PU

View(head(Ultra.Sag2))


grep("P", colnames(Ultra.Sag2)) ## column index = 102
grep("Mvalue", colnames(Ultra.Sag2)) ## column index = 97



Ultra.Sag2$Accept <- NA


for (i in 1:nrow(Ultra.Sag2)) {
  if (Ultra.Sag2[i,102] <= Ultra.Sag2[i,97]){
    Ultra.Sag2[i,103] <- "YES"
  }else{
    Ultra.Sag2[i,103] <- "NO"
  }
}

View(head(Ultra.Sag2))


Ultra.Sag2$PofA <- NA


for (i in 1:nrow(Ultra.Sag2)) {
  Ultra.Sag2[i,104] <- nrow(Ultra.Sag2[Ultra.Sag2$Accept=="YES",])/nrow(is.na(Ultra.Sag2)==FALSE)
  Ultra.Sag2[i,104] <- round((Ultra.Sag2[i,104])*100, digits = 0)
  
}



View(head(Ultra.Sag2))


str(Ultra.Sag2)

mean <- Ultra.Sag2$Mean
mean.n <- as.numeric(as.character(mean))

Ultra.Sag3 <- Ultra.Sag2
mean.n <- as.data.frame(mean.n)
Ultra.Sag3$Mean <- mean.n

str(Ultra.Sag3)
SD <- Ultra.Sag2$STDEV
SD.df <- as.data.frame(SD)
View(head(SD.df))

Ultra.Sag3$STDEV <- SD.df

str(Ultra.Sag3)

View(head(Ultra.Sag3))

### save as CSV file

write.csv(Ultra.Sag3$Mean, file = "Ultra_Sag_Mean.csv")

write.csv(Ultra.Sag3$STDEV, file = "Ultra_Sag_SD.csv")

write.csv(Ultra.Sag3$SampleSize, file = "Ultra_Sag_SampleSize.csv")

write.csv(Ultra.Sag3$Fvalue, file = "Ultra_Sag_Fvalue.csv")

write.csv(Ultra.Sag3$MSD, file = "Ultra_Sag_MSD.csv")

write.csv(Ultra.Sag3$Mvalue, file = "Ultra_Sag_Mvalue.csv")

write.csv(Ultra.Sag3$QL, file = "Ultra_Sag_QL.csv")

write.csv(Ultra.Sag3$QU, file = "Ultra_Sag_QU.csv")

write.csv(Ultra.Sag3$PL, file = "Ultra_Sag_PL.csv")

write.csv(Ultra.Sag3$PU, file = "Ultra_Sag_PU.csv")

write.csv(Ultra.Sag3$P, file = "Ultra_Sag_P.csv")

A <- Ultra.Sag3$Accept

A <- as.factor(A)

write.csv(A, file = "Ultra_Sag_Accept.csv")

write.csv(Ultra.Sag3$PofA, file = "Ultra_Sag_PofA.csv")




View(head(plyr3))
View(head(plyr4))


for (i in nrow(plyr3)) {
  if (is.na(plyr3[i,112])==TRUE){
    plyr3[i,112] ==0
  }
}


plyr3[is.na(plyr3$QU)==TRUE] <- 0
plyr4[is.na(plyr4$QL)==TRUE] <- 0


PU <- plyr3$QU
PL <- plyr4$QL
P <- PU+PL

Ultra.Sag2$PU <- NA
Ultra.Sag2$PL <- NA

Ultra.Sag2$PU <- PU
Ultra.Sag2$PL <- PL
Ultra.Sag2$P <- P


Ultra.Sag2$Accept <- NA

grep("P", colnames(Ultra.Sag2)) ## column index = 103
grep("Mvalue", colnames(Ultra.Sag2)) ## column index = 98



for (i in 1:nrow(Ultra.Sag2)) {
  if (Ultra.Sag2[i,103] <= Ultra.Sag2[i,98]){
    Ultra.Sag2[i,104] <- "YES"
  }else{
    Ultra.Sag2[i,104] <- "NO"
  }
}

View(head(Ultra.Sag2))


Ultra.Sag2$PofA <- NA


for (i in 1:nrow(Ultra.Sag2)) {
  Ultra.Sag2[i,105] <- nrow(Ultra.Sag2[Ultra.Sag2$Accept=="YES",])/nrow(is.na(Ultra.Sag2)==FALSE)
  Ultra.Sag2[i,105] <- (Ultra.Sag2[i,105])*100
  
}


View(head(Ultra.Diam2))


### save as CSV file

write.csv(Ultra.Sag2, file = "Ultra_SVS_Sag.0614.csv")








### calculate the QL and QU ###

Ultra.Sag$QL <- NA
Ultra.Sag$QU <- NA


grep("Mean", colnames(Ultra.Sag)) ## 90
grep("MINIMUM", colnames(Ultra.Sag)) ## 6
grep("STDEV", colnames(Ultra.Sag)) ## 91
grep("MAXIMUM", colnames(Ultra.Sag)) ## 8


for (i in 1:nrow(Ultra.Sag)) {
  Ultra.Sag[i,98] <- round((as.numeric(Ultra.Sag[i,90])-as.numeric(Ultra.Sag[i,6]))/as.numeric(Ultra.Sag[i,91]), digits=2) # QL
  Ultra.Sag[i,99] <- round((as.numeric(Ultra.Sag[i,8])-as.numeric(Ultra.Sag[i,90]))/as.numeric(Ultra.Sag[i,91]), digits=2) # QU
}

View(head(Ultra.Sag))


################################# use only two columns of Table B5 as the rounding would be very similar ####

Data = Ultra.Sag2

View(head(Table.B5.sub))
grep("QU", colnames(Ultra.Sag2)) ## column index = 100
grep("QL", colnames(Ultra.Sag2)) ## column index = 99

Data$PL <- NA
Data$PU <- NA

for(j in 1:nrow(Data)){
  for(i in 1:nrow(Table.B5.sub)){
    if (Data[j,99]<3.9 & Data[j,99] == Table.B5.sub[i,1]){
      Data[j,101] <- Table.B5.sub[i,2]
    }
    if (Data[j,100]<3.9 & Data[j,100] == Table.B5.sub[i,1]){
      Data[j,102] <- Table.B5.sub[i,2]
    } else{
    Data[j,101]=0
    Data[j,102]=0
    }
  }
  Data
}

View(head(Data)) ## OK


Ultra.Sag2 <- Data


Ultra.Diam2[is.na(Ultra.Diam2)] ## 0


Ultra.Sag2$P <-NA

Ultra.Sag2$P <- Ultra.Sag2$PL+Ultra.Sag2$PU


Ultra.Sag2$Accept <- NA

grep("P", colnames(Ultra.Sag2)) ## column index = 103
grep("Mvalue", colnames(Ultra.Sag2)) ## column index = 98



for (i in 1:nrow(Ultra.Sag2)) {
  if (Ultra.Sag2[i,103] <= Ultra.Sag2[i,98]){
    Ultra.Sag2[i,104] <- "YES"
  }else{
    Ultra.Sag2[i,104] <- "NO"
  }
}

View(head(Ultra.Sag2))


Ultra.Sag2$PofA <- NA


for (i in 1:nrow(Ultra.Sag2)) {
  Ultra.Sag2[i,105] <- nrow(Ultra.Sag2[Ultra.Sag2$Accept=="YES",])/nrow(is.na(Ultra.Sag2)==FALSE)
  Ultra.Sag2[i,105] <- (Ultra.Sag2[i,105])*100
  
}


View(head(Ultra.Diam2))


### save as CSV file

write.csv(Ultra.Sag2, file = "Ultra_SVS_Sag.0614.csv")


