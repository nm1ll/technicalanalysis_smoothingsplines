library(fTrading)
options(scipen=999)

library(RMySQL)
dbConnect(MySQL(), dbname = "cryptocurrency", username ="root",
          password = "asdf", host = "localhost", unix.socket = NULL, port = 0,
          client.flag = 0, groups = "rs-dbi", default.file = NULL)

mydb = dbConnect(MySQL(), user = 'root', password = 'asdf', dbname = 'cryptocurrency', host = 'localhost')
dbListTables(mydb)
dbListFields(mydb, 'gdax_BTCUSD')

res = dbSendQuery(mydb, "SELECT * FROM gdax_BTCUSD")
data = fetch(res, n = -1)

# Moving-Up-Stream
# Charting type I

volumes <- c()
returns_min1 <- c()
returns_min2 <- c()
returns_min3 <- c()
returns_min4 <- c()
returns_min5 <- c()
returns_min10 <- c()
returns_min15 <- c()
returns_min20 <- c()
returns_min25 <- c()
returns_min30 <- c()

# The loop
for(i in 1:(length(data$close)-35-30)){
timestamp <- data$timestamp[i:(34+i)]
price <- data$close[i:(34+i)]
volume <- data$volume[i:(34+i)]
price_trade <- data$close[(i+34):(i+65)]
times<- data$utc_datetime[(i+34)]

btcsmooth<-smooth.spline(timestamp,price,cv=FALSE)


#################################################
# Mark the turning points - extreme values
#################################################
n<-length(btcsmooth$x)
turning_points <- rep(0,n)
#first and last observation are regarded as dummy points
#And they will not become the turning points
#first observation as dummy point
turning_points[1] <- 1
#last observation as dummy point
turning_points[n] <- n
#this loop collects all turning points from the kernel estimator
for(j in 2:(n-1))
{
  delta1 <- btcsmooth$y[j] - btcsmooth$y[j-1]
  delta2 <- btcsmooth$y[j+1] - btcsmooth$y[j]
  
  if ((delta1*delta2) < 0){
    turning_points[j]<-j
  } else if ((delta1*delta2) == 0)
  {
    #print(paste("Found intermediate point",i))
  } else if ((delta1*delta2) > 0)
  {
    #print(paste("Found intermediate point",i))
  }        			
}

numberExtremeValue<-length(which((turning_points != 0) & (turning_points != 1) & (turning_points != 35)))
ExtremeValue<-which((turning_points != 0) & (turning_points != 1) & (turning_points != 35))
E1<-btcsmooth$y[ExtremeValue[numberExtremeValue-4]]
E2<-btcsmooth$y[ExtremeValue[numberExtremeValue-3]]
E3<-btcsmooth$y[ExtremeValue[numberExtremeValue-2]]
E4<-btcsmooth$y[ExtremeValue[numberExtremeValue-1]]
E5<-btcsmooth$y[ExtremeValue[numberExtremeValue]]
V1<-volume[ExtremeValue[numberExtremeValue-4]]
V2<-volume[ExtremeValue[numberExtremeValue-3]]
V3<-volume[ExtremeValue[numberExtremeValue-2]]
V4<-volume[ExtremeValue[numberExtremeValue-1]]

if (numberExtremeValue>4){
if (E1 <= E3 &
    E3 <= E5 &
    E2 <= E4 & 
    E2 > E1  &
    V1 > V3 &
    V2 < V4 )
{
  pattern_type <- "MovingUpReverse";
  min1_return <- round((price_trade[c(2)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min2_return <- round((price_trade[c(2+1)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min3_return <- round((price_trade[c(2+2)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min4_return <- round((price_trade[c(2+3)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min5_return <- round((price_trade[c(2+4)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min10_return <- round((price_trade[c(2+9)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min15_return <- round((price_trade[c(2+14)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min20_return <- round((price_trade[c(2+19)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min25_return <- round((price_trade[c(2+24)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
  min30_return <- round((price_trade[c(2+29)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
#  print(paste("|MovingUpReverse|",times,"|",E1,"|",E2,"|",E3,"|",E4,"|",E5,"|",price_trade[c(1)],"|",min1_return,"|",min2_return,"|",min3_return,"|",min4_return,"|",min5_return,"|",min10_return,"|",min15_return,"|",min20_return,"|",min25_return,"|",min30_return));
  
  returns_min1 <- c(returns_min1,min1_return)
  returns_min2 <- c(returns_min2,min2_return)
  returns_min3 <- c(returns_min3,min3_return)
  returns_min4 <- c(returns_min4,min4_return)
  returns_min5 <- c(returns_min5,min5_return)
  returns_min10 <- c(returns_min10,min10_return)
  returns_min15 <- c(returns_min15,min15_return)
  returns_min20 <- c(returns_min20,min20_return)
  returns_min25 <- c(returns_min25,min25_return)
  returns_min30 <- c(returns_min30,min30_return)
  volumes <- c(volumes, volume[35])
  
  #optional to draw or not 	
  #DRAWIt(DRAW,turning_points,pattern_type,stock_name,Date,myfit,ExtremeValue,numberExtremeValue,s,t,w,l,Returnmins,return.data,TA.price,TA.volume);
  }
}
}

# Baseline 1 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+1] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min1)
sd(returns_min1)
#plot(density(baseline_means))
pval_min1 <- sum(as.numeric(baseline_means > mean(returns_min1))) / length(baseline_means)
pval_min1

# Baseline 2 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+2] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min2)
sd(returns_min2)
#plot(density(baseline_means))
pval_min2 <- sum(as.numeric(baseline_means > mean(returns_min2))) / length(baseline_means)
pval_min2

# Baseline 3 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+3] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min3)
sd(returns_min3)
#plot(density(baseline_means))
pval_min3 <- sum(as.numeric(baseline_means > mean(returns_min3))) / length(baseline_means)
pval_min3 

# Baseline 4 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+4] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min4)
sd(returns_min4)
#plot(density(baseline_means))
pval_min4 <- sum(as.numeric(baseline_means > mean(returns_min4))) / length(baseline_means)
pval_min4

# Baseline 5 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+5] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min5)
sd(returns_min5)
#plot(density(baseline_means))
pval_min5 <- sum(as.numeric(baseline_means > mean(returns_min5))) / length(baseline_means)
pval_min5 

# Baseline 10 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+10] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min10)
sd(returns_min10)
#plot(density(baseline_means))
pval_min10 <- sum(as.numeric(baseline_means > mean(returns_min10))) / length(baseline_means)
pval_min10

# Baseline 15 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+15] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min15)
sd(returns_min15)
#plot(density(baseline_means))
pval_min15 <- sum(as.numeric(baseline_means > mean(returns_min15))) / length(baseline_means)
pval_min15

# Baseline 20 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+20] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min20)
sd(returns_min20)
#plot(density(baseline_means))
pval_min20 <- sum(as.numeric(baseline_means > mean(returns_min20))) / length(baseline_means)
pval_min20

# Baseline 25 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+25] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min25)
sd(returns_min25)
#plot(density(baseline_means))
pval_min25 <- sum(as.numeric(baseline_means > mean(returns_min25))) / length(baseline_means)
pval_min25

# Baseline 30 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+30] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min30)
sd(returns_min30)
#plot(density(baseline_means))
pval_min30 <- sum(as.numeric(baseline_means > mean(returns_min30))) / length(baseline_means)
pval_min30

# Number of trades
length(returns_min1)

# Average volume
mean(volumes)

# Proportion of simulated mean returns that are higher than the strategy mean return:
proportions <- data.frame(c(pval_min1,pval_min2,pval_min3,pval_min4,pval_min5,pval_min10,pval_min15,pval_min20,pval_min25,pval_min30))
rownames(proportions) <- c("1 min","2 min","3 min","4 min","5 min","10 min","15 min","20 min","25 min","30 min")
colnames(proportions) <- c("proportion")
proportions

##############################
# Moving-Up-Stream
# Charting type II

volumes <- c()
returns_min1 <- c()
returns_min2 <- c()
returns_min3 <- c()
returns_min4 <- c()
returns_min5 <- c()
returns_min10 <- c()
returns_min15 <- c()
returns_min20 <- c()
returns_min25 <- c()
returns_min30 <- c()


# The loop
for(i in 1:(length(data$close)-35-30)){
  timestamp <- data$timestamp[i:(34+i)]
  price <- data$close[i:(34+i)]
  volume <- data$volume[i:(34+i)]
  price_trade <- data$close[(i+34):(i+65)]
  times<- data$utc_datetime[(i+34)]
  
  btcsmooth<-smooth.spline(timestamp,price,cv=FALSE)
  
  
  #################################################
  # Mark the turning points - extreme values
  #################################################
  n<-length(btcsmooth$x)
  turning_points <- rep(0,n)
  #first and last observation are regarded as dummy points
  #And they will not become the turning points
  #first observation as dummy point
  turning_points[1] <- 1
  #last observation as dummy point
  turning_points[n] <- n
  #this loop collects all turning points from the kernel estimator
  for(j in 2:(n-1))
  {
    delta1 <- btcsmooth$y[j] - btcsmooth$y[j-1]
    delta2 <- btcsmooth$y[j+1] - btcsmooth$y[j]
    
    if ((delta1*delta2) < 0){
      turning_points[j]<-j
    } else if ((delta1*delta2) == 0)
    {
      #print(paste("Found intermediate point",i))
    } else if ((delta1*delta2) > 0)
    {
      #print(paste("Found intermediate point",i))
    }        			
  }
  
  numberExtremeValue<-length(which((turning_points != 0) & (turning_points != 1) & (turning_points != 35)))
  ExtremeValue<-which((turning_points != 0) & (turning_points != 1) & (turning_points != 35))
  E1<-btcsmooth$y[ExtremeValue[numberExtremeValue-4]]
  E2<-btcsmooth$y[ExtremeValue[numberExtremeValue-3]]
  E3<-btcsmooth$y[ExtremeValue[numberExtremeValue-2]]
  E4<-btcsmooth$y[ExtremeValue[numberExtremeValue-1]]
  E5<-btcsmooth$y[ExtremeValue[numberExtremeValue]]
  V1<-volume[ExtremeValue[numberExtremeValue-4]]
  V2<-volume[ExtremeValue[numberExtremeValue-3]]
  V3<-volume[ExtremeValue[numberExtremeValue-2]]
  V4<-volume[ExtremeValue[numberExtremeValue-1]]
  
  if (numberExtremeValue>4){
    if (E1 <= E3 &
        E3 <= E5 &
        E2 <= E4 & 
        E2 > E1  &
        V1 > 2*SMA(volume[1:35],n=35) )
    {
      pattern_type <- "MovingUpReverse";
      min1_return <- round((price_trade[c(2)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min2_return <- round((price_trade[c(2+1)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min3_return <- round((price_trade[c(2+2)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min4_return <- round((price_trade[c(2+3)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min5_return <- round((price_trade[c(2+4)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min10_return <- round((price_trade[c(2+9)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min15_return <- round((price_trade[c(2+14)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min20_return <- round((price_trade[c(2+19)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min25_return <- round((price_trade[c(2+24)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      min30_return <- round((price_trade[c(2+29)]-price_trade[c(2-1)])/price_trade[c(2-1)],4)
      #  print(paste("|MovingUpReverse|",times,"|",E1,"|",E2,"|",E3,"|",E4,"|",E5,"|",price_trade[c(1)],"|",min1_return,"|",min2_return,"|",min3_return,"|",min4_return,"|",min5_return,"|",min10_return,"|",min15_return,"|",min20_return,"|",min25_return,"|",min30_return));
      
      returns_min1 <- c(returns_min1,min1_return)
      returns_min2 <- c(returns_min2,min2_return)
      returns_min3 <- c(returns_min3,min3_return)
      returns_min4 <- c(returns_min4,min4_return)
      returns_min5 <- c(returns_min5,min5_return)
      returns_min10 <- c(returns_min10,min10_return)
      returns_min15 <- c(returns_min15,min15_return)
      returns_min20 <- c(returns_min20,min20_return)
      returns_min25 <- c(returns_min25,min25_return)
      returns_min30 <- c(returns_min30,min30_return)
      volumes <- c(volumes, volume[35])
      
      #optional to draw or not 	
      #DRAWIt(DRAW,turning_points,pattern_type,stock_name,Date,myfit,ExtremeValue,numberExtremeValue,s,t,w,l,Returnmins,return.data,TA.price,TA.volume);
    }
  }
}

# Baseline 1 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+1] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min1)
sd(returns_min1)
#plot(density(baseline_means))
pval_min1 <- sum(as.numeric(baseline_means > mean(returns_min1))) / length(baseline_means)
pval_min1

# Baseline 2 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+2] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min2)
sd(returns_min2)
#plot(density(baseline_means))
pval_min2 <- sum(as.numeric(baseline_means > mean(returns_min2))) / length(baseline_means)
pval_min2

# Baseline 3 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+3] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min3)
sd(returns_min3)
#plot(density(baseline_means))
pval_min3 <- sum(as.numeric(baseline_means > mean(returns_min3))) / length(baseline_means)
pval_min3 

# Baseline 4 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+4] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min4)
sd(returns_min4)
#plot(density(baseline_means))
pval_min4 <- sum(as.numeric(baseline_means > mean(returns_min4))) / length(baseline_means)
pval_min4

# Baseline 5 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+5] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min5)
sd(returns_min5)
#plot(density(baseline_means))
pval_min5 <- sum(as.numeric(baseline_means > mean(returns_min5))) / length(baseline_means)
pval_min5 

# Baseline 10 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+10] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min10)
sd(returns_min10)
#plot(density(baseline_means))
pval_min10 <- sum(as.numeric(baseline_means > mean(returns_min10))) / length(baseline_means)
pval_min10

# Baseline 15 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+15] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min15)
sd(returns_min15)
#plot(density(baseline_means))
pval_min15 <- sum(as.numeric(baseline_means > mean(returns_min15))) / length(baseline_means)
pval_min15

# Baseline 20 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+20] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min20)
sd(returns_min20)
#plot(density(baseline_means))
pval_min20 <- sum(as.numeric(baseline_means > mean(returns_min20))) / length(baseline_means)
pval_min20

# Baseline 25 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+25] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min25)
sd(returns_min25)
#plot(density(baseline_means))
pval_min25 <- sum(as.numeric(baseline_means > mean(returns_min25))) / length(baseline_means)
pval_min25

# Baseline 30 min
baseline_means <- c()
baseline_sds <- c()
for(i in 1:10000){
  price_index <- sample(1:43169,length(returns_min1))
  random_trades <- (data$close[price_index+30] - data$close[price_index]) / data$close[price_index]
  baseline_means <- c(baseline_means,mean(random_trades))
  baseline_sds <- c(baseline_sds,sd(random_trades))
}
mean(baseline_means)
mean(baseline_sds)
mean(returns_min30)
sd(returns_min30)
#plot(density(baseline_means))
pval_min30 <- sum(as.numeric(baseline_means > mean(returns_min30))) / length(baseline_means)
pval_min30

# Number of trades
length(returns_min1)

# Average volume
mean(volumes)

# Proportion of simulated mean returns that are higher than the strategy mean return:
proportions <- data.frame(c(pval_min1,pval_min2,pval_min3,pval_min4,pval_min5,pval_min10,pval_min15,pval_min20,pval_min25,pval_min30))
rownames(proportions) <- c("1 min","2 min","3 min","4 min","5 min","10 min","15 min","20 min","25 min","30 min")
colnames(proportions) <- c("proportion")
proportions