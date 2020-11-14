#install xlsx if necessary
library(readxl)
Weather <- read_excel("Weather.xlsx")
Cleaned_Data <- read_excel("Cleaned Data.xlsx")

names(Cleaned_Data)

Weather$Time2=paste(Weather$Date,substr(Weather$Time,12,16))

Weather$DAY=as.numeric(substr(Weather$Date,9,10))
Weather$TIME=as.numeric(substr(Weather$Time,12,13))
Weather$TIME2=as.numeric(substr(Weather$Time,15,16))

Cleaned_Data$DAY=as.numeric(substr(Cleaned_Data$time_stamp,3,4))
Cleaned_Data$TIME=as.numeric(substr(Cleaned_Data$time_stamp,11,12))
Cleaned_Data$TIME2=as.numeric(substr(Cleaned_Data$time_stamp,14,15))

Wdata=Weather[c("DAY","TIME","dgree","Rains")]
Cdata=Cleaned_Data[c("DAY","TIME","speed")]
Cdata$ID=paste(Cdata$DAY,Cdata$TIME)
Wdata$ID=paste(Wdata$DAY,Wdata$TIME)

library(tidyverse);library(tidyverse);library(rio);library(magrittr)


Cdata %>% group_by(ID) %>%
  summarise(DAY=mean(DAY),
            TIME=mean(TIME),
            speed=mean(speed)
  ) %T>% str->Cdata2
Cdata2=unique(Cdata2)

finaldata=unique(merge(Wdata,Cdata2,by="ID",all=FALSE))[c("ID","dgree","speed","Rains")]
finaldata=finaldata[!finaldata$ID=="18 15"&!finaldata$ID=="26 21",]

fit1=lm(speed~dgree+Rains,finaldata)
summary(fit1)

fit2=lm(speed~dgree+I(dgree^2)+Rains,finaldata)
summary(fit2)

shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

shrinkage(fit1)
shrinkage(fit2)
myfun <- function(x) {coef(fit2)[1]+coef(fit2)[2]*x+coef(fit2)[3]*x^2}
curve(myfun(x), from=min(finaldata$dgree), to=max(finaldata$dgree),ylab="speed",xlab="dgree")

Cdata2=Cleaned_Data[c("DAY","TIME","TIME2","speed","longitude","latitude")]


Cdata2$start1=c(rep(Cdata2[Cdata2$DAY==18,]$TIME[1],length(Cdata2[Cdata2$DAY==18,]$speed)),
               rep(Cdata2[Cdata2$DAY==19,]$TIME[1],length(Cdata2[Cdata2$DAY==19,]$speed)),
               rep(Cdata2[Cdata2$DAY==20,]$TIME[1],length(Cdata2[Cdata2$DAY==20,]$speed)),
               rep(Cdata2[Cdata2$DAY==21,]$TIME[1],length(Cdata2[Cdata2$DAY==21,]$speed)),
               rep(Cdata2[Cdata2$DAY==24,]$TIME[1],length(Cdata2[Cdata2$DAY==24,]$speed)),
               rep(Cdata2[Cdata2$DAY==25,]$TIME[1],length(Cdata2[Cdata2$DAY==25,]$speed)),
               rep(Cdata2[Cdata2$DAY==26,]$TIME[1],length(Cdata2[Cdata2$DAY==26,]$speed)),
               rep(Cdata2[Cdata2$DAY==27,]$TIME[1],length(Cdata2[Cdata2$DAY==27,]$speed)),
               rep(Cdata2[Cdata2$DAY==28,]$TIME[1],length(Cdata2[Cdata2$DAY==28,]$speed)),
               rep(Cdata2[Cdata2$DAY==31,]$TIME[1],length(Cdata2[Cdata2$DAY==31,]$speed)))

Cdata2$start2=c(rep(Cdata2[Cdata2$DAY==18,]$TIME2[1],length(Cdata2[Cdata2$DAY==18,]$speed)),
               rep(Cdata2[Cdata2$DAY==19,]$TIME2[1],length(Cdata2[Cdata2$DAY==19,]$speed)),
               rep(Cdata2[Cdata2$DAY==20,]$TIME2[1],length(Cdata2[Cdata2$DAY==20,]$speed)),
               rep(Cdata2[Cdata2$DAY==21,]$TIME2[1],length(Cdata2[Cdata2$DAY==21,]$speed)),
               rep(Cdata2[Cdata2$DAY==24,]$TIME2[1],length(Cdata2[Cdata2$DAY==24,]$speed)),
               rep(Cdata2[Cdata2$DAY==25,]$TIME2[1],length(Cdata2[Cdata2$DAY==25,]$speed)),
               rep(Cdata2[Cdata2$DAY==26,]$TIME2[1],length(Cdata2[Cdata2$DAY==26,]$speed)),
               rep(Cdata2[Cdata2$DAY==27,]$TIME2[1],length(Cdata2[Cdata2$DAY==27,]$speed)),
               rep(Cdata2[Cdata2$DAY==28,]$TIME2[1],length(Cdata2[Cdata2$DAY==28,]$speed)),
               rep(Cdata2[Cdata2$DAY==31,]$TIME2[1],length(Cdata2[Cdata2$DAY==31,]$speed)))
unique(Cdata2$start2)
Cdata2$hour=ifelse(Cdata2$TIME-Cdata2$start1==0,1,0)
Cdata2$fivemin=ifelse(Cdata2$TIME2-Cdata2$start2==5,1,0)

Cdata3=subset(Cdata2,Cdata2$fivemin==1&Cdata2$hour==1)

names(Cdata3)
Cdata3 %>% group_by(DAY) %>%
  summarise(TIME=mean(TIME),
            TIME2=mean(TIME2),
            speed=mean(speed),
            longitude=mean(longitude),
            latitude=mean(latitude)
  ) %T>% str->Cdata4
Cdata4=unique(Cdata4)

#include temperature
Cdata4$tem=c(Wdata[Wdata$ID=="18 10",]$dgree,
             Wdata[Wdata$ID=="19 12",]$dgree,
             mean(Wdata[Wdata$ID=="20 14",]$dgree),
             Wdata[Wdata$ID=="21 10",]$dgree,
             Wdata[Wdata$ID=="24 12",]$dgree,
             Wdata[Wdata$ID=="25 11",]$dgree,
             Wdata[Wdata$ID=="26 12",]$dgree,
             Wdata[Wdata$ID=="27 10",]$dgree,
             Wdata[Wdata$ID=="28 11",]$dgree,
             mean(Wdata[Wdata$ID=="31 10",]$dgree))
Cdata4$rain=c(Wdata[Wdata$ID=="18 10",]$Rains,
             Wdata[Wdata$ID=="19 12",]$Rains,
             mean(Wdata[Wdata$ID=="20 14",]$Rains),
             Wdata[Wdata$ID=="21 10",]$Rains,
             Wdata[Wdata$ID=="24 12",]$Rains,
             Wdata[Wdata$ID=="25 11",]$Rains,
             Wdata[Wdata$ID=="26 12",]$Rains,
             Wdata[Wdata$ID=="27 10",]$Rains,
             Wdata[Wdata$ID=="28 11",]$Rains,
             mean(Wdata[Wdata$ID=="31 10",]$Rains))
names(Cdata4)

finaldata2=Cdata4[!Cdata4$DAY==26,]
model1=lm(longitude~TIME+TIME2+tem+rain,finaldata2);summary(model1)
model2=lm(latitude~TIME+TIME2+tem+rain,finaldata2);summary(model2)

#cross validation
finaldata2$index=1:9
predict=matrix(NA,9,2)

for (i in 1:9) {
 predict[i,1]=predict(lm(longitude~TIME+TIME2+tem+rain,finaldata2,subset=!index==i),finaldata2[finaldata2$index==i,])
 predict[i,2]=predict(lm(latitude~TIME+TIME2+tem+rain,finaldata2,subset=!index==i),finaldata2[finaldata2$index==i,])
 }
finaldata2$long_predict=predict[,1]
finaldata2$lati_predict=predict[,2]

#predict the location of bomb by input the time, temperature and precipitation

