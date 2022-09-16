setwd("D:\\Semester 6\\ADW\\Finansial")
objects()
rm(list=ls(all=TRUE))
ls()

library(tseries) 
library(TSA)     
library(FinTS)   
library(fBasics) 
library(fGarch)  

#Memastikan data dengan urutan yang benar
mcd <- read.csv("MCD.csv",header=T) 
head(mcd) # memastikan data sudah terurut kronologis
tail(mcd) # memastikan data sudah terurut kronologis
mcd.close <- mcd$Close
plot.ts(mcd.close)
##par(mfrow=c(2,1))


# return FREN
rtrn.mcd.close <- diff(log(mcd.lose),lag=1) #membuat return


# log return
rtrn.mcd.close<- diff(log(mcd.close),lag=1) #membuat return
plot.ts(rtrn.mcd.close) # memplot return
abline(h = 0,col="green")#membuat garis horizontal


# ACF dan PACF logreturn
par(mfrow=c(2,1))
acf(rtrn.mcd.close)
pacf(rtrn.mcd.close)

# ACF dan PACF logreturn dikuadratkan
acf(rtrn.mcd.close^2)
pacf(rtrn.mcd.close^2)

# ACF dan PACF logreturn dimutlakkan
par(mfrow=c(2,1))
acf(abs(rtrn.mcd.close))
pacf(abs(rtrn.mcd.close))

# Uji Efek ARCH
ArchTest(rtrn.mcd.close)

# statistik dasar return
basicStats(rtrn.mcd.close)


#Melihat histogram data
par(mfrow=c(2,1))
hist(rtrn.mcd.close) # ini histogram default, kurang cocok
hist(rtrn.mcd.close,breaks="Scott",probability=TRUE)
# bandingkan dengan histogram bilangan acak N(0,1) 
# sebanyak 11415
x = rnorm(11415)
hist(x,breaks="Scott",probability=TRUE)


plot.ts(rtrn.mcd.close) # memplot return
abline(h = 0,col="red")#membuat garis horizontal 

# acf return mcd
par(mfrow=c(2,1))
acf(rtrn.mcd.close)
pacf(rtrn.FREN.Close)

# alternatif lain
library(astsa)
acf2(rtrn.mcd.close)

# acf return mutlak (absolute return)
par(mfrow=c(2,1))
acf(abs(rtrn.mcd.close))
pacf(abs(rtrn.mcd.close))

# acf return kuadrat (squared return)
acf(rtrn.mcd.close^2)
pacf(rtrn.mcd.close^2)

## Uji kenormalan return mcd
# library(tseries)
jarque.bera.test(rtrn.mcd.close)

## Uji autokorelasi
library(FinTS)
AutocorTest(rtrn.mcd.close) # return
AutocorTest(abs(rtrn.mcd.close)) # mutlak return
AutocorTest(rtrn.mcd.close^2)# kuadrat return

## Uji efek ARCH
ArchTest(rtrn.mcd.close)

## Identifikasi model ARCH
# par(mfrow=c(2,1))
acf(rtrn.mcd.close^2)
pacf(rtrn.mcd.close^2)

## astsa
library(astsa)
acf2(rtrn.mcd.close^2)

## EACF return mcd kuadrat
#library(TSA)
eacf(rtrn.mcd.close^2)

## ARCH(10) sampai dengan ARCH(1)
arch10.mcd <- garchFit(formula=~garch(10,0),data=rtrn.mcd.close)
summary(arch10.mcd)

arch9.mcd <- garchFit(formula=~garch(9,0),data=rtrn.mcd.close)
summary(arch9.mcd)

arch8.mcd <- garchFit(formula=~garch(8,0),data=rtrn.mcd.close)
summary(arch8.mcd)

arch7.mcd <- garchFit(formula=~garch(7,0),data=rtrn.mcd.close)
summary(arch7.mcd)


arch6.mcd <- garchFit(formula=~garch(6,0),data=rtrn.mcd.close)
summary(arch6.mcd)

arch5.mcd <- garchFit(formula=~garch(5,0),data=rtrn.mcd.close)
summary(arch5.mcd)

arch4.mcd <- garchFit(formula=~garch(4,0),data=rtrn.mcd.close)
summary(arch4.mcd)

arch3.mcd <- garchFit(formula=~garch(3,0),data=rtrn.mcd.close)
summary(arch3.mcd)

arch2.mcd <- garchFit(formula=~garch(2,0),data=rtrn.mcd.close)
summary(arch2.mcd)

arch1.mcd <- garchFit(formula=~garch(1,0),data=rtrn.mcd.close)
summary(arch1.mcd)

garch11.mcd <- garchFit(formula=~garch(1,1),data=rtrn.mcd.close)
summary(garch11.mcd)

garch12.mcd <- garchFit(formula=~garch(1,2),data=rtrn.mcd.close)
summary(garch12.mcd)

garch21.mcd <- garchFit(formula=~garch(2,1),data=rtrn.mcd.close)
summary(garch21.mcd)

garch22.mcd <- garchFit(formula=~garch(2,2),data=rtrn.mcd.close)
summary(garch21.mcd)

predict(garch12.mcd,6)

#membuat Plot pilihan (Disini qnorm selection:13)
plot(garch12.mcd)

# prediksi untk GARCH(1,1)
predict(garch11.mcd,12) 

#Uji kenormalan sisaan
library(tseries)
jarque.bera.test(rtrn.mcd.close)

#uji autokorelasi
library(FinTS)
AutocorTest(rtrn.mcd.close) # return
AutocorTest(abs(rtrn.mcd.close)) # mutlak return
AutocorTest(rtrn.mcd.close^2)# kuadrat return

#Luaran:
AutocorTest(rtrn.mcd.close) # return
  














