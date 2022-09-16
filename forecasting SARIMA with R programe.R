setwd("D:/SuhuUdaraJatim.csv")
getwd()
ls()
rm(list=ls(all=TRUE))
ls(all=TRUE)

#1. Memasukkan data ke R
SuhuUdara = read.csv2("~/ADW/Tugas/SuhuUdaraJatim.csv")
SuhuUdara[,-1]
t(SuhuUdara[,-1])
SuhuUdaraA = as.numeric(t(SuhuUdara[,-1]))
SuhuUdara_TS = ts(SuhuUdaraA, start = c(2006,1), end = c(2017,12), frequency = 12)
is.ts(SuhuUdara_TS) #jika output TRUE maka dara sudah time series

#3. Memplot data
plot(SuhuUdara_TS)
# Observasi: ada tren dan fluktuasi musiman
# Pada tahap ini kita menyimpulkan data tidak stasioner karena
# ada tren.

#4. Melihat ACF data (melihat seberapa kuat korelasi dalam data)
par(mfrow=c(2,1))
acf(as.vector(SuhuUdara_TS),lag=50)
acf(as.vector(SuhuUdara_TS),type="partial",lag=50)
# Kita bisa menggunakan fungsi acf2 pada library(astsa)
library(astsa)
acf2(as.vector(SuhuUdara_TS))
## 

#5. Uji formal kestasioneran menggunakan alfa = 0,05 (default)
library(tseries)
adf.test(SuhuUdara_TS) # hipotesis nol data tidak stasioner: diterima
# kpss.test(SuhuUdara_TS,null="Trend")
# pp.test(SuhuUdara_TS) # hipotesi nol: data tidak stasioner

# Kesimpulan: berdasarkan uji formal kestasioneran menggunakan
# uji ADF, data tidak stasioner

#6. Melakukan differencing terhadap tren
diff.tren.SuhuUdara_TS = diff(SuhuUdara_TS,lag=1) # differencing tren, lag = 1
# bisa juga tidak ditulis karena default lag = 1 pada R
plot(diff.tren.SuhuUdara_TS)
acf2(as.vector(diff.tren.SuhuUdara_TS))
# ACF memperlihatkan pola musiman yang kuat (berosilasi) seperti
# gelombang sinus-kosinus

#7. Melakukan differencing terhadap musim
diff.musim.SuhuUdara_TS = diff(diff.tren.SuhuUdara_TS,lag=12)
plot(diff.musim.SuhuUdara_TS)
# Untuk melihat perbedaan antara data yang belum didifferencing
# dan yang sudah didifferencing
# plot(SuhuUdara_TS)
# plot(diff.tren.SuhuUdara_TS)
# plot(diff.musim.cSuhuUdara_TS)
acf2(as.vector(diff.musim.SuhuUdara_TS))

#8. Spesifikasi model SARIMA(p,d,q)x(P,D,Q)_{12}
suhu.baru = SuhuUdara_TS
sarima.suhu.baru.011.011 = arima(suhu.baru,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
sarima.suhu.baru.011.111 = arima(suhu.baru,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
sarima.suhu.baru.110.011 = arima(suhu.baru,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12))
sarima.suhu.baru.111.011 = arima(suhu.baru,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
sarima.suhu.baru.210.011 = arima(suhu.baru,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))

sarima.suhu.baru.011.011$aic
sarima.suhu.baru.011.111$aic
sarima.suhu.baru.110.011$aic
sarima.suhu.baru.111.011$aic # model ini memiliki AIC terkecil di antara kelima kandidat
sarima.suhu.baru.210.011$aic

#9. Memilih model yang memiliki AIC paling kecil di antara kandidat
#model yang kita usulkan

sarima.aic = c(sarima.suhu.baru.011.011$aic,sarima.suhu.baru.011.111$aic,
sarima.suhu.baru.110.011$aic,sarima.suhu.baru.111.011$aic,sarima.suhu.baru.210.011$aic)
sarima.model = c("SARIMA(0,1,1)x(0,1,1)","SARIMA(0,1,1)x(1,1,1)","SARIMA(1,1,0)x(0,1,1)",
"SARIMA(1,1,1)x(0,1,1)","SARIMA(2,1,0)x(0,1,1)")
sarima.seleksi = cbind(sarima.model,sarima.aic)
sarima.seleksi = data.frame(sarima.seleksi)
sarima.seleksi

#10. Melakukan diagnostik model
## uji kenormalan

# plot residual model (tidak membentuk pola yang berisi tren)
res.sarima.suhu.baru.111.011 = sarima.suhu.baru.111.011$residuals
plot.ts(res.sarima.suhu.baru.111.011)
abline(h=0,col="red")

# plot kuantil-kuantil
qqnorm(res.sarima.suhu.baru.111.011)
qqline(res.sarima.suhu.baru.111.011)

# uji kenormalan
shapiro.test(res.sarima.suhu.baru.111.011) # Ho diterima (residual menyebar normal)

# diagnostik juga bisa dilakukan menggunakan fungsi sarima pada 
# library(astsa)
sarima(suhu.baru,1,1,1,0,1,1,12)

# uji autokorelasi Ljung-Box memperlihatkan p-value 
# melewati garis putus-putus, artinya tidak ada korelasi
# antarresidual

#11. Peramalan data suhu.baru
## Prediksi
forecast.suhu.baru = predict(sarima.suhu.baru.111.011,12) # peramalan untuk 12 periode
# batas atas
# forecast.suhu.baru$pred berisi nilai prediksi (ramalan)
# forecast.suhu.baru$se berisi galat standar (standard error)
U <- forecast.suhu.baru$pred + 2*forecast.suhu.baru$se # batas atas
L <- forecast.suhu.baru$pred - 2*forecast.suhu.baru$se # batas bawah
min.y <- min(suhu.baru,L)# batas minimum untuk sumbu y
max.y <- max(suhu.baru,U)# batas maksimum untuk sumbu y
ts.plot(suhu.baru,forecast.suhu.baru$pred,col=1:2,ylim=c(min.y,max.y))
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")


## Untuk melihat hasil ramalan 
## kita akan potong data suhu.baru
cut.suhu.baru <- suhu.baru[745:757]
cut.suhu.baru <- ts(cut.suhu.baru,start=c(2018,3),frequency=12)
ts.plot(cut.co2.baru,forecast.suhu.baru$pred,col=1:2,ylim=c(min.y,max.y))
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")
abline(v=end(cut.co2.baru),lty="dashed")

