#citirea datelor
File1<-read.csv("C:/fisiere CSV/fisiereCSV/amr30854.csv")
File2<-read.csv("C:/fisiere CSV/fisiereCSV/amr30857.csv")
File3<-read.csv("C:/fisiere CSV/fisiereCSV/amr30867.csv")
File4<-read.csv("C:/fisiere CSV/fisiereCSV/amr30868.csv")
File5<-read.csv("C:/fisiere CSV/fisiereCSV/amr30870.csv")
File6<-read.csv("C:/fisiere CSV/fisiereCSV/amr30878.csv")
File7<-read.csv("C:/fisiere CSV/fisiereCSV/amr31044.csv")
File8<-read.csv("C:/fisiere CSV/fisiereCSV/amr31057.csv")
File9<-read.csv("C:/fisiere CSV/fisiereCSV/amr31058.csv")
File10<-read.csv("C:/fisiere CSV/fisiereCSV/amr31061.csv")
File11<-read.csv("C:/fisiere CSV/fisiereCSV/amr31064.csv")
File12<-read.csv("C:/fisiere CSV/fisiereCSV/amr31066.csv")
File13<-read.csv("C:/fisiere CSV/fisiereCSV/amr31067.csv")
File14<-read.csv("C:/fisiere CSV/fisiereCSV/amr31068.csv")
File15<-read.csv("C:/fisiere CSV/fisiereCSV/amr31069.csv")
File16<-read.csv("C:/fisiere CSV/fisiereCSV/amr31076.csv")
File17<-read.csv("C:/fisiere CSV/fisiereCSV/amr31158.csv")
File18<-read.csv("C:/fisiere CSV/fisiereCSV/amr31159.csv")
File19<-read.csv("C:/fisiere CSV/fisiereCSV/amr31160.csv")
File20<-read.csv("C:/fisiere CSV/fisiereCSV/amr31163.csv")
File21<-read.csv("C:/fisiere CSV/fisiereCSV/amr48846.csv")

Consumption<-cbind(File1, File2, File3, File4, File5, File6, File7, File8, File9, File10, File11, File12, File13, File14, File15, File16, File17, File18, File19, File20, File21)

View(Consumption)

#formatarea datei
rdate<-as.Date(Consumption$Year, "%d/%m/%y")
fix(rdate)
View(rdate)


plot(Consumption$Consumator89~rdate, type = "l", col = "red", axes = F)
plot (Consumator420~Consumator467, Consumption)
with(Consumption, text(Consumator420~Consumator467, labels=rdate, pos=4, cex=.5))

#Creare dateframe cu variabile numerice
Dtframe <- Consumption[,2:505]
rownames(Dtframe,Consumption$Year) #etichetarea randurilor cu data
View(Dtframe)


#standardizarea observatiilor in vederea aplicarii analizei cluster
standardize <- function(x) {(x - mean(x))/sd(x)} #standardizarea observatiilor
Dtframe_std <-apply(Dtframe,2,standardize)


#calcularea distantelor dintre obiecte
distance <- dist(as.matrix(Dtframe_std))
dist_mat <- as.matrix(distance)
write.csv(dist_mat, file="C:/fisiere CSV/fisiereCSV/Distance.csv")


#reprezentarea grafica sub forma de heatmap
heatmap(Dtframe_std) #realizeaza analiza cluster ierarhic de tipul agregarii complete
?heatmap






#aplicarea functiei hclust pt realizarea analizei cluster

?hclust
# analiza cluster metoda agregarii complete
hc.c <- hclust(distance)
plot(hc.c, labels = Consumption$Year, hang=-1)
member.c <- cutree(hc.c,k=5)

# analiza cluster metoda distantelor medii
hc.a <- hclust(distance, method="average")
plot(hc.a, labels = Consumption$Year, hang=-1)


#identificarea centroizilor
aggregate(Dtframe_std , list(member.c), mean)


# analiza cluster metoda Ward
hc.w <- hclust(distance, method="ward.D2")
plot(hc.w, labels =Consumption$Year, hang=-1)
member.w <- cutree(hc.w,k=5)
table(member.w, member.c)
?cutree

aggregate(Dtframe_std, list(member.w), mean)
#variabilele a caror medie difera cel mai mult intre clase au cea mai mare putere de separare a grupelor


install.packages("cluster")
require (cluster)

#verificarea acuratetii clasificarii
plot(silhouette(cutree(hc.c, 5), distance))
plot(silhouette(cutree(hc.w, 5), distance))


#reprezentarea grafica a variantei in interiorul grupelor
k.c <- kmeans(Dtframe_std, 4)  
k.c

?kmeans

#dimensiunea clusterlor
k.c$size

#alocarea obiectelor in clase
k.c$cluster

#centroizii
k.c$centers

#variante totala
k.c$totss
#in interiorul grupelor
k.c$withinss
k.c$tot.withinss
# intre grupe
k.c$betweenss



#Forecasting
write.csv(Consumption, file="C:/fisiere CSV/fisiereCSV/Consumption.csv")
Consumption_prediction<-read.csv("C:/fisiere CSV/fisiereCSV/Consumption_prediction.csv")
View(Consumption_prediction)
plot(rdate, Consumption_prediction$Consum_mediu, type = 'l', xlab = 'Period', ylab = 'Consumption_medium')
library(MASS)
library(tseries)
library(forecast)
library(MASS)
install.packages('MASS')

#covert to ln format
lncosumption=log(Consumption_prediction$Consum_mediu[1:90])
lncosumption

#ACF, PACF and Dickey-Fuller test
acf(lncosumption, lag.max = 20)
pacf(lncosumption, lag.max = 20) #seria este stationara, medie constanta
#diferentiem pentru a compara ADF pentru ambele situatii
difflnconsumption = diff(lncosumption, 1)
difflnconsumption
adf.test(lncosumption)
adf.test(difflnconsumption)


#analizam valorile pt p_value si decidem daca respingem ipoteza
require(tseries)

#time series and auto.arima
require(forecast)
#convertim datele
consumptionArima<-ts(lncosumption, start = c(2014, 09), frequency=30)

acf(difflnconsumption, lag.max = 30)
pacf(difflnconsumption, lag.max = 30)


fitlnconsumption<-auto.arima(consumptionArima, max.p = 5, max.q = 5, max.P = 5, max.Q = 5, max.d = 3)
fitlnconsumption
arimaModel<-arima(consumptionArima, order = c(1,0,0))
arimaModel

exp(lncosumption)

#forecasted values from arima
forecastedvalues_ln = forecast(arimaModel, h=21)
?forecast
forecastedvalues_ln
plot(forecastedvalues_ln)
?forecast

forecastedvaluesextracted = as.numeric(forecastedvalues_ln$mean)
finalforectastvalue = exp(forecastedvaluesextracted)
finalforectastvalue

#percentage error
df<-data.frame(Consumption_prediction$Consum_mediu[91:111], finalforectastvalue)
col_headings<-c('actual_consumption', 'forecasted_consumption')
names(df)<-col_headings
attach(df)
percentage_error = ((df$'actual_consumption' - df$'forecasted_consumption')/(df$'actual_consumption'))
percentage_error
print(percentage_error)
mean(percentage_error)
View(df)
print(df)


#ljung box
Box.test(arimaModel$residuals, lag = 5, type='Ljung-Box')
Box.test(arimaModel$residuals, lag = 10, type='Ljung-Box')
Box.test(arimaModel$residuals, lag = 15, type='Ljung-Box')
#null iphotesize, residuals are random si conform testului lb seria de timp este rezonabila pentru arima
