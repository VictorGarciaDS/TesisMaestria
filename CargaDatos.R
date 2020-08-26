#LIBRERÍAS
library(plotly)
#CONSTANTES
#Obtenida en https://ycharts.com/indicators/10_year_treasury_rate
TenYearTreasure=0.0184
#Obtenida en https://ycharts.com/indicators/us_12month_trimmed_mean_pce_inflation_rate
Inflacion=(0.0179+0.0172)/2
#Usando https://www.wallstreetmojo.com/risk-free-rate-formula/
#r es la tasa de interés libre de riesgo
r=(1+TenYearTreasure)/(1+Inflacion)-1

#IMPORTA DATOS
setwd("/home/victor/Documentos/Carrera/Maestría/UNAM/Tesis/")
data=read.csv("UnderlyingOptionsEODQuotes_2016-06-01.csv")
#S, Precio del subyacente, constante en todos los registros
S=data$underlying_bid_1545[1]
#Se eliminan los datos que no se usan
VIX=data[,c(2,4,5)]
#Se estima el valor de la opción con el valor medio entre bid y ask del día
VIX$optionvalue=(data$bid_eod+data$ask_eod)/2

#Se divide entre la cantidad de segundos al año
VIX$maturity=as.double(difftime(strptime(as.character(VIX$expiration),
    format = "%Y-%m-%d"), strptime(as.character(VIX$quote_date),
    format = "%Y-%m-%d")))/(60*24*24*365)
#Se conservan solo los tiempos de madurez
VIX=VIX[,-c(1,2)]
#La madurez 0 indetermina d1, así que se elimina
VIX=VIX[-which(VIX$maturity==0),]

#Separando los precios de las opciones call y put
naux=nrow(VIX)/2
VIXCalls=VIX[-2*(1:naux),]
VIXPuts=VIX[-(2*(1:naux)-1),]

#Obteniendo el tamaño de la retícula de datos
#Se eliminan los valores repetidos de los strikes
strike=unique(VIX$strike)
m=length(strike)
maturity=unique(VIX$maturity)
n=length(maturity)