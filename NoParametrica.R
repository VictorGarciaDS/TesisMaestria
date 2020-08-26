#Se toma una muestra de los datos
VolCallsAtStrike70=VolCalls[37,]

#Se determinan los coeficientes obtenidos al realizar una regresión exponencial
Coef=as.numeric(lm(log(VolCallsAtStrike70)~
  log(maturity))$coefficients)
#Como se graficará en la forma At^(-b), se toma la exponencial del intercepto
Coef[1]=exp(Coef[1])

#Se ingresan a un data frame los valores de madurez junto a la muestra de datos tomada
NoParam<-data.frame(maturity, VolCallsAtStrike70)

#Se grafican los datos de NoParam junto con la curva de regresión
ggplot(data=NoParam, aes(x=maturity, y=VolCallsAtStrike70))+
  geom_point()+stat_function(fun=function(x)
  Coef[1]*x^Coef[2], col="red")+
  xlab("Tiempo de madurez (t)")+ylab(expression(psi(t)))
