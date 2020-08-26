#d1 sigue la notación usual de los modelos financieros, S es el precio del subyacente, K el strike, r la tasa de interés libre de riesgo y t el tiempo de madurez.
#x es la volatilidad, normalmente representada por sigma y será determinada más adelante
d1<-function(S,K, r, x, t)
{
  d<-log(S/K)+(r+x*x/2)*t
  d<-d/(x*sqrt(t))
  return(d)
}

#Definición de la fórmula de la opción Call europea
#Las variables son las mismas que en la función d1
Call<-function(S,K, r, x, t)
{
  d1=d1(S, K, r, x, t)
  #d2 se define en función de d1, la volatilidad y el tiempo de madurez
  d2=d1-x*sqrt(t)
  C=S*pnorm(d1,0,1)-K*exp(r*t)*pnorm(d2,0,1)
  return(C)
}
#La función Put se define de manera análoga a la Call
Put<-function(S,K, r, x, t)
{
  d1=d1(S, K, r, x, t)
  d2=d1-x*sqrt(t)
  P=K*exp(r*t)*(1-pnorm(d2,0,1))-S*(1-pnorm(d1,0,1))
  return(P)
}

#A continuación se modifican las funciones Call y Put para posteriormente obtener el valor de la volatilidad x con uniroot
CallFromVol<-function(S,K,r,x,t,C)
  return(Call(S,K,r,x,t)-C)

PutFromVol<-function(S,K,r,x,t,P)
  return(Put(S,K,r,x,t)-P)

#Se define la matriz de volatilidades para cada strike y madurez que se tienen en la tabla de precios
VolCalls=matrix(rep(0, m*n), nrow=m)
#Se llenan las volatilidades
for (i in 1:m)
  for (j in 1:n)
  {
    #Pointer es la posición de la tabla VIXCalls en la que el precio de strike es strike[i] y posee un tiempo de madurez maturity[j]
    pointer=intersect(which(VIXCalls$strike==strike[i]),
      which(VIXCalls$maturity==maturity[j]))
    #Por definición, pointer es un arreglo, así que debemos verificar si no está vacío.
    if(length(pointer)==1)
    {
      #Valuation es el precio del Call que se encuentra en la tabla VIXCalls
      Valuation=VIXCalls$optionvalue[pointer]
      #En la tabla de volatilidades, se ingresa el valor (en porcentaje) devuelto por uniroot aplicado a la función CallFromVol
      VolCalls[i,j]=uniroot(CallFromVol, c(0,5), S=S, K=strike[i],
        r=r, t=maturity[j], Valuation)$root
    }
  }

#VolPuts es la matriz de volatilidades correspondiente a las opciones put, se difine de la misma manera que VolCalls
VolPuts=matrix(rep(0, m*n), nrow=m)
for (i in 1:m)
  for (j in 1:n)
  {
    pointer=intersect(which(VIXPuts$strike==strike[i]),
      which(VIXPuts$maturity==maturity[j]))
    if(length(pointer)==1)
    {
      Valuation=VIXPuts$optionvalue[pointer]
      #Aunque se define de manera análoga, las soluciones se buscan en otro intervalo, ya que las opciones Put son menos comunes en el mercado
      VolPuts[i,j]=uniroot(PutFromVol, c(-2,2), S=S, K=strike[i], r=r,
        t=maturity[j], Valuation)$root
    }
  }
