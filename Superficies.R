#La siguiente función, se puede aplicar a las tablas ValCalls y VolPuts, sin embargo, al no existir tantas opciones put en el mercado, será más informativo solo usarla para los precios de las opciones call.
#El eje x es la columna maturity de la tabla volatilities, el eje y es la columna de strikes y el eje z son las volatilities
Plot3D<-function(volatilities)
{
  x=maturity
  y=strike
  z=volatilities
  End=floor(max(z))+1
  #Tras definir los valores a graficarse, se establece el tipo de gráfica y las variables a graficarse
  fig <- plot_ly(
    type = 'surface',
    contours = list(
      x = list(show = TRUE, start = 0.01, end = 0.5, size = 5,
        color = 'white'),
      z = list(show = TRUE, start = -0.5, end = End, size = 5)),
      #size 5 para calls, 2 para puts, para mejor visualización
    x = ~maturity,
    y = ~strike,
    z = ~volatilities)
  #Se establece el diseño y aspectos de la gráfica
  fig <- fig %>% layout(
    
    scene = list(
      xaxis = list(nticks = 9),
      zaxis = list(nticks = 4),
      camera = list(eye = list(x = 1, y = -1, z = 0.5)),
      aspectratio = list(x = .9, y = .9, z = 0.3)))
  
  return(fig)
}

Plot3D(VolCalls)
Plot3D(VolPuts)