# importo la data #
data <- rio::import('serie_residuales_fechas.csv')

# funciones de olea #
source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

# hagamos un gráfico para ver la serie #
plot(residuales ~ date, data = data, type = 'l')

# guardamos los residuales #
res <- data$residuales

# creo una secuencia con la ventana de dias #
seq <- seq(as.Date("2011-01-01"), as.Date("2012-12-31"), by = "day")

# creo la serie de tiempo
X <- ts(res, start = c(2011, as.numeric(format(seq[1], "%j"))), frequency = 365)
plot(X)

# la media la veo estacionaria, pero no asi la varianza #
# la ajustare mediante boxcox #

lambda <- forecast::BoxCox.lambda(X, method = "guerrero", lower = 0, upper = 1)

# ARMA (p,q) #
acf(X, lag.max = 10)
pacf(X, lag.max = 10)

# de los graficos podemos sacar lo siguiente #
p <- 5
q <- 10

# hacemos el modelo ARMA #
model0 <- forecast::Arima(X, order = c(5,0,10))

# todas las raices dentro del circulo, algunas tocando el borde de este #
plot(model0)

# veamos los residuales de este ARMA #
LSTS::ts.diag(model0$res)
# pasa la blancura #

salida.arima(X,model0)

# lo graficamos #
prediccion0 <- forecast::forecast(model0, h = 10, level = 0.95, fan = T)
plot(prediccion0)

# hagamos un autoarima #
model1 <- forecast::auto.arima(X, lambda = lambda)
plot(model1)

# veamos los residuales #
LSTS::ts.diag(model1$res)

# resultados #
salida.arima(X,model1)

# lo graficamos #
prediccion1 <- forecast::forecast(model1, h = 10, level = 0.95, fan = T)
plot(prediccion1)
