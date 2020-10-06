# Modelo monetario de tipo de cambio con RNAs
# 04 de julio de 2016

rm(list=ls())
graphics.off()

# Leer los datos
library(xlsx)
df = read.xlsx("datosModelo.xlsx", sheetName="datosANN", 
	colIndex = c(1:6), rowIndex = c(1:181))

# Dividir la muestra
datafit = df[1:144, ]
dataforecast = df[145:180, ]

# Cargar los pesos sinapticos iniciales
load("pesos/sw_1_9.rda")

# Capa oculta
oculta = c(9)

# Estimar la red neuronal
library(neuralnet)
nn <- neuralnet(
	s ~ y + inf + i + m, 
	data = datafit, 
	startweights = w0,
	hidden = oculta, 
	err.fct = "sse",
	threshold = 0.1,
	rep = 1,
	act.fct = "tanh",
	linear.output=TRUE,
	algorithm = "rprop+",
	lifesign = "minimal"
)

# Computar fuera de la muestra
nnOut = compute(nn, dataforecast[ , 3:6])

# Graficar los resultados dentro y fuera de la muestra
par(mfrow=c(2,1))
plot(datafit$fecha, datafit$s, type="n", xlab="", ylab="s", ylim=c(-2,2.5))
title(main="Dentro de la muestra")
lines(datafit$fecha, datafit$s, type="l", col="blue")
lines(datafit$fecha, nn$net.result[[1]], col="red")

plot(dataforecast$fecha, dataforecast$s, type="n", xlab="", ylab="s", ylim=c(-2,2.5))
title(main="Fuera de la muestra")
lines(dataforecast$fecha, dataforecast$s, type="l", col="blue")
lines(dataforecast$fecha, nnOut$net.result, col="red")

# Guardar los resultados en archivo Excel
inSample = cbind(nn$response, nn$net.result[[1]])
outOfSample = cbind(dataforecast$s, nnOut$net.result)
write.xlsx(inSample, "netResult.xlsx", sheetName = "inSample")
write.xlsx(outOfSample, "netResult.xlsx", sheetName = "outOfSample", append=TRUE)

# Para interpretar el resultado
library(NeuralNetTools)
graf.garson = garson(mod_in = nn, y_lab = "Importancia relativa")
