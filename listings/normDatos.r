# Normalización de datos para modelo monetario de tipo de cambio
# 04 de julio de 2016

setwd("~/Documentos/tesina/datos")
rm(list=ls())
graphics.off()

# Leer los datos
library(xlsx)
df = read.xlsx("datosModelo.xlsx", sheetName="datosREG", colIndex = c(1:6), rowIndex = c(1:181))

media = numeric(length = 5)
max = numeric(length = 5)
min = numeric(length = 5)
sd = numeric(length = 5)

# Para cada columna obtener la media, y poner los datos en el rango (-1,1)
for (i in c(1:5)) {
  media[i] = mean(df[,i+1])
  max[i] = max(df[,i+1])
  min[i] = min(df[,i+1])
  sd[i] = sd(df[,i+1])
  
  # Convertir los datos
  df[,i+1] = (df[,i+1] - media[i]) / sd[i]
}

write.xlsx(df, file="datosNorm.xlsx", sheetName = "datosNORM")
write.xlsx(media, file="datosNorm.xlsx", sheetName = "norm", append = TRUE)
write.xlsx(max, file="datosNorm.xlsx", sheetName = "max", append = TRUE)
write.xlsx(min, file="datosNorm.xlsx", sheetName = "min", append = TRUE)