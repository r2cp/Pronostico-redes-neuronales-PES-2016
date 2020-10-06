# DM test
# 7 de julio de 2016

setwd("~/Documentos/tesina/datos")
rm(list=ls())
graphics.off()

# Leer los datos
library(xlsx)
df = read.xlsx("compModelos.xlsx", sheetName="res", colIndex = c(1:4), rowIndex = c(1:37))

library(forecast)
horizonte = 10
dm.stat = numeric(length=horizonte)
dm.p = numeric(length=horizonte)

for (i in c(1:horizonte)) {
  # H0 = Los dos metodos tienen la misma precision de pronostico
  dm1 = dm.test (
    e1 = df$ann18,
    e2 = df$ann19,
    # "less" implica que Ha = metodo 2 tiene menor precision que metodo 1
    alternative = "less",
    h = i,
    power = 2
  )
  dm.stat[i] = dm1$statistic
  dm.p[i] = dm1$p.value
}

data.frame(cbind(dm.stat, dm.p))