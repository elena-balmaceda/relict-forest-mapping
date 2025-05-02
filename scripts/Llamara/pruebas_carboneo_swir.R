library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                                 sheet = "aleatorio")

data <- muestreo_aleatorio
print(data)

#ESTIMACIÓN DE NORMALIDAD
#BANDA 1
# Extracción de una columna específica como vector numérico
banda1 <- data$"Banda 1"
print(banda1)
plot(density(banda1))

#Prueba de Shapiro-Wilk
shapiro.test(banda1)
#p = 0.0012, los datos no se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.337, los datos se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.1811, los datos se distribuyen de forma normal.

#RESULTADO: LOS DATOS SE DISTRIBUYEN NORMALMENTE PARA LAS BANDAS 2 Y 3, PERO NO PARA LA 1.

#MUESTRA DIRIGIDA
muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                                sheet = "dirigido")
data1 <- muestreo_dirigido

#BANDA 1
banda1_d <- data1$"Banda 1"
print(banda1_d)
plot(density(banda1_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda1_d)
#p-value = 2.34e-05, los datos no se distribuyen de forma normal.

#BANDA 2
banda2_d <- data1$"Banda 2"
print(banda2_d)
plot(density(banda2_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda2_d)
#p-value = 0.3366, los datos se distribuyen de forma normal.

#BANDA 3
banda3_d <- data1$`Banda 3`
print(banda3_d)
plot(density(banda3_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda3_d)
#p-value = 0.1903, los datos se distribuyen de forma normal.

#RESULTADO: LOS DATOS SE DISTRIBUYEN DE FORMA NORMAL PARA LAS BANDAS 2 Y 3, PERO NO PARA LA 1. 


#U-TEST DE MANN WHITNEY (banda 1)
#BANDA 1
b1_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                           sheet = "1a")
b1_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                          sheet = "1d")
muestra_1a <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1a)
print(muestra_1d)

wilcox.test(muestra_1a, muestra_1d, exact = FALSE)
#p-value = 0.3446, no hay diferencias significativas entre las muestras. 

library(exactRankTests)
perm.test(muestra_1a,muestra_1d)
#p-value = 0.4492

#T-TEST (bandas 2 y 3)
  #BANDA 2
datos_b2 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                       sheet = "banda2")
t.test(Valor ~ Tipo_muestreo, data = datos_b2)
#p-value = 0.6295
#No se rechaza la hipótesis nula.

  #BANDA 3
datos_b3 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                       sheet = "banda3")
t.test(Valor ~ Tipo_muestreo, data = datos_b3)
#p-value = 0.4716
#No se rechaza la hipótesis nula.

#PRUEBAS COMPARANDO CON EL POLÍGONO COMPLETO DEL SLL
library(readxl)
muestreo_aleatorio2 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                                 sheet = "aleatorio2")
data2 <- muestreo_aleatorio2
print(data2)

#ESTIMACIÓN DE NORMALIDAD DEL NUEVO MUESTREO ALEATORIO (N=40, EN TODO EL POLÍGONO)
#BANDA 1
# Extracción de una columna específica como vector numérico
banda1 <- data2$"Banda 1"
print(banda1)
plot(density(banda1))

#Prueba de Shapiro-Wilk
shapiro.test(banda1)
#p = 0.001188, los datos no se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2 <- data2$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.0151, los datos no se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico
banda3 <- data2$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.07567, los datos se distribuyen de forma normal.

#RESULTADO: LOS DATOS SE DISTRIBUYEN DE FORMA NORMAL SÓLO PARA LA BANDA 3. EN EL MUESTRO DIRIGIDO TAMBIÉN, POR LO QUE SE HARÁ MANN WHITNEY PARA LAS BANDAS 1 Y 2 Y T-TEST PARA LA BANDA 3

#MANN WHITNEY
#BANDA 1
b1_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                           sheet = "1a2")
b1_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                          sheet = "1d")
muestra_1a2 <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1a2)
print(muestra_1d)

wilcox.test(muestra_1a2, muestra_1d, exact = FALSE)
#p-value = 0.06453, no hay diferencias significativas entre los datos. 

#BANDA 2
b2_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                           sheet = "2a2")
b2_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                          sheet = "2d")
muestra_2a2 <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2a2)
print(muestra_2d)

wilcox.test(muestra_2a2, muestra_2d, exact = FALSE)
#p-value = 0.0001409, existen diferencias significativas entre los datos. 

#T-TEST
#BANDA 3
datos_b3_ma2 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_carboneo_sll.xlsx", 
                       sheet = "banda3_ma2")
t.test(Valor ~ Tipo_muestreo, data = datos_b3_ma2)
#p-value = 2.635e-06
#Se rechaza la hipótesis nula, existen diferencias significativas entre los datos.

#PRUEBAS DE PERMUTACIÓN
library(exactRankTests)
perm.test(muestra_1a2, muestra_1d)
#p-value = 0.5878

perm.test(muestra_2a2, muestra_2d)
#p-value = 4.63e-05

#RESULTADO: EXISTEN DIFERENCIAS SIGNIFICATIVAS PARA LAS BANDAS 2 Y 3 CUANDO SE COMPARA EL MUESTRO DIRIGIDO DEL CARBONEO Y EL MUESTREO DEL POLÍGONO COMPLETO. 