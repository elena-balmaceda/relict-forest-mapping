library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                                 sheet = "aleatorio")

data <- muestreo_aleatorio

#ESTIMACIÓN DE NORMALIDAD
#BANDA 1
# Extracción de una columna específica como vector numérico
banda1 <- data$"Banda 1"
print(banda1)
plot(density(banda1))

# Prueba de Shapiro-Wilk
shapiro.test(banda1)
#p = 0.007, los datos no se distribuyen de forma normal.

#BANDA 2
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.00059, los datos no se distribuyen de forma normal.

#BANDA 3
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.00092, los datos no se distribuyen de forma normal.

#RESULTADO: LOS DATOS NO SE DISTRIBUYEN DE FORMA NORMAL PARA NINGUNA BANDA DEL MUESTREO ALEATORIO. 

#MUESTREO DIRIGIDO
muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                                sheet = "dirigido")
data1 <- muestreo_dirigido

#BANDA 1
banda1_d <- data1$"Banda 1"
print(banda1_d)
plot(density(banda1_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda1_d)
#p-value = 0.006, los datos no se distribuyen de forma normal.

#BANDA 2
banda2_d <- data1$"Banda 2"
print(banda2_d)
plot(density(banda2_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda2_d)
#p-value = 0.0829, los datos se distribuyen de forma normal.

#BANDA 3
banda3_d <- data1$"Banda 3"
print(banda3_d)
plot(density(banda3_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda3_d)
#p-value = 0.59, los datos se distribuyen de forma normal.

#RESULTADO: EN EL MUESTREO DIRIGIDO LOS DATOS SE DISTRIBUYEN DE FORMA NORMAL PARA LAS BANDAS 2 Y 3, PERO NO PARA LA 1. 

###

#MANN WHITNEY 
#BANDA 1
b1_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                           sheet = "1a")
b1_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                          sheet = "1d")
muestra_1a <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1a)
print(muestra_1d)

wilcox.test(muestra_1a, muestra_1d, exact = FALSE)
#p-value = 7.647e-07, existen diferencias significativas entre las muestras.

#BANDA 2
b2_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                           sheet = "2a")
b2_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                          sheet = "2d")
muestra_2a <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2a)
print(muestra_2d)

wilcox.test(muestra_2a, muestra_2d, exact = FALSE)
#p-value = 7.6e-06, existen diferencias significativas entre las muestras.

#BANDA 3
b3_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                           sheet = "3a")
b3_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_SWIR/datos_sv_Swir/datos_swir_sv.xlsx", 
                          sheet = "3d")
muestra_3a <- b3_aleatorio$Valor
muestra_3d <- b3_dirigido$Valor
print(muestra_3a)
print(muestra_3d)

wilcox.test(muestra_3a, muestra_3d, exact = FALSE)
#p-value = 0.0001606, existen diferencias significativas entre las muestras.

#RESULTADO: EXISTEN DIFERENCIAS SIGNIFICATIVAS ENTRE TODAS LAS BANDAS. 

library(exactRankTests)
perm.test(muestra_1a, muestra_1d)
perm.test(muestra_2a, muestra_2d)
perm.test(muestra_3a, muestra_3d)
