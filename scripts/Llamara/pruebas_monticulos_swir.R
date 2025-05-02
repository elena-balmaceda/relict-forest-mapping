library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
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
#p = 0.05391, los datos se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.02045, los datos no se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.005362, los datos no se distribuyen de forma normal.

#RESULTADO: LOS DATOS SE DISTRIBUYEN DE FORMA NORMAL SOLO PARA LA BANDA 1 DEL MUESTREO ALEATORIO. 

#PARA LA MUESTRA DIRIGIDA...

data1 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                                sheet = "dirigido")

#BANDA 1
banda1_d <- data1$"Banda 1"
print(banda1_d)
plot(density(banda1_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda1_d)
#p-value = 0.1572, los datos se distribuyen de forma normal.

#BANDA 2
banda2_d <- data1$"Banda 2"
print(banda2_d)
plot(density(banda2_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda2_d)
#p-value = 0.08737, los datos se distribuyen de forma normal.

#BANDA 3
banda3_d <- data1$`Banda 3`
print(banda3_d)
plot(density(banda3_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda3_d)
#p-value = 0.09652, los datos se distribuyen de forma normal.

#RESULTADO: LOS DATOS DE TODAS LAS BANDAS SE DISTRIBUYEN DE FORMA NORMAL PARA LA MUESTRA DIRIGIDA.

#BANDA 1
#T-TEST
datos_b1 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                       sheet = "banda1")
print(datos_b1)
t.test(Valor ~ Tipo_muestreo, data = datos_b1)
#p-value = 0.0002219
#Se rechaza la hipótesis nula, existen diferencias significativas.

#BANDA 2
#MANN WHITNEY
b2_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                           sheet = "2a")
b2_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                          sheet = "2d")
muestra_2a <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2a)
print(muestra_2d)

wilcox.test(muestra_2a, muestra_2d, exact = FALSE)
#p-value = 0.0001362, sí existen diferencias significativas entre las muestras. 

#BANDA 3
#MANN WHITNEY
b3_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                           sheet = "3a")
b3_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_monticulos_sll.xlsx", 
                          sheet = "3d")
muestra_3a <- b3_aleatorio$Valor
muestra_3d <- b3_dirigido$Valor
print(muestra_3a)
print(muestra_3d)

wilcox.test(muestra_3a, muestra_3d, exact = FALSE)
#p-value = 0.0002315, sí existen diferencias significativas entre las muestras. 

#PERMUTACIÓN
library(exactRankTests)
perm.test(muestra_2a, muestra_2d)
perm.test(muestra_3a, muestra_3d)
