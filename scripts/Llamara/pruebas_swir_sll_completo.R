library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_sll_completo.xlsx", 
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
#p = 9.107e-05, los datos no se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.0005707, los datos no se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.0006154, los datos no se distribuyen de forma normal.

#RESULTADO: NINGUNO DE LOS DATOS SE DISTRIBUYE DE FORMA NORMAL. 

muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_SWIR/datos_swir_sll/datos_swir_sll_completo.xlsx", 
                                 sheet = "dirigido")

data_d <- muestreo_dirigido
print(data_d)

banda1_d <- data_d$"Banda 1"
banda2_d <- data_d$"Banda 2"
banda3_d <- data_d$"Banda 3"

#MANN WHITNEY Y PERMUTACIÓN
library(exactRankTests)

#BANDA 1
wilcox.test(banda1, banda1_d, exact = FALSE)
#p-value = 0.8115, no hay diferencias significativas entre los datos.

perm.test(banda1, banda1_d)
#p-value = 0.9675

#BANDA 2
wilcox.test(banda2, banda2_d, exact = FALSE)
#p-value = 0.4773

perm.test(banda2, banda2_d)
#p-value = 0.246

#BANDA 3
wilcox.test(banda3, banda3_d, exact = FALSE)
#p-value = 0.1742

perm.test(banda3, banda3_d)
#p-value = 0.0242, hay diferencias significativas.


