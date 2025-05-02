library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_monticulos_ir.xlsx", 
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
#p = 0.02284, los datos no se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.01251, los datos no se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.003479, los datos nose distribuyen de forma normal.

#RESULTADO: NINGUNA BANDA SE DISTRIBUYE DE FORMA NORMAL PARA EL MUESTREO ALEATORIO, HAY QUE HACERLE MANN WHITNEY A TODO. 

muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_monticulos_ir.xlsx", 
                                sheet = "dirigido")
data1 <- muestreo_dirigido
print(data1)
banda1_d <- data1$`Banda 1`
banda2_d <- data1$`Banda 2`
banda3_d <- data1$`Banda 3`

#MANN WHITNEY Y PERMUTACIÓN
library(exactRankTests)

#BANDA 1
wilcox.test(banda1, banda1_d, exact=FALSE)
#p-value = 0.0003079
perm.test(banda1, banda1_d)
#p-value = 0.0001758

#BANDA 2
wilcox.test(banda2, banda2_d, exact = FALSE)
#p-value = 0.0003868
perm.test(banda2, banda2_d)
#p-value = 0.000186

#BANDA 3
wilcox.test(banda3, banda3_d, exact = FALSE)
#p-value = 0.0001402
perm.test(banda3, banda3_d)
#p-value = 0.0001068