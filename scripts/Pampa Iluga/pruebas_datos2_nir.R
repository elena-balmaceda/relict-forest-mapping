library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/NIR/muestreo_2/datos_ir_pi_muestreo2.xlsx", 
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
#p = 0.02, los datos no se distribuyen de forma normal.

#BANDA 2
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.01283, los datos no se distribuyen de forma normal.

#BANDA 3
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.005653, los datos no se distribuyen de forma normal.

#RESULTADO: LOS DATOS NO SE DISTRIBUYEN DE FORMA NORMAL PARA NINGUNA BANDA DEL MUESTREO ALEATORIO.
#NO ES NECESARIO ESTIMAR NORMALIDAD PARA EL DIRIGIDO PORQUE DE TODAS FORMAS HABRÁ QUE REALIZAR PRUEBAS NO PARAMÉTRICAS. 

muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/NIR/muestreo_2/datos_ir_pi_muestreo2.xlsx", 
                                sheet = "dirigido")
data1 <- muestreo_dirigido
banda1_d <- data1$"Banda 1"
banda2_d <- data1$"Banda 2"
banda3_d <- data1$"Banda 3"

#MANN WHITNEY

#BANDA 1
print(banda1)
print(banda1_d)
wilcox.test(banda1, banda1_d, exact = FALSE)

#p-value = 2.964e-05, hay diferencias significativas.

#BANDA 2
print(banda2)
print(banda2_d)
wilcox.test(banda2, banda2_d, exact = FALSE)

#p-value = 3.845e-05, hay diferencias significativas. 

#BANDA 3
print(banda3)
print(banda3_d)
wilcox.test(banda3, banda3_d, exact = FALSE)

#p-value = 6.255e-05, hay diferencias significativas.

#PERMUTACIÓN
library(exactRankTests)

perm.test(banda1, banda1_d)
#p-value = 4.338e-05
perm.test(banda2, banda2_d)
#p-value = 6.88e-05
perm.test(banda3, banda3_d)
#p-value = 0.0001123