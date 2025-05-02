library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_NIR/datos_sv_nir/datos_sv_nir.xlsx", 
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
#p = 0.002363, los datos no se distribuyen de forma normal.

#BANDA 2
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.002334, los datos no se distribuyen de forma normal.

#BANDA 3
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.00109, los datos no se distribuyen de forma normal.

#RESULTADO: LOS DATOS NO SE DISTRIBUYEN DE FORMA NORMAL PARA NINGUNA BANDA DEL MUESTREO ALEATORIO.
#NO ES NECESARIO ESTIMAR NORMALIDAD PARA EL DIRIGIDO PORQUE DE TODAS FORMAS HABRÁ QUE REALIZAR PRUEBAS NO PARAMÉTRICAS. 

data1 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/SUR VIEJO/SV_NIR/datos_sv_nir/datos_sv_nir.xlsx", 
                    sheet = "dirigido")
banda1_d <- data1$"Banda 1"
banda2_d <- data1$"Banda 2"
banda3_d <- data1$"Banda 3"

#MANN WHITNEY

  #BANDA 1
print(banda1)
print(banda1_d)
wilcox.test(banda1, banda1_d, exact = FALSE)

#p-value = 6.599e-05, hay diferencias significativas.

  #BANDA 2
print(banda2)
print(banda2_d)
wilcox.test(banda2, banda2_d, exact = FALSE)

#p-value = 0.0004557, hay diferencias significativas. 

  #BANDA 3
print(banda3)
print(banda3_d)
wilcox.test(banda3, banda3_d, exact = FALSE)

#p-value = 0.002084, hay diferencias significativas.

#PERMUTACIÓN
library(exactRankTests)

perm.test(banda1, banda1_d)
#p-value = 2.897e-05
perm.test(banda2, banda2_d)
#p-value = 0.0001452
perm.test(banda3, banda3_d)
#p-value = 0.0004083

#PRUEBA COMPLEMENTARIA : PRUEBA DE Kolmogorov-Smirnov
#BANDA 1
ks.test(banda1, banda1_d, simulate.p.value = TRUE)
#p-value = 0.0002407

#BANDA 2
ks.test(banda2, banda2_d, simulate.p.value = TRUE)
#p-value = 0.003323

#BANDA 3
ks.test(banda3, banda3_d, simulate.p.value = TRUE)
#p-value = 0.007213