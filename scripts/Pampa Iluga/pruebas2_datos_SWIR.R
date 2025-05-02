#PARA TODO EL ANÁLISIS SE utilizaron las distintas hojas del archivo muestreo2_psti_limpio_SWIR

#PARA LA MUESTRA ALEATORIA
data <- muestreo_aleatorio

#ESTIMACIÓN DE NORMALIDAD
  #BANDA 1
# Extracción de una columna específica como vector numérico
banda1 <- data$`Banda 1`
print(banda1)
plot(density(banda1))
hist(banda1)
# Prueba de Shapiro-Wilk
resultado <- shapiro.test(banda1)
print(resultado)
#el resultado es p=0,01216, se rechaza la h0 de que los datos provienen de una distribución normal.

#BANDA 2
banda2 <- data$`Banda 2`
print(banda2)
plot(density(banda2))
hist(banda2)
# Prueba de Shapiro-Wilk
resultado2 <- shapiro.test(banda2)
print(resultado2)
#el resultado es p=0094, se rechaza la H0 de que los datos provienen de una distribución normal. 

#BANDA 3
banda3 <- data$`Banda 3`
print(banda3)
plot(density(banda3))
hist(banda3)
# Prueba de Shapiro-Wilk
resultado3 <- shapiro.test(banda3)
print(resultado3)
#el resultado es p=01487, se rechaza la H0 de que los datos provienen de una distribución normal. 

####

#PARA LA MUESTRA DIRIGIDA
data1 <- muestreo_dirigido

#ESTIMACIÓN DE NORMALIDAD
#BANDA 1
# Extracción de una columna específica como vector numérico
banda1_d <- data1$`Banda 1`
print(banda1_d)
plot(density(banda1_d))
# Prueba de Shapiro-Wilk
resultado1_d <- shapiro.test(banda1_d)
print(resultado1_d)
#el resultado es p=0,0003269, se rechaza la H0 de que los datos provienen de una distribución normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2_d <- data1$`Banda 2`
print(banda2_d)
plot(density(banda2_d))
# Prueba de Shapiro-Wilk
resultado2_d <- shapiro.test(banda2_d)
print(resultado2_d)
#el resultado es p=4.98e-05, se rechaza la H0 de que los datos provienen de una distribución normal.

#BANDA 3
# Extracción de una columna específica como vector numérico. 
banda3_d <- data1$`Banda 3`
print(banda3_d)
plot(density(banda3_d))
# Prueba de Shapiro-Wilk
resultado3_d <- shapiro.test(banda3_d)
print(resultado3_d)
#el resultado es p=4.424e-06, no se rechaza la H0 de que los datos provienen de una distribución normal.

#RESULTADO: NINGUNA BANDA SE DISTRIBUYE DE FORMA NORMAL. 

#U TEST DE MANN WHITNEY
  #BANDA 1
library(readxl)
b1_aleatorio <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                           sheet = "1A")
b1_dirigido <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                           sheet = "1D")
muestra_1a <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1d)
print(muestra_1a)
resultado <- wilcox.test(muestra_1a, muestra_1d)
print(resultado)
#p=9.108e-05

  #BANDA 2
b2_aleatorio <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                           sheet = "2A")
b2_dirigido <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                                      sheet = "2D")
muestra_2a <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2d)
print(muestra_2a)
resultado2 <- wilcox.test(muestra_2a, muestra_2d)
print(resultado2)
#P=8.841e-05

  #BANDA 3
b3_aleatorio <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                           sheet = "3A")
b3_dirigido <- read_excel("~/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_2/muestreo2_pst_limpio_SWIR.xlsx", 
                           sheet = "3D")
muestra_3a <- b3_aleatorio$Valor
muestra_3d <- b3_dirigido$Valor
print(muestra_3d)
print(muestra_3a)
resultado3 <- wilcox.test(muestra_3a, muestra_3d)
print(resultado3)
#p=1.072e-05

#Para los empates de datos
library(exactRankTests)
#Suponiendo que tienes dos vectores de datos: muestra_1a y muestra_1d

#Banda 1
resultado <- wilcox.test(muestra_1a, muestra_1d)
valor_p <- resultado$p.value
print(valor_p)
#p=9.108114e-05

#Banda 2
resultado2 <- wilcox.test(muestra_2a, muestra_2d)
valor_p2 <- resultado2$p.value
print(valor_p2)
#p=4.733725e-05

#Banda 3
resultado3 <- wilcox.test(muestra_3a, muestra_3d)
valor_p3 <- resultado$p.value
print(valor_p3)
#p=9.108114e-05

#PRUEBA COMPLEMENTARIA : PRUEBA DE PERMUTACIÓN
  #BANDA 1
resultado_permutacion1 <- perm.test(muestra_1a, muestra_1d)
print(resultado_permutacion1)
#p=6.135e-05

#BANDA 2
resultado_permutacion2 <- perm.test(muestra_2a, muestra_2d)
print(resultado_permutacion2)
#p=4.031e-05

#BANDA 3
resultado_permutacion3 <- perm.test(muestra_3a, muestra_3d)
print(resultado_permutacion3)
#p=2.416e-05

#PRUEBA COMPLEMENTARIA : PRUEBA DE Kolmogorov-Smirnov
  #BANDA 1
resultado_ks1 <- ks.test(muestra_1a, muestra_1d, simulate.p.value = TRUE)
print(resultado_ks1)
#p-value = 0.001106
print(muestra_1a)
print(muestra_1d)

  #BANDA 2
resultado_ks2 <- ks.test(muestra_2a, muestra_2d, simulate.p.value = TRUE)
print(resultado_ks2)
#p-value = 0.001106
print(muestra_2a)
print(muestra_2d)

  #BANDA 3
resultado_ks3 <- ks.test(muestra_3a, muestra_3d, simulate.p.value = TRUE)
print(resultado_ks3)
#p-value = 0.001106
