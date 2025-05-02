#PARA TODO EL ANÁLISIS SE utilizaron las distintas hojas del archivo muestreo_pst_limpio_SWIR.
#PARA LA MUESTRA ALEATORIA
library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_1/muestreo_pst _limpio_SWIR.xlsx", 
                                 sheet = "aleatorio")
data <- muestreo_aleatorio

#ESTIMACIÓN DE NORMALIDAD
  #BANDA 1
# Extracción de una columna específica como vector numérico
banda1 <- data$`Banda_1`
print(banda1)
plot(density(banda1))
hist(banda1)
# Prueba de Shapiro-Wilk
resultado <- shapiro.test(banda1)
print(resultado)
    #el resultado es p=0,0032, se rechaza la H0 de que los datos provienen de una distribución normal. 

  #BANDA 2
banda2 <- data$`Banda_2`
print(banda2)
plot(density(banda2))
hist(banda2)
# Prueba de Shapiro-Wilk
resultado2 <- shapiro.test(banda2)
print(resultado2)
    #el resultado es p=0,0086, se rechaza la H0 de que los datos provienen de una distribución normal. 

  #BANDA 3
banda3 <- data$`Banda_3`
print(banda3)
plot(density(banda3))
hist(banda3)
# Prueba de Shapiro-Wilk
resultado3 <- shapiro.test(banda3)
print(resultado3)
    #el resultado es p=0,056, no se rechaza la H0 de que los datos provienen de una distribución normal.

####

#PARA LA MUESTRA DIRIGIDA
muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_1/muestreo_pst _limpio_SWIR.xlsx", 
                                 sheet = "dirigido")
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
    #el resultado es p=0,0024, se rechaza la H0 de que los datos provienen de una distribución normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2_d <- data1$`Banda 2`
print(banda2_d)
plot(density(banda2_d))
# Prueba de Shapiro-Wilk
resultado2_d <- shapiro.test(banda2_d)
print(resultado2_d)
#el resultado es p=0,0076, se rechaza la H0 de que los datos provienen de una distribución normal.

#BANDA 3
# Extracción de una columna específica como vector numérico. 
banda3_d <- data1$`Banda 3`
print(banda3_d)
plot(density(banda3_d))
# Prueba de Shapiro-Wilk
resultado3_d <- shapiro.test(banda3_d)
print(resultado3_d)
#el resultado es p=0,039, no se rechaza la H0 de que los datos provienen de una distribución normal.

#RESULTADO: la Banda 3 para ambas muestras se distribuye de forma normal. 

library(readxl)
#permutacion
library(exactRankTests)
perm.test(banda1, banda1_d)
perm.test(banda2, banda2_d)

#U TEST DE MANN WHITNEY
  #BANDA 1
b1_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_1/muestreo_pst _limpio_SWIR.xlsx", 
                           sheet = "1a")
b1_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/SWIR/muestreo_1/muestreo_pst _limpio_SWIR.xlsx", 
                           sheet = "1d")

b1_aleatorio <- B1_aleatorio
b1_dirigido <- B1_dirigido

muestra_1a <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1d)

print(muestra_1a)

muestra_1a_sorted <- sort(muestra_1a)
muestra_1d_sorted <- sort(muestra_1d)
print(muestra_1a_sorted)
print(muestra_1d_sorted)

resultado <- wilcox.test(muestra_1a_sorted, muestra_1d_sorted)
print(resultado)
#el resultado es p=2,267e-05, sí existen diferencias significativas entre las muestras.
#Ojo: hay empates de datos. 

  #BANDA 2
b2_aleatorio <- B2_aleatorio
b2_dirigido <- B2_dirigido
muestra_2a <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2d)
print(muestra_2a)
resultado2 <- wilcox.test(muestra_2a, muestra_2d)
print(resultado2)
#el resultado es p=0,00011, sí existen diferencias significativas entre las muestras. 
#Ojo: hay empates de datos.

  #BANDA 3
datos <- muestreo_banda3
t.test(Valor ~ Tipo_de_muestreo, data = datos)
#p-valor = 1.358e-06. Sí existen diferencias significativas entre ambas muestras.

#Para el tema de los empates de datos
install.packages("exactRankTests")
library(exactRankTests)
#Suponiendo que tienes dos vectores de datos: muestra_1a y muestra_1d
resultado <- wilcox.exact(muestra_1a, muestra_1d)
valor_p <- resultado$p.value
print(valor_p)
#p=9,177718e-06

resultado2 <- wilcox.exact(muestra_2a, muestra_2d)
valor_p2 <- resultado2$p.value
print(valor_p2)
#p=6.120623e-05

  #BANDA 3. PRUEBA 2: T-TEST DE WELCH
print(datos)
print(banda3_d)
print(banda3)
banda3_a <- banda3
print(banda3_a)
# Realizar el t-test de Welch
resultado_t_welch <- t.test(banda3_a, banda3_d, var.equal = FALSE)

# Imprimir los resultados
print(resultado_t_welch)
#p=1.358e-06
