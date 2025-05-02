#PARA TODO EL ANÁLISIS SE utilizaron las distintas hojas del archivo muestreo_pst_limpio_NIR
#PARA LA MUESTRA ALEATORIA
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/NIR/muestreo/muestreo_pst_limpio_nir.xlsx", 
                                 sheet = "Aleatorio")
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
#p=0.04311, se rechaza la h0 de que los datos se distribuyen de forma normal.

#BANDA 2
banda2 <- data$`Banda 2`
print(banda2)
plot(density(banda2))
hist(banda2)
# Prueba de Shapiro-Wilk
resultado2 <- shapiro.test(banda2)
print(resultado2)
#p=0.03703, se rechaza la h0 de que los datos se distribuyen de forma normal.

#BANDA 3
banda3 <- data$`Banda 3`
print(banda3)
plot(density(banda3))
hist(banda3)
# Prueba de Shapiro-Wilk
resultado3 <- shapiro.test(banda3)
print(resultado3)
#p=0.08743, no se rechaza la h0 de que los datos se distribuyen de forma normal.

####

#PARA LA MUESTRA DIRIGIDA
muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_PAMPA_ILUGA/NIR/muestreo/muestreo_pst_limpio_nir.xlsx", 
                                 sheet = "Dirigido")
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
#p=0.03589, se rechaza la h0 de que los datos se distribuyen de forma normal.

#BANDA 2
# Extracción de una columna específica como vector numérico
banda2_d <- data1$`Banda 2`
print(banda2_d)
plot(density(banda2_d))
# Prueba de Shapiro-Wilk
resultado2_d <- shapiro.test(banda2_d)
print(resultado2_d)
#p=0.01912, se rechaza la h0 de que los daos se distribuyen de forma normal.

#BANDA 3
# Extracción de una columna específica como vector numérico. 
banda3_d <- data1$`Banda 3`
print(banda3_d)
plot(density(banda3_d))
# Prueba de Shapiro-Wilk
resultado3_d <- shapiro.test(banda3_d)
print(resultado3_d)
#p=0.05081, no se rechaza la h0 de que los datos se distribuyen de forma normal.

#LA BANDA 3 PARA AMBAS MUESTRAS SE DISTRIBUYE DE FORMA NORMAL. 

#U TEST DE MANN WHITNEY
  #BANDA 1
b1_aleatorio <- b1_aleatorio
b1_dirigido <- b1_dirigido
muestra_1a <- b1_aleatorio$Valor
muestra_1d <- b1_dirigido$Valor
print(muestra_1d)
print(muestra_1a)
muestra1a <- sort(muestra_1a)
print(muestra1a)
sort(muestra_1d)
resultado <- wilcox.test(muestra_1a, muestra_1d)
print(resultado)
#p= 9.146e-06, existen diferencias significativas entre las muestras. 

  #BANDA 2
b2_aleatorio <- B2_aleatorio
b2_dirigido <- B2_dirigido
muestra_2a <- b2_aleatorio$Valor
muestra_2d <- b2_dirigido$Valor
print(muestra_2d)
print(muestra_2a)
resultado2 <- wilcox.test(muestra_2a, muestra_2d)
print(resultado2)
#p= 5.405e-06, existen diferencias significativas entre las muestras.

  #BANDA 3
datos <- muestreo_banda3
t.test(Valor ~ Tipo_de_muestreo, data = datos)
#p= 3.822e-07, existen diferencias significativas entre las muestras. 

#permutación
perm.test(banda1, banda1_d)
perm.test(banda2, banda2_d)
perm.test(banda3, banda3_d)

#para empates de datos
library("exactRankTests")
  #banda 1
resultado <- wilcox.exact(muestra_1a, muestra_1d)
valor_p <- resultado$p.value
print(valor_p)
#p=2.956408e-06

  #banda 2
resultado2 <- wilcox.exact(muestra_2a, muestra_2d)
valor_p2 <- resultado2$p.value
print(valor_p2)
#p=1.510366e-06

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
#p=3.822e-07






