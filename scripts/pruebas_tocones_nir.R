library(readxl)
muestreo_aleatorio <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_nir_tocones_sll.xlsx", 
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
#p = 0.49, los datos se distribuyen de forma normal.

#BANDA 2
banda2 <- data$"Banda 2"
print(banda2)
plot(density(banda2))

#Prueba de Shapiro-Wilk
shapiro.test(banda2)
#p = 0.28, los datos se distribuyen de forma normal.

#BANDA 3
banda3 <- data$"Banda 3"
print(banda3)
plot(density(banda3))

#Prueba de Shapiro-Wilk
shapiro.test(banda3)
#p = 0.173, los datos se distribuyen de forma normal.

#RESULTADO: TODAS LAS BANDAS SE DISTRIBUYEN DE FORMA NORMAL PARA EL MUESTREO ALEATORIO.

#PARA LA MUESTRA DIRIGIDA
muestreo_dirigido <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_nir_tocones_sll.xlsx", 
                                sheet = "dirigido")
data1 <- muestreo_dirigido

#BANDA 1
banda1_d <- data1$"Banda 1"
print(banda1_d)
plot(density(banda1_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda1_d)
#p-value = 0.75, los datos se distribuyen de forma normal.

#BANDA 2
banda2_d <- data1$"Banda 2"
print(banda2_d)
plot(density(banda2_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda2_d)
#p-value = 0.429, los datos se distribuyen de forma normal.

#BANDA 3
banda3_d <- data1$"Banda 3"
print(banda3_d)
plot(density(banda3_d))

#Prueba de Shapiro-Wilk
shapiro.test(banda3_d)
#p-value = 0.674, los datos se distribuyen de forma normal.

#RESULTADO: TODAS LAS BANDAS SE DISTRIBUYEN DE FORMA NORMAL PARA EL MUESTREO DIRIGIDO.

#T-TEST

#BANDA 1
datos_b1 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_nir_tocones_sll.xlsx", 
                       sheet = "banda1")
t.test(Valor ~ Tipo_muestreo, data = datos_b1)
#p-value = 0.08188
#No se rechaza la hipótesis nula.

#BANDA 2
datos_b2 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_nir_tocones_sll.xlsx", 
                       sheet = "banda2")
t.test(Valor ~ Tipo_muestreo, data = datos_b2)
#p-value = 0.01141
#Se rechaza la hipótesis nula, hay diferencias significativas.

#BANDA 3
datos_b3 <- read_excel("/Users/elenabalmaceda/Desktop/TESIS/Prueba_LLAMARA_PINTADOS/LLAMARA/LLAMARA_NIR/datos_nir_sll/datos_nir_tocones_sll.xlsx", 
                       sheet = "banda3")
t.test(Valor ~ Tipo_muestreo, data = datos_b3)
#p-value = 0.0004622
#Se rechaza la hipótesis nula, hay diferencias significativas.

#RESULTADO: HAY DIFERENCIAS SIGNIFICATIVAS ENTRE LAS MUESTRAS PARA LAS BANDAS 2 Y 3 EN LLAMARA NORTE.