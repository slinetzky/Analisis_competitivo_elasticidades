rm(list = ls())
getwd()
install.packages("psych")
install.packages("writexl")
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
library(dplyr)
library(readxl)
data_hw1<-read_excel("data_HW1_2023.xlsx")
str(data_hw1)
data_hw1$store <- as.factor(data_hw1$store)
data_hw1$promo_tide128 <- as.factor(data_hw1$promo_tide128)
data_hw1$promo_tide64 <- as.factor(data_hw1$promo_tide64)
data_hw1$promo_wisk64 <- as.factor(data_hw1$promo_wisk64)

head(data_hw1)
summary(data_hw1)


#Delete weeks outside the 1-187 relevant range
#Eliminar semanas fuera del rango 1-187 relevante
data_hw1 <- data_hw1[!(data_hw1$week>187),]

#PREGUNTA 1.1:
#Three graphs, one for each product:
#Hacemos tres gráficos, uno para cada producto:
library(ggplot2)

ventas_tide128_por_semana <- ggplot(data_hw1, aes(week, q_tide128)) +
  geom_point( alpha=0.5, shape=".") +
  geom_smooth(method="lm", se=FALSE, color="royalblue2") +
  labs(x="semana", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide128 por Semana") +
  theme(plot.title = element_text(hjust = 0.5))

ventas_tide64_por_semana <- ggplot(data_hw1, aes(week, q_tide64)) +
  geom_point(alpha=0.5, shape=".")+
  geom_smooth(method="lm", color= "red2") +
  ylim(0,2000) +
  labs(x="semana", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide64 por Semana") +
  theme(plot.title = element_text(hjust = 0.5))

ventas_wisk64_por_semana <- ggplot(data_hw1, aes(week, q_wisk64)) +
  geom_point(alpha=0.5, shape=".")+
  geom_smooth(method="lm", color= "springgreen2") +
  ylim(0,1000) +
  labs(x="semana", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Wisk64 por Semana") +
  theme(plot.title = element_text(hjust = 0.5))
#Abrupt rise in sales in certain weeks
#Wisk64 has less sales than tide64. More sales in Tide128 overall
####Podemos ver que hay  saltos abruptos en las ventas en semanas determinadas 
####A primera vista, pareciera que wisk64 consigue menores ventas que tide64, y el de mayor ventas es Tide128


#Grouping products by color:
#Generamos gráfico agrupando productos por color:
ventas_semanales_total <- ggplot(data_hw1) +
  geom_col(aes(week, q_tide128, fill="royalblue2"), color=NA, alpha=0.6) +
  geom_col(aes(week, q_tide64, fill="red2"), color=NA, alpha=0.6) +
  geom_col(aes(week, q_wisk64, fill="springgreen2"), color=NA, alpha=0.6) +
  labs(x="semana", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas por Semana") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_identity(name="Producto:",breaks=c("royalblue2", "red2", "springgreen2"),
                       labels=c("Tide 128", "Tide 64", "Wisk 64"),guide="legend")
#Sudden rises in sales do not match between products
#Se observa que los saltos abruptos en ventas no tienden a coincidir entre productos



#PREGUNTA 1.2:
#Looking for heterogeneity between products
#Buscamos heterogeneidad entre productos
ventas_promedio_tide128_por_local <- aggregate(data_hw1$q_tide128, list(data_hw1$store), mean)
colnames(ventas_promedio_tide128_por_local) <- c("store", "ventas_promedio")
grafico_ventas_promedio_tide128_por_local <- ggplot(ventas_promedio_tide128_por_local, 
                                                    aes(store, ventas_promedio)) +
  geom_col(fill="royalblue2", color=NA) +
  labs(x="local", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide128 por Local") +
  theme(plot.title = element_text(hjust = 0.5))

ventas_promedio_tide64_por_local <- aggregate(data_hw1$q_tide64, list(data_hw1$store), mean)
colnames(ventas_promedio_tide64_por_local) <- c("store", "ventas_promedio")
grafico_ventas_promedio_tide64_por_local <- ggplot(ventas_promedio_tide64_por_local, 
                                                    aes(store, ventas_promedio)) +
  geom_col(fill="red2",color=NA) +
  labs(x="local", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide64 por Local") +
  theme(plot.title = element_text(hjust = 0.5))

ventas_promedio_wisk64_por_local <- aggregate(data_hw1$q_wisk64, list(data_hw1$store), mean)
colnames(ventas_promedio_wisk64_por_local) <- c("store", "ventas_promedio")
grafico_ventas_promedio_wisk64_por_local <- ggplot(ventas_promedio_wisk64_por_local, 
                                                    aes(store, ventas_promedio)) +
  geom_col(fill="springgreen2", color=NA) +
  labs(x="local", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Wisk64 por Local") +
  theme(plot.title = element_text(hjust = 0.5))
#There is heterogeneity
#Sí, se observa heterogeneidad entre locales para los tres productos

#PREGUNTA 1.3:
#Price and quantity relation:
#Relacion precio cantidad
grafico_ventas_semanales_vs_precio_tide128 <- ggplot(data_hw1, aes(p_tide128, q_tide128)) +
  geom_smooth(color="royalblue2", method="lm") +
  geom_point(alpha=0.5, shape=".") +
  ylim(-250,1500) +
  labs(x="precio", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide128 v/s precio") +
  theme(plot.title = element_text(hjust = 0.5))
  

grafico_ventas_semanales_vs_precio_tide64 <- ggplot(data_hw1, aes(p_tide64, q_tide64)) +
  geom_smooth(color="red2", method="lm") +
  geom_point(alpha=0.5, shape=".") +
  ylim(-100,1500) +
  labs(x="precio", y="n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Tide64 v/s precio") +
  theme(plot.title = element_text(hjust = 0.5))

grafico_ventas_semanales_vs_precio_wisk64 <- ggplot(data_hw1, aes(p_wisk64, q_wisk64)) +
  geom_smooth(color="springgreen2", method="lm") +
  geom_point(alpha=0.5, shape=".") +
  ylim(-250,1000) +
  labs(x="precio", y= "n° de ventas") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas de Wisk64 v/s precio") +
  theme(plot.title = element_text(hjust = 0.5))


#Pregunta 1.4
#Plot log(sales) vs log(price)
#graficar log(ventas) vs log(precios)
data_hw1$ventas_totales <- data_hw1$q_tide128 + data_hw1$q_tide64 + data_hw1$q_wisk64
data_hw1$log_q_tide128 <- log(data_hw1$q_tide128)
data_hw1$log_q_tide64 <- log(data_hw1$q_tide64)
data_hw1$log_q_wisk64 <- log(data_hw1$q_wisk64)
data_hw1$log_p_tide128 <- log(data_hw1$p_tide128)
data_hw1$log_p_tide64 <- log(data_hw1$p_tide64)
data_hw1$log_p_wisk64 <- log(data_hw1$p_wisk64)

grafico_log_ventas_precios <- ggplot(data_hw1) + 
  geom_smooth(aes(log_p_tide128, log_q_tide128, color="royalblue2") , method="lm") +
  geom_point(aes(log_p_tide128, log_q_tide128), alpha=0.5, shape=".", color="royalblue2") +
  geom_smooth(aes(log_p_tide64, log_q_tide64, color="red2"), method="lm") +
  geom_point(aes(log_p_tide64, log_q_tide64), alpha=0.5, shape=".", color="red2") +
  geom_smooth(aes(log_p_wisk64, log_q_wisk64,color="springgreen2"), method="lm") +
  geom_point(aes(log_p_wisk64, log_q_wisk64), alpha=0.5, shape=".", color="springgreen2") +
  labs(x="log(precios)", y= "log(ventas)") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Ventas vs Precio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_identity(name="Producto:",breaks=c("royalblue2", "red2", "springgreen2"),
                      labels=c("Tide 128", "Tide 64", "Wisk 64"),guide="legend") 
#Similar slopes between products
#Log transofrmation let us look at changes in percentage terms
#This is better because not all products come in the same size; differences in scales make gross changes not comparable

#Se observan pendientes similares entre productos
#La transofrmación log nos permite analizar los cambios en precios y ventas en términos porcentuales. 
#Esto es necesario debidoa las diferentes escalas entre productos, no podemos comparar productos más grandes con otros más chicos sin estandarizar primero


#All products have similar relationship between price and quantity sold
#Sudden rises in sales in certain weeks don't fit the linear model well

#Todos los productos tienen una relación parecida entre sus precios y sus ventas en términos relativos.
#Los saltos en las ventas de los productos en semanas determinadas que no se ajustan bien al modelo lineal


#PREGUNTA 2:
#2.1
#Correlation
#Correlaciones
library(psych)
describe(subset(data_hw1, select=c(q_tide128, q_tide64, q_wisk64, p_tide128, p_tide64, p_wisk64)))

#2.2
cor(data_hw1$q_tide128, data_hw1$p_tide128)
cor(data_hw1$q_tide64, data_hw1$p_tide64)
cor(data_hw1$q_wisk64, data_hw1$p_wisk64)


cor(data_hw1$log_q_tide128, data_hw1$log_p_tide64)
cor(data_hw1$log_q_tide128, data_hw1$log_p_wisk64)

cor(data_hw1$log_q_tide64, data_hw1$log_p_tide128)
cor(data_hw1$log_q_tide64, data_hw1$log_p_wisk64)

cor(data_hw1$log_q_wisk64, data_hw1$log_p_tide128)
cor(data_hw1$log_q_wisk64, data_hw1$log_p_tide64)



#2.3
#Promotion and No Promotion weeks for each product
#Cantidad de observaciones que estuvo en promoción y la cantidad de obs. en las que no tuvo promoción cada producto:
freq_promo_tide128 <- table(data_hw1$promo_tide128)
freq_promo_tide64 <- table(data_hw1$promo_tide64)
freq_promo_wisk64 <- table(data_hw1$promo_wisk64)

freq_promo_tide128[2]/ (freq_promo_tide128[1] +freq_promo_tide128[2])
freq_promo_tide64[2]/ (freq_promo_tide64[1] +freq_promo_tide64[2])
freq_promo_wisk64[2]/ (freq_promo_wisk64[1] +freq_promo_wisk64[2])
#Tide128 had promotion 50.378% of observations, tide64 29.618% and wisk64 9.367%
#vemos que tide128 estuvo en promocion un 50,376% de las observaciones, tide64 un 29,619% y wisk64 un 9,367% de las observaciones

#2.4
#Group promotion per store, and per product
#Agrupamos promociones por local y por producto
promo_tide128_por_local <- data_hw1 %>%
  group_by(store, promo_tide128) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
freq_promo_tide128_local <- subset( promo_tide128_por_local, promo_tide128==1)
graf_freq_promo_tide128_local <- ggplot(freq_promo_tide128_local, aes(store, freq, group=1))+
  geom_point(color="royalblue2") +
  geom_line() +
  ylim(0,1) +
  labs(x="local", y= "frecuencia") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Frecuencia Promocional Tide 128 por Local") +
  theme(plot.title = element_text(hjust = 0.5))
  
#No significant differnces in promotional intensisite per store for tide128
#no se ven diferfencias significativas en la intensidad promocional por tienda para tide 128.


promo_tide64_por_local <- data_hw1 %>%
  group_by(store, promo_tide64) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
freq_promo_tide64_local <- subset( promo_tide64_por_local, promo_tide64==1)
graf_freq_promo_tide64_local <- ggplot(freq_promo_tide64_local, aes(store, freq, group=1))+
  geom_point(color="red2") +
  geom_line() +
  labs(x="local", y= "frecuencia") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Frecuencia Promocional Tide 64 por Local") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
#No significant differences for promotional intensisty per store for tide64
#no se ven diferfencias significativas en la intensidad promocional por tienda para tide 64.


promo_wisk64_por_local <- data_hw1 %>%
  group_by(store, promo_wisk64) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
freq_promo_wisk64_local <- subset( promo_wisk64_por_local, promo_wisk64==1)
graf_freq_promo_wisk64_local <- ggplot(freq_promo_wisk64_local, aes(store, freq, group=1))+
  geom_point(color = "springgreen2") +
  geom_line() +
  ylim(0,1) +
  labs(x="local", y= "frecuencia") + 
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title=element_text(face="bold")) +
  ggtitle("Frecuencia Promocional Wisk 64 por Local") +
  theme(plot.title = element_text(hjust = 0.5))
#No significant differences in promotional intensity per store fro wisk64
#no se ven diferfencias significativas en la intensidad promocional por tienda para wsik64.

#No significant differences overall in promotional intensity
#No parecieran haber diferencias significativas entre las intensidades promocionales por producto de las distintas tiendas


#Pregunta 3
#Linear regression model to explain sales
#Modelo de regresión lineal para epxlicar ventas
model_tide128 <- lm(log_q_tide128 ~ log_p_tide128 + log_p_tide64 + log_p_wisk64 + promo_tide128 + promo_tide64 +
                      promo_wisk64 + store + week, data=data_hw1)
SMT128 <-summary(model_tide128)
SMT128$coefficients[1:7,]

model_tide64 <- lm(log_q_tide64 ~ log_p_tide64 + log_p_tide128 + log_p_wisk64 + promo_tide64 + promo_tide128 + 
                     promo_wisk64 + store + week, data=data_hw1)
SMT64 <- summary(model_tide64)
SMT64$coefficients[1:7,]

model_wisk64 <- lm(log_q_wisk64 ~ log_p_tide128 + log_p_tide64 + log_p_wisk64 + promo_tide128 + promo_tide64 + 
                     promo_wisk64 + store + week, data=data_hw1)
SMW64 <- summary(model_wisk64)
SMW64$coefficients[1:7,]

#3.2
#Constants caputre fixed differences between stores because of external factors
#Los alpha sub s capturan diferencias fijas entre locales debido a características externas


#Verify heterogeneity between stores.
anova_t128 <- anova(model_tide128)
anova_t64 <- anova(model_tide64)
anova_w64 <- anova(model_wisk64)
anova_t128[7,]
anova_t64[7,]
anova_w64[7,]
#There is evidence to say that stores are different between them in term of sales
#Esto nos demuestra que la varianza entre los distintos locales es significativamente distinta de cero para los tres productos. 


#3.3
#Look for time trend
#Vemos si es que hay tendencia temporal.
SMT128$coefficients[93,]
SMT64$coefficients[93,]
SMW64$coefficients[93,]
#There is evidence to confirm the existance of a time trend
#Hay tendencia para confirmar la existencia de una tendencia temporal

#Tide 128: Each week sales drop 0.23%, on average
#Tide 128 Cada semana las ventas caen un 0,23%, en promedio

#Tide 64: Each week sales drop 0.75%, on average
#Tide 64: Cada semana caen  las ventas un 0,75%, en promedio

#Wisk 64: Each week sales increase by 0.23%, on average
#Wisk 64: Cada semana, las ventas suben, en promedio, un 0,23%

#3.4

#Coefficients in model represent elasticities because of logarithmic transformation
#Debido a las transformaciones logarítmicas, los coeficientes del model reportan las elasticidades pertinentes a su variable.

#Price-sales elasticities
#Elasticidades Precio Ventas:
#Tide 128: Tide128= -4,5584106 ; Tide64 = 0.1511111 ; Wisk64 = 0.6165177
#Tide 64: Tide64=-3.9781609 ; Tide128 = 1.2020542 ; Wisk64 = 0.3470890
#Wisk 64: Wisk64 =-4.7862303 ; Tide128= -0.3101532 ; Tide 64= 0.4598756


#3.5
#Promotional elasticity (percentage change when promotion is active v/s no active promotion)
#Elasticidad promocional (cambio porcentual en las ventas cuando hay promoción v/s cuando no hay)
#Tide 128: Tide 128= 100*0.1807205 ; Tide64 = 100*-0.0962716 ; Wisk64 = 100*-0.0001689
#Tide 64: Tide 64=100*0.2353755 ; Tide128 = 100*-0.2330167 ; Wisk64 = 100*-0.5108207
#Wisk 64: Wisk 64 =100*0.2970592 ; Tide128= 100*0.0096328 ; Tide 64=100*-0.1102152