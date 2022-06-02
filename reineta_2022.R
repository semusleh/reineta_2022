library(data.table)
library(dplyr)
library(formattable)
library(tidyverse)#remover columnas vacias
library(tidyr)
library(pivottabler)
library(janitor)
library(lattice)
library(mgcv)#GAM
library(readxl)
library(ggplot2)
library(ggpubr) #multiplot ggplot con una sola leyenda

setwd("C:/Github-DER/project reineta_2022/data reineta_2022")

reineta_2022<-read_excel("Bitacoras enmalle - Artesanal hist.xlsx")
reineta_2022$year<-format(as.Date(reineta_2022$FECHA_LANCE, format="%d/%m/%Y"),"%Y")

reineta_2022$cpue<-reineta_2022$PESO/reineta_2022$HORA_DE_REPOSO

#complementación de datos de profundidad

table(is.na(reineta_2022$PROFUNDIDAD_LM_ENM))
table(is.na(reineta_2022$PROFUNDIDAD))

reineta_2022$prof<-NA
for(i in 1:dim(filtrado_3)[1]){
  if (is.na(filtrado_3$PROFUNDIDAD_LM_ENM)[i]){
    reineta_2022$prof[i]<-filtrado_3$PROFUNDIDAD[i]}
  else {reineta_2022$prof[i]<-filtrado_3$PROFUNDIDAD_LM_ENM[i]}
}

table(is.na(reineta_2022$prof))


range(reineta_2022$year) #enmalle empieza el 2004
#Espinel restringido espacialmente a Regiones de Valparaíso y Los Ríos (Resolución N 1333-2013)

#COD_ESPECIE 27
#PESO Columna más completa que la de abajo 
#PESO_TOTAL_CAPTURA no tan completa
#Profundidad de 0 a 300m

#CPUE

#Esfuerzo 1: LONGITUD_RED
#Esfuerzo 2: TAMANIO_MALLA
#Esfuerzo 3: NUMERO_PANIOS
#Esfuerzo 4: HORA_DE_REPOSO


range(reineta_2022$LONGITUD_RED, na.rm = T)
range(reineta_2022$TAMANIO_MALLA, na.rm = T)
range(reineta_2022$NUMERO_PANIOS, na.rm = T)
summary(reineta_2022$HORA_DE_REPOSO, na.rm = T)

reineta_2022$dif_viaje_h <- difftime(reineta_2022$FECHA_HORA_RECALADA,
                 reineta_2022$FECHA_HORA_ZARPE, units = "hours")

summary(as.vector(reineta_2022$dif_viaje_h))


dif_hviaje_hreposo<-reineta_2022$dif_viaje_h-(reineta_2022$HORA_DE_REPOSO)
dif_hviaje_mreposo<-reineta_2022$dif_viaje_h-(reineta_2022$HORA_DE_REPOSO/60)

summary(as.vector(dif_hviaje_hreposo))
summary(as.vector(dif_hviaje_mreposo))


df<-data.frame(dif_hviaje_hreposo, col1="black", dif_hviaje_mreposo, col2="black")

for(i in 1:dim(df)[1]){
ifelse(df$dif_hviaje_hreposo[i] > 0,
       df$col1[i] <- "red",
       df$col1[i] <- "blue")
  
  ifelse(df$dif_hviaje_mreposo[i] > 0,
         df$col2[i] <- "red",
         df$col2[i] <- "blue")
}

plot(dif_hviaje_hreposo, pch=16, col=df$col1)
plot(dif_hviaje_mreposo, pch=16, col=df$col2)

a1<-ggplot(df, aes(y=dif_hviaje_hreposo, x=seq(1:dim(df)[1])), fill=col1)+
  geom_point(alpha = 4/10, colour= df$col1)+
  labs(title = "Diferencia tiempo de viaje y de reposo", subtitle = "Tiempo de reposo en horas")+
  xlab("registros")

a2<-ggplot(df, aes(y=dif_hviaje_mreposo, x=seq(1:dim(df)[1])), fill=col1)+
  geom_point(alpha = 4/10, colour= df$col2)+
  labs(title = "Diferencia tiempo de viaje y de reposo", subtitle = "Tiempo de reposo/60 (min)")+
  xlab("registros")


ggarrange(a1, a2,
          ncol=2, nrow=1)


h0<-ggplot(reineta_2022)+
  geom_histogram(aes(x=dif_viaje_h))+
  geom_histogram(aes(x=HORA_DE_REPOSO))
  
h1<-ggplot(reineta_2022, aes(x=LONGITUD_RED))+
  geom_histogram() 

h2<-ggplot(reineta_2022, aes(x=TAMANIO_MALLA))+
  geom_histogram() 

h3<-ggplot(reineta_2022, aes(x=NUMERO_PANIOS))+
  geom_histogram() 

h4<-ggplot(reineta_2022, aes(x=HORA_DE_REPOSO))+
  geom_histogram() 

ggarrange(h1, h2, h3, h4,h0,
          ncol=2, nrow=3)

data_reineta_pca_h<-(reineta_2022[,c("LONGITUD_RED", "TAMANIO_MALLA", "NUMERO_PANIOS", "HORA_DE_REPOSO")])

data_reineta_pca_h[is.na(data_reineta_pca_h)] <- 0


esfuerzo_pca<-prcomp(data_reineta_pca_h ,scale = T)
esfuerzo_pca$x

reineta_2022<-cbind(reineta_2022, esfuerzo_pca$x)


reineta_2022$cpue<-(reineta_2022$PESO/reineta_2022$PC1 )


acumm_reineta_2022 <- reineta_2022 %>%
  group_by(year) %>% 
  summarise_at(c("PESO", "LONGITUD_RED", "TAMANIO_MALLA", "NUMERO_PANIOS", "HORA_DE_REPOSO"),
               funs(sum), na.rm = TRUE)

summ_reineta_2022

ggplot(acumm_reineta_2022)

q1<-ggplot(data=acumm_reineta_2022, aes(y=PESO, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura en peso")

q2<-ggplot(data=acumm_reineta_2022, aes(y=LONGITUD_RED, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en NUMERO_PANIOS")

q3<-ggplot(data=acumm_reineta_2022, aes(y=TAMANIO_MALLA, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en TAMANIO_MALLA")

q4<-ggplot(data=acumm_reineta_2022, aes(y=NUMERO_PANIOS, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en NUMERO_PANIOS")

q5<-ggplot(data=acumm_reineta_2022, aes(y=HORA_DE_REPOSO, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en HORA_DE_REPOSO")

ggarrange(q1, q2, q3, q4,q5,
          ncol=2, nrow=3)

ggarrange(q1, q5,
          ncol=1, nrow=2)

names(reineta_2022)

hist(reineta_2022$NUMERO_PANIOS, breaks = 100)
boxplot(NUMERO_PANIOS ~ COD_BARCO, data = reineta_2022, las=2)

hist(reineta_2022$HORA_DE_REPOSO, breaks = 100)
hist(reineta_2022$HORA_DE_REPOSO/60, breaks = 100)
hist(reineta_2022$HORA_DE_REPOSO/100, breaks = 100)

hist(reineta_2022$HORA_DE_REPOSO[reineta_2022$HORA_DE_REPOSO<600])

boxplot(HORA_DE_REPOSO ~ COD_BARCO, data = reineta_2022, las=2)
summary(reineta_2022$HORA_DE_REPOSO[reineta_2022$HORA_DE_REPOSO>0])

ggplot(data = reineta_2022, aes(y=HORA_DE_REPOSO, group=COD_BARCO)) +
  geom_boxplot()+
  labs(title = "HORA_DE_REPOSO",
       y = "HORA_DE_REPOSO", x = "COD_BARCO") + 
  facet_wrap(~ year)

ggplot(data = reineta_2022, aes(y=NUMERO_PANIOS, group=COD_BARCO)) +
  geom_boxplot()+
  labs(title = "NUMERO_PANIOS",
       y = "NUMERO_PANIOS", x = "COD_BARCO") + 
  facet_wrap(~ year)


#capturas PESO
hist(reineta_2022$PESO, breaks = 40)
filtrado_1<-subset(reineta_2022, reineta_2022$PESO <= 10000)

#esfuerzo HORA_DE_REPOSO
hist(reineta_2022$HORA_DE_REPOSO/60, breaks = 200, xlim = c(0, 50))
filtrado_2<-subset(filtrado_1, filtrado_1$HORA_DE_REPOSO <= 1800) #30h / podría ser 24h

#esfuerzo NUMERO_PANIOS
hist(reineta_2022$NUMERO_PANIOS, breaks = 200, xlim = c(0, 50))
filtrado_3<-subset(filtrado_2, filtrado_2$NUMERO_PANIOS <= 50) 

#esfuerzo PROFUNDIDAD
hist(reineta_2022$prof, xlim = c(0, 1000), breaks = 10000)
filtrado_4<-subset(filtrado_3, filtrado_3$prof <= 300) 

#esfuerzo CPUE
hist(cpue_reineta_2022, breaks=100)
filtrado_5<-subset(filtrado_4, filtrado_4$cpue <= 20) 

print(paste("Base de datos inicial=",dim(reineta_2022)[1]))
print(paste("Filtro PESO <= 10000",dim(filtrado_1)[1]))
print(paste("Filtro HORA_DE_REPOSO <= 1800=",dim(filtrado_2)[1]))
print(paste("Filtro NUMERO_PANIOS <= 50)=",dim(filtrado_3)[1]))
print(paste("Filtro prof <= 300 m=",dim(filtrado_4)[1]))
print(paste("Filtro cpue <= 20 m=",dim(filtrado_5)[1]))

hist(filtrado_5$cpue, breaks = 50)




