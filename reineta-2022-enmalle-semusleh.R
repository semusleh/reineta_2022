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
library(zoo) #cambiar a trimestre
library(stringr) #separar caracteres HORA_DE_REPOSO
library(tidymv)
library(mgcViz) #visualizar resultados gam

setwd("C:/Github-DER/project reineta_2022/data reineta_2022")

reineta_2022<-read_excel("Bitacoras enmalle - Artesanal hist.xlsx")
Maestro_naves<-read_excel("Maestro naves.xlsx")

reineta_2022$year<-format(as.Date(reineta_2022$FECHA_LANCE, format="%d/%m/%Y"),"%Y")
reineta_2022$month<-format(as.Date(reineta_2022$FECHA_LANCE, format="%d/%m/%Y"),"%m")

reineta_2022$Hora_reposo<-round(reineta_2022$HORA_DE_REPOSO/100+(
  as.numeric(str_sub(reineta_2022$HORA_DE_REPOSO,-2,-1))/60),2)

reineta_2022$f_year<-as.factor(reineta_2022$year)
reineta_2022$f_month<-as.factor(reineta_2022$month)

reineta_2022$cpue_hr<-reineta_2022$PESO/reineta_2022$Hora_reposo
reineta_2022$cpue_ml<-(reineta_2022$PESO)/(reineta_2022$LONGITUD_RED)

reineta_2022$trimestre <- zoo::as.yearqtr(reineta_2022$FECHA_LANCE,   # Convert dates to quarterly
                                 format = "%Y-%m-%d")

reineta_2022$trimestre_Q <- as.factor(str_sub(reineta_2022$trimestre,-2,-1)) #Trimestre sin año

reineta_2022$COD_BARCO<-as.factor(reineta_2022$COD_BARCO)


#complementación de datos de profundidad

table(is.na(reineta_2022$PROFUNDIDAD_LM_ENM))
table(is.na(reineta_2022$PROFUNDIDAD))

reineta_2022$prof<-NA
for(i in 1:dim(reineta_2022)[1]){
  if (is.na(reineta_2022$PROFUNDIDAD_LM_ENM)[i]){
    reineta_2022$prof[i]<-reineta_2022$PROFUNDIDAD[i]}
  else {reineta_2022$prof[i]<-reineta_2022$PROFUNDIDAD_LM_ENM[i]}
}

#enmalle empieza el 2004
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

reineta_2022_yr_mo<-reineta_2022 %>%
  group_by(year, month)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, year, month)%>%
  summarise_all(funs(sum(!is.na(.))))

reineta_2022_yr<-reineta_2022 %>%
  group_by(year)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, year)%>%
  summarise_all(funs(sum(!is.na(.))))  

reineta_2022_yr_sum<-reineta_2022 %>%
  group_by(year)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, year)%>%
  summarise_all(funs(sum), na.rm= T)  


format_table(reineta_2022_yr, format="markdown",
             align =c("c","c","c", "c", "c"), digits=3,
             list(`Indicator Name` = formatter(
               "span", style = ~ style(color = "grey",font.weight = "bold")) 
             ))

q1<-ggplot(data=reineta_2022_yr_sum, aes(y=PESO/1000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura en peso")

q2<-ggplot(data=reineta_2022_yr_sum, aes(y=LONGITUD_RED, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en LONGITUD_RED")

q3<-ggplot(data=reineta_2022_yr_sum, aes(y=TAMANIO_MALLA, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en TAMANIO_MALLA")

q4<-ggplot(data=reineta_2022_yr_sum, aes(y=NUMERO_PANIOS, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en NUMERO_PANIOS")

q5<-ggplot(data=reineta_2022_yr_sum, aes(y=Hora_reposo, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en Hora de reposo")

ggarrange(q1, q2, q3, q4,q5,
          ncol=2, nrow=3)

ggarrange(q1, q5,
          ncol=1, nrow=2)



hist(reineta_2022$NUMERO_PANIOS, breaks = 100)
boxplot(NUMERO_PANIOS ~ COD_BARCO, data = reineta_2022, las=2)

hist(reineta_2022$Hora_reposo)

boxplot(HORA_DE_REPOSO ~ COD_BARCO, data = reineta_2022, las=2)
summary(reineta_2022$HORA_DE_REPOSO[reineta_2022$HORA_DE_REPOSO>0])

#Ejemplo variabilidad Esfuerzo por COD_BARCO

boxplot(HORA_DE_REPOSO~COD_BARCO, data=reineta_2022[reineta_2022$year=="2021",], las=2, xlab="")
boxplot(NUMERO_PANIOS~COD_BARCO, data=reineta_2022[reineta_2022$year=="2021",], las=2, xlab="")
boxplot(TAMANIO_MALLA~COD_BARCO, data=reineta_2022[reineta_2022$year=="2021",], las=2, xlab="")
boxplot(LONGITUD_RED~COD_BARCO, data=reineta_2022[reineta_2022$year=="2021",], las=2, xlab="")

asd1<-ggplot(data = reineta_2022, aes(y=Hora_reposo, group=COD_BARCO)) +
  geom_boxplot()+
  labs(title = "Variabilidad en Hora_reposo",
       y = "Hora_reposo", x = "COD_BARCO") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~ year, scales = "free")

asd2<-ggplot(data = reineta_2022[reineta_2022$LONGITUD_RED < 4000,], 
       aes(y=LONGITUD_RED, group=COD_BARCO)) +
  geom_boxplot()+
  labs(title = "Variabilidad en Largo de red (m)",
       y = "LONGITUD_RED", x = "COD_BARCO") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~ year, scales = "free")


### Filtros ###

#capturas PESO
hist(reineta_2022$PESO, breaks = 40, 
     main = "Histograma Peso captura", sub="Línea roja = 10000 kg",
     xlab = "PESO", ylab="Frecuencia")
abline(v=10000, col="red", lty=2)

filtrado_1<-subset(reineta_2022, reineta_2022$PESO <= 10000)

#esfuerzo HORA_DE_REPOSO
hist(reineta_2022$Hora_reposo, breaks = 200,
     main = "Histograma Esfuerzo en H de reposo", sub="Línea roja = 1800 min (30h)",
     xlab = "HORA_DE_REPOSO", ylab="Frecuencia")
abline(v=24, col="red", lty=2)

filtrado_2<-subset(filtrado_1, filtrado_1$Hora_reposo <= 24)

#esfuerzo NUMERO_PANIOS
hist(reineta_2022$NUMERO_PANIOS, breaks = 200, 
     main = "Histograma Esfuerzo en N de paños", sub="Línea roja = 50 paños",
     xlab = "NUMERO_PANIOS", ylab="Frecuencia")
abline(v=50, col="red", lty=2)

filtrado_3<-subset(filtrado_2, filtrado_2$NUMERO_PANIOS <= 50) 

#PROFUNDIDAD
hist(reineta_2022$prof, xlim = c(0, 1000), breaks = 100000,
     main = "Histograma Profundiad", sub="Rojo = 300 y Azúl = 50",
     xlab = "Profundidad", ylab="Frecuencia")
abline(v=300, col="red", lty=2)
abline(v=50, col="blue", lty=2)

filtrado_4<-subset(filtrado_3, filtrado_3$prof <= 50) #50 m

#esfuerzo LONGITUD_RED
hist(reineta_2022$LONGITUD_RED, xlim = c(0, 4000), breaks = 1000,
     main = "Histograma Esfuerzo en LONGITUD_RED", sub="Rojo = 300",
     xlab = "LONGITUD_RED", ylab="Frecuencia")
abline(v=3500, col="red", lty=2)

filtrado_5<-subset(filtrado_4, filtrado_4$LONGITUD_RED <= 3500) 

#esfuerzo CPUE horas de reposo
hist(reineta_2022$cpue_hr, breaks=100,
     main = "Histograma CPUE (kg/hr)", sub="Línea roja = 20",
     xlab = "CPUE", ylab="Frecuencia")
abline(v=2000, col="red", lty=2)

filtrado_6<-subset(filtrado_5, filtrado_5$cpue_hr <= 2000) 

#esfuerzo CPUE
hist(reineta_2022$cpue_ml, breaks=1000, xlim=c(0, 100),
     main = "Histograma CPUE (kg/m)", sub="Línea roja = 20",
     xlab = "CPUE", ylab="Frecuencia")
abline(v=20, col="red", lty=2)

filtrado_7<-subset(filtrado_6, filtrado_6$cpue_ml <= 20)

#Trimestre de la operación
barplot(table(reineta_2022$trimestre_Q), main="Registros por trimestre",
        las=1)

filtrado_8<-subset(filtrado_7, filtrado_7$trimestre_Q != "Q3")

#Año de la operación

table(filtrado_8$f_year)
filtrado_9<-subset(filtrado_8, filtrado_8$year != 2004) #50 m

print(paste("Base de datos inicial=",dim(reineta_2022)[1]))
print(paste("Filtro PESO <= 10000",dim(filtrado_1)[1]))
print(paste("Filtro HORA_DE_REPOSO <= 24h=",dim(filtrado_2)[1]))
print(paste("Filtro NUMERO_PANIOS <= 50)=",dim(filtrado_3)[1]))
print(paste("Filtro prof <= 50 m=",dim(filtrado_4)[1]))
print(paste("Filtro LONGITUD_RED <= 3500 m=",dim(filtrado_5)[1]))
print(paste("Filtro cpue_hr <= 2000 kg/hr=",dim(filtrado_6)[1]))
print(paste("Filtro cpue_ml <= 20 kg/m=",dim(filtrado_7)[1]))
print(paste("Filtro trimestre_Q != Q3 (1 registro)",dim(filtrado_8)[1]))
print(paste("Filtro f_year >= 2016 (1 registro)",dim(filtrado_9)[1]))

dim(filtrado_9)

hist(filtrado_9$cpue_hr, breaks = 30)
hist(log(filtrado_7$cpue_hr), breaks = 30)

hist(filtrado_9$cpue_ml, breaks = 30)
hist(log(filtrado_7$cpue_ml), breaks = 30)

hist(filtrado_9$PESO, breaks = 30)
hist(log(filtrado_7$PESO), breaks = 30)

hist(filtrado_9$Hora_reposo, breaks = 30)
hist(log(filtrado_7$HORA_DE_REPOSO), breaks = 30)


reineta_2022_yr_postf<-filtrado_9 %>%
  group_by(year)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, cpue_hr, cpue_ml, prof, year, month)%>%
  summarise_all(funs(sum(!is.na(.))))  

reineta_2022_yr_sum_postf<-filtrado_9 %>%
  group_by(year)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, cpue_hr, cpue_ml, prof, year)%>%
  summarise_all(funs(sum), na.rm= T)  

write.csv(data.frame(year=reineta_2022_yr_sum_postf$year,
                     captura=reineta_2022_yr_sum_postf$PESO,
                     esfuerzo=reineta_2022_yr_sum_postf$LONGITUD_RED,
                     cpue=round(reineta_2022_yr_sum_postf$cpue_ml, 2)),
          file="cpue_nominal_enmalle-2022.csv")

formattable(reineta_2022_yr_postf, format="markdown", caption = "Data por columna por año",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

# Plots post filtrado cpue_hr

w1<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=PESO/10000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura acumulada anual")+
  ylab("Toneladas")

w2<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=Hora_reposo, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo acumulada anual")+
  ylab("Toneladas")

w3<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=cpue_hr, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("cpue nominal acumulada anual")+
  ylab("Toneladas")

# Plot post filtrado cpue_ml

e1<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=PESO/10000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura acumulada anual")+
  ylab("Toneladas")

e2<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=LONGITUD_RED, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo acumulada anual")+
  ylab("Toneladas")

e3<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=cpue_ml, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("cpue nominal acumulada anual")+
  ylab("Toneladas")


ggarrange(e1, e2, e3,
          ncol=1, nrow=3)

logcpuedata<-data.frame(logpeso=log(filtrado_9$PESO),
                        logcpue_hr=log(filtrado_9$cpue_hr),
                        logcpue_ml=log(filtrado_9$cpue_ml),
                        logcpue_comb=log(filtrado_9$PESO/(filtrado_9$Hora_reposo*filtrado_9$LONGITUD_RED)),
                        cpue_hr=filtrado_9$cpue_hr,
                        cpue_ml=filtrado_9$cpue_ml,
                        f_year=filtrado_9$f_year,
                        f_month=filtrado_9$f_month, 
                        trimestre=as.factor(filtrado_9$trimestre),
                        trimestre_Q=as.factor(filtrado_9$trimestre_Q),
                        COD_BARCO=filtrado_9$COD_BARCO, 
                        LATITUD=filtrado_9$LATITUD, 
                        LONGITUD=filtrado_9$LONGITUD)

# Filtro de valores finitos (hay uno que se va a infinito)

dim(logcpuedata)

logcpuedata<-logcpuedata %>% 
  filter_at(vars(names(logcpuedata)[1:4]), all_vars(!is.infinite(.)))

dim(logcpuedata)

logcpue_hr<-ggplot(logcpuedata, aes(x=logcpue_hr, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_hr")


logcpue_ml<-ggplot(logcpuedata, aes(x=logcpue_ml, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_ml")


logcpue_comb<-ggplot(logcpuedata, aes(x=logcpue_comb, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_comb")

ggarrange(logcpue_hr, logcpue_ml, logcpue_comb,
          ncol=1, nrow=3)

# Vínculo con maestro naves para obtener nuevas variables para la CPUE estandarizada

Maestro_naves<-data.frame(COD_BARCO=Maestro_naves$COD_BARCO,
                          Eslora=Maestro_naves$ESLORA,
                          Manga=Maestro_naves$MANGA,
                          Punta=Maestro_naves$PUNTAL,
                          Potencia=Maestro_naves$POTENCIA_MOTOR,
                          TRG=Maestro_naves$TRG)

#mismo formato para ambos dataframe en la columna de COD_BARCO
Maestro_naves$COD_BARCO<-as.factor(Maestro_naves$COD_BARCO)

logcpuedata_naves<-left_join(x=logcpuedata, y=Maestro_naves, by ="COD_BARCO") 

logcpuedata_naves_count<-logcpuedata_naves %>%
  group_by(f_year)  %>%
  summarise_all(funs(sum(!is.na(.))))  


formattable(logcpuedata_naves_count, format="markdown", caption = "Data por columna por año",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

###############
# Exploración de relación entre CPUE y variables


names(logcpuedata_naves)

boxplot(cpue_ml~trimestre_Q, data=logcpuedata_naves, las=2)

boxplot(log(cpue_ml)~f_month, data=logcpuedata_naves)

#table(reineta_2022$f_month)

ggplot(logcpuedata_naves, aes(y=cpue_ml, x=trimestre, fill=trimestre_Q))+
  geom_boxplot(alpha=0.5, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "black")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_year, fill=f_year)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_month, fill=f_month)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(logcpuedata_naves, aes(y=logcpue_ml, x=-LATITUD/10000)) +
  geom_point(alpha=0.3, position="identity")+
  xlim(c(-75, -30))

ggplot(logcpuedata_naves, aes(y=logcpue_ml, x=-LONGITUD/10000)) +
  geom_point(alpha=0.3, position="identity")+
  xlim(c(-75, -65))

ggplot(logcpuedata_naves, aes(x=logcpue_ml, color=f_month, fill=f_month)) +
  geom_histogram(alpha=0.3, position="identity")

#table(reineta_2022$COD_PESQUERIA)
#6 es encuesta
#122 Observadores

############## MAPAS

# Dato latitud impresiso de origen. mejor no usar para filtros

# filtrado_10<-logcpuedata_naves %>%
#   filter(lat >= -40 & lat <= -30) %>%
#   filter(lon >= -80 & lon <= -70)

dim(logcpuedata_naves)
dim(filtrado_10)

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

Chile <- ne_countries(scale = "medium", returnclass = "sf", country= "Chile")
world <- ne_countries(scale = "medium", returnclass = "sf")

lat_grados<-as.numeric(str_sub(logcpuedata_naves$LATITUD, 1,2))
lat_minutos<-as.numeric(str_sub(logcpuedata_naves$LATITUD, 3,4))
lat_segundos<-as.numeric(str_sub(logcpuedata_naves$LATITUD, 5,6))

logcpuedata_naves$lat<-round(lat_grados+(lat_minutos/60)+(lat_segundos/3600),2)*-1

lon_grados<-as.numeric(str_sub(logcpuedata_naves$LONGITUD, 1,2))
lon_minutos<-as.numeric(str_sub(logcpuedata_naves$LONGITUD, 3,4))
lon_segundos<-as.numeric(str_sub(logcpuedata_naves$LONGITUD, 5,6))

logcpuedata_naves$lon<-round(lon_grados+(lon_minutos/60)+(lon_segundos/3600),2)*-1

write.csv(file="logcpuedata_naves.csv", logcpuedata_naves, row.names = F)

ggplot(data = Chile) +
  geom_sf()+
  coord_sf(xlim = c(-80, -70), ylim = c(-40, -30))+
  geom_point(data = filtrado_10,
             mapping = aes(x = lon, y = lat), colour = "red")+
  facet_wrap(~f_year, ncol = 6)


#####

ggplot(logcpuedata_naves, aes(y=cpue_ml, x=trimestre, fill=trimestre_Q))+
  geom_boxplot(alpha=0.5, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "black")+
  theme(axis.text.x = element_text(angle = 90))






logcpuedata_naves



###############
# Normalización de CPUE

# https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

hist(logcpuedata$logcpue_ml, breaks = 30)

qqnorm(logcpuedata$logcpue_ml)+
qqline(logcpuedata$logcpue_ml) 

logcpue_ml_1<-logcpuedata$logcpue_ml
cpue_ml_1<-logcpuedata$cpue_ml

moments::skewness(cpue_ml_1, na.rm = TRUE)
moments::skewness(logcpue_ml_1, na.rm = TRUE)

ss0<-sqrt(cpue_ml_1)
ss1<-log(max(logcpue_ml_1+1)-logcpue_ml_1, base = 10)
ss2<-1/(max(logcpue_ml_1+1) - logcpue_ml_1)
  

hist(ss0, breaks = 30)
hist(log(ss0), breaks = 30)
hist(ss1, breaks = 30)

qqnorm(ss0)+
  qqline(ss0) 

qqnorm(ss1)+
  qqline(ss1) 

qqnorm(ss2)+
  qqline(ss2) 

ggdensity(ss1, ss1 = "", fill = "lightgray", 
          title = "log(max(logcpue_ml_1+1)-logcpue_ml_1, base = 10)") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(logcpue_ml_1, logcpue_ml_1 = "", fill = "lightgray", 
          title = "logcpue_ml_1") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

shapiro.test(ss0)
shapiro.test(ss1)
shapiro.test(ss2)

##################
library("fitdistrplus")
# Evaluar ajustes con distribuciones

# plotdist(m3aend$Logcpue, histo = TRUE, demp =T,col="gray")
# ?plotdist
# descdist(m3aend$Logcpue, boot = 100)
# Francisco Contreras16:51
# https://rpubs.com/aafernandez1976/fitdistrplus

##################





if(!require(GDAdata)){install.packages("GDAdata")}
if(!require(rcompanion)){install.packages("rcompanion")}

library("GDAdata")
library(rcompanion)

# https://www.rdocumentation.org/packages/rcompanion/versions/2.3.7/topics/blom

qqnorm(logcpue_ml_1)+
  qqline(logcpue_ml_1, col='red')


Blom = blom(logcpue_ml_1)

qqnorm(Blom)+
qqline(Blom, col='red')

hist(Blom)


shapiro.test(Blom)

logcpuedata$Blom<-Blom

x_qbeta <- seq(0, 1, by = 0.02) 
y_rbeta <- rbeta(1000, shape1 = 8, shape2 = 2)        # Draw N beta distributed values

hist(logcpue_ml_1, breaks = 30)
hist(y_rbeta, breaks = 30)  

ks.test(y_rbeta, logcpue_ml_1)



##############################################################################
#Estandarización cpue (Primeros modelos)

gam_mod1<-lm(logcpue_ml ~ + LATITUD + LONGITUD + f_year + trimestre_Q + COD_BARCO,
             data= logcpuedata)

gam_mod2<-lm(Blom ~ + LATITUD + LONGITUD + f_year + trimestre_Q + COD_BARCO,
             data= logcpuedata)

summary(gam_mod1)
summary(gam_mod2)

names(logcpuedata)

gam_mod1<-gam(logcpue_hr ~ s(LATITUD, LONGITUD) + f_year,
              data= logcpuedata)

gam_mod2<-gam(logcpue_hr ~ te(LATITUD, LONGITUD) + f_year,
              data= logcpuedata)

gam_mod3<-gam(logcpue_hr ~ f_year + trimestre + s(LATITUD, LONGITUD) + COD_BARCO,
              data= logcpuedata)

lm_Mod3<-lm(logcpue_hr ~ f_year + LATITUD + LONGITUD + trimestre + COD_BARCO,
            data= logcpuedata)

summary(gam_mod1)
summary(gam_mod2)
summary(gam_mod3)
summary(lm_Mod3)

gam_mod3_logcpue_hr<-gam(logcpue_hr ~ f_year + trimestre + s(LATITUD, LONGITUD) + COD_BARCO,
                         data= logcpuedata)
summary(gam_mod3)

hist(logcpuedata$logcpue_hr, breaks = 30) #No es suficientemente normal

gam_mod3_logcpue_ml<-gam(cpue_ml ~ f_year + trimestre_Q +
                           s(LATITUD, LONGITUD) + COD_BARCO, data= logcpuedata,
                         ma.action=na.omit, method="REML", Tweedie(1.8, "log"))

summary(gam_mod3_logcpue_ml)

gam_mod3_logcpue_ml_out <- getViz(gam_mod3_logcpue_ml)
print(plot(gam_mod3_logcpue_ml_out, allTerms = T), pages = 1)

str(gam_mod3_logcpue_ml_out)

gam_mod3_logcpue_ml_out$xlevels$f_year  

gam_mod3_logcpue_ml_out        

devexp_gam_mod3_logcpue_ml<-round(summary(gam_mod3_logcpue_ml)$dev.expl, 2)

print(paste0("Deviance explained: ", devexp_gam_mod3_logcpue_comb, "%"))