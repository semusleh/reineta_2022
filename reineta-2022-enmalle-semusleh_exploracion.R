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

#cod_especie

boxplot(reineta_2022$PESO ~ reineta_2022$COD_ESPECIE)

reineta_2022_cod27<-reineta_2022 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") 

dim(reineta_2022_cod27)

#capturas PESO
hist(reineta_2022_cod27$PESO, breaks = 40, 
     main = "Histograma Peso captura", sub="Línea roja = 19000 kg",
     xlab = "PESO", ylab="Frecuencia")
abline(v=19000, col="red", lty=2)

filtrado_1<-subset(reineta_2022_cod27, reineta_2022_cod27$PESO <= 19000)

#esfuerzo HORA_DE_REPOSO
# hist(reineta_2022_cod27$Hora_reposo, breaks = 200,
#      main = "Histograma Esfuerzo en H de reposo", sub="Línea roja = 1800 min (30h)",
#      xlab = "HORA_DE_REPOSO", ylab="Frecuencia")
# abline(v=24, col="red", lty=2)

# filtrado_2<-subset(filtrado_1, filtrado_1$Hora_reposo <= 24)

#esfuerzo NUMERO_PANIOS
# hist(reineta_2022_cod27$NUMERO_PANIOS, breaks = 200, 
#      main = "Histograma Esfuerzo en N de paños", sub="Línea roja = 50 paños",
#      xlab = "NUMERO_PANIOS", ylab="Frecuencia")
# abline(v=50, col="red", lty=2)

# filtrado_3<-subset(filtrado_2, filtrado_2$NUMERO_PANIOS <= 50) 

#esfuerzo LONGITUD_RED
hist(filtrado_1$LONGITUD_RED, xlim = c(0, 4000), breaks = 1000,
     main = "Histograma Esfuerzo en LONGITUD_RED", sub="Rojo = 300-3600",
     xlab = "LONGITUD_RED", ylab="Frecuencia")
abline(v=3600, col="red", lty=2)
abline(v=300, col="red", lty=2)

filtrado_2<-subset(filtrado_1, filtrado_1$LONGITUD_RED <= 3600) 
filtrado_3<-subset(filtrado_2, filtrado_2$LONGITUD_RED > 300) 

#esfuerzo CPUE horas de reposo
# hist(reineta_2022_cod27$cpue_hr, breaks=100,
#      main = "Histograma CPUE (kg/hr)", sub="Línea roja = 20",
#      xlab = "CPUE", ylab="Frecuencia")
# abline(v=2000, col="red", lty=2)
# 
# filtrado_6<-subset(filtrado_5, filtrado_5$cpue_hr <= 2000) 

#esfuerzo CPUE
# hist(filtrado_3$cpue_ml, breaks=1000,
#      main = "Histograma CPUE (kg/m)", sub="Línea roja = 20",
#      xlab = "CPUE", ylab="Frecuencia")
# abline(v=20, col="red", lty=2)

#filtrado_5<-subset(filtrado_4, filtrado_4$cpue_ml <= 20)

#PROFUNDIDAD
hist(filtrado_3$prof, xlim = c(0, 100), breaks = 100000,
     main = "Histograma Profundiad", sub="Azúl = 50",
     xlab = "Profundidad", ylab="Frecuencia")
abline(v=50, col="blue", lty=2)

filtrado_4<-subset(filtrado_3, filtrado_3$prof <= 50) #50 m

#Trimestre de la operación

table(filtrado_3$trimestre_Q)
table(filtrado_4$trimestre_Q)

barplot(table(filtrado_4$trimestre_Q), main="Registros por trimestre (filtro 50m prof)",
        las=1)

barplot(table(filtrado_3$trimestre_Q), main="Registros por trimestre",
        las=1)

barplot(table(filtrado_4$f_year), main="Registros por año (filtro 50m prof)",
        las=2)

barplot(table(filtrado_3$f_year), main="Registros por año",
        las=2)

#Año de la operación

table(filtrado_4$f_year)
filtrado_5<-subset(filtrado_4, filtrado_4$year != 2004) #50 m

print(paste("Base de datos inicial=",dim(reineta_2022)[1]))
print(paste("Filtro PESO <= 19000",dim(filtrado_1)[1]))
print(paste("Filtro LONGITUD_RED <= 3600 m=",dim(filtrado_2)[1]))
print(paste("Filtro LONGITUD_RED <= 300 m=",dim(filtrado_3)[1]))
print(paste("Filtro prof <= 50 m=",dim(filtrado_4)[1]))
print(paste("Filtro f_year >= 2006 (1 registro en 2005)",dim(filtrado_5)[1]))

################## 
#Filtros Seguimiento
#capturas PESO

dim(reineta_2022_cod27)
filtrado_seg1<-filter(reineta_2022_cod27, PESO > 1 | PESO <= 23000)
dim(filtrado_seg1)

hist(reineta_2022_cod27$PESO, breaks= 100)

filtrado_seg2<-filter(filtrado_seg1,  LONGITUD_RED <= 3600)
filtrado_seg3<-filter(filtrado_seg1,  LONGITUD_RED > 300)

hist(filtrado_seg1$LONGITUD_RED)

dim(filtrado_seg1)
dim(filtrado_seg2)
dim(filtrado_seg3)

summary(filtrado_seg3$LONGITUD_RED)
summary(filtrado_seg3$PESO)

#filtrado_seg4<-subset(filtrado_seg3, filtrado_seg3$prof <= 50) #50 m
#dim(filtrado_seg4)
# 
# table(filtrado_seg5$year)
# 
# filtrado_seg5<-subset(filtrado_seg4, filtrado_seg4$year != 2004) #quitar 2004 por pocos datos

hist(filtrado_seg3$cpue_ml, breaks = 30, las=1)
hist(log(filtrado_seg3$cpue_ml), breaks = 30, las=1)

hist(filtrado_seg3$PESO, breaks = 30, las=1)
hist(log(filtrado_seg3$PESO), breaks = 30, las=1)

hist(filtrado_seg3$LONGITUD_RED, breaks = 30, las=1)
hist(log(filtrado_seg3$LONGITUD_RED), breaks = 30, las=1)

#seg

reineta_2022_seg_yr_postf<-filtrado_seg3 %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, year, month)%>%
  summarise_all(funs(sum(!is.na(.))))

reineta_2022_seg_yr_sum_postf<-filtrado_seg3 %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, year)%>%
  summarise_all(funs(sum), na.rm= T)  

#######################

hist(filtrado_5$cpue_ml, breaks = 30)
hist(log(filtrado_5$cpue_ml), breaks = 30)

hist(filtrado_5$PESO, breaks = 30)
hist(log(filtrado_5$PESO), breaks = 30)

hist(filtrado_5$LONGITUD_RED, breaks = 30)
hist(log(filtrado_5$LONGITUD_RED), breaks = 30)

reineta_2022_yr_postf<-filtrado_5 %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, year, month)%>%
  summarise_all(funs(sum(!is.na(.))))  

reineta_2022_yr_sum_postf<-filtrado_5 %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, year)%>%
  summarise_all(funs(sum), na.rm= T) 

ncpue <- filtrado_5 %>% group_by(f_year) %>% 
  summarise (f1 = mean(PESO, na.rm = TRUE),
             f2 = mean(LONGITUD_RED, na.rm = TRUE),
             cpue = f1/f2,
             cpuesd = sd(PESO/LONGITUD_RED, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)


plot(ncpue$f_year, ncpue$cpue, )

ggplot(data=ncpue, aes(y=cpue, x=f_year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("CPUE nominal (mean(PESO)/mean(LONGITUD_RED)")+
  ylab("CPUE")

e3

####
#Comparaciones


write.csv(data.frame(year=reineta_2022_yr_sum_postf$year,
                     captura=reineta_2022_yr_sum_postf$PESO,
                     esfuerzo=reineta_2022_yr_sum_postf$LONGITUD_RED,
                     cpue=round(reineta_2022_yr_sum_postf$cpue_ml, 2)),
          file="cpue_nominal_enmalle-2022.csv")

write.csv(data.frame(year=reineta_2022_seg_yr_postf$year,
                     captura=reineta_2022_yr_sum_postf$PESO,
                     esfuerzo=reineta_2022_yr_sum_postf$LONGITUD_RED,
                     cpue=round(reineta_2022_yr_sum_postf$cpue_ml, 2)),
          file="cpue_nominal_enmalle-2022_filtrosseguimientos.csv")

formattable(reineta_2022_yr_postf, format="markdown", 
            caption = "Data por columna por año filtros propios",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

formattable(reineta_2022_seg_yr_postf, format="markdown", 
            caption = "Data por columna por año filtros de seguimiento",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

# Plots post filtrado cpue_hr
# 
# w1<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=PESO/10000, x=year, group=1))+
#   geom_line()+
#   geom_point()+
#   ggtitle("Captura acumulada anual")+
#   ylab("Toneladas")
# 
# w2<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=Hora_reposo, x=year, group=1))+
#   geom_line()+
#   geom_point()+
#   ggtitle("Esfuerzo acumulada anual")+
#   ylab("Toneladas")
# 
# w3<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=cpue_hr, x=year, group=1))+
#   geom_line()+
#   geom_point()+
#   ggtitle("cpue nominal acumulada anual")+
#   ylab("Toneladas")

# Plot post filtrado cpue_ml

e1<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=PESO/10000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura acumulada anual")+
  ylab("Toneladas")

e2<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=LONGITUD_RED/1000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo acumulada anual")+
  ylab("Km de red")

e3<-ggplot(data=reineta_2022_yr_sum_postf, aes(y=cpue_ml, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("cpue nominal acumulada anual")+
  ylab("Kg/m de red")

f1<-ggplot(data=reineta_2022_seg_yr_sum_postf, aes(y=PESO/10000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura acumulada anual")+
  ylab("Km de red")

f2<-ggplot(data=reineta_2022_seg_yr_sum_postf, aes(y=LONGITUD_RED/1000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo acumulada anual")+
  ylab("Kg/m de red")

f3<-ggplot(data=reineta_2022_seg_yr_sum_postf, aes(y=cpue_ml, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("cpue nominal acumulada anual")+
  ylab("Toneladas")

nominal_plot<-ggarrange(ggarrange(e1, e2, e3,
                    ncol=1, nrow=3),
          ggarrange(f1, f2, f3,
                    ncol=1, nrow=3), ncol = 2)

nominal_plot<-ggarrange(ggarrange(e1, e2, e3,
                                  ncol=1, nrow=3),
                        ggarrange(f1, f2, f3,
                                  ncol=1, nrow=3), ncol = 2)


annotate_figure(nominal_plot, top  = text_grob("CPUE nominal filtros propios y seguimiento", 
                                                color = "blue", face = "bold", size = 14))


logcpuedata<-data.frame(logpeso=log(filtrado_5$PESO),
                        logcpue_hr=log(filtrado_5$cpue_hr),
                        logcpue_ml=log(filtrado_5$cpue_ml),
                        logcpue_comb=log(filtrado_5$PESO/(filtrado_5$Hora_reposo*filtrado_5$LONGITUD_RED)),
                        cpue_hr=filtrado_5$cpue_hr,
                        cpue_ml=filtrado_5$cpue_ml,
                        captura=filtrado_5$PESO,
                        esfuerzo=filtrado_5$LONGITUD_RED,
                        f_year=filtrado_5$f_year,
                        f_month=filtrado_5$f_month, 
                        trimestre=as.factor(filtrado_5$trimestre),
                        trimestre_Q=as.factor(filtrado_5$trimestre_Q),
                        COD_BARCO=filtrado_5$COD_BARCO, 
                        LATITUD=filtrado_5$LATITUD, 
                        LONGITUD=filtrado_5$LONGITUD,
                        PUERTO_RECALADA=filtrado_5$PUERTO_RECALADA)

logcpuedata_seg<-data.frame(logpeso=log(filtrado_seg3$PESO),
                        logcpue_hr=log(filtrado_seg3$cpue_hr),
                        logcpue_ml=log(filtrado_seg3$cpue_ml),
                        logcpue_comb=log(filtrado_seg3$PESO/(filtrado_seg3$Hora_reposo*filtrado_seg3$LONGITUD_RED)),
                        cpue_hr=filtrado_seg3$cpue_hr,
                        cpue_ml=filtrado_seg3$cpue_ml,
                        captura=filtrado_seg3$PESO,
                        esfuerzo=filtrado_seg3$LONGITUD_RED,
                        f_year=filtrado_seg3$f_year,
                        f_month=filtrado_seg3$f_month, 
                        trimestre=as.factor(filtrado_seg3$trimestre),
                        trimestre_Q=as.factor(filtrado_seg3$trimestre_Q),
                        COD_BARCO=filtrado_seg3$COD_BARCO, 
                        LATITUD=filtrado_seg3$LATITUD, 
                        LONGITUD=filtrado_seg3$LONGITUD,
                        PUERTO_RECALADA=filtrado_seg3$PUERTO_RECALADA)

# Filtro de valores finitos (hay uno que se va a infinito)

dim(logcpuedata)

logcpuedata<-logcpuedata %>% 
  filter_at(vars(names(logcpuedata)[1:4]), all_vars(!is.infinite(.)))

logcpuedata_seg<-logcpuedata_seg %>% 
  filter_at(vars(names(logcpuedata_seg)[1:4]), all_vars(!is.infinite(.)))

dim(logcpuedata)
dim(logcpuedata_seg)

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

#

logcpue_hr_seg<-ggplot(logcpuedata_seg, aes(x=logcpue_hr, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_hr")


logcpue_ml_seg<-ggplot(logcpuedata_seg, aes(x=logcpue_ml, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_ml")


logcpue_comb_seg<-ggplot(logcpuedata_seg, aes(x=logcpue_comb, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Log_peso vs Log cpue_comb")

nominal_cap_cpue<-ggarrange(ggarrange(logcpue_hr, logcpue_ml, logcpue_comb,
                                  ncol=1, nrow=3),
                            ggarrange(logcpue_hr_seg, logcpue_ml_seg, logcpue_comb_seg,
                                  ncol=1, nrow=3), ncol = 2)

annotate_figure(nominal_cap_cpue, top  = text_grob("Filtros propios y seguimiento", 
                                               color = "blue", face = "bold", size = 14))

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
logcpuedata_seg_naves<-left_join(x=logcpuedata_seg, y=Maestro_naves, by ="COD_BARCO") 

logcpuedata_naves_count<-logcpuedata_naves %>%
  group_by(f_year)  %>%
  summarise_all(funs(sum(!is.na(.)))) 

logcpuedata_seg_naves_count<-logcpuedata_seg_naves %>%
  group_by(f_year)  %>%
  summarise_all(funs(sum(!is.na(.))))  


formattable(logcpuedata_naves_count, format="markdown", caption = "Data por columna por año",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

formattable(logcpuedata_seg_naves_count, format="markdown", caption = "Data por columna por año",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

###############
# Exploración de relación entre CPUE y variables

names(logcpuedata_naves)

boxplot(log(cpue_ml)~f_month, data=logcpuedata_naves)

boxplot(cpue_ml~trimestre_Q, data=logcpuedata_naves, las=2)
boxplot(cpue_ml~trimestre_Q, data=logcpuedata_seg_naves, las=2)

#table(reineta_2022$f_month)

k1<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=trimestre, fill=trimestre_Q))+
  geom_boxplot(alpha=0.5, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "black")+
  theme(axis.text.x = element_text(angle = 90))

k2<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_year, fill=f_year)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

k3<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_month, fill=f_month)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

kk1<-ggplot(logcpuedata_seg_naves, aes(y=cpue_ml, x=trimestre, fill=trimestre_Q))+
  geom_boxplot(alpha=0.5, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "black")+
  theme(axis.text.x = element_text(angle = 90))

kk2<-ggplot(logcpuedata_seg_naves, aes(y=cpue_ml, x=f_year, fill=f_year)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

kk3<-ggplot(logcpuedata_seg_naves, aes(y=cpue_ml, x=f_month, fill=f_month)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))

factores_cpue<-ggarrange(ggarrange(k1, kk1, ncol=2, common.legend = TRUE, legend= "none"),
                         ggarrange(k2, kk2, ncol=2, common.legend = TRUE, legend= "none"),
                         ggarrange(k3, kk3, ncol=2, common.legend = TRUE, legend= "none"),
                         nrow=3)

annotate_figure(factores_cpue, top  = text_grob("Filtros propios y seguimiento", 
                                                 color = "blue", face = "bold", size = 14))
#########
# ggplot(logcpuedata_naves, aes(y=logcpue_ml, x=-LATITUD/10000)) +
#   geom_point(alpha=0.3, position="identity")+
#   xlim(c(-75, -30))
# 
# ggplot(logcpuedata_naves, aes(y=logcpue_ml, x=-LONGITUD/10000)) +
#   geom_point(alpha=0.3, position="identity")+
#   xlim(c(-75, -65))

# ggplot(logcpuedata_naves, aes(x=logcpue_ml, color=f_month, fill=f_month)) +
#   geom_histogram(alpha=0.3, position="identity")

#table(reineta_2022$COD_PESQUERIA)
#6 es encuesta
#122 Observadores

#####

# ggplot(logcpuedata_naves, aes(y=cpue_ml, x=trimestre, fill=trimestre_Q))+
#   geom_boxplot(alpha=0.5, position="identity")+
#   geom_jitter(width=0.2, alpha=0.4, color= "black")+
#   theme(axis.text.x = element_text(angle = 90))

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
ss1<-log(max(logcpue_ml_1+1)-logcpue_ml_1, base = 10) #Este me parece mejor para normalizar
ss2<-1/(max(logcpue_ml_1+1) - logcpue_ml_1) 
  

hist(ss0, breaks = 30)
hist(log(ss0), breaks = 30)

qqnorm(ss0)+
  qqline(ss0)

ggplot(ss1, breaks = 30)

qqnorm(ss2)+
  qqline(ss2) 

#
ss1_hist<-ggplot(as.data.frame(ss1), aes(x=ss1)) +
   geom_histogram(position="identity")
ss1_qqnorm<-qqnorm(ss1)+
  qqline(ss1) 
ss1_density<-ggdensity(ss1, ss1 = "", fill = "lightgray", 
          title = "log(max(logcpue_ml_1+1)-logcpue_ml_1, base = 10)") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggarrange(ss1_hist, ss1_qqnorm, ss1_density, ncol=3)
#

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

# https://rpubs.com/aafernandez1976/fitdistrplus


plotdist(logcpue_ml_1, histo = TRUE, demp =T,col="gray")
plotdist(ss1, histo = TRUE, demp =T,col="gray")

descdist(ss1, boot = 100)
descdist(logcpue_ml_1, boot = 100)

logcpuedata$log_cpue_ml_ss1 <- ss1
logcpuedata_naves$log_cpue_ml_ss1 <- ss1


##############################################################################

# numero de observaciones por anho, captura total y esfuerzo
mysum <- aggregate(logcpuedata[,"f_year"],by=list(Year=logcpuedata$f_year),length) # crea un objeto con el numero de observaciones por anho.
names(mysum) <- c("Year","Nobs") # nombra las columnas del objeto
tmp <- aggregate(logcpuedata[,c("captura","esfuerzo")],by=list(Year=logcpuedata$f_year),sum,na.rm=TRUE) # nos da el numero de anzuelos y los kilos de atun por anho.
mysum <- merge(mysum,tmp,by="Year")     # combina los dos objectos por la variable en comun "Year"
mysum
mysum$CPUE=mysum$captura/mysum$esfuerzo*1000 # Calcula la CPUE en una nueva columna
mysum

#Estandarización cpue (Primeros modelos)

glm_mod0<-glm(logcpue_ml ~ + f_year, data= logcpuedata_naves,
              family= gaussian(link="identity"))

glm_mod1<-glm(logcpue_ml ~ + f_year + trimestre_Q,
              data= logcpuedata_naves, family= gaussian(link="identity"))

glm_mod2<-glm(logcpue_ml ~ + f_year + trimestre_Q + COD_BARCO,
              data= logcpuedata_naves, family= gaussian(link="identity"))

glm_mod3<-glm(logcpue_ml ~ + f_year + trimestre_Q + COD_BARCO + f_month,
             data= logcpuedata_naves, family= gaussian(link="identity"))

glm_mod3_seg<-glm(logcpue_ml ~ + f_year + trimestre_Q + COD_BARCO + f_month,
              data= logcpuedata_seg_naves, family= gaussian(link="identity"))

glm_mod4<-glm(logcpue_ml ~ + f_year + trimestre_Q + COD_BARCO + f_month + PUERTO_RECALADA,
              data= logcpuedata_naves, family= gaussian(link="identity"))

summary(glm_mod0)
summary(glm_mod1)
summary(glm_mod2)
summary(glm_mod3)
summary(glm_mod4)

glm_mod0_anova <- anova(glm_mod0,test="Chisq")
glm_mod1_anova <- anova(glm_mod1,test="Chisq")
glm_mod2_anova <- anova(glm_mod2,test="Chisq")
glm_mod3_anova <- anova(glm_mod3,test="Chisq")
glm_mod3_anova_seg <- anova(glm_mod3_seg, test="Chisq")
glm_mod4_anova <- anova(glm_mod4,test="Chisq")

AIC(glm_mod0, glm_mod1, glm_mod2, glm_mod3, glm_mod4)
#

res_glm_mod3<-glm_mod3$residuals

res_glm_mod3_hist<-ggplot(as.data.frame(res_glm_mod3), aes(x=res_glm_mod3)) +
  geom_histogram(position="identity")

res_glm_mod3_qqnorm<-qqnorm(res_glm_mod3)+
  qqline(res_glm_mod3) 

res_glm_mod3_density<-ggdensity(res_glm_mod3, res_glm_mod3 = "", fill = "lightgray", 
                       title = "residuos") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

plot(glm_mod3, 1)
plot(glm_mod3$residuals)

glm_mod3$coefficients
output<-glm_mod3$coefficients[1:nrow(mysum)]

glm_mod3_seg$coefficients
output_seg<-glm_mod3_seg$coefficients[1:nrow(mysum)]

# tabla de desvianzas (ejemplo cuando hay muchos datos) 
DevTable <- as.data.frame(anova(glm_mod3,test="Chisq"))
DevTable$PercDevExp <- 100*(DevTable$Deviance/(max(DevTable[,4])-min(DevTable[,4]))) #calcular el porcentaje de desvianza explicada por cada variabel
row.names(DevTable)[DevTable$PercDevExp >= 5.0]   # cuales son mayores del 5%? 
DevTable
write.table(DevTable,"Tabla de desvianzas.csv",row.names = T,sep=",") #exportar la tabla



output<-glm_mod3$coefficients[1:nrow(mysum)]
Indice_Est<-data.frame(Year=mysum$Year,CPUE_Nominal=mysum$CPUE,Coef=output)
#Indice_Est_seg<-data.frame(Year=mysum$Year,CPUE_Nominal=mysum$CPUE,Coef=output_seg)

Indice_Est$CPUE_est<-exp(c(output[1],output[1]+output[-1])) # recordar que estabamos trabajando en escala log
# ademas lso coeficientes estimados son estimados en relacion al intercepto
Indice_Est_seg$CPUE_est<-exp(c(output_seg[1],output_seg[1]+output_seg[-1])) # recordar que estabamos trabajando en escala log


# plot CPUE nominal y estandarizada (escaladas por la media)
par(mfrow=c(1,1),mai=c(1.1,1.1,.5,.2))

plot(x=Indice_Est$Year, y=Indice_Est$CPUE_Nominal/mean(Indice_Est$CPUE_Nominal), 
     col="red", pch=19, xlab="Year", ylab="Scaled CPUE", ylim=c(0,3), las=2)

lines(x=Indice_Est$Year,y=Indice_Est$CPUE_est/mean(Indice_Est$CPUE_est),lwd=2,col="blue")
lines(x=seq(2005:2021),y=Indice_Est_seg$CPUE_est/mean(Indice_Est_seg$CPUE_est),lwd=2,col="red")

legend("topright",c("nominal", "estandarizada", "estandarizada_seg"), col = c("black","blue", "red"), bty="n",
       text.col = 1, pch = c(16,-1),lty=c(0,1),lwd=c(-1,2.5))  















####

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














######################

############## MAPAS

# Dato latitud impresiso de origen. mejor no usar para filtros

 filtrado_10<-logcpuedata_naves %>%
   filter(lat >= -40 & lat <= -30) %>%
   filter(lon >= -80 & lon <= -70)

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

#write.csv(file="logcpuedata_naves.csv", logcpuedata_naves, row.names = F)

ggplot(data = Chile) +
  geom_sf()+
  coord_sf(xlim = c(-80, -70), ylim = c(-40, -30))+
  geom_point(data = filtrado_10,
             mapping = aes(x = lon, y = lat), colour = "red")+
  facet_wrap(~f_year, ncol = 6)
