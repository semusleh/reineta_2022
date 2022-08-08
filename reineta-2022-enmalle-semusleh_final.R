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
library(jtools) #comparación de modelos
library(AICcmodavg) #tabla comparación de modelos


setwd("C:/Google Drive-DER/Working folders/project reineta_2022/data reineta_2022")

reineta_2022<-read_excel("Bitacoras enmalle - Artesanal hist.xlsx")
Maestro_naves<-read_excel("Maestro naves.xlsx")

reineta_2022$year<-format(as.Date(reineta_2022$FECHA_LANCE, format="%d/%m/%Y"),"%Y")
reineta_2022$month<-format(as.Date(reineta_2022$FECHA_LANCE, format="%d/%m/%Y"),"%m")

reineta_2022$Hora_reposo<-round(reineta_2022$HORA_DE_REPOSO/100+(
  as.numeric(str_sub(reineta_2022$HORA_DE_REPOSO,-2,-1))/60),2)

reineta_2022$f_year<-as.factor(reineta_2022$year)
reineta_2022$f_month<-as.factor(reineta_2022$month)

reineta_2022$cpue_ml<-(reineta_2022$PESO)/(reineta_2022$LONGITUD_RED)

reineta_2022$trimestre <- zoo::as.yearqtr(reineta_2022$FECHA_LANCE,   # Convert dates to quarterly
                                          format = "%Y-%m-%d")

reineta_2022$trimestre_Q <- as.factor(str_sub(reineta_2022$trimestre,-2,-1)) #Trimestre sin año

reineta_2022$COD_BARCO<-as.factor(reineta_2022$COD_BARCO)

reineta_2022$cpue_hr<-reineta_2022$PESO/reineta_2022$Hora_reposo
reineta_2022$cpue_ml<-(reineta_2022$PESO)/(reineta_2022$LONGITUD_RED)


dim(reineta_2022)

#complementación de datos de profundidad

table(is.na(reineta_2022$PROFUNDIDAD_LM_ENM))
table(is.na(reineta_2022$PROFUNDIDAD))

reineta_2022$prof<-NA
for(i in 1:dim(reineta_2022)[1]){
  if (is.na(reineta_2022$PROFUNDIDAD_LM_ENM)[i]){
    reineta_2022$prof[i]<-reineta_2022$PROFUNDIDAD[i]}
  else {reineta_2022$prof[i]<-reineta_2022$PROFUNDIDAD_LM_ENM[i]}
}

reineta_2022_yr<-reineta_2022 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") %>%
  group_by(f_year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, f_year)%>%
  summarise_all(funs(sum(!is.na(.))))

reineta_2022_yr <- reineta_2022 %>% 
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") %>%
  group_by(f_year) %>% #filter(COD_PESQUERIA %in% c(5,92)) %>%
  select(PESO, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, f_year, year) %>%
  summarise_all(funs(sum(!is.na(.)))) 


reineta_2022_yr<-reineta_2022 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, year)%>%
  summarise_all(funs(sum(!is.na(.))))  

reineta_2022_yr_mean<-reineta_2022 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") %>%
  group_by(year)  %>%
  select(PESO, Hora_reposo, LONGITUD_RED, TAMANIO_MALLA, NUMERO_PANIOS, prof, year)%>%
  summarise_all(funs(mean(., na.rm = TRUE)))  

format_table(reineta_2022_yr, format="markdown",
             align =c("c","c","c", "c", "c"), digits=3,
             list(`Indicator Name` = formatter(
               "span", style = ~ style(color = "grey",font.weight = "bold")) 
             ))

q1<-ggplot(data=reineta_2022_yr_mean, aes(y=PESO/1000, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Captura en peso  sin filtro")

q2<-ggplot(data=reineta_2022_yr_mean, aes(y=LONGITUD_RED, x=year, group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Esfuerzo en LONGITUD_RED sin filtro")

ggarrange(q1, q2,
          ncol=1, nrow=2)

#Ejemplo variabilidad Esfuerzo por COD_BARCO

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

reineta_2022_cod27<-reineta_2022 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") 

dim(reineta_2022_cod27)

#capturas PESO
hist(reineta_2022_cod27$PESO, breaks = 40, 
     main = "Histograma Peso captura", sub="Línea roja = 19000 kg",
     xlab = "PESO", ylab="Frecuencia")
abline(v=19000, col="red", lty=2)

filtrado_1<-subset(reineta_2022_cod27, reineta_2022_cod27$PESO <= 19000)

#esfuerzo LONGITUD_RED
hist(filtrado_1$LONGITUD_RED, xlim = c(0, 4000), breaks = 1000,
     main = "Histograma Esfuerzo en LONGITUD_RED", sub="Rojo = 300-3600",
     xlab = "LONGITUD_RED", ylab="Frecuencia")
abline(v=3600, col="red", lty=2)
abline(v=300, col="red", lty=2)

filtrado_2<-subset(filtrado_1, filtrado_1$LONGITUD_RED <= 3600) 
filtrado_3<-subset(filtrado_2, filtrado_2$LONGITUD_RED > 300) 

#profundidad prof
hist(filtrado_3$prof, xlim = c(0, 100), breaks = 100000,
     main = "Histograma Profundiad", sub="Azúl = 50",
     xlab = "Profundidad", ylab="Frecuencia")
abline(v=50, col="blue", lty=2)

filtrado_4<-subset(filtrado_3, filtrado_3$prof <= 50) #50 m

#Trimestre de la operación

table(filtrado_4$trimestre_Q)

barplot(table(filtrado_4$trimestre_Q), main="Registros por trimestre (filtro 50m prof)",
        las=1)

barplot(table(filtrado_4$f_year), main="Registros por año (filtro 50m prof)",
        las=2)

barplot(table(filtrado_3$f_year), main="Registros por año",
        las=2)

#Año de la operación
#Quitar 2004 porque hay un sólo dato y 2022 por información incompleta

table(filtrado_4$f_year)
filtrado_5<-subset(filtrado_4, filtrado_4$year != 2004 &
                   filtrado_4$year != 2022) #50 m

table(filtrado_5$f_year)

## Reporte filtrado

print(paste("Base de datos inicial=",dim(reineta_2022_cod27)[1]))
print(paste("Filtro PESO <= 19000",dim(filtrado_1)[1]))
print(paste("Filtro LONGITUD_RED <= 3600 m=",dim(filtrado_2)[1]))
print(paste("Filtro LONGITUD_RED <= 300 m=",dim(filtrado_3)[1]))
print(paste("Filtro prof <= 50 m=",dim(filtrado_4)[1]))
print(paste("Filtro f_year != c(2004, 2022)",dim(filtrado_5)[1]))

filtrado_4_table<-filtrado_4 %>%
  filter(COD_ESPECIE == "27" & ESPECIE_OBJETIVO == "27") %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, prof, year)%>%
  summarise_all(funs(sum(!is.na(.))))  

format_table(filtrado_4_table, format="markdown",
             align =c("c","c","c", "c", "c"), digits=3,
             list(`Indicator Name` = formatter(
               "span", style = ~ style(color = "grey",font.weight = "bold")) 
             ))


#######################
# Hist post filtros

h1<-ggplot(filtrado_5, aes(cpue_ml)) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(cpue_ml)), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist CPUE", x = "CPUE Kg/m")+
  theme_bw()

h2<-ggplot(filtrado_5, aes(log(cpue_ml))) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(log(cpue_ml))), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist log(CPUE)", x = "log(CPUE) Kg/m")+
  theme_bw()

h3<-ggplot(filtrado_5, aes(PESO)) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(PESO)), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist captura", x = "captura en Kg")+
  theme_bw()

h4<-ggplot(filtrado_5, aes(log(PESO))) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(log(PESO))), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist log(captura)", x = "Log(captura) en Kg")+
  theme_bw()

h5<-ggplot(filtrado_5, aes(LONGITUD_RED)) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(LONGITUD_RED)), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist Esfuerzo en Longitud de red", x = "Longitud de red en m")+
  theme_bw()

h6<-ggplot(filtrado_5, aes(log(LONGITUD_RED))) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(log(LONGITUD_RED))), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist log(Esfuerzo) en Longitud de red", x = "Log(Longitud) de red en m")+
  theme_bw()

ggarrange(ggarrange(h1, h2, ncol=2, labels = c("A", "B")),
          ggarrange(h3, h4, ncol=2, labels = c("C", "D")),
          ggarrange(h5, h6, ncol=2, labels = c("E", "F")),
          nrow = 3)

ggsave("./plots/hitogramas_post flitros.png", width = 25, height = 25,
       units = c("cm"), dpi = 300)


reineta_2022_yr_postf<-filtrado_5 %>%
  group_by(year)  %>%
  select(PESO, LONGITUD_RED, cpue_ml, prof, year, month)%>%
  summarise_all(funs(sum(!is.na(.))))  

formattable(reineta_2022_yr_postf, format="markdown", 
            caption = "Data por columna por año filtros propios",
            align =c("c","c","c", "c", "c"), digits=3,
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

temp<-filtrado_5

# TABLAS
# ------------ agrupamos lesfuerzo por ano ----------------
effort <- temp %>% group_by(f_year) %>% #filter(COD_PESQUERIA %in% c(5,92)) %>%
  summarise (effort = mean(LONGITUD_RED, na.rm = TRUE),
             effortsd = sd(LONGITUD_RED, na.rm = TRUE),
             effortn = n(),
             efforte = qnorm(0.975)*effortsd/sqrt(effortn),
             effortl = effort - efforte,
             effortr = effort + efforte)
effort
#write.csv(esfuerzo, file = "tablas/dfp.csv", row.names = T)

# ------------ captura promedio por a?o  ----------------
ncatch <- temp %>% group_by(f_year) %>% 
  summarise (catch = mean(PESO, na.rm = TRUE),
             catchsd = sd(PESO, na.rm = TRUE),
             catchn = n(),
             catche = qnorm(0.975)*catchsd/sqrt(catchn),
             catchl = catch - catche,
             catchr = catch + catche)
ncatch
#write.csv(ncatch, file = "tablas/dfp.csv", row.names = T)

cpue_nominal <- temp %>% group_by(f_year) %>% 
  summarise (captura = mean(PESO, na.rm = TRUE),
             esfuerzo = mean(LONGITUD_RED, na.rm = TRUE),
             cpue = captura/esfuerzo,
             cpuesd = sd(PESO/LONGITUD_RED, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

cpue_nominal
#write.csv(cpue_nominal, file = "tablas/cpue_nominal.csv", row.names = F)

p1 <- ggplot(effort, aes(f_year, group=1)) +
  geom_point(aes(y=effort, size=effortn), colour="blue", alpha=0.5, shape=19) +
  geom_line(aes(y=effort), colour="blue") + 
  geom_ribbon(aes(ymin=effortl, ymax=effortr), alpha=0.2) +
  theme_bw() + 
  theme( axis.text=element_text(size=9)) +
  theme(axis.title.x=element_blank()) + 
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 15, unit = "pt"))+
  ylab('Esfuerzo en metro de red (m)')

p2 <- ggplot(ncatch, aes(f_year, group=1)) +  
  geom_point(aes(y=catch, size=catchn), colour="blue", alpha=0.5, shape=19) +
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) +
  theme_bw() + 
  theme( axis.text=element_text(size=9)) +
  theme(axis.title.x=element_blank()) + 
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 15, unit = "pt"))+
  ylab('Captura (Kg)') 

p3 <- ggplot(cpue_nominal, aes(f_year, group=1)) +  
  geom_point(aes(y=cpue, size=cpuen), colour="blue", alpha=0.5, shape=19) +
  geom_line(aes(y=cpue), colour="blue") + 
  geom_ribbon(aes(ymin=cpuel, ymax=cpuer), alpha=0.2)+
  theme_bw() + 
  theme( axis.text=element_text(size=9)) +
  theme(axis.title.x=element_blank()) + 
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 15, unit = "pt"))+
  ylab('Rendimiento (Kg/m)') + xlab('Años') 

ggarrange(p1, p2, p3, ncol=1, labels = c("A", "B", "C"))
ggsave("./plots/cap-esf-cpue.png", width = 20, height = 20,
       units = c("cm"), dpi = 300)

####
#Tabla para estandarización de cpue


logcpuedata<-data.frame(logpeso=log(filtrado_5$PESO),
                        logcpue_ml=log(filtrado_5$cpue_ml),
                        cpue_ml=filtrado_5$cpue_ml,
                        logcpue_hr=log(filtrado_5$cpue_hr),
                        cpue_hr=filtrado_5$cpue_hr,
                        cpue_comb=filtrado_5$PESO/(filtrado_5$Hora_reposo*filtrado_5$LONGITUD_RED),
                        logcpue_comb=log(filtrado_5$PESO/(filtrado_5$Hora_reposo*filtrado_5$LONGITUD_RED)),
                        captura=filtrado_5$PESO,
                        esfuerzo=filtrado_5$LONGITUD_RED,
                        esfuerzo_hr=filtrado_5$Hora_reposo,
                        f_year=filtrado_5$f_year,
                        f_month=filtrado_5$f_month, 
                        trimestre=as.factor(filtrado_5$trimestre),
                        trimestre_Q=as.factor(filtrado_5$trimestre_Q),
                        COD_BARCO=filtrado_5$COD_BARCO, 
                        LATITUD=filtrado_5$LATITUD, 
                        LONGITUD=filtrado_5$LONGITUD,
                        PUERTO_RECALADA=filtrado_5$PUERTO_RECALADA)

# Filtro de valores finitos (hay uno que se va a infinito)
# dim(logcpuedata)
# 
# logcpuedata<-logcpuedata %>% 
#   filter_at(vars(names(logcpuedata)[1:4]), all_vars(!is.infinite(.)))

logcpue_ml<-ggplot(logcpuedata, aes(x=logcpue_ml, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Captura vs CPUE en longitud de red")+
  ylab("Log(Captura)")+
  xlab("Log(CPUE longitud de red)")+
  theme_bw()

logcpue_hr<-ggplot(logcpuedata, aes(x=logcpue_hr, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Captura vs CPUE en tiempo de reposo")+
  ylab("Log(Captura)")+
  xlab("Log(CPUE tiempo de reposo)")+
  theme_bw()

logcpue_comb<-ggplot(logcpuedata, aes(x=logcpue_comb, y=logpeso))+
  geom_point()+
  geom_smooth(method="lm", col="blue")+
  stat_regline_equation(label.y = 7.5)+
  stat_cor(label.y = 8.5)+
  ggtitle("Captura vs CPUE combinando tiempo de reposo y longitud de red")+
  ylab("Log(Captura)")+
  xlab("Log(CPUE tiempo de reposo * longitud de red)")+
  theme_bw()

comparaciones_cpue<-ggarrange(logcpue_ml, logcpue_hr, logcpue_comb,
                                      ncol=1, nrow=3, labels = c("A", "B", "C"))

ggsave("./plots/comparaciones_cpue.png", width = 20, height = 25,
       units = c("cm"), dpi = 300)


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

# Exploración de relación entre CPUE y variables

logcpuedata_naves$f_PUERTO_RECALADA<-as.factor(logcpuedata_naves$PUERTO_RECALADA)

boxplot(cpue_ml~f_PUERTO_RECALADA, data=logcpuedata_naves)

names(logcpuedata_naves)

boxplot(log(cpue_ml)~f_month, data=logcpuedata_naves)

boxplot(cpue_ml~trimestre_Q, data=logcpuedata_naves, las=2)

#table(reineta_2022$f_month)

k1<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=trimestre))+
  geom_boxplot(alpha=0.5, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("CPUE")+
  xlab("Trimestres por año")

k2<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_year)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme(axis.text.x = element_text(angle = 90))+
  theme_bw()+
  ylab("CPUE")+
  xlab("Años")

k3<-ggplot(logcpuedata_naves, aes(y=cpue_ml, x=f_month)) +
  geom_boxplot(alpha=0.3, position="identity")+
  geom_jitter(width=0.2, alpha=0.4, color= "grey")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("CPUE")+
  xlab("Meses")

factores_cpue<-ggarrange(k3, k2, k1, ncol=1, nrow=3,
                         common.legend = TRUE, legend= "none",
                         labels = c("A", "B", "C"))

# annotate_figure(factores_cpue, top  = text_grob("Factores post filtros", 
#                                                 color = "black", face = "bold", size = 14))

ggsave("./plots/cpue_factores.png", width = 20, height = 20,
       units = c("cm"), dpi = 300)


###############
# Normalización de CPUE

# https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

h7<-ggplot(filtrado_5, aes(log(LONGITUD_RED))) +
  geom_histogram(position="identity", bins = 60, color = "black", fill = "gray")+
  geom_vline(aes(xintercept = mean(log(LONGITUD_RED))), 
             linetype = "dashed", size = 0.6)+
  labs(title = "Hist log(Esfuerzo) en Longitud de red", x = "Log(Longitud) de red en m")

hist(logcpuedata$cpue_ml, breaks = 30)

qqnorm(logcpuedata$logcpue_ml)+
  qqline(logcpuedata$logcpue_ml) 

logcpue_ml_1<-logcpuedata$logcpue_ml
cpue_ml_1<-logcpuedata$cpue_ml

moments::skewness(cpue_ml_1, na.rm = TRUE)
moments::skewness(logcpue_ml_1, na.rm = TRUE)

ss0<-sqrt(cpue_ml_1)
ss1_log<-log(cpue_ml_1)
ss1_asd<-log10(max(cpue_ml_1) - cpue_ml_1)
ss1<-log(max(logcpue_ml_1+1)-logcpue_ml_1, base = 10) #Este me parece mejor para normalizar
ss2<-1/(max(cpue_ml_1+1) - cpue_ml_1) 

ss3<-log(log(cpue_ml_1+1))

hist(ss1)
hist(ss1_asd, breaks=50)

hist(ss0, breaks = 30)

qqnorm(ss0)+
  qqline(ss0) 

hist(log(ss0), breaks = 30)

hist(ss2, breaks = 50)

qqnorm(ss0)+
  qqline(ss0)

ggplot(ss1, breaks = 30)

qqnorm(ss2)+
  qqline(ss2) 


hist(ss3, breaks = 50)

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
shapiro.test(ss3)

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
mysum <- aggregate(logcpuedata[,"f_year"],by=list(Year=logcpuedata$f_year),length) 
# crea un objeto con el numero de observaciones por anho.

names(mysum) <- c("Year","Nobs") # nombra las columnas del objeto

tmp <- aggregate(logcpuedata[,c("captura","esfuerzo")],by=list(Year=logcpuedata$f_year),sum,na.rm=TRUE) 
# nos da el numero de anzuelos y los kilos de atun por anho.

mysum <- merge(mysum,tmp,by="Year")     
# combina los dos objectos por la variable en comun "Year"

mysum
mysum$CPUE=mysum$captura/mysum$esfuerzo*1000 # Calcula la CPUE en una nueva columna
mysum

#Estandarización cpue (Primeros modelos)

glm_mod0<-glm(cpue_ml ~ + f_year, data= logcpuedata_naves,
              family= gaussian(link="log"))

glm_mod1<-glm(cpue_ml ~ + f_year + trimestre_Q,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_mod2<-glm(cpue_ml ~ + f_year + trimestre_Q + COD_BARCO,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_mod3<-glm(cpue_ml ~ + f_year + trimestre_Q + COD_BARCO + f_month,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_mod4<-glm(cpue_ml ~ + f_year + trimestre_Q + COD_BARCO + f_month + f_PUERTO_RECALADA,
              data= logcpuedata_naves, family= gaussian(link="log"))


##

glm_m0<-glm(cpue_ml ~ + f_year, data= logcpuedata_naves,
              family= gaussian(link="log"))

glm_m1<-glm(cpue_ml ~ + f_year + trimestre_Q,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_m2<-glm(cpue_ml ~ + f_year + trimestre_Q + f_month,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_m3<-glm(cpue_ml ~ + f_year + trimestre_Q + f_month + f_PUERTO_RECALADA,
              data= logcpuedata_naves, family= gaussian(link="log"))

glm_m4<-glm(cpue_ml ~ + f_year + trimestre_Q + f_month + f_PUERTO_RECALADA + COD_BARCO,
            data= logcpuedata_naves, family= gaussian(link="log"))

glm_m5<-glm(cpue_ml ~ + f_year + f_month + f_PUERTO_RECALADA,
            data= logcpuedata_naves, family= gaussian(link="log"))


drop1(glm_m4)

##

full.model <- glm_m4
# Stepwise regression model
step.model <- MASS::stepAIC(glm_m4, direction = "both", 
                      trace = FALSE)
summary(step.model)

anova(step.model)

##



resids = residuals(glm_m4, type="partial")
plot( resids)

plot(fitted(glm_m4), residuals(glm_m4, type="pearson"))

drop1(glm_m4, test = 'Chisq')

summary(glm_m0)

glm_m0_anova <- anova(glm_m0,test="Chisq")
glm_m1_anova <- anova(glm_m1,test="Chisq")
glm_m2_anova <- anova(glm_m2,test="Chisq")
glm_m3_anova <- anova(glm_m3,test="Chisq")
glm_m4_anova <- anova(glm_m4,test="Chisq")
glm_m5_anova <- anova(glm_m5,test="Chisq")

100*with(summary(glm_m0), 1 - deviance/null.deviance); formula(glm_m0)
100*with(summary(glm_m1), 1 - deviance/null.deviance); formula(glm_m1)
100*with(summary(glm_m2), 1 - deviance/null.deviance); formula(glm_m2)
100*with(summary(glm_m3), 1 - deviance/null.deviance); formula(glm_m3)
100*with(summary(glm_m4), 1 - deviance/null.deviance); formula(glm_m4)
100*with(summary(glm_m5), 1 - deviance/null.deviance); formula(glm_m5)

AIC(glm_m0, glm_m1, glm_m2, glm_m3, glm_m4, glm_m5)


modelos<-list(glm_m0, glm_m1, glm_m2, glm_m3, glm_m4, glm_m5)
nombres<-c("glm_m0", "glm_m1", "glm_m2", "glm_m3", "glm_m4", "glm_m5")

mod_comp<-AICcmodavg::aictab(cand.set = modelos, modnames= nombres, sort  = F)

flextable(format(mod_comp, digits = 3, na.encode = FALSE))


#

res_glm<-glm_m4$residuals

res_hist<-ggplot(as.data.frame(res_glm), aes(x=res_glm)) +
  geom_histogram(position="identity", bins = 20)+
  theme_bw()+
  xlab("Residuos")+
  ylab("Frecuencia")

res_qqnorm<-qqnorm(res_glm)+
  qqline(res_glm) 

res_density<-ggdensity(res_glm, res_glm = "", fill = "lightgray", 
                                title = "") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")+
  xlab("Residuos")+
  ylab("Densidad")

res_points<-ggplot(as.data.frame(res_glm), 
                            aes(y=res_glm, x=(1:length(res_glm)))) +
  geom_point()+
  xlab("")+
  ylab("Residuos")

ggarrange(res_hist, res_qqnorm, res_density, res_points,
          labels = c("A", "B", "C", "D"))

ggsave("./plots/residuos_m4.png", width = 20, height = 15,
       units = c("cm"), dpi = 300)


glm_mod3gama$coefficients
output_glm3_gama<-glm_mod3gama$coefficients[1:nrow(mysum)]

#Diagnóstico modelo lineal

win.graph()         
par(mfrow=c(4,3))
termplot(glm_m5, se = TRUE,ylim = "free")
plot(glm_m5)


# numero de observaciones por anho, captura total y esfuerzo
mysum <- aggregate(logcpuedata[,"f_year"],by=list(Year=logcpuedata$f_year),length) # crea un objeto con el numero de observaciones por anho.
names(mysum) <- c("Year","Nobs") # nombra las columnas del objeto
tmp <- aggregate(logcpuedata[,c("captura","esfuerzo")],by=list(Year=logcpuedata$f_year),sum,na.rm=TRUE) # nos da el numero de anzuelos y los kilos de atun por anho.
mysum <- merge(mysum,tmp,by="Year")     # combina los dos objectos por la variable en comun "Year"
mysum
mysum$CPUE=mysum$captura/mysum$esfuerzo*1000 # Calcula la CPUE en una nueva columna
mysum


# tabla de desvianzas (ejemplo cuando hay muchos datos) 
DevTable <- as.data.frame(anova(glm_m4,test="Chisq"))
DevTable$PercDevExp <- 100*(DevTable$Deviance/(max(DevTable[,4])-min(DevTable[,4]))) #calcular el porcentaje de desvianza explicada por cada variabel
row.names(DevTable)[DevTable$PercDevExp >= 5.0]   # cuales son mayores del 5%? 
DevTable
#write.table(DevTable,"Tabla de desvianzas.csv",row.names = T,sep=",") #exportar la tabla

output<-glm_m4$coefficients[1:nrow(mysum)]

Indice_Est<-data.frame(Year=mysum$Year,CPUE_Nominal=mysum$CPUE,Coef=output)

Indice_Est$CPUE_est<-exp(c(output[1],output[1]+output[-1])) # recordar que estabamos trabajando en escala log
# ademas lso coeficientes estimados son estimados en relacion al intercepto


# plot CPUE nominal y estandarizada (escaladas por la media)
par(mfrow=c(1,1),mai=c(1.1,1.1,.5,.2))

plot(x=Indice_Est$Year, y=Indice_Est$CPUE_Nominal/mean(Indice_Est$CPUE_Nominal), 
     col="red", pch=19, xlab="Year", ylab="Scaled CPUE", ylim=c(0,3), las=2)

lines(x=Indice_Est$Year,y=Indice_Est$CPUE_est/mean(Indice_Est$CPUE_est),lwd=2,col="blue")

legend("topright",c("nominal", "estandarizada"), col = c("black","blue"), bty="n",
       text.col = 1, pch = c(16,-1),lty=c(0,1),lwd=c(-1,2.5))  



# ------------  Extrayendo el efecto anual ----------------

mysum <- aggregate(logcpuedata[,"f_year"],by=list(Year=logcpuedata$f_year),length) # crea un objeto con el numero de observaciones por anho.
names(mysum) <- c("Year","Nobs") # nombra las columnas del objeto
tmp <- aggregate(logcpuedata[,c("captura","esfuerzo")],by=list(Year=logcpuedata$f_year),sum,na.rm=TRUE) # nos da el numero de anzuelos y los kilos de atun por anho.
mysum <- merge(mysum,tmp,by="Year")     # combina los dos objectos por la variable en comun "Year"
mysum
mysum$CPUE=mysum$captura/mysum$esfuerzo*1000 # Calcula la CPUE en una nueva columna
mysum


# seleccionar solo el intercepto  y el resto de los a?os

# mod_reineta<-glm_mod3gama
# names(mod_reineta)
# output <- mod_reineta$coefficients[1:nrow(mysum)]

output_m3 <- glm_m3$coefficients[1:nrow(mysum)]
output_m4 <- glm_m4$coefficients[1:nrow(mysum)]

#Crear data frame con los resultados 
Index_frame <- data.frame(Year=mysum$Year,CPUE_Nominal=mysum$CPUE, 
                          Coef_m3=output_m3, Coef_m4=output_m4, Coef_m5=output_m5)

Index_frame$CPUE_std_m3 <- exp(c(output_m3[1],output_m3[1]+output_m3[-1])) # recordar que estabamos trabajando en escala log
# ademas los coeficientes estimados son estimados en relacion al intercepto
Index_frame$CPUE_std_m4 <- exp(c(output_m4[1],output_m4[1]+output_m4[-1]))


Index_frame$Year <- mysum$Year
Index_frame$Index_std_m3 <- Index_frame$CPUE_std_m3/mean(Index_frame$CPUE_std_m3)
Index_frame$Index_std_m4 <- Index_frame$CPUE_std_m4/mean(Index_frame$CPUE_std_m4)


Index_frame$Index_nom <- Index_frame$CPUE_Nominal/mean(Index_frame$CPUE_Nominal)
#Index_frame$Std_Error <- sqrt(diag(vcov(mod_dalc)))[1:nrow(mysum)]


# ------------  Grafico cpue en ggplot  ----------------

# Plot Absoluto
# p11 <- ggplot(data = Index_frame, aes(x = Year, group=1)) + 
#   geom_line(aes(y = CPUE_std_m3, colour = 'CPUE std', linetype = 'CPUE std')) +
#   geom_line(aes(y = CPUE_std_m4, colour = 'CPUE std', linetype = 'CPUE std')) +
#   geom_line(aes(y = CPUE_std_m5, colour = 'CPUE std', linetype = 'CPUE std')) +
#   
#   geom_line(aes(y = CPUE_Nominal, colour = 'CPUE nom', linetype = 'CPUE nom')) +
#   
#   scale_color_manual(name = '',
#                      values = c('black', 'black'),
#                      limits = c('CPUE std', 'CPUE nom'),
#                      breaks = c('CPUE std', 'CPUE nom')) +
#   
#   
#   scale_linetype_manual(name = '',
#                         values = c('solid', 'longdash'),
#                         limits = c('CPUE std', 'CPUE nom'),
#                         breaks = c('CPUE std', 'CPUE nom'))


# Plot Relativo
p12 <- ggplot(data = Index_frame, aes(x = Year, group=1)) + 
  geom_line(aes(y = Index_std_m3, colour = 'Indice estand m3', linetype = 'Indice estand m3'),
            size=1) +
  geom_line(aes(y = Index_std_m4, colour = 'Indice estand m4', linetype = 'Indice estand m4'),
            size=1) +
  geom_line(aes(y = Index_nom, colour = 'Indice nominal', linetype = 'Indice nominal'),
           size=1) +

  scale_color_manual(name="",labels  = c('Indice estandarizado modelo 3',
                              'Indice estandarizado modelo 4',
                              'Indice nominal'), 
                     values = c('blue', 'red', 'black')) +

  scale_linetype_manual(name="", labels  = "",
                        values = c('solid', 'solid', 'longdash'), guide=FALSE)+

  theme_bw() + 
  theme(legend.position = 'bottom') + ylab('Indice Relativo') + xlab('Años') 

p12


ggsave("./plots/cpue_std.png", width = 20, height = 10,
       units = c("cm"), dpi = 300)


#Exportaciónde CPUE estandarizada y normalizada

write.csv(Index_frame, file= "Tabla_indices_Reineta.csv")









# Gráfico antiguo


# numero de observaciones por anho, captura total y esfuerzo
# mysum <- aggregate(logcpuedata[,"f_year"],by=list(Year=logcpuedata$f_year),length) # crea un objeto con el numero de observaciones por anho.
# names(mysum) <- c("Year","Nobs") # nombra las columnas del objeto
# tmp <- aggregate(logcpuedata[,c("captura","esfuerzo")],by=list(Year=logcpuedata$f_year),sum,na.rm=TRUE) # nos da el numero de anzuelos y los kilos de atun por anho.
# mysum <- merge(mysum,tmp,by="Year")     # combina los dos objectos por la variable en comun "Year"
# mysum
# mysum$CPUE=mysum$captura/mysum$esfuerzo*1000 # Calcula la CPUE en una nueva columna
# mysum
# 
# 
# # tabla de desvianzas (ejemplo cuando hay muchos datos)
# DevTable <- as.data.frame(anova(glm_mod3gama,test="Chisq"))
# DevTable$PercDevExp <- 100*(DevTable$Deviance/(max(DevTable[,4])-min(DevTable[,4]))) #calcular el porcentaje de desvianza explicada por cada variabel
# row.names(DevTable)[DevTable$PercDevExp >= 5.0]   # cuales son mayores del 5%?
# DevTable
# #write.table(DevTable,"Tabla de desvianzas.csv",row.names = T,sep=",") #exportar la tabla
# 
# output<-glm_mod3$coefficients[1:nrow(mysum)]
# output_gama<-glm_mod3$coefficients[1:nrow(mysum)]
# 
# Indice_Est<-data.frame(Year=mysum$Year,CPUE_Nominal=mysum$CPUE,Coef=output)
# 
# Indice_Est$CPUE_est<-exp(c(output[1],output[1]+output[-1])) # recordar que estabamos trabajando en escala log
# # ademas lso coeficientes estimados son estimados en relacion al intercepto
# 
# 
# # plot CPUE nominal y estandarizada (escaladas por la media)
# par(mfrow=c(1,1),mai=c(1.1,1.1,.5,.2))
# 
# plot(x=Indice_Est$Year, y=Indice_Est$CPUE_Nominal/mean(Indice_Est$CPUE_Nominal),
#      col="red", pch=19, xlab="Year", ylab="Scaled CPUE", ylim=c(0,3), las=2)
# 
# lines(x=Indice_Est$Year,y=Indice_Est$CPUE_est/mean(Indice_Est$CPUE_est),lwd=2,col="blue")
# 
# legend("topright",c("nominal", "estandarizada"), col = c("black","blue"), bty="n",
#        text.col = 1, pch = c(16,-1),lty=c(0,1),lwd=c(-1,2.5))


## TABLAS

# 
# flextable::flextable(round(glm_m1_anova,2))
# library(flextable)
# library(magrittr)
# 
# numbers <- c(500, 1.4, 1999999, 0.3333, 0.00001)
# dataset <- data.frame(
#   id = c(1:5),
#   numbers = numbers)
# 
# 
# regulartable(dataset)
# 
# eeldren_ft <- function(x){
#   z <- format(x, digits = NULL, na.encode = FALSE)
#   z <- flextable(z)
#   autofit(z)
#   
#   
# }
# 
# 
# # 
# # 
# # 
# eeldren_ft(mod_comp)
# 
# 
# p_val_format <- function(x){
#   z <- scales::pvalue_format()(x)
#   z[!is.finite(x)] <- ""
#   z
# }
# 
# library(flextable)
# library(broom)
# library(magrittr)
# 
# 
# model_logit <- glm(response ~ trt + grade, trial, family = binomial)
# broom::tidy(model_logit)
# 
# df<-tidy(glm_m0)
# 
# mod <- anova(glm_m0, glm_m1, glm_mod2, glm_m3)
# 
# # summarize model fit with tidiers
# df<-tidy(mod)
# glance(mod)
# 
# 
# flextable(df) %>% 
#   set_formatter(values = list("p.value" = p_val_format) ) %>% 
#   colformat_int("df", suffix = " dof") %>% 
#   # more cosmetics
#   set_header_labels(term = "Term", df = "",
#                     "sumsq" = "Sum of squares", 
#                     meansq = "Mean squared error", 
#                     statistic = "Statistic", p.value = "p-value"
#   ) %>% 
#   autofit()
# 
# 
# 
# library("texreg")
# 
# texreg(glm_m0_anova)
# 
# summ(glm_m0)
# plot_summs(glm_mod0, glm_mod1, glm_mod3)
# 
# ##
# install.packages("gtsummary")
# library(gtsummary)
# 
# tbl_regression(glm_m4_anova)
# 
# glm_m0_anova %>%
#   tidy() %>%
#   kable()
# 
# 
# library(stargazer)
# 
# stargazer(c(glm_m0_anova, glm_m1_anova, glm_m2_anova,glm_m3_anova),
#           type = 'text',
#           title=c("cero", "uno", "dos", "tres", "cuatro"))
# 
# 
# stargazer(linear.1, linear.2, title="Regression Results",
#           dep.var.labels=c("Overall Rating","High Rating"),
#           covariate.labels=c("Handling of Complaints","No Special Privileges",
#                              "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
#           omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE)
# 
# 
# AIC(glm_m0, glm_m1, glm_m2, glm_m3, glm_m4)
# 
# export_summs(glm_m0, glm_m1, glm_m2, exp = T, 
#              to.file = "xlsx", file.name = "plots/test.xlsx")
# 
# export_summs(glm_m3, glm_m4,  exp = T, 
#              to.file = "xlsx", file.name = "plots/test.xlsx")
