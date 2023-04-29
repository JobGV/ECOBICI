
library(dplyr); library(ggplot2)

year<-c("2018","2019","2020","2021")
mes<-c("01","02","03","04","05","06","07","08","09","10","11","12")

for (i in 4:4) {
ruta <-paste0("C:\\Users\\benga\\OneDrive - El Colegio de México A.C\\Proyecto\\Redes\\Datos\\",year[i],"\\",year[i],"-",mes[1],".csv")
datos <- read.csv(ruta)

for (j in 2:12) {
  ruta <-paste0("C:\\Users\\benga\\OneDrive - El Colegio de México A.C\\Proyecto\\Redes\\Datos\\",year[i],"\\",year[i],"-",mes[j],".csv")
  aux<-read.csv(ruta)
  datos<-rbind(datos,aux)
}

datos<-datos %>% filter(Fecha_Retiro==Fecha_Arribo) 
datos$Fecha_Retiro_completa<-lubridate::dmy_hms(paste0(datos$Fecha_Retiro," ",datos$Hora_Retiro))
datos$Fecha_Arribo_completa<-lubridate::dmy_hms(paste0(datos$Fecha_Arribo," ",datos$Hora_Arribo))
datos$Tiempo_Traslado<-lubridate::minutes((lubridate::seconds_to_period(datos$Fecha_Arribo_completa-datos$Fecha_Retiro_completa)))
datos$Tiempo_Traslado<-lubridate::minute(datos$Tiempo_Traslado)

datos$Horario<-case_when(lubridate::hms(datos$Hora_Arribo)>=lubridate::hms("00:00:00") & lubridate::hms(datos$Hora_Arribo)<=lubridate::hms("05:00:00") ~1,
                         lubridate::hms(datos$Hora_Arribo)>lubridate::hms("05:00:00") & lubridate::hms(datos$Hora_Arribo)<=lubridate::hms("09:00:00") ~ 2,
                         lubridate::hms(datos$Hora_Arribo)>lubridate::hms("09:00:00") & lubridate::hms(datos$Hora_Arribo)<=lubridate::hms("17:00:00") ~ 3,
                         lubridate::hms(datos$Hora_Arribo)>lubridate::hms("17:00:00") & lubridate::hms(datos$Hora_Arribo)<=lubridate::hms("21:00:00") ~ 4,
                         lubridate::hms(datos$Hora_Arribo)>lubridate::hms("21:00:00") & lubridate::hms(datos$Hora_Arribo)<=lubridate::hms("24:00:00") ~ 5)

datos$Grupo_Edad<-case_when(datos$Edad_Usuario>0 & datos$Edad_Usuario <= 25 ~ 1,
                            datos$Edad_Usuario>25 & datos$Edad_Usuario <= 35 ~ 2,
                            datos$Edad_Usuario>35 & datos$Edad_Usuario <= 45 ~ 3,
                            datos$Edad_Usuario>45 & datos$Edad_Usuario <= 55 ~ 4,
                            datos$Edad_Usuario>55 ~ 5)

datos <- datos %>% filter(Ciclo_Estacion_Retiro<=480 & Ciclo_Estacion_Arribo <=480)
datos <- datos %>% filter(Tiempo_Traslado>0)

# Viajes totales. Para Generar las bases por género y horario simplemente se coloca la variable en el group by 
Tabla<-datos %>% group_by(Ciclo_Estacion_Retiro,Ciclo_Estacion_Arribo) %>% summarise(Viajes=n(),Tiempo_Promedio=mean(Tiempo_Traslado)) %>% as.data.frame()
Tabla <- Tabla %>%  filter(Viajes > 365)

rutaS<-paste0("C:\\Users\\benga\\OneDrive - El Colegio de México A.C\\Proyecto\\Redes\\Datos\\Clean\\",year[i],".csv")
write.csv(Tabla,rutaS,row.names = FALSE)

rm(datos)
rm(aux)
rm(Tabla)
}
