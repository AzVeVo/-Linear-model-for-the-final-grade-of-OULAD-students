library(dplyr)

dataOULAD <- read.table("OULAD_data_unified.txt",header = T)
dataOULAD <- dataOULAD %>% filter(code_module== "DDD", code_presentation == "2013J")
dataOULAD <- dataOULAD[,-c(1,2,3,15)]
#any(is.na(dataOULAD$sum_click_total)) Verificar valores NA

ind <- which(is.na(dataOULAD[,10])) #indice de valores NA para la variable score_exam
dataOULAD <- dataOULAD[-ind,] #eliminamos las observaciones NA 

dataOULAD$imd_band[dataOULAD$imd_band == ""] <- NA # se remplazan los valores en blanco de la variable imd_band por NA´S
dataOULAD$imd_band[dataOULAD$imd_band == "10-20"] <- "10-20%" # se corrige errores de tipeo de la clase "10-20%" de ka variable imd_band
ind <- which(is.na(dataOULAD[,4])) # indice de valores NA para la variable imd_band

dataOULAD <- dataOULAD[-ind,] #se eliminan las observaciones NA de la variable imd_band


ind <- which(is.na(dataOULAD[,11])) # indice de valores NA para la variable wscore_tmas
dataOULAD <- dataOULAD[-ind,] #se eliminan las observaciones NA de la variable wscore_tmas
str(dataOULAD)


#VARIABLES CATEGORICAS
#gender (nominal)
#region (nominal)
#highest_education (nominal)
#imd_band (ordinal??)
#age_band (ordinal)
#disability (nominal)
#final_result (nominal)

#VOLVEMOS FACTOR LAS VARIABLES CATEGORICAS
dataOULAD$gender <- as.factor(dataOULAD$gender)
dataOULAD$region <- as.factor(dataOULAD$region)
dataOULAD$highest_education <- as.factor(dataOULAD$highest_education)
dataOULAD$imd_band <- as.factor(dataOULAD$imd_band)
dataOULAD$age_band <- as.factor(dataOULAD$age_band)
dataOULAD$disability <- as.factor(dataOULAD$disability)
dataOULAD$final_result <- as.factor(dataOULAD$final_result)


#NIVELES
levels(dataOULAD$gender)
levels(dataOULAD$region)
levels(dataOULAD$highest_education)
#levels(dataOULAD$imd_band)
#levels(dataOULAD$age_band)
levels(dataOULAD$disability)
levels(dataOULAD$final_result)

#RECODIFICACIÓN DE LA VARIABLE REGION

#South Region(no)  SUR
#East Midlands Region(no)  ESTE
#East Anglian Region (mejor) ESTE
#South East Region(no) SUR
#North Region(no) NORTE
#London Region(no) SUR
#Scotland(no) NORTE
#North Western Region(no) NORTE
#Ireland(no) OESTE
#Yorkshire Region(no) NORTE
#South West Region(no) SUR
#Wales(no) SUR
#North Region(no) NORTE
#West Midlands Region(no) OESTE

#RECODIFICACIÓN DE LA VARIABLE REGION
levels(dataOULAD$region)
dataOULAD <- dataOULAD %>%
  mutate(region = case_when(region == "South Region" ~ "South",
                            region == "East Midlands Region" ~ "East",
                            region == "East Anglian Region" ~ "East",
                            region == "South East Region" ~ "South",
                            region == "North Region" ~ "North",
                            region == "London Region" ~ "South",
                            region == "Scotland" ~ "North",
                            region == "North Western Region" ~ "North",
                            region == "Ireland" ~ "West",
                            region == "Yorkshire Region" ~ "North",
                            region == "South West Region" ~ "South",
                            region == "Wales" ~ "South",
                            region == "North Region" ~ "North",
                            region== "West Midlands Region" ~ "West"))
dataOULAD$region <- as.factor(dataOULAD$region)

#CAMBIO DE NIVELES BASE
#VARIABLE REGION
#Variables y coeficientes significativos
#west (2)
#North (1)
#East (2)
#South (1)



#VARIABLE imd_band
#0-10% (3)
#10-20% (-3)
#20-30% (1)
#30-40% (3)
#40-50% (2)
#50-60% (4)
#60-70% (0)
#70-80% (3)
#80-90% (0)
#90-100% (1)

#VARIABLE age_band
#0-35 (0)
#35-55 (0)
#55<= (0)

# variable highest_education
#A Level or Equivalent (1)
# No Formal quals (0)
#HE Qualification (0)
# Post Graduate Qualification(0)
# Lower Than A Level (1)


#variable disability
# N (0)
# Y (0)

#Codificacion para variable Ordinal imd_band

#MODELO CON VARIABLES CATEGORICAS ORDINALES
levels(as.factor(dataOULAD$imd_band))
dataOULAD <- dataOULAD %>%
  mutate(imd_band = case_when(imd_band == "0-10%" ~ 1,
                              imd_band == "10-20%" ~ 2,
                              imd_band == "20-30%" ~ 3,
                              imd_band == "30-40%" ~ 4,
                              imd_band == "40-50%" ~ 5,
                              imd_band == "50-60%" ~ 6,
                              imd_band == "60-70%" ~ 7,
                              imd_band == "70-80%" ~ 8,
                              imd_band == "80-90%" ~ 9,
                              imd_band == "90-100%" ~ 10))




#Codificacion para la variable sum_click_total
#quantile(dataOULAD$sum_click_total, prob=c(0.30,0.60,1))

dataOULAD <- dataOULAD %>%
  mutate(sum_click_total = case_when(sum_click_total<852~ "Low",
                                     852<=sum_click_total&sum_click_total<1419 ~ "Moderate",
                                     1419<=sum_click_total&sum_click_total<= 15931 ~ "High"))

dataOULAD$sum_click_total <- as.factor(dataOULAD$sum_click_total)


#############################################################################################################
#ANÁLISIS GENERAL INDIVIDUAL

plot(x=dataOULAD$gender,y=dataOULAD$score_exam, col=c("pink","lightblue"),
     xlab="Género",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$gender,dataOULAD$score_exam), col = "blue",lwd=3)


plot(x=dataOULAD$imd_band,y=dataOULAD$score_exam,col="gray",
     xlab="IMD_band",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$imd_band,dataOULAD$score_exam), col = "red")

dataOULAD$highest_education
plot(x=dataOULAD$highest_education,y=dataOULAD$score_exam,col=c("gray"),
     xlab="Nivel de educación más alto",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$highest_education,dataOULAD$score_exam), col = "red")



dataOULAD$age_band
plot(x=dataOULAD$age_band,y=dataOULAD$score_exam,xlab="Rango de edad",
     ylab = "Puntaje del examen",col=c("purple","pink","lightblue"))
lines(lowess(dataOULAD$age_band,dataOULAD$score_exam), col = "red")


plot(x=dataOULAD$final_result,y=dataOULAD$score_exam, col=c("purple","pink","lightblue"),
     xlab="Resultado final",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$final_result,dataOULAD$score_exam), col = "red")


plot(x=dataOULAD$sum_click_total,y=dataOULAD$score_exam,col=c("purple","pink","lightblue"),
     xlab="Participación clicks",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$sum_click_total,dataOULAD$score_exam), col = "red")

dataOULAD$region
plot(x=dataOULAD$region,y=dataOULAD$score_exam,col=c("purple","pink","lightblue"),
     xlab="Región",
     ylab = "Puntaje del examen")
lines(lowess(dataOULAD$region,dataOULAD$score_exam), col = "red")


plot(x=dataOULAD$wscore_tmas,y=dataOULAD$score_exam,xlab="Puntaje TMA",
     ylab = "Puntaje del examen",col="purple")
lines(lowess(dataOULAD$wscore_tmas,dataOULAD$score_exam), col = "red")
library(descriptr)



table(dataOULAD$studied_credits)
plot(x=dataOULAD$studied_credits,y=dataOULAD$score_exam,xlab="Créditos estudiados",
     ylab = "Puntaje del examen",col="purple")
lines(lowess(dataOULAD$studied_credits,dataOULAD$score_exam), col = "red")


table(dataOULAD$disability)
plot(x=dataOULAD$disability,y=dataOULAD$score_exam,xlab="Discapacidad",
     ylab = "Puntaje del examen",col="pink")
lines(lowess(dataOULAD$disability,dataOULAD$score_exam), col = "red")


table(dataOULAD$num_of_prev_attempts)
plot(x=dataOULAD$num_of_prev_attempts,y=dataOULAD$score_exam,xlab="# de intentos",
     ylab = "Puntaje del examen",col="purple")
lines(lowess(dataOULAD$num_of_prev_attempts,dataOULAD$score_exam), col = "red")


#############################################################3
#EXPLORACION GRAFICA CON LAS VARIABLES QUE AYUDAN A EXPLICAR LA NOTA DE EXAMEN
pairs(dataOULAD[,c(2,9,10,11,12)], panel = function(x, y) {
  points(x, y)
  lines(lowess(x, y), col = "red")
})

#Las variables que se pueden encontrar útiles para explicar el score_exam son: region,final_resultFail, final_resultPass, wscore_tmas y la variable protegida para análisis posterior sum_click_total.

#Modelos-----------------------------------------------------------------------------------------------------




#b <- boxcox(mode_inte0)
#lambda <- b$x[which.max(b$y)]
#Lambda exacto

mod1 <-lm(score_exam~.-gender-highest_education-age_band-imd_band-num_of_prev_attempts-studied_credits-disability,data= dataOULAD)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
#Identiicamos observaciones atípicas influyentes

resd1 <- MASS::studres(mod1)
indatip <- which(abs((resd1)) > 2.5)
length(indatip) #Existen 15 observaciones atípicas
dataOULAD <- dataOULAD[-indatip,]

dff <- dffits(mod1)
indics <- which(dff > 1)
length(indics) #Por lo cual no hay observaciones atípicas influyentes


#Modelo sin observaciones atípicas no influyentes

mod1 <-lm(score_exam~.-gender-highest_education-age_band-imd_band-num_of_prev_attempts-studied_credits-disability,data= dataOULAD)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)

#COMPROBACION FORMAL DE LOS SUPUESTOS
library(car)
#install.packages("tseries")
resd1 <- residuals(mod1)
(m1 <-mean(resd1)) #existe linealidad

(n1 <-tseries::jarque.bera.test(resd1)) # existe normalidad en los residuos

(h1 <-lmtest::bptest(mod1)) # No Existe homocedasticidad

#Correccion de los supuestos usando MCP

#Modelo 1
#definir pesos para usar
wt<- 1/lm(abs(mod1$residuals)~mod1$fitted.values)$fitted.values^2
wlsmod1 <- lm (score_exam~.-gender-highest_education-age_band-imd_band-num_of_prev_attempts-studied_credits-disability,data= dataOULAD,weights= wt)
summary(wlsmod1)
par(mfrow=c(2,2))
plot(wlsmod1)
par(mfrow=c(1,2))

(m1w <- mean(residuals(wlsmod1)))# Exsiste linealidad 
(n1w <- tseries::jarque.bera.test(wlsmod2$residuals)) # Existe Normalidad en los residuos
(h1w <- lmtest::bptest(wlsmod1)) # Existe Homocedasticidad

##############################################################################################################

#SAgregando efectos de Interaccion 

mod2<-lm(formula = score_exam ~ region + final_result + wscore_tmas + 
           sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total, 
         data = dataOULAD)
#Identiicamos observaciones atípicas influyentes

resd2 <- MASS::studres(mod2)
indatip <- which(abs((resd2)) > 2.5)
length(indatip) #Existen 15 observaciones atípicas
dataOULAD <- dataOULAD[-indatip,]

dff <- dffits(mod2)
indics <- which(dff > 1)
length(indics) #Por lo cual no hay observaciones atípicas influyentes

mod2 <- lm(score_exam ~ region + final_result + wscore_tmas + 
             sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total, 
           data = dataOULAD)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)


resd2 <- residuals(mod2)
(m2 <- mean(resd2)) #existe linealidad
(n2 <- tseries::jarque.bera.test(resd2)) #existe normalidad en los residuos
(h2 <- lmtest::bptest(mod2)) # No Existe homocedasticidad

#Correccion de los supuestos usando MCP
#Modelo2

wt<- 1/lm(abs(mod2$residuals)~mod2$fitted.values)$fitted.values^2

wlsmod2 <- lm (score_exam ~ region + final_result + wscore_tmas + 
                 sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total, 
               data = dataOULAD,weights = wt)
summary(wlsmod2)
par(mfrow=c(2,2))
plot(wlsmod2)
(m2w <- mean(residuals(wlsmod2))) # Exsiste linealidad 
(n2w <- tseries::jarque.bera.test(wlsmod2$residuals)) # No Existe Normalidad en los residuos
(h2w <- lmtest::bptest(wlsmod2)) # Existe Homocedasticidad


###############################################################################################################

#Si agregamos un efecto cuadratico a wscore_tmas

mod3 <- lm(score_exam ~ region + final_result + wscore_tmas + 
             sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total+I((wscore_tmas-mean(wscore_tmas))^2),data= dataOULAD)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3)

#Identificamos observaciones atípicas influyentes
resd3 <- MASS::studres(mod3)
indatip <- which(abs((resd3)) > 2.5)
length(indatip) #Existen 15 observaciones atípicas
dataOULAD <- dataOULAD[-indatip,]


dff <- dffits(mod3)
indics <- which(dff > 1)
length(indics) #Por lo cual no hay observaciones atípicas influyentes

mod3 <- lm(score_exam ~ region + final_result + wscore_tmas + 
             sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total+I((wscore_tmas-mean(wscore_tmas))^2),data= dataOULAD)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3)

#COMPROBACION FORMAL DE LOS SUPUESTOS

resd3 <- residuals(mod3)
(m3 <- mean(resd3)) #existe linealidad
(n3 <- tseries::jarque.bera.test(resd3)) # No existe normalidad en los residuos
(h3 <- lmtest::bptest(mod3)) #Existe heterocedasticidad


#Correccion de los supuestos usando MCP
#Modelo3
wt<- 1/lm(abs(mod3$residuals)~mod3$fitted.values)$fitted.values^2

wlsmod3 <- lm (score_exam ~ region + final_result + wscore_tmas + 
                 sum_click_total + final_result:wscore_tmas + wscore_tmas:sum_click_total+I((wscore_tmas-mean(wscore_tmas))^2),data= dataOULAD,weights = wt)
summary(wlsmod3)
par(mfrow=c(2,2))
plot(wlsmod3)



(m3w <- mean(residuals(wlsmod3))) # Exsiste linealidad 
(n3w <- tseries::jarque.bera.test(wlsmod3$residuals)) # Existe Normalidad en los residuos
(h3w <- lmtest::bptest(wlsmod3)) # Existe Homocedasticidad

#############################################################################################################
#TABLAS CON LOS VALORES P DE LAS PRUEBAS

tabla1 <- data.frame("Media de los errores" = c(m1,m2,m3),
                     "Linealidad" = c("Si","Si","Si"),
                     "Valor p Prueba de Normalidad" = c(n1$p.value,n2$p.value,n3$p.value),
                     "Normalidad" = c("Si","Si","Si"),
                     "Valor p Prueba de Homocedasticidad" = c(h1$p.value,h2$p.value,h3$p.value),
                     "Homocedasticidad" = c("No","No","No"))
colnames(tabla1) <- c("Media","Linealidad","Valor p N","Normalidad","Valor p H","Homocedasticidad")
row.names(tabla1) <- c("Modelo 1", "Modelo 2", "Modelo 3")

tabla2 <- data.frame("Media de los errores" = c(m1w,m2w,m3w),
                     "Linealidad" = c("Si","Si","Si"),
                     "Valor p Prueba de Normalidad" = c(n1w$p.value,n2w$p.value,n3w$p.value),
                     "Normalidad" = c("Si","Si","Si"),
                     "Valor p Prueba de Homocedasticidad" = c(h1w$p.value,h2w$p.value,h3w$p.value),
                     "Homocedasticidad" = c("Si","Si","Si"))
colnames(tabla2) <- c("Media","Linealidad","Valor p N","Normalidad","Valor p H","Homocedasticidad")
row.names(tabla2) <- c("WLSModelo 1", "WLSModelo 2", "WLSModelo 3")
#############################################################################################################
#SELECCION DEL MODELO USANDO AIC

aic <- as.data.frame(t(c(AIC(wlsmod1),AIC(wlsmod2),AIC(wlsmod3))))
colnames(aic) <- c("Modelo1 WLS","Modelo2 WLS","Modelo3 WLS")
row.names(aic) <- c("AIC")

#El menor AIC corresponde al Modelo 3



# Ya que se realizo el modelamiento con la base de datos completa se realizan Inferencias PoSI 

library(PoSI)
#mod <- lm(score_exam~.,data = dataOULAD)

disModfin <- model.matrix(wlsmod3)
set.seed(42)
PoSImod <- PoSI(disModfin)
summary(PoSImod)

#IC con 95% de confianza

LI <- coef(wlsmod3)-summary(PoSImod)[1,1]*summary(wlsmod3)$coefficients[,2]
LS <- coef(wlsmod3)+summary(PoSImod)[1,1]*summary(wlsmod3)$coefficients[,2]
Ic <- cbind(LI,LS)
Ic

# wscore_tmas, wscore_tmas^2 final_resultPass COEFICIENTES SIGNIFICATIVOS MEDIANTE INFERENCIA PoSI


#Prediccion

lowpart <-predict.lm(wlsmod3,data.frame(region = "East",
                                        final_result="Pass",
                                        wscore_tmas = 90,
                                        sum_click_total= "Low"),interval = c("prediction"),
                     level = 0.95)
highpart <- predict.lm(wlsmod3,data.frame(region = "East",
                                          final_result="Pass",
                                          wscore_tmas = 90,
                                          sum_click_total= "High"), interval = c("prediction"),level = 0.95)

dif <- highpart-lowpart
dif



