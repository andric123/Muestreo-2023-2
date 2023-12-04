############### Proyecto final Muestreo Estadístico #######################
#Andrés Mauricio Rico Parada
#Nicolás Quenguan

setwd("E:/Users/usuario/Documents/Muestreo")
library(dplyr)
library(survey)
library(sampling)

base <- read.csv("saber_pro.csv")

base2 <- select(base, INST_COD_INSTITUCION,INST_NOMBRE_INSTITUCION,INST_ORIGEN, PUNT_GLOBAL, MOD_INGLES_PUNT, ESTU_INSE_INDIVIDUAL,ESTU_PAGOMATRICULACREDITO,ESTU_HORASSEMANATRABAJA)
base2 <- na.omit(base2)

base2 <- base2[base2$ESTU_PAGOMATRICULACREDITO != "", ]
base2 <- base2[base2$ESTU_HORASSEMANATRABAJA  != "", ]

base2 <- within(base2,{
  Credito <- as.numeric(ifelse(ESTU_PAGOMATRICULACREDITO=="Si",1,0))
})

base2$Trabaja10 <- ifelse(base2$ESTU_HORASSEMANATRABAJA == "Menos de 10 horas", 0, base2$ESTU_HORASSEMANATRABAJA)
base2$Trabaja10[base2$Trabaja10 != "0"] <- 1
base2$Trabaja10 <- as.numeric(base2$Trabaja10)

base2$Caracter <- ifelse(base2$INST_ORIGEN %in% c("NO OFICIAL - CORPORACIÓN", "NO OFICIAL - FUNDACIÓN"), "Privada", 
                          ifelse(base2$INST_ORIGEN %in% c("OFICIAL MUNICIPAL", "OFICIAL NACIONAL", "OFICIAL DEPARTAMENTAL"), "Pública", NA))

base2<-na.omit(base2)

base2 <- subset(base2, select = -c(ESTU_PAGOMATRICULACREDITO, ESTU_HORASSEMANATRABAJA,INST_ORIGEN))

universidades <- base2 %>%
  group_by(INST_COD_INSTITUCION, INST_NOMBRE_INSTITUCION, Caracter) %>%
  summarise(
    x = n(),
    puntaje = mean(PUNT_GLOBAL),
    var_punt = var(PUNT_GLOBAL),
    ingles = sum(MOD_INGLES_PUNT),
    horas_trab = sum(Trabaja10),
    credito = sum(Credito),
    inse=sum(ESTU_INSE_INDIVIDUAL)
  )

################# Tamaño de muestra ###################

#### Esto sólo es una prueba para ver en cuánto está más o menos el rho y poder estimar n####
t_muestras <- floor(universidades$tamaño_muestra)
a <- as.data.frame(cbind(codigos_publicas,t_public))
muestras2<-NULL
for (i in 1:nrow(a)){
  muestras <- base2 %>% 
    filter(INST_COD_INSTITUCION==as.numeric(a[i,1][[1]])) %>% 
    sample_n(size=as.numeric(a[i,2][[1]]))
  
  muestras2 <- rbind(muestras2,muestras)
}
a<-as.data.frame(cbind(codigos,t_muestras))
SCD <- muestras2 %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(n=n(),var = var(PUNT_GLOBAL))
SCD$scd = (SCD$n-1)*SCD$var
SCD <- sum(SCD$scd)
SCT <- (nrow(muestras2)-1)*var(muestras2$PUNT_GLOBAL)
1-SCD/SCT

######## Tamaño de muestra por UPM ####
universidades$tamaño_muestra = (1.96^2*universidades$var_punt)/((0.05*universidades$puntaje)^2+1.96^2*(universidades$var_punt/universidades$x))


codigos_publicas <- publicas$INST_COD_INSTITUCION
codigos_privadas <- privadas$INST_COD_INSTITUCION
t_public <- publicas$tamaño_muestra
t_privadas <- privadas$tamaño_muestra

publicas <- universidades[universidades$Caracter=="Pública",]
privadas <- universidades[universidades$Caracter=="Privada",]

nI <- 27


n_e1 <- nI*(nrow(publicas)*sd(publicas$puntaje))/(nrow(publicas)*sd(publicas$puntaje)+nrow(privadas)*sd(privadas$puntaje))

n_e2 <- nI*(nrow(privadas)*sd(privadas$puntaje))/(nrow(publicas)*sd(publicas$puntaje)+nrow(privadas)*sd(privadas$puntaje))

n_e1 <- 8
n_e2 <- 19

publicas$pkI=inclusionprobabilities(publicas$x,n_e1)
set.seed(123)
sampleind <- UPrandompivotal(pik = publicas$pkI)
publicas_selec <- data.frame(publicas[sampleind == 1, ])


privadas$pkI=inclusionprobabilities(privadas$x,n_e1)
set.seed(123)
sampleind2 <- UPrandompivotal(pik = privadas$pkI)
privadas_selec <- data.frame(privadas[sampleind2 == 1, ])

publicas_selec$tamaño_muestra <- ceiling(publicas_selec$tamaño_muestra)
privadas_selec$tamaño_muestra <- ceiling(privadas_selec$tamaño_muestra)
table <- knitr::kable(publicas_selec[,c(2,11)], format = "latex")
table


v_sample1<-as.data.frame(cbind(publicas_selec$INST_COD_INSTITUCION,publicas_selec$tamaño_muestra))
v_sample2<-as.data.frame(cbind(privadas_selec$INST_COD_INSTITUCION,privadas_selec$tamaño_muestra))


muestra_publicas <- NULL
set.seed(123)
for (i in 1:nrow(v_sample1)){
  muestras <- base2 %>% 
    filter(INST_COD_INSTITUCION==as.numeric(v_sample1[i,1][[1]])) %>% 
    sample_n(size=as.numeric(v_sample1[i,2][[1]]),replace = FALSE)
  
  muestra_publicas <- rbind(muestra_publicas,muestras)
}

muestra_privadas <- NULL
for (i in 1:nrow(v_sample2)){
  muestras <- base2 %>% 
    filter(INST_COD_INSTITUCION==as.numeric(v_sample2[i,1][[1]])) %>% 
    sample_n(size=as.numeric(v_sample2[i,2][[1]]),replace=FALSE)
  
  muestra_privadas<- rbind(muestra_privadas,muestras)
}

#Lo hice a mano
pi_uni_pu <- muestra_publicas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumy=sum(PUNT_GLOBAL))

publicas_selec<-cbind(publicas_selec,sum=pi_uni_pu$sumy)
publicas_selec$tamaño_muestra <- ceiling(publicas_selec$tamaño_muestra)

############ pi estimadores por universidad ####################
pi_uni_pu <- publicas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sum/tamaño_muestra)

pi_uni_priv <- muestra_privadas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumy=sum(PUNT_GLOBAL))

privadas_selec<-cbind(privadas_selec,sum=pi_uni_priv$sumy)
privadas_selec$tamaño_muestra <- ceiling(privadas_selec$tamaño_muestra)

pi_uni_priv <- privadas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sum/tamaño_muestra)

############ pi estimadores por estrato ################

pi_pub <- sum(pi_uni_pu$pi_est/publicas_selec$pkI)
pi_priv <- sum(pi_uni_priv$pi_est/privadas_selec$pkI)

pi_pub
pi_priv

################## Estimador media poblacional ###############
(pi_pub + pi_priv)/nrow(base2) 

prueba <- base2 %>% 
  filter(Caracter=="Pública")
sum(prueba$PUNT_GLOBAL)

prueba2 <- base2 %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(tot_univ = sum(PUNT_GLOBAL))


var_pu <- 1/(8*7)*sum((pi_uni_pu$pi_est/publicas_selec$pkI-pi_pub/8)^2)
var_priv <- 1/(19*18)*sum((pi_uni_priv$pi_est/privadas_selec$pkI-pi_priv/19)^2)

sqrt(var_pu)/pi_pub
sqrt(var_priv)/pi_priv

estimador_pun <- pi_pub+pi_priv
var_est <- var_pu + var_priv 
cv_est <- sqrt(var_est)/estimador_pun
cv_est

########################## Lo mismo para Modulo inglés ##################################
#Lo hice a mano
pi2_uni_pu <- muestra_publicas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumingl=sum(MOD_INGLES_PUNT))

publicas_selec<-cbind(publicas_selec,sumingl=pi2_uni_pu$sumingl)

pi2_uni_pu <- publicas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumingl/tamaño_muestra)


pi2_uni_priv <- muestra_privadas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumingl=sum(MOD_INGLES_PUNT))

privadas_selec<-cbind(privadas_selec,sumingl=pi2_uni_priv$sumingl)

pi2_uni_priv <- privadas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumingl/tamaño_muestra)

pi2_pub <- sum(pi2_uni_pu$pi_est/publicas_selec$pkI)
pi2_priv <- sum(pi2_uni_priv$pi_est/privadas_selec$pkI)

pi2_pub
pi2_priv

(pi2_pub + pi2_priv)/nrow(base2)

var2_pu <- 1/(8*7)*sum((pi2_uni_pu$pi_est/publicas_selec$pkI-pi2_pub/8)^2)
var2_priv <- 1/(19*18)*sum((pi2_uni_priv$pi_est/privadas_selec$pkI-pi2_priv/19)^2)


estimador2_pun <- pi2_pub+pi2_priv
var_est2 <- var2_pu + var2_priv 
cv_est2 <- sqrt(var_est2)/estimador2_pun
cv_est2

####################### Lo mismo para el indicador INSE ######################
pi3_uni_pu <- muestra_publicas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(suminse=sum(ESTU_INSE_INDIVIDUAL))

publicas_selec<-cbind(publicas_selec,suminse=pi3_uni_pu$suminse)

pi3_uni_pu <- publicas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*suminse/tamaño_muestra)

pi3_uni_priv <- muestra_privadas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(suminse=sum(ESTU_INSE_INDIVIDUAL))

privadas_selec<-cbind(privadas_selec,suminse=pi3_uni_priv$suminse)

pi3_uni_priv <- privadas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*suminse/tamaño_muestra)

pi3_pub <- sum(pi3_uni_pu$pi_est/publicas_selec$pkI)
pi3_priv <- sum(pi3_uni_priv$pi_est/privadas_selec$pkI)

pi3_pub
pi3_priv

(pi3_pub + pi3_priv)/nrow(base2)

var3_pu <- 1/(8*7)*sum((pi3_uni_pu$pi_est/publicas_selec$pkI-pi3_pub/8)^2)
var3_priv <- 1/(19*18)*sum((pi3_uni_priv$pi_est/privadas_selec$pkI-pi3_priv/19)^2)


estimador3_pun <- pi3_pub+pi3_priv
var_est3 <- var3_pu + var3_priv 
cv_est3 <- sqrt(var_est3)/estimador3_pun
cv_est3

############################## Lo mismo para los que tienen crédito #############

pi4_uni_pu <- muestra_publicas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumcred=sum(Credito))

publicas_selec<-cbind(publicas_selec,sumcred=pi4_uni_pu$sumcred)

pi4_uni_pu <- publicas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumcred/tamaño_muestra)

pi4_uni_priv <- muestra_privadas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumcred=sum(Credito))

privadas_selec<-cbind(privadas_selec,sumcred=pi4_uni_priv$sumcred)

pi4_uni_priv <- privadas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumcred/tamaño_muestra)

pi4_pub <- sum(pi4_uni_pu$pi_est/publicas_selec$pkI)
pi4_priv <- sum(pi4_uni_priv$pi_est/privadas_selec$pkI)

pi4_pub
pi4_priv

(pi4_pub + pi4_priv)/nrow(base2)

var4_pu <- 1/(8*7)*sum((pi5_uni_pu$pi_est/publicas_selec$pkI-pi4_pub/8)^2)
var4_priv <- 1/(19*18)*sum((pi5_uni_priv$pi_est/privadas_selec$pkI-pi4_priv/19)^2)


estimador4_pun <- pi4_pub+pi4_priv
var_est4 <- var4_pu + var4_priv 
cv_est4 <- sqrt(var_est4)/estimador4_pun
cv_est4

################# Lo mismo para los que trabajan ########################


pi5_uni_pu <- muestra_publicas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumtrab=sum(Trabaja10))

publicas_selec<-cbind(publicas_selec,sumtrab=pi5_uni_pu$sumtrab)

pi5_uni_pu <- publicas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumtrab/tamaño_muestra)

pi5_uni_priv <- muestra_privadas %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(sumtrab=sum(Trabaja10))

privadas_selec<-cbind(privadas_selec,sumtrab=pi5_uni_priv$sumtrab)

pi5_uni_priv <- privadas_selec %>% 
  group_by(INST_COD_INSTITUCION) %>% 
  summarise(pi_est=x*sumtrab/tamaño_muestra)

pi5_pub <- sum(pi5_uni_pu$pi_est/publicas_selec$pkI)
pi5_priv <- sum(pi5_uni_priv$pi_est/privadas_selec$pkI)

pi5_pub
pi5_priv

(pi5_pub + pi5_priv)/nrow(base2)

var5_pu <- 1/(8*7)*sum((pi5_uni_pu$pi_est/publicas_selec$pkI-pi5_pub/8)^2)
var5_priv <- 1/(19*18)*sum((pi5_uni_priv$pi_est/privadas_selec$pkI-pi5_priv/19)^2)


estimador5_pun <- pi5_pub+pi5_priv
var_est5 <- var5_pu + var5_priv 
cv_est5 <- sqrt(var_est5)/estimador5_pun
cv_est5


