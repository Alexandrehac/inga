#inga<- read.table("R_inga.csv", header = TRUE, sep = ";", colClasses = c("factor", "double", "factor", "double", "double", "double"))

library(ExpDes.pt)


dad_inga<-read.table("dad_inga_part.txt", header=T)
colnames(dad_inga)

#var TME

fat2.dic (dad_inga$fat1,
          dad_inga$fat2,
          dad_inga$tme_t,
          quali= c(TRUE, F),
          mcomp = "duncan",
          fac.names= c("fat1" , "fa2"),
          sigT = 0.05,
          sigF = 0.05)

library("MASS")
attach(dad_inga)

boxplot(TME ~ fat1*fat2)

boxcox(TME ~ fat1*fat2, data=dad_inga, plotit=T)
boxcox(porcg ~ tempo*tipo, data=r_sementes_araca, lam=seq(-0.5, 0.5, 1/10))
porcg_t <- (r_sementes_araca$porcg^(0.3) - 1)/0.3


tme_t<-sqrt(dad_inga$TME+1)

dad_inga["tme_t"] <- tme_t

plot(tme_t)

#var ive 

fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$ive_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

ive_t<-sqrt(dad_inga$IVE+1)

dad_inga["ive_t"] <- ive_t

#var E 

fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$ger_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

ger_t<-asin(sqrt(dad_inga$GER/100))*(180/pi) ##transformado por arco seno raiz x/100

dad_inga["ger_t"] <- ger_t


#comp aereo 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$c.aereo_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)


c.aereo_t<-sqrt(dad_inga$c.aereo+1)

dad_inga["c.aereo_t"] <- c.aereo_t



#var numero raizes 

fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$raizes_t, quali= c(TRUE, F), mcomp = "duncan",
 fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

raizes_t<-sqrt(dad_inga$raizes+1)

dad_inga["raizes_t"] <- raizes_t


#var msa 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$masa, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

raizes_t<-sqrt(dad_inga$raizes+1)

dad_inga["raizes_t"] <- raizes_t


#var mfa 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$mfa, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

#var msr 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$msr_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

msr_t<-sqrt(dad_inga$msr+1)

dad_inga["msr_t"] <- msr_t


#var mfr 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$mfr_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

mfr_t<-sqrt(dad_inga$mfr+1)

dad_inga["mfr_t"] <- mfr_t


#var raizes comprimento


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$c.raiz_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

c.raiz_t<-sqrt(dad_inga$c.raiz+1)

dad_inga["c.raiz_t"] <- c.raiz_t

#var numero de folhas 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$folhas_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

folhas_t<-sqrt(dad_inga$folhas+1)

dad_inga["folhas_t"] <- folhas_t

#var numero dcaules 


fat2.dic (dad_inga$fat1, dad_inga$fat2, dad_inga$caules_t, quali= c(TRUE, F), mcomp = "duncan",
          fac.names= c("fat1" , "fa2"), sigT = 0.05, sigF = 0.05)

caules_t<-sqrt(dad_inga$caules+1)

dad_inga["caules_t"] <- caules_t
