# clean al
rm(list=ls())

wdir = "C:/Users/lucap/Desktop/università/DATA_SCIENCE/STAT_DAT/Stat_Meth_DS/Project/Project/smdproject2021/"
setwd(wdir)

#Carico Dataset già in parte sistemato per la precedente domanda
load(file= 'DfLastYear_AB.R')

#per prima cosa aumento la granularità di Età
df_last_year_def$Età_bin <- df_last_year_def$Età

df_last_year_def$Età_bin[df_last_year_def$Età == 0 | df_last_year_def$Età == 1] <- '0-1'
df_last_year_def$Età_bin[df_last_year_def$Età == 2 | df_last_year_def$Età == 3 |
                         df_last_year_def$Età == 4] <- '2-4'
df_last_year_def$Età_bin[df_last_year_def$Età == 5 | df_last_year_def$Età == 6 |
                           df_last_year_def$Età == 7] <- '5-7'
df_last_year_def$Età_bin[df_last_year_def$Età == 8 | df_last_year_def$Età == 9 |
                           df_last_year_def$Età == 10] <- '8-10'
df_last_year_def$Età_bin[df_last_year_def$Età >= 11 & df_last_year_def$Età <= 15] <- '11-15'
df_last_year_def$Età_bin[df_last_year_def$Età >= 16 & df_last_year_def$Età <= 20] <- '16-20'
df_last_year_def$Età_bin[df_last_year_def$Età >= 21 & df_last_year_def$Età <= 30] <- '21-30'
df_last_year_def$Età_bin[df_last_year_def$Età >= 31 & df_last_year_def$Età <= 40] <- '31-40'
df_last_year_def$Età_bin[df_last_year_def$Età >= 41 & df_last_year_def$Età <= 60] <- '41-60'
df_last_year_def$Età_bin[df_last_year_def$Età >= 61 & df_last_year_def$Età <= 80] <- '61-80'
df_last_year_def$Età_bin[df_last_year_def$Età >= 81 & df_last_year_def$Età <= 122] <- '81-122'
df_last_year_def$Età_bin <- as.factor(df_last_year_def$Età_bin)
table(df_last_year_def$Età_bin)

 
#Scelgo l'anno 2018 in quanto contiene un maggiore numero di aziende
df_2018 <- subset(df_last_year_def, LastAccountingClosingDate == '2018')
#Seleziono le aziende fallite
Failed_2018 <- subset(df_2018, Status == 'Failed')

#inizio l'analisi chiedendomi quale sia la PD condizionata dall'età nel 2018 
#Prob GENERALE
Tot_Eta <- data.frame(table(df_2018$Età_bin))
Failed_Eta <- data.frame(table(Failed_2018$Età_bin))
Failed_Eta$Freq <- (Failed_Eta$Freq / Tot_Eta$Freq)
colnames(Failed_Eta) <- c('Età', 'Probability_of_default')
Failed_Eta

ggplot(Failed_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Legal Form and Status: a percentage point of view")

#A stesso modo mi chiedo come sia distribuita la PD condizionata alla dimensione nel 2018
#Prob GENERALE
Tot_Dimensione <- data.frame(table(df_2018$DimensioneAzienda))
Failed_Dimensione <- data.frame(table(Failed_2018$DimensioneAzienda))
Failed_Dimensione$Freq <- (Failed_Dimensione$Freq / Tot_Dimensione$Freq)
colnames(Failed_Dimensione) <- c('Dimensione', 'Probability_of_Default')
Failed_Dimensione

ggplot(Failed_Dimensione, aes(y=Probability_of_Default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Failed distribution considering Dimension: a percentage point of view")

#Adesso comincio un'analisi più dettagliata. Mi chiedo infatti se la PD cambi per 
#le varie età e dimensioni dell'azienda, andando a considerare zona geografiche diverse.

#partiamo dal settentrione - Età
Settentrione_2018 <- subset(df_2018, Grouped_Regions == 'Italia Settentrionale')

Tot_Settentrione <- data.frame(table(Settentrione_2018$Età_bin))
Failed_Settentrione <- subset(Settentrione_2018, Status == 'Failed')
Failed_Settentrione_Eta <-  data.frame(table(Failed_Settentrione$Età_bin))
Failed_Settentrione_Eta$Freq <- (Failed_Settentrione_Eta$Freq / Tot_Settentrione$Freq)
colnames(Failed_Settentrione_Eta) <- c('Età', 'Probability_of_default')
Failed_Settentrione_Eta

ggplot(Failed_Settentrione_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Settentrionale: Failed distribution considering Age")

#partiamo dal settentrione - Dimensione

Tot_Settentrione_D <- data.frame(table(Settentrione_2018$DimensioneAzienda))
Failed_Settentrione <- subset(Settentrione_2018, Status == 'Failed')
Failed_Settentrione_Dimensione <-  data.frame(table(Failed_Settentrione$DimensioneAzienda))
Failed_Settentrione_Dimensione$Freq <- (Failed_Settentrione_Dimensione$Freq / Tot_Settentrione_D$Freq)
colnames(Failed_Settentrione_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Settentrione_Dimensione
  
ggplot(Failed_Settentrione_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
    geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Settentrionale: Failed distribution considering Dimension")


#centro - Età
centro_2018 <- subset(df_2018, Grouped_Regions == 'Italia Centrale')

Tot_centro <- data.frame(table(centro_2018$Età_bin))
Failed_centro <- subset(centro_2018, Status == 'Failed')
Failed_centro_Eta <-  data.frame(table(Failed_centro$Età_bin))
Failed_centro_Eta$Freq <- (Failed_centro_Eta$Freq / Tot_centro$Freq)
colnames(Failed_centro_Eta) <- c('Età', 'Probability_of_default')
Failed_centro_Eta

ggplot(Failed_centro_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Centrale: Failed distribution considering Age")

#centro - Dimensione

Tot_centro_D <- data.frame(table(centro_2018$DimensioneAzienda))
Failed_centro <- subset(centro_2018, Status == 'Failed')
Failed_centro_Dimensione <-  data.frame(table(Failed_centro$DimensioneAzienda))
Failed_centro_Dimensione$Freq <- (Failed_centro_Dimensione$Freq / Tot_centro_D$Freq)
colnames(Failed_centro_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_centro_Dimensione

ggplot(Failed_centro_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Centrale: Failed distribution considering Dimension")


#partiamo dal meridione - Età
meridione_2018 <- subset(df_2018, Grouped_Regions == 'Italia Meridionale')

Tot_meridione <- data.frame(table(meridione_2018$Età_bin))
Failed_meridione <- subset(meridione_2018, Status == 'Failed')
Failed_meridione_Eta <-  data.frame(table(Failed_meridione$Età_bin))
Failed_meridione_Eta$Freq <- (Failed_meridione_Eta$Freq / Tot_meridione$Freq)
colnames(Failed_meridione_Eta) <- c('Età', 'Probability_of_default')
Failed_meridione_Eta

ggplot(Failed_meridione_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Meridionale: Failed distribution considering Age")

#partiamo dal meridione - Dimensione

Tot_meridione_D <- data.frame(table(meridione_2018$DimensioneAzienda))
Failed_meridione <- subset(meridione_2018, Status == 'Failed')
Failed_meridione_Dimensione <-  data.frame(table(Failed_meridione$DimensioneAzienda))
Failed_meridione_Dimensione$Freq <- (Failed_meridione_Dimensione$Freq / Tot_meridione_D$Freq)
colnames(Failed_meridione_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_meridione_Dimensione

ggplot(Failed_meridione_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Meridionale: Failed distribution considering Dimension")


#partiamo dal insulare - Età
insulare_2018 <- subset(df_2018, Grouped_Regions == 'Italia Insulare')

Tot_insulare <- data.frame(table(insulare_2018$Età_bin))
Failed_insulare <- subset(insulare_2018, Status == 'Failed')
Failed_insulare_Eta <-  data.frame(table(Failed_insulare$Età_bin))
Failed_insulare_Eta$Freq <- (Failed_insulare_Eta$Freq / Tot_insulare$Freq)
colnames(Failed_insulare_Eta) <- c('Età', 'Probability_of_default')
Failed_insulare_Eta

ggplot(Failed_insulare_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Insulare: Failed distribution considering Age")

#partiamo dal insulare - Dimensione

Tot_insulare_D <- data.frame(table(insulare_2018$DimensioneAzienda))
Failed_insulare <- subset(insulare_2018, Status == 'Failed')
Failed_insulare_Dimensione <-  data.frame(table(Failed_insulare$DimensioneAzienda))
Failed_insulare_Dimensione$Freq <- (Failed_insulare_Dimensione$Freq / Tot_insulare_D$Freq)
colnames(Failed_insulare_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_insulare_Dimensione

ggplot(Failed_insulare_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Italia Insulare: Failed distribution considering Dimension")


#Continuo l'analisi vedendo se la PD cambia per le varie età e dimensioni dell'azienda considerando settori diversi.

#partiamo dal Settore A - Età
A_2018 <- subset(df_2018, ATECO == 'A')

Tot_A <- data.frame(table(A_2018$Età_bin))
Failed_A <- subset(A_2018, Status == 'Failed')
Failed_A_Eta <-  data.frame(table(Failed_A$Età_bin))
Failed_A_Eta$Freq <- (Failed_A_Eta$Freq / Tot_A$Freq)
colnames(Failed_A_Eta) <- c('Età', 'Probability_of_default')
Failed_A_Eta

ggplot(Failed_A_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section A Ateco: Failed distribution considering Età")

#partiamo dal Settore A - Dimensione

Tot_A_D <- data.frame(table(A_2018$DimensioneAzienda))
Failed_A <- subset(A_2018, Status == 'Failed')
Failed_A_Dimensione <-  data.frame(table(Failed_A$DimensioneAzienda))
Failed_A_Dimensione$Freq <- (Failed_A_Dimensione$Freq / Tot_A_D$Freq[2:3])
colnames(Failed_A_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_A_Dimensione

ggplot(Failed_A_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section A Ateco: Failed distribution considering Dimension")


#Settore BCDE - Età
BCDE_2018 <- subset(df_2018, ATECO == 'BCDE')

Tot_BCDE <- data.frame(table(BCDE_2018$Età_bin))
Failed_BCDE <- subset(BCDE_2018, Status == 'Failed')
Failed_BCDE_Eta <-  data.frame(table(Failed_BCDE$Età_bin))
Failed_BCDE_Eta$Freq <- (Failed_BCDE_Eta$Freq / Tot_BCDE$Freq)
colnames(Failed_BCDE_Eta) <- c('Età', 'Probability_of_default')
Failed_BCDE_Eta

ggplot(Failed_BCDE_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section BCDE Ateco: Failed distribution considering Età")

#Settore BCDE - Dimensione

Tot_BCDE_D <- data.frame(table(BCDE_2018$DimensioneAzienda))
Failed_BCDE <- subset(BCDE_2018, Status == 'Failed')
Failed_BCDE_Dimensione <-  data.frame(table(Failed_BCDE$DimensioneAzienda))
Failed_BCDE_Dimensione$Freq <- (Failed_BCDE_Dimensione$Freq / Tot_BCDE_D$Freq)
colnames(Failed_BCDE_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_BCDE_Dimensione

ggplot(Failed_BCDE_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section BCDE Ateco: Failed distribution considering Dimension")


#Settore F - Età
F_2018 <- subset(df_2018, ATECO == 'F')

Tot_F <- data.frame(table(F_2018$Età_bin))
Failed_F <- subset(F_2018, Status == 'Failed')
Failed_F_Eta <-  data.frame(table(Failed_F$Età_bin))
Failed_F_Eta$Freq <- (Failed_F_Eta$Freq / Tot_F$Freq)
colnames(Failed_F_Eta) <- c('Età', 'Probability_of_default')
Failed_F_Eta

ggplot(Failed_F_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section F Ateco: Failed distribution considering Età")

#Settore F - Dimensione

Tot_F_D <- data.frame(table(F_2018$DimensioneAzienda))
Failed_F <- subset(F_2018, Status == 'Failed')
Failed_F_Dimensione <-  data.frame(table(Failed_F$DimensioneAzienda))
Failed_F_Dimensione$Freq <- (Failed_F_Dimensione$Freq / Tot_F_D$Freq)
colnames(Failed_F_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_F_Dimensione

ggplot(Failed_F_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section F Ateco: Failed distribution considering Dimension")


#Settore GHI - Età
GHI_2018 <- subset(df_2018, ATECO == 'GHI')

Tot_GHI <- data.frame(table(GHI_2018$Età_bin))
Failed_GHI <- subset(GHI_2018, Status == 'Failed')
Failed_GHI_Eta <-  data.frame(table(Failed_GHI$Età_bin))
Failed_GHI_Eta$Freq <- (Failed_GHI_Eta$Freq / Tot_GHI$Freq)
colnames(Failed_GHI_Eta) <- c('Età', 'Probability_of_default')
Failed_GHI_Eta

ggplot(Failed_GHI_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section GHI Ateco: Failed distribution considering Età")

#Settore GHI - Dimensione

Tot_GHI_D <- data.frame(table(GHI_2018$DimensioneAzienda))
Failed_GHI <- subset(GHI_2018, Status == 'Failed')
Failed_GHI_Dimensione <-  data.frame(table(Failed_GHI$DimensioneAzienda))
Failed_GHI_Dimensione$Freq <- (Failed_GHI_Dimensione$Freq / Tot_GHI_D$Freq)
colnames(Failed_GHI_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_GHI_Dimensione

ggplot(Failed_GHI_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section GHI Ateco: Failed distribution considering Dimension")


#Settore J - Età
J_2018 <- subset(df_2018, ATECO == 'J')

Tot_J <- data.frame(table(J_2018$Età_bin))
Failed_J <- subset(J_2018, Status == 'Failed')
Failed_J_Eta <-  data.frame(table(Failed_J$Età_bin))
Failed_J_Eta$Freq <- (Failed_J_Eta$Freq / Tot_J$Freq)
colnames(Failed_J_Eta) <- c('Età', 'Probability_of_default')
Failed_J_Eta

ggplot(Failed_J_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section J Ateco: Failed distribution considering Età")

#Settore J - Dimensione

Tot_J_D <- data.frame(table(J_2018$DimensioneAzienda))
Failed_J <- subset(J_2018, Status == 'Failed')
Failed_J_Dimensione <-  data.frame(table(Failed_J$DimensioneAzienda))
Failed_J_Dimensione$Freq <- (Failed_J_Dimensione$Freq / Tot_J_D$Freq)
colnames(Failed_J_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_J_Dimensione

ggplot(Failed_J_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section J Ateco: Failed distribution considering Dimension")


#Settore K - Età
K_2018 <- subset(df_2018, ATECO == 'K')

Tot_K <- data.frame(table(K_2018$Età_bin))
Failed_K <- subset(K_2018, Status == 'Failed')
Failed_K_Eta <-  data.frame(table(Failed_K$Età_bin))
Failed_K_Eta$Freq <- (Failed_K_Eta$Freq / Tot_K$Freq)
colnames(Failed_K_Eta) <- c('Età', 'Probability_of_default')
Failed_K_Eta

ggplot(Failed_K_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section K Ateco: Failed distribution considering Età")

#Settore K - Dimensione

Tot_K_D <- data.frame(table(K_2018$DimensioneAzienda))
Failed_K <- subset(K_2018, Status == 'Failed')
Failed_K_Dimensione <-  data.frame(table(Failed_K$DimensioneAzienda))
Failed_K_Dimensione$Freq <- (Failed_K_Dimensione$Freq / Tot_K_D$Freq)
colnames(Failed_K_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_K_Dimensione

ggplot(Failed_K_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section K Ateco: Failed distribution considering Dimension")


#Settore L - Età
L_2018 <- subset(df_2018, ATECO == 'L')

Tot_L <- data.frame(table(L_2018$Età_bin))
Failed_L <- subset(L_2018, Status == 'Failed')
Failed_L_Eta <-  data.frame(table(Failed_L$Età_bin))
Failed_L_Eta$Freq <- (Failed_L_Eta$Freq / Tot_L$Freq)
colnames(Failed_L_Eta) <- c('Età', 'Probability_of_default')
Failed_L_Eta

ggplot(Failed_L_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section L Ateco: Failed distribution considering Età")

#Settore L - Dimensione

Tot_L_D <- data.frame(table(L_2018$DimensioneAzienda))
Failed_L <- subset(L_2018, Status == 'Failed')
Failed_L_Dimensione <-  data.frame(table(Failed_L$DimensioneAzienda))
Failed_L_Dimensione$Freq <- (Failed_L_Dimensione$Freq / Tot_L_D$Freq)
colnames(Failed_L_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_L_Dimensione

ggplot(Failed_L_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section L Ateco: Failed distribution considering Dimension")


#Settore MN - Età
MN_2018 <- subset(df_2018, ATECO == 'MN')

Tot_MN <- data.frame(table(MN_2018$Età_bin))
Failed_MN <- subset(MN_2018, Status == 'Failed')
Failed_MN_Eta <-  data.frame(table(Failed_MN$Età_bin))
Failed_MN_Eta$Freq <- (Failed_MN_Eta$Freq / Tot_MN$Freq)
colnames(Failed_MN_Eta) <- c('Età', 'Probability_of_default')
Failed_MN_Eta

ggplot(Failed_MN_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section MN Ateco: Failed distribution considering Età")

#Settore MN - Dimensione

Tot_MN_D <- data.frame(table(MN_2018$DimensioneAzienda))
Failed_MN <- subset(MN_2018, Status == 'Failed')
Failed_MN_Dimensione <-  data.frame(table(Failed_MN$DimensioneAzienda))
Failed_MN_Dimensione$Freq <- (Failed_MN_Dimensione$Freq / Tot_MN_D$Freq)
colnames(Failed_MN_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_MN_Dimensione

ggplot(Failed_MN_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section MN Ateco: Failed distribution considering Dimension")


#Settore OPQ - Età
OPQ_2018 <- subset(df_2018, ATECO == 'OPQ')

Tot_OPQ <- data.frame(table(OPQ_2018$Età_bin))
Failed_OPQ <- subset(OPQ_2018, Status == 'Failed')
Failed_OPQ_Eta <-  data.frame(table(Failed_OPQ$Età_bin))
Failed_OPQ_Eta$Freq <- (Failed_OPQ_Eta$Freq / Tot_OPQ$Freq)
colnames(Failed_OPQ_Eta) <- c('Età', 'Probability_of_default')
Failed_OPQ_Eta

ggplot(Failed_OPQ_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section OPQ Ateco: Failed distribution considering Età")

#Settore OPQ - Dimensione

Tot_OPQ_D <- data.frame(table(OPQ_2018$DimensioneAzienda))
Failed_OPQ <- subset(OPQ_2018, Status == 'Failed')
Failed_OPQ_Dimensione <-  data.frame(table(Failed_OPQ$DimensioneAzienda))
Failed_OPQ_Dimensione$Freq <- (Failed_OPQ_Dimensione$Freq / Tot_OPQ_D$Freq[2:3])
colnames(Failed_OPQ_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_OPQ_Dimensione

ggplot(Failed_OPQ_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section OPQ Ateco: Failed distribution considering Dimension")


#Settore RSTU - Età
RSTU_2018 <- subset(df_2018, ATECO == 'RSTU')

Tot_RSTU <- data.frame(table(RSTU_2018$Età_bin))
Failed_RSTU <- subset(RSTU_2018, Status == 'Failed')
Failed_RSTU_Eta <-  data.frame(table(Failed_RSTU$Età_bin))
Failed_RSTU_Eta$Freq <- (Failed_RSTU_Eta$Freq / Tot_RSTU$Freq)
colnames(Failed_RSTU_Eta) <- c('Età', 'Probability_of_default')
Failed_RSTU_Eta

ggplot(Failed_RSTU_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section RSTU Ateco: Failed distribution considering Età")

#Settore RSTU - Dimensione

Tot_RSTU_D <- data.frame(table(RSTU_2018$DimensioneAzienda))
Failed_RSTU <- subset(RSTU_2018, Status == 'Failed')
Failed_RSTU_Dimensione <-  data.frame(table(Failed_RSTU$DimensioneAzienda))
Failed_RSTU_Dimensione$Freq <- (Failed_RSTU_Dimensione$Freq / Tot_RSTU_D$Freq[2:3])
colnames(Failed_RSTU_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_RSTU_Dimensione

ggplot(Failed_RSTU_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Section RSTU Ateco: Failed distribution considering Dimension")


#Continuo l'analisi vedendo se la PD cambia per le varie età e dimensioni dell'azienda considerando forme legali diverse.

#partiamo da S.p.a - Età
Spa_2018 <- subset(df_2018, LegalForm == 'S.P.A.')

Tot_Spa <- data.frame(table(Spa_2018$Età_bin))
Failed_Spa <- subset(Spa_2018, Status == 'Failed')
Failed_Spa_Eta <-  data.frame(table(Failed_Spa$Età_bin))
Failed_Spa_Eta$Freq <- (Failed_Spa_Eta$Freq / Tot_Spa$Freq)
colnames(Failed_Spa_Eta) <- c('Età', 'Probability_of_default')
Failed_Spa_Eta

ggplot(Failed_Spa_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Spa: Failed distribution considering Età")

#partiamo da S.p.a - Dimensione

Tot_Spa_D <- data.frame(table(Spa_2018$DimensioneAzienda))
Failed_Spa <- subset(Spa_2018, Status == 'Failed')
Failed_Spa_Dimensione <-  data.frame(table(Failed_Spa$DimensioneAzienda))
Failed_Spa_Dimensione$Freq <- (Failed_Spa_Dimensione$Freq / Tot_Spa_D$Freq)
colnames(Failed_Spa_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Spa_Dimensione

ggplot(Failed_Spa_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Spa: Failed distribution considering Dimension")


#Consortium - Età
Consortium_2018 <- subset(df_2018, LegalForm == 'Consortium')

Tot_Consortium <- data.frame(table(Consortium_2018$Età_bin))
Failed_Consortium <- subset(Consortium_2018, Status == 'Failed')
Failed_Consortium_Eta <-  data.frame(table(Failed_Consortium$Età_bin))
Failed_Consortium_Eta$Freq <- (Failed_Consortium_Eta$Freq / Tot_Consortium$Freq)
colnames(Failed_Consortium_Eta) <- c('Età', 'Probability_of_default')
Failed_Consortium_Eta

ggplot(Failed_Consortium_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Consortium: Failed distribution considering Età")

#Consortium - Dimensione

Tot_Consortium_D <- data.frame(table(Consortium_2018$DimensioneAzienda))
Failed_Consortium <- subset(Consortium_2018, Status == 'Failed')
Failed_Consortium_Dimensione <-  data.frame(table(Failed_Consortium$DimensioneAzienda))
Failed_Consortium_Dimensione$Freq <- (Failed_Consortium_Dimensione$Freq / Tot_Consortium_D$Freq)
colnames(Failed_Consortium_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Consortium_Dimensione

ggplot(Failed_Consortium_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Consortium: Failed distribution considering Dimension")


#Other - Età
Other_2018 <- subset(df_2018, LegalForm == 'Other')

Tot_Other <- data.frame(table(Other_2018$Età_bin))
Failed_Other <- subset(Other_2018, Status == 'Failed')
Failed_Other_Eta <-  data.frame(table(Failed_Other$Età_bin))
Failed_Other_Eta$Freq <- (Failed_Other_Eta$Freq / Tot_Other$Freq)
colnames(Failed_Other_Eta) <- c('Età', 'Probability_of_default')
Failed_Other_Eta

ggplot(Failed_Other_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Other: Failed distribution considering Età")


#Other - Dimensione

Tot_Other_D <- data.frame(table(Other_2018$DimensioneAzienda))
Failed_Other <- subset(Other_2018, Status == 'Failed')
Failed_Other_Dimensione <-  data.frame(table(Failed_Other$DimensioneAzienda))
Failed_Other_Dimensione$Freq <- (Failed_Other_Dimensione$Freq / Tot_Other_D$Freq[2:3])
colnames(Failed_Other_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Other_Dimensione

ggplot(Failed_Other_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Other: Failed distribution considering Dimension")


#Srl - Età
Srl_2018 <- subset(df_2018, LegalForm == 'S.R.L.')

Tot_Srl <- data.frame(table(Srl_2018$Età_bin))
Failed_Srl <- subset(Srl_2018, Status == 'Failed')
Failed_Srl_Eta <-  data.frame(table(Failed_Srl$Età_bin))
Failed_Srl_Eta$Freq <- (Failed_Srl_Eta$Freq / Tot_Srl$Freq)
colnames(Failed_Srl_Eta) <- c('Età', 'Probability_of_default')
Failed_Srl_Eta

ggplot(Failed_Srl_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl: Failed distribution considering Età")


#Srl - Dimensione

Tot_Srl_D <- data.frame(table(Srl_2018$DimensioneAzienda))
Failed_Srl <- subset(Srl_2018, Status == 'Failed')
Failed_Srl_Dimensione <-  data.frame(table(Failed_Srl$DimensioneAzienda))
Failed_Srl_Dimensione$Freq <- (Failed_Srl_Dimensione$Freq / Tot_Srl_D$Freq)
colnames(Failed_Srl_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Srl_Dimensione

ggplot(Failed_Srl_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl: Failed distribution considering Dimension")


#Scarl - Età
Scarl_2018 <- subset(df_2018, LegalForm == 'S.C.A.R.L.')

Tot_Scarl <- data.frame(table(Scarl_2018$Età_bin))
Failed_Scarl <- subset(Scarl_2018, Status == 'Failed')
Failed_Scarl_Eta <-  data.frame(table(Failed_Scarl$Età_bin))
Failed_Scarl_Eta$Freq <- (Failed_Scarl_Eta$Freq / Tot_Scarl$Freq)
colnames(Failed_Scarl_Eta) <- c('Età', 'Probability_of_default')
Failed_Scarl_Eta

ggplot(Failed_Scarl_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Scarl: Failed distribution considering Età")


#Scarl - Dimensione

Tot_Scarl_D <- data.frame(table(Scarl_2018$DimensioneAzienda))
Failed_Scarl <- subset(Scarl_2018, Status == 'Failed')
Failed_Scarl_Dimensione <-  data.frame(table(Failed_Scarl$DimensioneAzienda))
Failed_Scarl_Dimensione$Freq <- (Failed_Scarl_Dimensione$Freq / Tot_Scarl_D$Freq[2:3])
colnames(Failed_Scarl_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Scarl_Dimensione

ggplot(Failed_Scarl_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Scarl: Failed distribution considering Dimension")


#Scarlpa - Età
Scarlpa_2018 <- subset(df_2018, LegalForm == 'S.C.A.R.L.P.A.')

Tot_Scarlpa <- data.frame(table(Scarlpa_2018$Età_bin))
Failed_Scarlpa <- subset(Scarlpa_2018, Status == 'Failed')
Failed_Scarlpa_Eta <-  data.frame(table(Failed_Scarlpa$Età_bin))
Failed_Scarlpa_Eta$Freq <- (Failed_Scarlpa_Eta$Freq / Tot_Scarlpa$Freq)
colnames(Failed_Scarlpa_Eta) <- c('Età', 'Probability_of_default')
Failed_Scarlpa_Eta

ggplot(Failed_Scarlpa_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Scarlpa: Failed distribution considering Età")


#Scarlpa - Dimensione

Tot_Scarlpa_D <- data.frame(table(Scarlpa_2018$DimensioneAzienda))
Failed_Scarlpa <- subset(Scarlpa_2018, Status == 'Failed')
Failed_Scarlpa_Dimensione <-  data.frame(table(Failed_Scarlpa$DimensioneAzienda))
Failed_Scarlpa_Dimensione$Freq <- (Failed_Scarlpa_Dimensione$Freq / Tot_Scarlpa_D$Freq)
colnames(Failed_Scarlpa_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Scarlpa_Dimensione

ggplot(Failed_Scarlpa_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Scarlpa: Failed distribution considering Dimension")


#Srl one-person - Età
Srl_one_person_2018 <- subset(df_2018, LegalForm == 'S.R.L. one-person')

Tot_Srl_one_person <- data.frame(table(Srl_one_person_2018$Età_bin))
Failed_Srl_one_person <- subset(Srl_one_person_2018, Status == 'Failed')
Failed_Srl_one_person_Eta <-  data.frame(table(Failed_Srl_one_person$Età_bin))
Failed_Srl_one_person_Eta$Freq <- (Failed_Srl_one_person_Eta$Freq / Tot_Srl_one_person$Freq)
colnames(Failed_Srl_one_person_Eta) <- c('Età', 'Probability_of_default')
Failed_Srl_one_person_Eta

ggplot(Failed_Srl_one_person_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl_one_person: Failed distribution considering Età")


#Srl one-person - Dimensione

Tot_Srl_one_person_D <- data.frame(table(Srl_one_person_2018$DimensioneAzienda))
Failed_Srl_one_person <- subset(Srl_one_person_2018, Status == 'Failed')
Failed_Srl_one_person_Dimensione <-  data.frame(table(Failed_Srl_one_person$DimensioneAzienda))
Failed_Srl_one_person_Dimensione$Freq <- (Failed_Srl_one_person_Dimensione$Freq / Tot_Srl_one_person_D$Freq)
colnames(Failed_Srl_one_person_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Srl_one_person_Dimensione

ggplot(Failed_Srl_one_person_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl_one_person: Failed distribution considering Dimension")


#Srl simplified - Età
Srl_simplified_2018 <- subset(df_2018, LegalForm == 'S.R.L. simplified')

Tot_Srl_simplified <- data.frame(table(Srl_simplified_2018$Età_bin))
Failed_Srl_simplified <- subset(Srl_simplified_2018, Status == 'Failed')
Failed_Srl_simplified_Eta <-  data.frame(table(Failed_Srl_simplified$Età_bin))
Failed_Srl_simplified_Eta$Freq <- (Failed_Srl_simplified_Eta$Freq / Tot_Srl_simplified$Freq)
colnames(Failed_Srl_simplified_Eta) <- c('Età', 'Probability_of_default')
Failed_Srl_simplified_Eta

ggplot(Failed_Srl_simplified_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl_simplified: Failed distribution considering Età")

#Srl simplified - Dimensione

Tot_Srl_simplified_D <- data.frame(table(Srl_simplified_2018$DimensioneAzienda))
Failed_Srl_simplified <- subset(Srl_simplified_2018, Status == 'Failed')
Failed_Srl_simplified_Dimensione <-  data.frame(table(Failed_Srl_simplified$DimensioneAzienda))
Failed_Srl_simplified_Dimensione$Freq <- (Failed_Srl_simplified_Dimensione$Freq / Tot_Srl_simplified_D$Freq)
colnames(Failed_Srl_simplified_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Srl_simplified_Dimensione


ggplot(Failed_Srl_simplified_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Srl_simplified: Failed distribution considering Dimension")


#Social cooperative company - Età
Social_2018 <- subset(df_2018, LegalForm == 'Social cooperative company')

Tot_Social <- data.frame(table(Social_2018$Età_bin))
Failed_Social <- subset(Social_2018, Status == 'Failed')
Failed_Social_Eta <-  data.frame(table(Failed_Social$Età_bin))
Failed_Social_Eta$Freq <- (Failed_Social_Eta$Freq / Tot_Social$Freq)
colnames(Failed_Social_Eta) <- c('Età', 'Probability_of_default')
Failed_Social_Eta

ggplot(Failed_Social_Eta, aes(y=Probability_of_default, x=Età)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Social cooperative company: Failed distribution considering Età")


#Social cooperative company - Dimensione

Tot_Social_D <- data.frame(table(Social_2018$DimensioneAzienda))
Failed_Social <- subset(Social_2018, Status == 'Failed')
Failed_Social_Dimensione <-  data.frame(table(Failed_Social$DimensioneAzienda))
Failed_Social_Dimensione$Freq <- (Failed_Social_Dimensione$Freq / Tot_Social_D$Freq[2:3])
colnames(Failed_Social_Dimensione) <- c('Dimensione', 'Probability_of_default')
Failed_Social_Dimensione

ggplot(Failed_Social_Dimensione, aes(y=Probability_of_default, x=Dimensione)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  ggtitle("Social cooperative company: Failed distribution considering Dimension")

