# clean all
rm(list=ls())

library(caret)

wdir = "C:/Users/lucap/Desktop/università/DATA_SCIENCE/STAT_DAT/Stat_Meth_DS/Project/Project/smdproject2021/"
setwd(wdir)


load(file= 'DfLastYear_AB.R')
hist(df_last_year_def$Log_Asset)
#L'anno 2018 è quello che presenta il maggior numero di aziende con il closing date.
df_2018_last <- data.frame(df_last_year_def[df_last_year_def$LastAccountingClosingDate %in% "2018",])

#L'analisi che verrà ora implementata riguarda la comparazione fra attive e fallite nel 2018
#Prima si indagherà se vi è differenza tenendo in comparazione della loro età, dimensione, Sezione Ateco
#Successivamente per Età e dimensione indagheremo se vi sono delle differenze
#in base ala sezione Ateco o forma legale.

#Adesso divido il dataset con solo le aziende che hanno l'ultimo anno di rendicontazione nel 2018
#in base al loro status
library(carData)
Failed_2018 <- subset(df_2018_last, Status == "Failed")
Active_2018 <- subset(df_2018_last, Status == 'Active')

#density of Età compared
library(ggplot2)
ggplot(df_last_year_def, 
       aes(x=Età, fill=Status)) +
       geom_density(alpha=0.4) + labs(title="Età density curve", x="Età")

#Test per vedere se la media è significativamente diversa.
#Utilizzo t-test H0 = 
library(BSDA)
t.test(Failed_2018$Età, Active_2018$Età)
#RIFIUTO IPOTESI NULLA

ggplot(df_last_year_def, 
       aes(x=Log_Asset, fill=Status)) +
       geom_density(alpha=0.4) + labs(title="Età density curve", x="LogAsset")

#test per verificare la normalità
library(nortest)
ad.test(Failed_2018$Log_Asset) #rigetto normalità
ad.test(Active_2018$Log_Asset) #Rigetto normalità

#uso t-test
t.test(Failed_2018$Log_Asset, Active_2018$Log_Asset)
#rigetto ipotesi nulla

#creo Dataset utili alla visualizzazione delle ulteriori analisi. (Legal Form)
Tot_legalForm <- data.frame(table(df_2018_last$LegalForm))

Failed_2018_LegalForm <- data.frame(table(Failed_2018$LegalForm))
Failed_2018_LegalForm$Status <- 'Failed'
Failed_2018_LegalForm$Freq <- (Failed_2018_LegalForm$Freq / Tot_legalForm$Freq)*100
colnames(Failed_2018_LegalForm) <- c('LegalForm', 'Percentage', 'Status')

Active_2018_LegalForm <- data.frame(table(Active_2018$LegalForm))
Active_2018_LegalForm$Status <- 'Active'
Active_2018_LegalForm$Freq <- (Active_2018_LegalForm$Freq / Tot_legalForm$Freq)*100
colnames(Active_2018_LegalForm) <- c('LegalForm', 'Percentage', 'Status')

Legal_form_graphic <- rbind(Failed_2018_LegalForm, Active_2018_LegalForm)

#creo Dataset utili alla visualizzazione delle ulteriori analisi. (ATECO)
Tot_ATECO <- data.frame(table(df_2018_last$ATECO))

Failed_2018_ATECO <- data.frame(table(Failed_2018$ATECO))
Failed_2018_ATECO$Status <- 'Failed'
Failed_2018_ATECO$Freq <- (Failed_2018_ATECO$Freq / Tot_ATECO$Freq)*100
colnames(Failed_2018_ATECO) <- c('ATECO', 'Percentage', 'Status')


Active_2018_ATECO <- data.frame(table(Active_2018$ATECO))
Active_2018_ATECO$Status <- 'Active'
Active_2018_ATECO$Freq <- (Active_2018_ATECO$Freq / Tot_ATECO$Freq)*100
colnames(Active_2018_ATECO) <- c('ATECO', 'Percentage', 'Status')
ATECO_graphic <- rbind(Failed_2018_ATECO, Active_2018_ATECO)

#creo Dataset utili alla visualizzazione delle ulteriori analisi. (Regions)
Tot_GroupedRegions <- data.frame(table(df_2018_last$Grouped_Regions))

Failed_2018_GroupedRegions <- data.frame(table(Failed_2018$Grouped_Regions))
Failed_2018_GroupedRegions$Status <- 'Failed'
Failed_2018_GroupedRegions$Freq <- (Failed_2018_GroupedRegions$Freq / Tot_GroupedRegions$Freq)*100
colnames(Failed_2018_GroupedRegions) <- c('GroupedRegions', 'Percentage', 'Status')

Active_2018_GroupedRegions <- data.frame(table(Active_2018$Grouped_Regions))
Active_2018_GroupedRegions$Status <- 'Active'
Active_2018_GroupedRegions$Freq <- (Active_2018_GroupedRegions$Freq / Tot_GroupedRegions$Freq)*100
colnames(Active_2018_GroupedRegions) <- c('GroupedRegions', 'Percentage', 'Status')

Grouped_Regions_graphic <- rbind(Failed_2018_GroupedRegions, Active_2018_GroupedRegions)


#BarPlot Active/Failed LegalForm
ggplot(Legal_form_graphic, aes(fill=Status, y=Percentage, x=LegalForm)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Legal Form and Status: a percentage point of view")

#BarPlot Active/Failed ATECO
ggplot(ATECO_graphic, aes(fill=Status, y=Percentage, x=ATECO)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Grouped Ateco and Status: a percentage point of view")

S#BarPlot Active/Failed Grouped Regions
ggplot(Grouped_Regions_graphic, aes(fill=Status, y=Percentage, x=GroupedRegions)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Grouped Regions and Status: a percentage point of view")

#creo dataset per comparazione distribuzioni domanda A (LEGAL FORM)
df_2018_LegalFormSplit <- split(df_2018_last, df_2018_last$LegalForm)
Spa_2018 <- df_2018_LegalFormSplit$S.P.A.
Srl_2018 <- df_2018_LegalFormSplit$S.R.L.
Srl_One_P_2018 <- df_2018_LegalFormSplit$`S.R.L. one-person`
Srl_S_2018 <- df_2018_LegalFormSplit$`S.R.L. simplified`
Social_2018 <- df_2018_LegalFormSplit$`Social cooperative company`
Other_2018 <- df_2018_LegalFormSplit$Other
Cons_2018 <- df_2018_LegalFormSplit$Consortium
Scarl_2018 <- df_2018_LegalFormSplit$S.C.A.R.L.
Scarlpa_2018 <- df_2018_LegalFormSplit$S.C.A.R.L.P.A.


#Spa comapred by dimension
ggplot(Spa_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Spa: Dimension Focus", x="Log_Dimensione")

ad.test(Spa_2018$Log_Asset[Spa_2018$Status == 'Active']) #rigetto normalità
t.test(Spa_2018$Log_Asset[Spa_2018$Status == 'Active'], Spa_2018$Log_Asset[Spa_2018$Status == 'Failed'])
#Rigetto ipotesi nulla

#Srl comapred by dimension
ggplot(Srl_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl: Dimension Focus", x="Log_Dimensione")

ad.test(Srl_2018$Log_Asset[Srl_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_2018$Log_Asset[Srl_2018$Status == 'Active'], Srl_2018$Log_Asset[Srl_2018$Status == 'Failed'])
#rigetto H0

#Srl one person comapred by dimension
ggplot(Srl_One_P_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl one person: Dimension Focus", x="Log_Dimensione")

ad.test(Srl_One_P_2018$Log_Asset[Srl_One_P_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_One_P_2018$Log_Asset[Srl_One_P_2018$Status == 'Active'], Srl_One_P_2018$Log_Asset[Srl_One_P_2018$Status == 'Failed'])
#rigetto H0

#Srl simplified comapred by dimension
ggplot(Srl_S_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl simplified: Dimension Focus", x="Log_Dimensione")

ad.test(Srl_S_2018$Log_Asset[Srl_S_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_S_2018$Log_Asset[Srl_S_2018$Status == 'Active'], Srl_S_2018$Log_Asset[Srl_S_2018$Status == 'Failed'])
#rigetto H0

#Social cooperative company comapred by dimension
ggplot(Social_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Social cooperative company: Dimension Focus", x="Log_Dimensione")

ad.test(Social_2018$Log_Asset[Social_2018$Status == 'Active']) #Rigetto H0

t.test(Social_2018$Log_Asset[Social_2018$Status == 'Active'], Social_2018$Log_Asset[Social_2018$Status == 'Failed'],  var.equal = TRUE)
#rigetto H0

#Consortium comapred by dimension
ggplot(Cons_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Consortium: Dimension Focus", x="Log_Dimensione")

ad.test(Cons_2018$Log_Asset[Cons_2018$Status == 'Active']) #rigetto normalità
t.test(Cons_2018$Log_Asset[Cons_2018$Status == 'Active'], Cons_2018$Log_Asset[Cons_2018$Status == 'Failed'])
#rigetto H0


#Other comapred by dimension
ggplot(Other_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Other: Dimension Focus", x="Log_Dimensione")

ad.test(Other_2018$Log_Asset[Other_2018$Status == 'Active']) #rigetto normalità
t.test(Other_2018$Log_Asset[Other_2018$Status == 'Active'], Other_2018$Log_Asset[Other_2018$Status == 'Failed'])
#rigetto H0 

#Scarl comapred by dimension
ggplot(Scarl_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Scarl: Dimension Focus", x="Log_Dimensione")

ad.test(Scarl_2018$Log_Asset[Scarl_2018$Status == 'Active']) #rigetto normalità
t.test(Scarl_2018$Log_Asset[Scarl_2018$Status == 'Active'], Scarl_2018$Log_Asset[Scarl_2018$Status == 'Failed'])
#rigetto H0

#Scarlpa comapred by dimension
ggplot(Scarlpa_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Scarlpa: Dimension Focus", x="Log_Dimensione")

ad.test(Scarlpa_2018$Log_Asset[Scarlpa_2018$Status == 'Active']) #rigetto normalità
t.test(Scarlpa_2018$Log_Asset[Scarlpa_2018$Status == 'Active'], Scarlpa_2018$Log_Asset[Scarlpa_2018$Status == 'Failed'])
#rigetto H0



#Spa comapred by Età
ggplot(Spa_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Spa: Età Focus", x="Età")

ad.test(Spa_2018$Età[Spa_2018$Status == 'Active']) #rigetto normalità
t.test(Spa_2018$Età[Spa_2018$Status == 'Active'], Spa_2018$Età[Spa_2018$Status == 'Failed'])
#Rigetto ipotesi nulla

#Srl comapred by età
ggplot(Srl_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl: Età Focus", x="Età")

ad.test(Srl_2018$Età[Srl_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_2018$Età[Srl_2018$Status == 'Active'], Srl_2018$Età[Srl_2018$Status == 'Failed'])
#rigetto H0

#Srl one person comapred by Età
ggplot(Srl_One_P_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl one person: Età Focus", x="Età")

ad.test(Srl_One_P_2018$Età[Srl_One_P_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_One_P_2018$Età[Srl_One_P_2018$Status == 'Active'], Srl_One_P_2018$Età[Srl_One_P_2018$Status == 'Failed'])
#rigetto H0 all'1%

#Srl simplified comapred by Età
ggplot(Srl_S_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Srl simplified: Età Focus", x="Età")

ad.test(Srl_S_2018$Età[Srl_S_2018$Status == 'Active']) #rigetto normalità
t.test(Srl_S_2018$Età[Srl_S_2018$Status == 'Active'], Srl_S_2018$Età[Srl_S_2018$Status == 'Failed'])
#RIGETTO H0

#Social cooperative company comapred by Età
ggplot(Social_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Social cooperative company: Età Focus", x="Età")

ad.test(Social_2018$Età[Social_2018$Status == 'Active']) #rigetto H0
t.test(Social_2018$Età[Social_2018$Status == 'Active'], Social_2018$Età[Social_2018$Status == 'Failed'])
#rigetto H0

#Consortium comapred by Età
ggplot(Cons_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Consortium: Età Focus", x="Età")

ad.test(Cons_2018$Età[Cons_2018$Status == 'Active']) #rigetto normalità
t.test(Cons_2018$Età[Cons_2018$Status == 'Active'], Cons_2018$Età[Cons_2018$Status == 'Failed'])
# RIGETTO H0


#Other comapred by Età
ggplot(Other_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Other: Età Focus", x="Età")

ad.test(Other_2018$Età[Other_2018$Status == 'Active']) #rigetto normalità
t.test(Other_2018$Età[Other_2018$Status == 'Active'], Other_2018$Età[Other_2018$Status == 'Failed'])
#Accetto H0

#Scarl comapred by Età
ggplot(Scarl_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Scarl: Età Focus", x="Età")

ad.test(Scarl_2018$Età[Scarl_2018$Status == 'Active']) #rigetto normalità
t.test(Scarl_2018$Età[Scarl_2018$Status == 'Active'], Scarl_2018$Età[Scarl_2018$Status == 'Failed'])
#rigetto H0

#Scarlpa comapred by Età
ggplot(Scarlpa_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Scarlpa: Età Focus", x="Età")

ad.test(Scarlpa_2018$Età[Scarlpa_2018$Status == 'Active']) #rigetto normalità
t.test(Scarlpa_2018$Età[Scarlpa_2018$Status == 'Active'], Scarlpa_2018$Età[Scarlpa_2018$Status == 'Failed'])
#rigetto H0


#creo dataset per comparazione distribuzioni domanda A (LEGAL FORM)
df_2018_ATECOSplit <- split(df_2018_last, df_2018_last$ATECO)
A_2018 <- df_2018_ATECOSplit$A
BCDE_2018 <- df_2018_ATECOSplit$BCDE
F_2018 <- df_2018_ATECOSplit$F
GHI_2018 <- df_2018_ATECOSplit$GHI
J_2018 <- df_2018_ATECOSplit$J
K_2018 <- df_2018_ATECOSplit$K
L_2018 <- df_2018_ATECOSplit$L
MN_2018 <- df_2018_ATECOSplit$MN
OPQ_2018 <- df_2018_ATECOSplit$OPQ
RSTU_2018 <- df_2018_ATECOSplit$RSTU


#Section A comapred by dimension
ggplot(A_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section A Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(A_2018$Log_Asset[A_2018$Status == 'Active']) #rigetto normalità
t.test(A_2018$Log_Asset[A_2018$Status == 'Active'], A_2018$Log_Asset[A_2018$Status == 'Failed'])
#Rigetto ipotesi nulla

#Sction BCDE comapred by dimension
ggplot(BCDE_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section BCDE Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(BCDE_2018$Log_Asset[BCDE_2018$Status == 'Active']) #rigetto normalità
t.test(BCDE_2018$Log_Asset[BCDE_2018$Status == 'Active'], BCDE_2018$Log_Asset[BCDE_2018$Status == 'Failed'])
#rigetto H0

#Section F one person comapred by dimension
ggplot(F_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section F Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(F_2018$Log_Asset[F_2018$Status == 'Active']) #rigetto normalità
t.test(F_2018$Log_Asset[F_2018$Status == 'Active'], F_2018$Log_Asset[F_2018$Status == 'Failed'])
#rigetto H0

#Section GHI comapred by dimension
ggplot(GHI_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section GHI Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(GHI_2018$Log_Asset[GHI_2018$Status == 'Active']) #rigetto normalità
t.test(GHI_2018$Log_Asset[GHI_2018$Status == 'Active'], GHI_2018$Log_Asset[GHI_2018$Status == 'Failed'])
#rigetto H0

#Social cooperative company comapred by dimension
ggplot(J_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section J Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(J_2018$Log_Asset[J_2018$Status == 'Active']) #rigetto normalità
t.test(J_2018$Log_Asset[J_2018$Status == 'Active'], J_2018$Log_Asset[J_2018$Status == 'Failed'])
#rigetto H0

#Section K comapred by dimension
ggplot(K_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section K Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(K_2018$Log_Asset[K_2018$Status == 'Active']) #rigetto normalità
t.test(K_2018$Log_Asset[K_2018$Status == 'Active'], K_2018$Log_Asset[K_2018$Status == 'Failed'])
#rigetto H0


#Section L comapred by dimension
ggplot(L_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section L Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(L_2018$Log_Asset[L_2018$Status == 'Active']) #rigetto normalità
t.test(L_2018$Log_Asset[L_2018$Status == 'Active'], L_2018$Log_Asset[L_2018$Status == 'Failed'])
#rigetto H0

#Section MN comapred by dimension
ggplot(MN_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section MN Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(MN_2018$Log_Asset[MN_2018$Status == 'Active']) #rigetto normalità
t.test(MN_2018$Log_Asset[MN_2018$Status == 'Active'], MN_2018$Log_Asset[MN_2018$Status == 'Failed'])
#rigetto H0

#Section OPQ comapred by dimension
ggplot(OPQ_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section OPQ Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(OPQ_2018$Log_Asset[OPQ_2018$Status == 'Active']) #rigetto normalità
t.test(OPQ_2018$Log_Asset[OPQ_2018$Status == 'Active'], OPQ_2018$Log_Asset[OPQ_2018$Status == 'Failed'])
#rigetto H0

#Section RSTU comapred by dimension
ggplot(RSTU_2018, 
       aes(x=Log_Asset, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section RSTU Ateco: Dimension Focus", x="Log_Dimensione")

ad.test(RSTU_2018$Log_Asset[RSTU_2018$Status == 'Active']) #rigetto normalità
t.test(RSTU_2018$Log_Asset[RSTU_2018$Status == 'Active'], RSTU_2018$Log_Asset[RSTU_2018$Status == 'Failed'])
#rigetto H0


#Section A comapred by Età
ggplot(A_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section Ateco A: Età Focus", x="Età")

ad.test(A_2018$Età[A_2018$Status == 'Active']) #rigetto normalità
t.test(A_2018$Età[A_2018$Status == 'Active'], A_2018$Età[A_2018$Status == 'Failed'])
#Rigetto ipotesi nulla

#Section BCDE comapred by età
ggplot(BCDE_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section Ateco BCDE: Età Focus", x="Età")

ad.test(BCDE_2018$Età[BCDE_2018$Status == 'Active']) #rigetto normalità
t.test(BCDE_2018$Età[BCDE_2018$Status == 'Active'], BCDE_2018$Età[BCDE_2018$Status == 'Failed'])
#rigetto H0

#Section F one person comapred by Età
ggplot(F_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section Ateco F: Età Focus", x="Età")

ad.test(F_2018$Età[F_2018$Status == 'Active']) #rigetto normalità
t.test(F_2018$Età[F_2018$Status == 'Active'], F_2018$Età[F_2018$Status == 'Failed'])
#rigetto H0

#Section GHI simplified comapred by Età
ggplot(GHI_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section GHI Ateco: Età Focus", x="Età")

ad.test(GHI_2018$Età[GHI_2018$Status == 'Active']) #rigetto normalità
t.test(GHI_2018$Età[GHI_2018$Status == 'Active'], GHI_2018$Età[GHI_2018$Status == 'Failed'])
# RIGETTO H0

#Section J comapred by Età
ggplot(J_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section J Ateco: Età Focus", x="Età")

ad.test(J_2018$Età[J_2018$Status == 'Active']) #rigetto normalità
t.test(J_2018$Età[J_2018$Status == 'Active'], J_2018$Età[J_2018$Status == 'Failed'])
#Rigetto H0 al 5%

#Section K comapred by Età
ggplot(K_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section K: Età Focus", x="Età")

ad.test(K_2018$Età[K_2018$Status == 'Active']) #rigetto normalità
t.test(K_2018$Età[K_2018$Status == 'Active'], K_2018$Età[K_2018$Status == 'Failed'])
#Rigetto H0

#Section L comapred by Età
ggplot(L_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section L Ateco: Età Focus", x="Età")

ad.test(L_2018$Età[L_2018$Status == 'Active']) #rigetto H0
t.test(L_2018$Età[L_2018$Status == 'Active'], L_2018$Età[L_2018$Status == 'Failed'])
#rigetto H0

#Section MN comapred by Età
ggplot(MN_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section MN Ateco: Età Focus", x="Età")

ad.test(MN_2018$Età[MN_2018$Status == 'Active']) #rigetto normalità
t.test(MN_2018$Età[MN_2018$Status == 'Active'], MN_2018$Età[MN_2018$Status == 'Failed'])
#RIGETTO H0

#Section OPQ comapred by Età
ggplot(OPQ_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section OPQ Ateco: Età Focus", x="Età")

ad.test(OPQ_2018$Età[OPQ_2018$Status == 'Active']) #rigetto normalità
t.test(OPQ_2018$Età[OPQ_2018$Status == 'Active'], OPQ_2018$Età[OPQ_2018$Status == 'Failed'])
#rigetto H0

#Section RSTU comapred by Età
ggplot(RSTU_2018, 
       aes(x=Età, fill=Status)) +
  geom_density(alpha=0.4) + labs(title="Active and Failed Section RSTU Ateco: Età Focus", x="Età")

ad.test(RSTU_2018$Età[RSTU_2018$Status == 'Active']) #rigetto normalità
t.test(RSTU_2018$Età[RSTU_2018$Status == 'Active'], RSTU_2018$Età[RSTU_2018$Status == 'Failed'])
#Accetto H0


#Per svolgere la seconda parte della domanda A, andiamo a considerare non più solo l'anno 2018,
#ma bensì anche il 2019 e il 2017. In particolare ci concentreremo su un' analisi che riguarda
#solo le aziende fallite.

df_2017 <- data.frame(df_last_year_def[df_last_year_def$LastAccountingClosingDate %in% "2017",])
df_2016 <- data.frame(df_last_year_def[df_last_year_def$LastAccountingClosingDate %in% "2016",])
df_2016_2018 <- rbind(df_2016, df_2017, df_2018_last)
df_2016_2018$LastAccountingClosingDate <- as.character(df_2016_2018$LastAccountingClosingDate)
Failed_2016_2018 <- subset(df_2016_2018, Status == "Failed")
Failed_2017 <- subset(Failed_2016_2018, LastAccountingClosingDate == '2017')
Failed_2018 #lo stesso di prima
Failed_2016 <- subset(Failed_2016_2018, LastAccountingClosingDate == '2016')


#Failed Età density compared Over Time
ggplot(Failed_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Età density curve Over Time", x="Età")

#Test per vedere se la media è significativamente diversa.
#Prima verifico normalità
ad.test(Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2016']) #rigetto normalità
ad.test(Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2017']) #rigetto normalità

t.test(Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2016'],
       Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2017'])
#RIFIUTO IPOTESI NULLA
t.test(Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2016'],
       Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2018'])
#RIFIUTO IPOTESI NULLA
t.test(Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2017'],
       Failed_2016_2018$Età[Failed_2016_2018$LastAccountingClosingDate == '2018'])
#RIFIUTO IPOTESI NULLA

#Failed Dimension density compared Over Time
ggplot(Failed_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Dimension density curve Over Time", x="LogAsset")

#Test per vedere se la media è significativamente diversa.
#Prima verifico normalità
ad.test(Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2016']) #rigetto normalità
ad.test(Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2017']) #rigetto normalità

t.test(Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2016'],
       Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto H0
t.test(Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2016'],
       Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2018'])
#Rifiuto H0
t.test(Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2017'],
       Failed_2016_2018$Log_Asset[Failed_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto H0


#creo Dataset utili alla visualizzazione delle ulteriori analisi. (Regions)

Failed_2017_GroupedRegions <- data.frame(table(Failed_2017$Grouped_Regions))
Failed_2017_GroupedRegions$LastAccountingClosingDate <- '2017'
Failed_2017_GroupedRegions$Freq <- Failed_2017_GroupedRegions$Freq / sum(Failed_2017_GroupedRegions$Freq)
colnames(Failed_2017_GroupedRegions) <- c('GroupedRegions', 'Freq', 'LastAccountingClosingDate')

Failed_2018_GroupedRegions_new <- data.frame(table(Failed_2018$Grouped_Regions))
Failed_2018_GroupedRegions_new$LastAccountingClosingDate <- '2018'
Failed_2018_GroupedRegions_new$Freq <- Failed_2018_GroupedRegions_new$Freq / sum(Failed_2018_GroupedRegions_new$Freq)
colnames(Failed_2018_GroupedRegions_new) <- c('GroupedRegions', 'Freq', 'LastAccountingClosingDate')

Failed_2016_GroupedRegions <- data.frame(table(Failed_2016$Grouped_Regions))
Failed_2016_GroupedRegions$LastAccountingClosingDate <- '2016'
Failed_2016_GroupedRegions$Freq <- Failed_2016_GroupedRegions$Freq / sum(Failed_2016_GroupedRegions$Freq)
colnames(Failed_2016_GroupedRegions) <- c('GroupedRegions', 'Freq', 'LastAccountingClosingDate')

Grouped_Regions_graphic_new <- rbind(Failed_2018_GroupedRegions_new, Failed_2017_GroupedRegions, Failed_2016_GroupedRegions)

ggplot(Grouped_Regions_graphic_new, aes(fill=LastAccountingClosingDate, y=Freq, x=GroupedRegions)) + 
  geom_bar(position="dodge", stat="identity")

#creo Dataset utili alla visualizzazione delle ulteriori analisi. (ATECO)

Failed_2017_ATECO <- data.frame(table(Failed_2017$ATECO))
Failed_2017_ATECO$LastAccountingClosingDate <- '2017'
Failed_2017_ATECO$Freq <- Failed_2017_ATECO$Freq / sum(Failed_2017_ATECO$Freq)
colnames(Failed_2017_ATECO) <- c('ATECO', 'Freq', 'LastAccountingClosingDate')

Failed_2018_ATECO_new <- data.frame(table(Failed_2018$ATECO))
Failed_2018_ATECO_new$LastAccountingClosingDate <- '2018'
Failed_2018_ATECO_new$Freq <- Failed_2018_ATECO_new$Freq / sum(Failed_2018_ATECO_new$Freq)
colnames(Failed_2018_ATECO_new) <- c('ATECO', 'Freq', 'LastAccountingClosingDate')

Failed_2016_ATECO <- data.frame(table(Failed_2016$ATECO))
Failed_2016_ATECO$LastAccountingClosingDate <- '2016'
Failed_2016_ATECO$Freq <- Failed_2016_ATECO$Freq / sum(Failed_2016_ATECO$Freq)
colnames(Failed_2016_ATECO) <- c('ATECO', 'Freq', 'LastAccountingClosingDate')

ATECO_graphic_new <- rbind(Failed_2018_ATECO_new, Failed_2017_ATECO, Failed_2016_ATECO)

ggplot(ATECO_graphic_new, aes(fill=LastAccountingClosingDate, y=Freq, x=ATECO)) + 
  geom_bar(position="dodge", stat="identity")


#creo Dataset utili alla visualizzazione delle ulteriori analisi. (LegalForm)

Failed_2017_LegalForm <- data.frame(table(Failed_2017$LegalForm))
Failed_2017_LegalForm$LastAccountingClosingDate <- '2017'
Failed_2017_LegalForm$Freq <- Failed_2017_LegalForm$Freq / sum(Failed_2017_LegalForm$Freq)
colnames(Failed_2017_LegalForm) <- c('LegalForm', 'Freq', 'LastAccountingClosingDate')

Failed_2018_LegalForm_new <- data.frame(table(Failed_2018$LegalForm))
Failed_2018_LegalForm_new$LastAccountingClosingDate <- '2018'
Failed_2018_LegalForm_new$Freq <- Failed_2018_LegalForm_new$Freq / sum(Failed_2018_LegalForm_new$Freq)
colnames(Failed_2018_LegalForm_new) <- c('LegalForm', 'Freq', 'LastAccountingClosingDate')

Failed_2016_LegalForm <- data.frame(table(Failed_2016$LegalForm))
Failed_2016_LegalForm$LastAccountingClosingDate <- '2016'
Failed_2016_LegalForm$Freq <- Failed_2016_LegalForm$Freq / sum(Failed_2016_LegalForm$Freq)
colnames(Failed_2016_LegalForm) <- c('LegalForm', 'Freq', 'LastAccountingClosingDate')

LegalForm_graphic_new <- rbind(Failed_2018_LegalForm_new, Failed_2017_LegalForm, Failed_2016_LegalForm)

ggplot(LegalForm_graphic_new, aes(fill=LastAccountingClosingDate, y=Freq, x=LegalForm)) + 
  geom_bar(position="dodge", stat="identity")


#creo dataset per comparazione distribuzioni domanda A (LEGAL FORM)
Failed_2016_2018_LegalFormSplit <- split(Failed_2016_2018, Failed_2016_2018$LegalForm)
Spa_2016_2018 <- Failed_2016_2018_LegalFormSplit$S.P.A.
Srl_2016_2018 <- Failed_2016_2018_LegalFormSplit$S.R.L.
Srl_One_P_2016_2018 <- Failed_2016_2018_LegalFormSplit$`S.R.L. one-person`
Srl_S_2016_2018 <- Failed_2016_2018_LegalFormSplit$`S.R.L. simplified`
Social_2016_2018 <- Failed_2016_2018_LegalFormSplit$`Social cooperative company`
Other_2016_2018 <- Failed_2016_2018_LegalFormSplit$Other
Cons_2016_2018 <- Failed_2016_2018_LegalFormSplit$Consortium
Scarl_2016_2018 <- Failed_2016_2018_LegalFormSplit$S.C.A.R.L.
Scarlpa_2016_2018 <- Failed_2016_2018_LegalFormSplit$S.C.A.R.L.P.A.

#CONSIDERANDO LOG DIMENSIONE
#Spa 
ggplot(Spa_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Spa: Dimension Focus over time", x="Log_Dimensione")

ad.test(Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2016'],
       Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla 

t.test(Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2016'],
       Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto H0

t.test(Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2017'],
       Spa_2016_2018$Log_Asset[Spa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#SRL
ggplot(Srl_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl: Dimension Focus over time", x="Log_Dimensione")

ad.test(Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_2016_2018$Log_Asset[Srl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#SRL One Person
ggplot(Srl_One_P_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl One Person: Dimension Focus over time", x="Log_Dimensione")

ad.test(Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

t.test(Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_One_P_2016_2018$Log_Asset[Srl_One_P_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla


#SRL Simplified
ggplot(Srl_S_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl Simplified: Dimension Focus over time", x="Log_Dimensione")

ad.test(Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2017'],  var.equal = TRUE)
#Rigetto ipotesi nulla

t.test(Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_S_2016_2018$Log_Asset[Srl_S_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

#Social Cooperative companies
ggplot(Social_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Social Cooperative Companies: Dimension Focus over time", x="Log_Dimensione")

ad.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2016']) #Accetto H0
ad.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0
ad.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2018']) #Accetto H0

#essenso le distribuzioni legate al 2016, 2017 e al 2018 risultate significativamente distribuite come una normale
#eseguo l'F test per valutare se esse hanno la solita varianza al fine di utilizzare poi il t test per confrontarne la media
var.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2016'],
         Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2018']) #Rigetto H0


t.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2016'],
       Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2016'],
       Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

t.test(Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2017'],
       Social_2016_2018$Log_Asset[Social_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

#Other
ggplot(Other_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Others Companies: Dimension Focus over time", x="Log_Dimensione")

ad.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2016']) #Accetto H0 al 2%
ad.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2017']) #Accetto H0
ad.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2018']) #Rigetto H0

#essenso le distribuzioni legate al 2016 e 2017 risultate significativamente distribuite come una normale
#eseguo l'F test per valutare se esse hanno la solita varianza al fine di utilizzare poi il t test per confrontarne la media
var.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2016'],
         Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2017']) #Accetto H0
var.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2016'],
         Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2018']) #Rifiuto H0
var.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2017'],
         Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2018']) #Accetto H0
t.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2016'],
       Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2016'],
       Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

t.test(Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2017'],
       Other_2016_2018$Log_Asset[Other_2016_2018$LastAccountingClosingDate == '2018'], var.equal = TRUE)
#Accetto ipotesi nulla

#Scarlpa
ggplot(Scarlpa_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Scarlpa: Dimension Focus over time", x="Log_Dimensione")

ad.test(Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2016'],
       Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla all'1%

t.test(Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2016'],
       Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

t.test(Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2017'],
       Scarlpa_2016_2018$Log_Asset[Scarlpa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#Scarl
ggplot(Scarl_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Scarl: Dimension Focus over time", x="Log_Dimensione")

ad.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2016']) #Accetto H0 al 2%
ad.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0 
ad.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2018']) #Rigetto H0

t.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2016'],
       Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla 

t.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2016'],
       Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2017'],
       Scarl_2016_2018$Log_Asset[Scarl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#CONSIDERANDO ETà
#Spa 
ggplot(Spa_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Spa: Età Focus over time", x="Log_Dimensione")

ad.test(Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2016'],
       Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2016'],
       Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2017'],
       Spa_2016_2018$Età[Spa_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla al 1%

#SRL
ggplot(Srl_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl: Età Focus over time", x="Log_Dimensione")

ad.test(Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_2016_2018$Età[Srl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#SRL One Person
ggplot(Srl_One_P_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl One Person: Età Focus over time", x="Log_Dimensione")

ad.test(Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_One_P_2016_2018$Età[Srl_One_P_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#SRL Simplified
ggplot(Srl_S_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Srl Simplified: Età Focus over time", x="Log_Dimensione")

ad.test(Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2016'],
       Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2017'],
       Srl_S_2016_2018$Età[Srl_S_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla 

#Social Cooperative companies
ggplot(Social_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Social Cooperative Companies: Età Focus over time", x="Log_Dimensione")

ad.test(Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2016']) #Accetto H0
ad.test(Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2017']) #Accetto H0

t.test(Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2016'],
       Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2016'],
       Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2017'],
       Social_2016_2018$Età[Social_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

#Other
ggplot(Other_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Others Companies: Età Focus over time", x="Log_Dimensione")

ad.test(Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2016']) #Accetto H0
ad.test(Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2016'],
       Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2016'],
       Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

t.test(Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2017'],
       Other_2016_2018$Età[Other_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla

#Scarlpa
ggplot(Scarlpa_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Scarlpa: Età Focus over time", x="Log_Dimensione")

ad.test(Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2016'],
       Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2016'],
       Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2017'],
       Scarlpa_2016_2018$Età[Scarlpa_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#Scarl
ggplot(Scarl_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed Scarl: Dimension Focus over time", x="Log_Dimensione")

ad.test(Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2016'],
       Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2016'],
       Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2017'],
       Scarl_2016_2018$Età[Scarl_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto ipotesi nulla


#creo dataset per comparazione distribuzioni domanda A (LEGAL FORM)
Failed_2016_2018_GroupedRegionSplit <- split(Failed_2016_2018, Failed_2016_2018$Grouped_Regions)
Settentrione_2016_2018 <- Failed_2016_2018_GroupedRegionSplit$`Italia Settentrionale`
Meridione_2016_2018 <- Failed_2016_2018_GroupedRegionSplit$`Italia Meridionale`
Centro_2016_2018 <- Failed_2016_2018_GroupedRegionSplit$`Italia Centrale`
Insulare_2016_2018 <- Failed_2016_2018_GroupedRegionSplit$`Italia Insulare`


#CONSIDERANDO LOG DIMENSIONE
#Italia Settentrionale
ggplot(Settentrione_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Italia Settentrionale Dimension Focus over time", x="Log_Dimensione")

ad.test(Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2016'],
       Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2017'])
#Rifiuto ipotesi nulla

t.test(Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2016'],
       Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2018'])
#Accetto H0

t.test(Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2017'],
       Settentrione_2016_2018$Log_Asset[Settentrione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#Italia Centrale
ggplot(Centro_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Centro Italia: Dimension Focus over time", x="Log_Dimensione")

ad.test(Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2016'],
       Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla al 4%

t.test(Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2016'],
       Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla al 4%

t.test(Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2017'],
       Centro_2016_2018$Log_Asset[Centro_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#Italia Meridionale
ggplot(Meridione_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Italia Meridionale: Dimension Focus over time", x="Log_Dimensione")

ad.test(Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0 

t.test(Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2016'],
       Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2017'], var.equal = TRUE)
#Accetto ipotesi nulla

t.test(Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2016'],
       Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2017'],
       Meridione_2016_2018$Log_Asset[Meridione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#Italia Insulare
ggplot(Insulare_2016_2018, 
       aes(x=Log_Asset, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed comapnies in Italia Insulare: Dimension Focus over time", x="Log_Dimensione")

ad.test(Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2016'],
       Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2016'],
       Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2018'], var.equal = TRUE)
#Rigetto ipotesi nulla

t.test(Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2017'],
       Insulare_2016_2018$Log_Asset[Insulare_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla 


#CONSIDERANDO Età

#Italia Settentrionale
ggplot(Settentrione_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Italia Settentrionale: Età Focus over time", x="Log_Dimensione")

ad.test(Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2016'],
       Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla

t.test(Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2016'],
       Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto H0

t.test(Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2017'],
       Settentrione_2016_2018$Età[Settentrione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#Italia Centrale
ggplot(Centro_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Centro Italia: Età Focus over time", x="Log_Dimensione")

ad.test(Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2016'],
       Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2017'])
#Accetto ipotesi nulla al 4%

t.test(Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2016'],
       Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2017'],
       Centro_2016_2018$Età[Centro_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

#Italia Meridionale
ggplot(Meridione_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed companies in Italia Meridionale: Età Focus over time", x="Log_Dimensione")

ad.test(Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0 

t.test(Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2016'],
       Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2016'],
       Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2017'],
       Meridione_2016_2018$Età[Meridione_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


#Italia Insulare
ggplot(Insulare_2016_2018, 
       aes(x=Età, fill=LastAccountingClosingDate)) +
  geom_density(alpha=0.4) + labs(title="Failed comapnies in Italia Insulare: Dimension Focus over time", x="Log_Dimensione")

ad.test(Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2016']) #Rigetto H0
ad.test(Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2017']) #Rigetto H0

t.test(Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2016'],
       Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2017'])
#Rigetto ipotesi nulla

t.test(Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2016'],
       Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla

t.test(Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2017'],
       Insulare_2016_2018$Età[Insulare_2016_2018$LastAccountingClosingDate == '2018'])
#Rigetto ipotesi nulla


