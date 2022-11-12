# clean all
rm(list=ls())

#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

# working dir
wdir = "C:/Users/lucap/Desktop/università/DATA_SCIENCE/STAT_DAT/Stat_Meth_DS/Project/Project/smdproject2021/"
setwd(wdir)

# to reload the whole dataset at once, run
load(file="aida.RData")
df <- aida
head(df)
summary(df)

# default: bankruptcy, dissolved, in liquidation; 
# active: active, def of pay, receiv.

df$`Status` <- ifelse(aida$`Legal status` == "Bankruptcy" | aida$`Legal status` == "In liquidation" | aida$`Legal status` == "Dissolved (liquidation)" | aida$`Legal status` == "Dissolved" | aida$`Legal status` == "Dissolved (merger)"| aida$`Legal status` == "Dissolved (bankruptcy)",'Failed','Active')


#SAVE.IMAGE PER SALVARE LE VARIABILI CARICATE
#ATTACH(NOME DATAFRAME PER CHIAMARE LE VARIABILI DIRETTAMENTE)
#tapply
#sapply                     IMPLICIT LOOPS
#lapply
#apply
#replicate
#RETICULATE

df$DimensioneAzienda <- NA
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` <=10 & df$`Total assetsth EURLast avail. yr` <= 20000] <- 'Micro'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` >10 & df$`Number of employeesLast avail. yr` <= 50 &
                                     df$`Total assetsth EURLast avail. yr` > 20000 & df$`Total assetsth EURLast avail. yr` <= 100000] <- 'Small'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` <= 10 &
                                     df$`Total assetsth EURLast avail. yr` > 20000 & df$`Total assetsth EURLast avail. yr` <= 100000] <- 'Small'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` >10 & df$`Number of employeesLast avail. yr` <= 50 &
                                     df$`Total assetsth EURLast avail. yr` <=20000] <- 'Small'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` <= 10 &
                                     df$`Total assetsth EURLast avail. yr` >100000] <- 'Medium - Large'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` >50 & df$`Total assetsth EURLast avail. yr` > 100000] <- 'Medium - Large'
df$DimensioneAzienda[df$`Number of employeesLast avail. yr` >10 & df$`Number of employeesLast avail. yr` <=50 & 
                                    df$`Total assetsth EURLast avail. yr` > 100000] <- 'Medium - Large'
table(df$DimensioneAzienda)
#Adesso andiamo a crerare la variabile che ci regola l'età dell'aziende all'interno del nostro dataset.
#Questo lo si fa andando a sotrarre alla variabile last accounting closing date la variabile in corporation year.

df$Età <- df$`Last accounting closing date`- df$`Incorporation year`
summary(df$Età)

#istogramma dimostra criticità dei valori nel dominio della variabile (sensato per valori uguali o maggiori di 0)
#Si decide di attribuire dunque un valore nullo a quelle osservazioni < 0 così da essere poi successivamente rimosse
df$Età[df$Età < 0] <- NA

library(ggplot2)
ggplot(df, aes(x=Età)) + 
  geom_histogram(binwidth=4, fill="#69b3a2", color="#e9ecef", alpha=0.9)
summary(df$Età)

#DIvidiamo il dataset tenendo in cosniderazione solo un numero limitato di variabili
#ritenute rilevanti per le nostre analisi e operiamo un'ulteriore suddivisione
#tenendo in considerazione prima solo l'ultimo anno di rendicontazione, e poi gli ultimi 3
df_last_year <- df[,c(1,5,12,15,24,27,32,35,36,38,47,50,53,58,59,62,71,74,78,81,82,83 )]
df_Meno1 <- df[,c(1,6,13,16,25,28,33,35,36,39,48,51,54,58,60,63,72,74,79,81,82,83)]
table(df_last_year$Status)
summary(df_last_year)
#Andiamo adesso a trattare i missing values

#Liberia VIM utile per la visualizzazione dei missing values 
library(VIM)
aggr_plot <- aggr(df_last_year[2:5], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df_last_year), cex.axis=.5, gap=1, ylab=c("Histogram of missing data","Pattern"))
aggr_plot$missings

#Importo libreria Hmisc per migliorare le possibilità di visualizzazione del mio dataset
library(Hmisc)
md.pattern(df_last_year)

#Visualizzazione a mio parere molto interessante offerta da MICE. Tale output ci dice per 
#quanti valori si ha che le relative colonne non presentano missing value (1) oppure se 
#presentano missing value (0). Il valore fornito nell'ultima colonna dell'ultima parte (quella che
#inizia con 0 1 1 2 1 2...) ci dice quante colonne presentano missing value per quel determinato
#numero di valori nel dataset.


#LIBRERIA VIM: molto utile per visuallizare meglio missing values e dati imputati
#I due boxplot a sinistra e in basso stanno a comparare la distribuzione dei valori osservati
#in entrambe le variabili (quelli in blu) e tra i valori osservati solo in una variabile
#(quelli a lato e di sotto in rosso). Inoltre i numeri ci stanno ad indicare quanti missing ci
#sono per l'attributo su x, su y e quanti in comune.
#Per avere dati MAR o MCAR ci aspettiamo che i due boxplot siano molto simili (blu e rosso).
library(VIM)
marginplot(df_last_year[c(12,4)])
marginplot(df_last_year[c(5,6)])
pbox(df_last_year[,c(13,14,15)], pos = 3)

#comincio lavoro per sostituire i missing
library(mice)
colnames(df_last_year)<- c('ATECO','CashFlow','CurrentLiabOnTotAsset','CurrentRatioLastYear',
                                                     'EBITDAOnVenditeLastYear','EBITDALastYear','InterestOnTurnoverLastYear',
                                                     'LastAccountingClosingDate','LegalForm','LeverageLastYear',
                                                     'NWC','numberOfEmployeesLastYear','ProfitLastYear','RegisteredOfficeAddress',
                                                     'ROALastYear','ROELastYear','SolvencyRatioLastYear','TaxCodeNumber',
                                                     'TotAssetLastYear','Status','DimensioneAzienda','Età')
df_mice_run <- mice(df_last_year[,-c(1, 8, 9, 14, 18, 20)],m=3,maxit=5,meth='pmm',seed=42)


#Plot della densità utile a valutare la bontà dei dati imputati.
#stripplot(df_mice_run, pch = 20, cex = 1.2)

plot(density(df_mice_run$data$ROE, na.rm = T), lwd=2)
lines(density(df_mice_run$imp$ROE$`1`), col='red')
lines(density(df_mice_run$imp$ROE$`2`), col='green')
lines(density(df_mice_run$imp$ROE$`3`), col='blue')


#prendo il dataset creato n 
df_last_year_new<- complete(df_mice_run, 1)
df_last_year_new <- cbind(df_last_year[,c(1, 8, 9, 14, 18, 20)], df_last_year_new)
#elimino quei pochi missing values che erano contenuti nei factors
df_last_year_new <- na.omit(df_last_year_new)

plot(density(df_last_year_new$EBITDALastYear), lwd=2)
lines(density(df_last_year$EBITDALastYear, na.rm = T), col='red')
t.test(df_last_year_new$EBITDALastYear,
       df_last_year$EBITDALastYear)
plot(density(df_last_year_new$ROELastYear), lwd=2)
lines(density(df_last_year$ROELastYear, na.rm = T), col='red')
t.test(df_last_year_new$ROELastYear,
       df_last_year$ROELastYear)


#Adesso ricerco eventuali outliers.
# Divido entrambi i dataset tenendo in considerazione la dimensione dell'azienda
# Questo perchè voglio evitare che tutte le aziende di grande dimensione siano considerate
# come outliers
library(BBmisc)
df_split_dimensione <- split(df_last_year_new, df_last_year_new$DimensioneAzienda)
df_split_micro <- df_split_dimensione$Micro
df_split_Small <- df_split_dimensione$Small
df_split_Medium_Large <- df_split_dimensione$`Medium - Large`

#Adesso andiamo ad importare una libreria che ci possa aiutare nel nostro intento.
#L'idea è quello di utilizzare il LOF, che attraverso il concetto di reachability
#distance ci aiuta a identificare gli outliers. Tutte quelle osservazioni che hanno
#un LOF sensibilmente maggiore di 1 saranno possibili outliers.
library(outForest)

#SE NON SI VUOLE VISUALIZZARE SULLA CONSOLE L'AVANZARE DEI PROCESSI DI OUTFOREST,
#IMPOSTARE VERBOSA=0.

#Calcolo outliers per micro aziende del dataset con solo last year
out <- outForest(df_split_micro[,-c(1,5)], splitrule = "extratrees", 
                 num.trees = 5, verbose = 1, seed=500, replace = "pmm",
                 threshold=5, max_prop_outliers = 0.01, max_n_outliers = 15000)

out$n_outliers
out$outliers
out$outliers$replacement
plot(out)
plot(out, what = "scores")

df_micro <-cbind(out$Data, df_split_micro[,c(1,5)])


#Calcolo outliers per no micro aziende del dataset con solo last year
out1 <- outForest(df_split_Small[,-c(1,5)], splitrule = "extratrees", 
                  num.trees=5, verbose = 1, seed=500, replace = "pmm",
                  threshold=5, max_prop_outliers = 0.01, max_n_outliers = 1780)

out1$n_outliers
out1$outliers
out1$outliers$replacement
plot(out1)
plot(out1, what = "scores")

df_Small <- cbind(out1$Data, df_split_Small[,c(1,5)])


#Calcolo outliers per micro aziende del dataset con tutti gli anni
out2 <- outForest(df_split_Medium_Large[,-c(1,5)], splitrule = "extratrees", 
                     num.trees= 5, verbose = 1, seed=500, replace = "pmm",
                     threshold=5, max_prop_outliers = 0.01, max_n_outliers = 64)

out2$n_outliers
out2$outliers
out2$outliers$replacement
plot(out2)
plot(out2, what = "scores")

df_Medium_Large <- cbind(out2$Data, df_split_Medium_Large[,c(1,5)])

df_last_year_def <- rbind(df_micro, df_Small, df_Medium_Large)

save(df_last_year_def,file="DfLastYear.R")
save(df_Meno1, file= "Df_Meno1.R")

load(file = "DfLastYear.R")
#Andiamo a raggruppare i vari codici ATECO per divisioni seguendo l'Istat
df_last_year_def$ATECO <- as.character(df_last_year_def$ATECO)

df_last_year_def$ATECO[df_last_year_def$ATECO >= '000000' & df_last_year_def$ATECO <= '040000'] <- 'A'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '050000' & df_last_year_def$ATECO <= '099999'] <- 'B'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '100000' & df_last_year_def$ATECO <= '331999'] <- 'C'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '332000' & df_last_year_def$ATECO <= '359999'] <- 'D'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '360000' & df_last_year_def$ATECO <= '399999'] <- 'E'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '410000' & df_last_year_def$ATECO <= '440000'] <- 'F'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '450000' & df_last_year_def$ATECO <= '480000'] <- 'G'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '490000' & df_last_year_def$ATECO <= '540000'] <- 'H'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '550000' & df_last_year_def$ATECO <= '570000'] <- 'I'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '580000' & df_last_year_def$ATECO <= '639999'] <- 'J'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '640000' & df_last_year_def$ATECO <= '670000'] <- 'K'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '680000' & df_last_year_def$ATECO <= '689999'] <- 'L'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '690000' & df_last_year_def$ATECO <= '760000'] <- 'M'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '770000' & df_last_year_def$ATECO <= '830000'] <- 'N'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '840000' & df_last_year_def$ATECO <= '849999'] <- 'O'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '850000' & df_last_year_def$ATECO <= '859999'] <- 'P'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '860000' & df_last_year_def$ATECO <= '890000'] <- 'Q'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '900000' & df_last_year_def$ATECO <= '939999'] <- 'R'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '940000' & df_last_year_def$ATECO <= '969999'] <- 'S'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '970000' & df_last_year_def$ATECO <= '989999'] <- 'T'
df_last_year_def$ATECO[df_last_year_def$ATECO >= '990000' & df_last_year_def$ATECO <= '999999'] <- 'U'

#Aumento ancora la granularità seguendo le disposizioni dell'istat
df_last_year_def$ATECO[df_last_year_def$ATECO == 'B' | df_last_year_def$ATECO == 'C' |
                         df_last_year_def$ATECO == 'D' | df_last_year_def$ATECO == 'E'] <- 'BCDE'
df_last_year_def$ATECO[df_last_year_def$ATECO == 'G' | df_last_year_def$ATECO == 'H' |
                         df_last_year_def$ATECO == 'I'] <- 'GHI'
df_last_year_def$ATECO[df_last_year_def$ATECO == 'M' | df_last_year_def$ATECO == 'N'] <- 'MN'
df_last_year_def$ATECO[df_last_year_def$ATECO == 'O' | df_last_year_def$ATECO == 'P' |
                         df_last_year_def$ATECO == 'Q'] <- 'OPQ'
df_last_year_def$ATECO[df_last_year_def$ATECO == 'R' | df_last_year_def$ATECO == 'S' |
                         df_last_year_def$ATECO == 'T' | df_last_year_def$ATECO == 'U'] <- 'RSTU'

df_last_year_def$ATECO <- as.factor(df_last_year_def$ATECO)
table(df_last_year_def$ATECO)

#Vado ad aumentare anche la granularità delle regioni italiane.
df_last_year_def$Grouped_Regions <- df_last_year_def$RegisteredOfficeAddress
df_last_year_def$Grouped_Regions <- as.character(df_last_year_def$Grouped_Regions)

df_last_year_def$Grouped_Regions[df_last_year_def$Grouped_Regions == 'Emilia-Romagna' | df_last_year_def$Grouped_Regions == 'Friuli-Venezia Giulia' | df_last_year_def$Grouped_Regions == 'Liguria' | df_last_year_def$Grouped_Regions == 'Lombardia' | df_last_year_def$Grouped_Regions == 'Piemonte' | df_last_year_def$Grouped_Regions == 'Trentino-Alto Adige' | df_last_year_def$Grouped_Regions == "Valle d'Aosta/Vallée d'Aoste"| df_last_year_def$Grouped_Regions == 'Veneto'] <- 'Italia Settentrionale'
df_last_year_def$Grouped_Regions[df_last_year_def$Grouped_Regions == 'Lazio' | df_last_year_def$Grouped_Regions == 'Marche' | df_last_year_def$Grouped_Regions == 'Toscana' | df_last_year_def$Grouped_Regions == 'Umbria'] <- 'Italia Centrale'
df_last_year_def$Grouped_Regions[df_last_year_def$Grouped_Regions == 'Abruzzo' | df_last_year_def$Grouped_Regions == 'Basilicata' | df_last_year_def$Grouped_Regions == 'Campania' | df_last_year_def$Grouped_Regions == 'Calabria' | df_last_year_def$Grouped_Regions == 'Molise' | df_last_year_def$Grouped_Regions == 'Puglia']  <- 'Italia Meridionale'
df_last_year_def$Grouped_Regions[df_last_year_def$Grouped_Regions == 'Sardegna' | df_last_year_def$Grouped_Regions == 'Sicilia'] <- 'Italia Insulare'
table(df_last_year_def$Grouped_Regions)


#mi creo anche una variabile che tenga conto del Log dei Tot Asset. Verrà utilizzata per l'analisi sulle dimensioni delle aziende.
df_last_year_def$Log_Asset <- log(df_last_year_def$TotAssetLastYear)

#Concentro le minoranze delle fore legali in Other
df_last_year_def[df_last_year_def == 'Association'] <- 'Other'
df_last_year_def[df_last_year_def == 'Foundation'] <- 'Other'
df_last_year_def[df_last_year_def == 'Public agency'] <- 'Other'
df_last_year_def[df_last_year_def == 'S.C.A.R.I.'] <- 'Other'
df_last_year_def[df_last_year_def == 'S.A.S.'] <- 'Other'
df_last_year_def[df_last_year_def == 'Mutual aid society'] <- 'Other'
df_last_year_def[df_last_year_def == 'S.A.P.A.'] <- 'Other'
df_last_year_def[df_last_year_def == 'Foreign company'] <- 'Other'
df_last_year_def[df_last_year_def == 'S.N.C.'] <- 'Other'
df_last_year_def$LegalForm <- droplevels(df_last_year_def$LegalForm) 
table(df_last_year_def$LegalForm)


#Salvo un ulteriore dataset nella directory utile per tutta la domanda A
save(df_last_year_def,file="DfLastYear_AB.R")