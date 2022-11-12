# clean al
rm(list=ls())

wdir = "C:/Users/lucap/Desktop/università/DATA_SCIENCE/STAT_DAT/Stat_Meth_DS/Project/Project/smdproject2021/"
setwd(wdir)

load(file = 'Df_Meno1.R')

#### é POSSIBILE UTILIZZARE LA FUBZIONE LOAD ALLA RIGA 96
#Adesso come precedentemente, sostituisco i missing values con la libreria MICE

library(mice)
colnames(df_Meno1) <- c('ATECO','CashFlowMENO1','CurrentLiabOnTotAssetMENO1','CurrentRatioMENO1',
                        'EBITDAOnVenditeMENO1','EBITDAMENO1','InterestOnTurnoverMENO1',
                        'LastAccountingClosingDate','LegalForm','LeverageMENO1',
                        'NWCMENO1','numberOfEmployeesLastYearMENO1','ProfitMENO1','RegisteredOfficeAddress',
                        'ROAMENO1','ROEMENO1','SolvencyRatioMENO1','TaxCodeNumber',
                        'TotAssetMENO1','Status','DimensioneAzienda','Età')
df_mice_run <- mice(df_Meno1[,-c(1, 8, 9, 14, 18, 20)],m=3,maxit=5,meth='pmm',seed=42)

#prendo il dataset creato n 
df_Meno1_new<- complete(df_mice_run, 1)
df_Meno1<- cbind(df_Meno1[,c(1, 8, 9, 14, 18, 20)], df_Meno1_new)
#elimino quei pochi missing values che erano contenuti nei factors
df_Meno1<- na.omit(df_Meno1)
colnames(df_Meno1) <- c('ATECO','LastAccountingClosingDate','LegalForm','RegisteredOfficeAddress',
                        'TaxCodeNumber','Status','CashFlowMENO1','CurrentLiabOnTotAssetMENO1',
                        'CurrentRatioMENO1','EBITDAOnVenditeMENO1','EBITDAMENO1',
                        'InterestOnTurnoverMENO1','LeverageMENO1',
                        'NWCMENO1','numberOfEmployeesLastYearMENO1','ProfitMENO1',
                        'ROAMENO1','ROEMENO1','SolvencyRatioMENO1',
                        'TotAssetMENO1','DimensioneAzienda','Età')
#Adesso ricerco eventuali outliers.
# Divido entrambi i dataset tenendo in considerazione la dimensione dell'azienda
# Questo perchè voglio evitare che tutte le aziende di grande dimensione siano considerate
# come outliers
library(BBmisc)
df_split_dimensione <- split(df_Meno1, df_Meno1$DimensioneAzienda)
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

df_Meno1_trattato <- rbind(df_micro, df_Small, df_Medium_Large)

save(df_Meno1_trattato, file= "Df_Meno1_Trattato.R")
load(file = 'Df_Meno1_Trattato.R')


#Adesso cominciamo la costruzione del modello
#L idea è quella di utilizzare il dataset Df_Meno 1 per allenare il nostro modello,
#e di utilizzare come test solo l ultimo anno di dati (dataset che importo dopo)
#Inizio innanzitutto a valutare la correlazione tra le variabili
#Per farlo testo la normalità o meno della distribuzione dei dati.
df_Meno1 <- df_Meno1_trattato
library(nortest)
ad.test(df_Meno1$CashFlowMENO1)
ad.test(df_Meno1$CurrentLiabOnTotAssetMENO1)
ad.test(df_Meno1$CurrentRatioMENO1)
ad.test(df_Meno1$EBITDAOnVenditeMENO1)
ad.test(df_Meno1$EBITDAMENO1)
ad.test(df_Meno1$InterestOnTurnoverMENO1)
ad.test(df_Meno1$LeverageMENO1)
ad.test(df_Meno1$NWCMENO1)
ad.test(df_Meno1$numberOfEmployeesLastYearMENO1)
ad.test(df_Meno1$ProfitMENO1)
ad.test(df_Meno1$ROAMENO1)
ad.test(df_Meno1$ROEMENO1)
ad.test(df_Meno1$SolvencyRatioMENO1)
ad.test(df_Meno1$TotAssetMENO1)
ad.test(df_Meno1$Età)

#Com'era prevedibile i dati non sono distribuiti normalmente, dunque non utilizzeremo il pearson method.
#decidiamo di utilizzare il metodo di SPearman

res_cor <- cor(df_Meno1[,-c(1,2,3,4,21,22,19)], method= 'spearman')
res_cor[res_cor <0.5] <- NA

#Essendoci molte variabili correlate tra di loro ed volendo diminuire la numerosità della variabili indipendenti,
#Andiamo a creare un nuovo dataset allo scopo di costruire il train
Train <- df_Meno1
Train$LastAccountingClosingDate <- NULL
Train$LegalForm <- NULL
Train$RegisteredOfficeAddress <- NULL
Train$ATECO <- NULL
Train$DimensioneAzienda <- NULL
Train$TaxCodeNumber <- NULL
Train$ProfitMENO1 <- NULL
Train$ROAMENO1 <- NULL
Train$NWCMENO1 <- NULL
Train$EBITDAMENO1 <- NULL
#Essendoci molte variabili altamente correlate tra loro, devo procedere evitando al massimo il problema della multicollinearità.
#Per evitarlo si utilizzerà in comparazione due metodi diversi, uno che sfrutta l'elasticnet Logit e l'altra che invece sfrutta
#ls dtepwise regression

#Prima però imposto status come variabile binaria 0, 1
Train$Status[Train$Status == 'Active'] <- 0
Train$Status[Train$Status == 'Failed'] <- 1
Train$Status <- as.numeric(Train$Status)

res_cor2 <-cor(Train, method = 'spearman')

library(corrplot)
library(RColorBrewer)
corrplot(res_cor2, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


colnames(Train) <- c('Status','CashFlow','CurrentLiabOnTotAsset',
                        'CurrentRatio','EBITDAOnVendite',
                        'InterestOnTurnover','Leverage','numberOfEmployees',
                        'ROE','SolvencyRatio',
                        'TotAsset','Età')

library(car)
fit = glm(Status~., data=Train ,family=binomial("logit"))

summary(fit)
confint(fit)
exp(coef(fit))

#Adesso mi creo gli intrevalli di confidenza non più semplicemente per i coefficienti, ma bensì per gli Odds
exp(cbind(OR = coef(fit), confint(fit)))

#Per controllare se posso ancora migliorare la mia regressione da un punto di vista della muticollinearità,
#utilizzo il variance inflation factor, considering a cutoff of 5
vif(fit)
#Noone shown a value which go over the treshold of 5!


# variable selection: stepwise regression 
library(MASS)
step = stepAIC(fit, direction="backward")
summary(step)
#Anche con la funzione stepAIC troviamo i medesimi risultati. 
#Andiamo allora  valutare la bontà del nostro modello.


load(file = 'DfLastYear.R')
df_last_year <- df_last_year_def[,-c(1,2,3,9,12,14,15,19,21,22)]
colnames(df_last_year) <- c('Status','CashFlow','CurrentLiabOnTotAsset',
                     'CurrentRatio','EBITDAOnVendite',
                     'InterestOnTurnover','Leverage','numberOfEmployees',
                     'ROE','SolvencyRatio',
                     'TotAsset','Età')
df_last_year$Status[df_last_year$Status == 'Active'] <- 0
df_last_year$Status[df_last_year$Status == 'Failed'] <- 1
df_last_year$Status <- as.numeric(df_last_year$Status)

library(caret)
#TRAIN FIT
prediction_train <- predict(fit, newdata = Train, type = "response")
prediction_train_bin <- ifelse(prediction_train > 0.5,1,0)
confusionMatrix(data = factor(as.numeric(prediction_train_bin>0.5)), reference = factor(Train$Status), positive = '1')

#TEST FIT
prediction_test <- predict(fit, newdata = df_last_year, type = "response")
prediction_test_bin <- ifelse(prediction_test > 0.5,1,0)
confusionMatrix(data = factor(as.numeric(prediction_test_bin>0.5)), reference = factor(df_last_year$Status), positive='1')


library(glmnet)
x = model.matrix(Status~., Train)[,-c(1)]
y = Train$Status
x_test = model.matrix(Status~., df_last_year)[,-c(1)]
y_test = df_last_year$Status

# cross-validation search of lambda
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")
cv.lasso$lambda.min
# Fit the final model on the training data
model = glmnet(x, y, alpha = 1, family = "binomial",
               lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
exp(coef(model))

#TRAIN
prediction_train_Lasso <- predict(model, newx = x, type = "response")
prediction_train_Lasso_bin <- ifelse(prediction_train_Lasso > 0.5,1,0)
confusionMatrix(data = factor(as.numeric(prediction_train_Lasso_bin>0.5)), reference = factor(y), positive = '1')

#TEST
prediction_test_Lasso <- predict(model, newx = x_test, type = "response")
prediction_test_Lasso_bin <- ifelse(prediction_test_Lasso > 0.5,1,0)
confusionMatrix(data = factor(as.numeric(prediction_test_Lasso_bin>0.5)), reference = factor(y_test), positive='1')

library(classifierplots)
#ROC TEST LOGISTIC
roc_plot(df_last_year$Status, prediction_test, resamp=50)

#ROC TEST LASSO
roc_plot(df_last_year$Status, prediction_train_Lasso[,c(1)], resamp=50)

#Calibration plot logistic test
library(gbm)
calibrate.plot(y_test, prediction_test, xlim=c(0,1), ylim=c(0,1))
#calibraton plot Lasso
calibrate.plot(y_test, prediction_test_Lasso[,c(1)], xlim=c(0,1), ylim=c(0,1))

#un'altra misura di bontà
library(ResourceSelection)
hosmer_logistic <- hoslem.test(df_last_year$Status, fitted(fit), g=10)
hosmer_logistic
