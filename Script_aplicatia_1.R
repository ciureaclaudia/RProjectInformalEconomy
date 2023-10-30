# Import the libraries
library(readxl)
library(tidyverse)
library(magrittr)
library(car)
library(FinTS)
library(moments)
library(whitestrap)
library(olsrr)
library(lmtest)
library(sandwich)
library(tseries)
library(mltools)
library(MLmetrics)
library(caret)
library(corrplot)
library(RColorBrewer)
library(Boruta)
library(dplyr)
library(glmnet)

# Import the data
library(readxl)
df <- read_excel("df.xlsx")
view(df)


# Model de regresie simpla
summary(df$`Coeficientul GINI`)
skewness(df$`Coeficientul Gini`)
kurtosis(df$`Coeficientul GINI`)

summary(df$`Rata saraciei`)
skewness(df$`Rata saraciei`)
kurtosis(df$`Rata saraciei`)



person(df$`Coeficientul GINI`)
person(df$`Rata saraciei`)


scatter_plot <- ggplot(df, aes(`Coeficientul GINI`, `Rata saraciei`))
scatter_plot + geom_point() + labs(x = "coeficientul gini", y = "rata saraciei") 





cor(x=df$`Rata saraciei`,y=df$`Coeficientul GINI`,method=c("pearson"))


summary(df$`Rata saraciei`)

#Modelul de regresie simpla (testarea tuturor variabilelor independente pentru a putea observa cel mai mare R2 -> inseamna ca aveam heteroschedasticitate)
# Guvernanta
model <- lm(`Coeficientul GINI` ~ `Accesul la informatie`, df)
summary(model) # R2 = 11.78%, semnificativ la 90%

model <- lm(`Coeficientul GINI` ~ `Drepturile civile`, df)
summary(model) # nesemnificativ

model <- lm(`Coeficientul GINI` ~ `Statul de drept`, df)
summary(model) # R2 = 17.79%, semnificativ la 95%

model <- lm(`Coeficientul GINI` ~ `Procese electorale`, df)
summary(model) # R2 = 11.78%, semnificativ la 90%

# Social
model <- lm(`Coeficientul GINI` ~ `Rata saraciei`, df)
summary(model) # R2 = 53.8%, semnificativ la 99%

model <- lm(`Coeficientul GINI` ~ `Egalitatea de gen in parlament`, df)
summary(model) #nesemnificativ 

model <- lm(`Coeficientul GINI` ~ `Satisfactia vietii`, df)
summary(model) # R2 = 28.42%, semnificativ la 99%

model <- lm(`Coeficientul GINI` ~ `Cheltuieli cu sanatatea`, df)
summary(model) # nesemnificativ

model <- lm(`Coeficientul GINI` ~ `Speranta de viata`, df)
summary(model) # nesemnificativ

model <- lm(`Coeficientul GINI` ~ `Mortalitatea infantila`, df)
summary(model) # R2 = 23.22%, semnificativ la 95%

model <- lm(`Coeficientul GINI` ~ `Statutul medical perceput`, df)
summary(model) # nesemn

model <- lm(`Coeficientul GINI` ~ `Rata fertilitatii`, df)
summary(model) # nemeses

model <- lm(`Coeficientul GINI` ~ `Rata saraciei in randul copiilor`, df)
summary(model) # R2 = 38.41%, semnificativ la 99%

model <- lm(`Coeficientul GINI` ~ `Rata de crestere economica`, df)
summary(model) # nesemnificativ

model <- lm(`Coeficientul GINI` ~ `PIB per capita`, df)
summary(model) # nesemn

model <- lm(`Coeficientul GINI` ~ `Inflatie`, df)
summary(model) # nesemn


#Graficul valorilor observate 
ggplot(data = df, mapping = aes(x = `Rata saraciei`, y = `Coeficientul GINI`)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="Graficul valorilor observate")


# Satisfactia vietii are cel mai mare nivel de R2, asadar continuam sa testam cu el

model <- lm(`Coeficientul GINI` ~ `Rata saraciei`, df)
summary(model) # R2 = 53.8%, semnificativ la 99%

# Ipoteze pe reziduuri

# Heteroschedasticitatea

# Graficul reziduurilor fata de valorile estimate de model
df %<>% mutate(uhat = resid(model)) # reziduuri
df %<>% mutate(yhat = fitted(model)) # valori estimate

ggplot(data = df, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate',title ="Graficul reziduurilor")  # din grafic pare ca avem hetero

#testan sa vedem daca avem hetero
bptest(model) # nu avem hetero
white_test(model) # nu avem hetero
ArchTest(df$uhat) # nu avem hetero conditionata

# Autocorelare 

acf(model$residuals) # nu avem 
dwtest(model) # avem la 90% autocorelare
bgtest(model, order = 2) # nu avem
bgtest(model, order = 3) # nu avem
# concluzionam ca nu avem autocorelare

# Normalitate
ols_plot_resid_fit(model)
ols_plot_resid_qq(model)
ols_plot_resid_hist(model)
ols_plot_resid_box(model) # avem o valoarea outliar

# Teste pentru normalitate
shapiro.test(df$uhat) # normal distribuite
jarque.bera.test(df$uhat) # normal distribuite

# Prognoze pt Muntenegru
prognoza <- data.frame(`Rata saraciei` = c(16.8))
prognoza <- prognoza %>%
  rename('Rata saraciei' = Rata.saraciei)
y_pred_scenariu <- predict(model, newdata = prognoza)
y_pred_scenariu # 34.98 coef Gini pt Muntenegru
# valoarea reala este 36.8 

# Acuratetea modelului
set.seed(123)
training.samples <- df$`Coeficientul GINI` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

model_acuratete <- lm(`Coeficientul GINI` ~ `Rata saraciei`, train.data)
summary(model_acuratete) # R2 = 54.8%, semnificativ la 99% (mai bine putin ca modelul
# pe tot esationul)

y_pred <- predict(model_acuratete, newdata = test.data)
y_pred

RMSE(y_pred, test.data$`Coeficientul GINI`)
MAE(y_pred, test.data$`Coeficientul GINI`)
mse(y_pred, test.data$`Coeficientul GINI`)
MAPE(y_pred, test.data$`Coeficientul GINI`)

#prognoza pe interval 


#Ipotezele modelului de regresie
#       1. Modelul este liniar in parametri
#       2. Nr de observatii > nr variabile independente
#       3. Modelul de regresie este corect specificat    
#       4. Variabilitatea in x este pozitiva
#       5. Media reziduurilor este 0
#       6. Testare multicoliniaritate
#       7. Reziduurile nu sunt corelate cu variabilele independente
#       8. Reziduurile sunt homoscedastice
#       9. Reziduurile nu sunt autocorelate
#      10. Reziduurile sunt normal distribuite


#REGRESIE MULTIPLA

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# GINI=19.18 + 0.91* Rata saraciei + 0.68 * Acces la info



model_multiple<-lm(`Coeficientul GINI`~`Rata saraciei`+`Accesul la informatie`,df)
summary(model_multiple)
#R2 -> 54.05% semnificativ la 99%

#Afisarea coeficientilor
coef(model_multiple)

# Ipoteza 2 - Nr de observatii=27 > nr variabile independente=2
nobs(model_multiple) > (model_multiple$rank - 1)


# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru 
#coef variabilelor independente au semn pozitiv


# Ipoteza 4 - Variabilitatea in x este pozitiva
var(df$`Coeficientul GINI`) #18.49
var(df$`Rata saraciei`) #13.03
var(df$`Accesul la informatie`) #   0.13
#toate valorile > 0 => ipoteza acceptata


# Ipoteza 5 - Media reziduurilor este 0
mean(model_multiple$residuals) # medie aproape de 0 => ipoteza acceptata


# Ipoteza 6 - Testare multicoliniaritate cu VIF
vif(model_multiple) #avem 1.38 -> nu avem valori pt VIF > 10 => ipoteza acceptata
# {VIF = variance inflation factor}
# Multicol este intalnita atunci cand regresorii sunt puternici corelati intre ei 


#reziduu=val reala-val estimata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(df$`Coeficientul GINI`, model_multiple$residuals) # p-value < 0.1 => sunt corelate
cor.test(df$`Rata saraciei`, model_multiple$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(df$`Accesul la informatie`, model_multiple$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza nu acceptata ??????????????????



# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_multiple) #p-value 0.06249 < 0.1 -> avem hetero
white_test(model_multiple) #P-value: 0.666461 > 0.1 -> avem homo

#logaritmam modelul pt a scapa de hetero

model_log1 <- lm(`Coeficientul GINI`~log(`Rata saraciei`)+log(`Accesul la informatie`),df)
summary(model_log1)

bptest(model_log1) #p-value 0.1024 > 0.1 -> avem homoscheda
white_test(model_log1) #P-value: 0.625427

# Ipoteza 9 - Reziduurile nu sunt autocorelate 

# Autocorelarea este intalnita atunci cand reziduurile sunt corelate cu ele  insusi. Cand reziduurile sunt autocorelate, inseamna ca valoarea curenta
# este dependenta de valoarile cu decalaj (anterioare). Aceasta ipoteza este  mai frecvent incalcata in randul seriilor de timp si mai putin intalnita 
# atunci cand se utilizeaza date transversale (la nivelul unui singur an)


acf(model_log1$residuals) # nu sunt autocorelate
# fiecare bat din grafic reprezinta laguri pt ca lagurile depasesc intervalul punctat, lent putem deduce ca reziduurile
# sunt autocorelate (lag0 = valoarea prezenta, lag1 = valoarea de ieri, lag2 = valoarea de acum 2 zile samd)


dwtest(model_log1) # p-value=0.1162 > 0.1 => reziduuri nonautocorelate 
bgtest(model_log1) # p-value=0.2672 > 0.1 => reziduuri nonautocorelate 


# Ipoteza 10 -  Reziduurile sunt normal distribuite 
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(model_log1$residuals) # pvalue = 0.6993 <  statTest=0.7153 reziduurile sunt normale distribuite
shapiro.test(model_log1$residuals) #p-value =  0.7259 ; statTest = 0.97461 -> accept H0 : sunt norm distrib



ggplot(data = df) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') 



# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_log1)
dev.off()
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_log1)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_log1)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_log1)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_log1) 



#Asimetria (skewness)
skewness(model_multiple$uhat)
kurtosis(model_multiple$uhat)
#kurtosis > 3 => distributie platicurtica
#reziduurile nu sunt normal distribuite



#PROGNOZA

# Prognoze
prognozaNoua <- data.frame(`Rata saraciei` = c(17), `Accesul la informatie`=c(2))
prognozaNoua <- prognozaNoua %>%
  rename('Rata saraciei' = Rata.saraciei,'Accesul la informatie'=Accesul.la.informatie)
y_pred <- predict(model_log1, newdata = prognozaNoua)
y_pred # valoarea previzionata pentru Muntenegru este 33.95632 


# Acuratetea modelului
set.seed(123)
training.samples <- df$`Coeficientul GINI` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

model_acuratete1 <- lm(`Coeficientul GINI` ~ `Rata saraciei`+`Accesul la informatie`, train.data)
summary(model_acuratete1) # R2 = 55.19%, semnificativ la 99% (mai bine putin ca modelul
# pe tot esationul)

y_pred1 <- predict(model_acuratete1, newdata = test.data)
y_pred1


# interpretare ??

RMSE(y_pred1, test.data$`Coeficientul GINI`)
MAE(y_pred1, test.data$`Coeficientul GINI`)
mse(y_pred1, test.data$`Coeficientul GINI`)
MAPE(y_pred1, test.data$`Coeficientul GINI`)


#REGRESIA MULTIPLA CU VARIABILA DUMMY
#Forma liniara

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# GINI = 22.79 + 0.84* Rata saraciei + (-1.0019) * Acces la info + (-2.0307) * Tari ECE


model_dummy<-lm(`Coeficientul GINI`~`Rata saraciei`+`Accesul la informatie`+`Tari ECE`,df)
summary(model_dummy)
#R2 -> 58.24% semnificativ la 99%
coef(model_dummy)


# Ipoteza 2 - Nr de observatii=27 > nr variabile independente=3
nobs(model_dummy) > (model_dummy$rank - 1)


# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru 
# coef variabilelor independente au semn negativ
#Afisarea coeficientilor
coef(model_dummy)

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(df$`Coeficientul GINI`) #18.49
var(df$`Rata saraciei`) #13.03
var(df$`Accesul la informatie`) #   0.13
var(df$`Tari ECE`) #0.25
#toate valorile > 0 => ipoteza acceptata


# Ipoteza 5 - Media reziduurilor este 0
mean(model_dummy$residuals) # medie aproape de 0 => ipoteza acceptata



# Ipoteza 6 - Testare multicoliniaritate cu VIF
vif(model_dummy) #avem 1.45 -> nu avem valori pt VIF > 10 => ipoteza acceptata
# daca valoarea lui VIF este mai mare de 1, inseamna ca regresorii sunt partial corelati intre ei
# {VIF = variance inflation factor}
# Multicol este intalnita atunci cand regresorii sunt puternici corelati intre ei 


#reziduu (uhat) = val reala (y) - val estimata (yhat)

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(df$`Coeficientul GINI`, model_dummy$residuals) # p-value < 0.1 => sunt corelate
cor.test(df$`Rata saraciei`, model_dummy$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(df$`Accesul la informatie`, model_dummy$residuals)
cor.test(df$`Tari ECE`, model_dummy$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza nu acceptata ??????????????????



# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_dummy) #p-value 0.1062 > 0.1 -> sunt homo
white_test(model_dummy) # p-value 0.553069 > 0. -> sunt homo


# Ipoteza 9 - Reziduurile nu sunt autocorelate  #? avem autocor la dwtest; ce facem???????????

# Autocorelarea este intalnita atunci cand reziduurile sunt corelate cu ele  insusi. Cand reziduurile sunt autocorelate, inseamna ca valoarea curenta
# este dependenta de valoarile cu decalaj (anterioare). Aceasta ipoteza este  mai frecvent incalcata in randul seriilor de timp si mai putin intalnita 
# atunci cand se utilizeaza date transversale (la nivelul unui singur an)


acf(model_dummy$residuals) # nu sunt autocorelate
# fiecare bat din grafic reprezinta laguri pt ca lagurile depasesc intervalul punctat, lent putem deduce ca reziduurile
# sunt autocorelate (lag0 = valoarea prezenta, lag1 = valoarea de ieri, lag2 = valoarea de acum 2 zile samd)


dwtest(model_dummy) # p-value=0.09039 < 0.1 => reziduuri autocorelate 
bgtest(model_dummy) # p-value=0.2118 > 0.1 => reziduuri nonautocorelate 


# Ipoteza 10 -  Reziduurile sunt normal distribuite 
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(model_dummy$residuals) # pvalue = 0.944 > statTest = 0.115 reziduurile sunt normale distribuite
shapiro.test(model_multiple$residuals) #p-value = 0.9828 > 0.05 -> accept H0 : sunt norm distrib

ols_plot_resid_box(model_dummy)

ols_plot_resid_hist(model_dummy)

ols_plot_resid_qq(model_dummy)



#PROGNOZA
# Prognoze
prognozaNouaDummy <- data.frame(`Rata saraciei` = c(16.8), `Accesul la informatie`=c(2),`Tari ECE`=c(1))
prognozaNouaDummy <- prognozaNouaDummy %>%
  rename('Rata saraciei' = Rata.saraciei,'Accesul la informatie'=Accesul.la.informatie, 'Tari ECE'=Tari.ECE)
y_pred1 <- predict(model_log1, newdata = prognozaNouaDummy)
y_pred1 # valoarea previzionata pentru Muntenegru este 33.85


# Acuratetea modelului
set.seed(123)
training.samples <- df$`Coeficientul GINI` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

model_acuratete2 <- lm(`Coeficientul GINI` ~ `Rata saraciei`+`Accesul la informatie`+`Tari ECE`, train.data)
summary(model_acuratete2) # R2 = 58.76%, semnificativ la 99% (mai bine ca modelul
# pe tot esationul)

y_pred2 <- predict(model_acuratete2, newdata = test.data)
y_pred2


# interpretare ??

RMSE(y_pred2, test.data$`Coeficientul GINI`)
MAE(y_pred2, test.data$`Coeficientul GINI`)
mse(y_pred2, test.data$`Coeficientul GINI`)
MAPE(y_pred2, test.data$`Coeficientul GINI`)




# Regresia cu variabile dummy si termeni de interactiune

#generare termeni de interactiune
#interactiunea intre acces la informatie si tari ece
df %<>% mutate(accesXinfo = `Accesul la informatie`*`Tari ECE`)
view(df$accesXinfo)



# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# GINI = 20.6287 + 0.87* Rata saraciei + 0.44 * Acces la info + (-1.5) * accesXinfo


model_interactiune<-lm(`Coeficientul GINI`~`Rata saraciei`+`Accesul la informatie`+`accesXinfo`,df)
summary(model_interactiune)
#R2 este 57,61% semnficativ la 99%




# Ipoteza 2 - Nr de observatii=27 > nr variabile independente=3
nobs(model_interactiune) > (model_interactiune$rank - 1)


# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru 
# coef variabilelor independente au semn negativ
#Afisarea coeficientilor
#termenul de interactiune are semn negativ
coef(model_interactiune)

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(df$`Coeficientul GINI`) #18.49
var(df$`Rata saraciei`) #13.03
var(df$`Accesul la informatie`) #   0.13
var(df$`accesXinfo`) #0.29
#toate valorile > 0 => ipoteza acceptata


# Ipoteza 5 - Media reziduurilor este 0
mean(model_interactiune$residuals) # medie aproape de 0 => ipoteza acceptata



# Ipoteza 6 - Testare multicoliniaritate cu VIF
vif(model_interactiune) #avem 1.41 -> nu avem valori pt VIF > 10 => ipoteza acceptata
# daca valoarea lui VIF este mai mare de 1, inseamna ca regresorii sunt partial corelati intre ei
# {VIF = variance inflation factor}
# Multicol este intalnita atunci cand regresorii sunt puternici corelati intre ei 


#reziduu (uhat) = val reala (y) - val estimata (yhat)

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(df$`Coeficientul GINI`, model_interactiune$residuals) # p-value < 0.1 => sunt corelate
cor.test(df$`Rata saraciei`, model_interactiune$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(df$`Accesul la informatie`, model_interactiune$residuals)
cor.test(df$`accesXinfo`, model_interactiune$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza nu acceptata ??????????????????



# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_interactiune) #p-value 0.117 > 0.1 -> sunt homo
white_test(model_interactiune) # p-value 0.636638 > 0. -> sunt homo


# Ipoteza 9 - Reziduurile nu sunt autocorelate  #? avem autocor la dwtest; ce facem???????????

# Autocorelarea este intalnita atunci cand reziduurile sunt corelate cu ele  insusi. Cand reziduurile sunt autocorelate, inseamna ca valoarea curenta
# este dependenta de valoarile cu decalaj (anterioare). Aceasta ipoteza este  mai frecvent incalcata in randul seriilor de timp si mai putin intalnita 
# atunci cand se utilizeaza date transversale (la nivelul unui singur an)


acf(model_interactiune$residuals) # nu sunt autocorelate
# fiecare bat din grafic reprezinta laguri pt ca lagurile depasesc intervalul punctat, lent putem deduce ca reziduurile
# sunt autocorelate (lag0 = valoarea prezenta, lag1 = valoarea de ieri, lag2 = valoarea de acum 2 zile samd)


dwtest(model_interactiune) # p-value=0.08296 < 0.1 => reziduuri autocorelate 
bgtest(model_interactiune) # p-value=0.1991 > 0.1 => reziduuri nonautocorelate 


# Ipoteza 10 -  Reziduurile sunt normal distribuite 
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(model_interactiune$residuals) # pvalue = 0.9071 > statTest = 0.195 reziduurile sunt normale distribuite
shapiro.test(model_interactiune$residuals) #p-value = 0.8474 < 0.97935 -> accept H1 : nu sunt norm distrib

ols_plot_resid_box(model_interactiune)

ols_plot_resid_hist(model_interactiune)

ols_plot_resid_qq(model_interactiune)

#Regresia Ridge
model_ridge<-lm(`Coeficientul GINI`~`Rata saraciei`+`Accesul la informatie`+`Tari ECE`+`Satisfactia vietii`,df)
summary(model_ridge)

#valorile previzionate 
model_predict<-data.frame(`Rata saraciei`=c(14),
                          `Accesul la informatie`=c(10),
                          `Tari ECE`=c(1),
                          `Satisfactia vietii`=c(16.8)
                          )


y_predict<-predict(model_ridge,newdata = model_predict)
y_predict
# Definim variabila raspuns
y <- df$`Coeficientul GINI`

# Definim predictorii
x <- data.matrix(df[, c('Rata saraciei', 'Accesul la informatie', 'Tari ECE', 'Satisfactia vietii')])


# Estimam modelul ridge (alpha = 0)
model_ridge1 <- glmnet(x, y, alpha = 0)
summary(model_ridge1)

# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
#cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.78

# testarea valorii lamda 
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 



# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model_ridge1, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)


# Regresia LASSO - functioneaza similar cu regresie Ridge doar ca incearca sa minimizeze
# SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)

# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
# regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda 

# testarea valorii lamda
plot(cv_model) 


# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 
# daca unul din coeficienti este 0 inseamna ca acea variabila nu este importanta
# si de aceea nu o estimeaza modelul

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim 
#cel mai bun model
# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample

new <- matrix(c(17,2,2,6), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 59.37%



# Elastic net regression - functioneaza similar cu Ridge si LASSO doar ca 
# adauga ambele penalitati SSR + lambda*sum(beta^2) + lambda*sum(|beta|)
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0100


# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 


#cel mai bun model #ELASTIC NET
# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Prognoze 

y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample

new <- matrix(c(16.8,2,1,6), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 59.37%

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim 



# Algoritmul Boruta 

# ne arata care sunt var semnif si care nu sunt 

set.seed(111)
boruta.bank_train <- Boruta(`Coeficientul GINI`~., data = df, doTrace = 2)
print(boruta.bank_train)

# Vom selecta atributele importante 
getSelectedAttributes(boruta.bank_train, withTentative = T)

# Vom reimplementa un model de regresie cu aceste atribute
model_boruta <- lm(`Coeficientul GINI` ~ `Rata saraciei` +`Efectul de redistribuire` + `Cheltuieli publice cu cercetarea` + `Chletuieli private cu cercetarea`+`Rata saraciei in randul varstnicilor`, df)
summary(model_boruta) 

#Prognoza pe interval de incredere


