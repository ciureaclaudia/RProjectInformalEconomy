library(tidyverse)
library(gplots)
library(plm)
library(readxl)
library(readxl)
library(foreign)
library(lmtest)

# citirea datelor de tip Panel din excel
data<-read_xlsx("C:/Users/claud/Downloads/Aplicatia 2 Econometrie Proiect/Aplicatia 2 Econometrie Proiect/Registru1.xlsx")
print(data)

# statistici descriptive
summary(data)

# declararea setului de date de tip panel
pd.df<-pdata.frame(data,index=c("Tară","an"),drop.index = TRUE)
print(pd.df)
# Corelatia dintre gini/spatiu/timp
coplot(Gini ~ an|Tară, type="l", data=data) 


# explorarea heterogenitatii presupune ca avem diferente intre unitatile studiate. Graficul traseaza un interval de incredere de 95% in jurul mediilor

# Explorarea heterogeneitatii in sectiunea transversala
# Interpretare: avem tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala
plotmeans(Gini~Tară,main="Heterogenitate in randul tarilor",data=data)


# Explorarea heterogenitatii in sectiunea temporala
# Interpretare: Ani cu rata mai mare si ani cu rata mai mica => avem heterogenitate temporala dar mai mica decat in cazul heterigenitatii transversale
plotmeans(Gini~an,main="Heterogenitate in timp",data = data)


# Model OLS - model clasic de regresie liniara (Nu ia in calcul heterogenitatea in timp)
ols<-lm(Gini~mmwics +em_ed_ter +ch_ed +ch_he ,data=data)
summary(ols) #output -> Rpatrat= 67%
yhat<-ols$fitted.values #valori estimate
ggplot(data,aes(x=mmwics,  y=Gini))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme_bw()


# Model FE(efecte fixe) => interpretare: avem toate variabilele semnificative -> continuam cu ele
fe<-plm( Gini ~ mmwics + em_ed_ter + ch_ed + ch_he , data ,index=c('Tară','an'),model='within')
summary(fe)
# Unbalanced Panel: n = nr de paneluri 10 || T = ani 12|| N = nr total de observatii 120


# Alegerea celei mai adecvate variante de model prin testarea intre regresie OLS vs fixed effects panel model
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value < 0.05 =>interpretare: se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(Gini ~ mmwics + em_ed_ter + ch_ed + ch_he, data, index = c('Tară','an'),model = 'between')
summary(re)


# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: RE
# H1: FE
phtest(fe,re) # p-value < 0.05 => interpretare: respinge H0->se recomanda model FE

# Interpretare test Hausmann: In urma testului Hausmann am decis sa utilizam modelul FE


############################################

#FE Testarea efectelor fixe in timp
fixed.time <- plm(Gini ~ mmwics + em_ed_ter + ch_ed + ch_he + factor(an), data=data, index=c("Tară","an"), model="within")

# H0:  NU sunt necesare efectele fixe in timp
# H1:  sunt 
pFtest(fixed.time, fe) # p-value > 0.05 =>       Nu se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value > 0.05  =>     nu este nevoie sa se utilizeze efecte fixe in timp 

# Cele doua teste sunt inconcluzive => vom alege varianta in care nu avem efecte fixe in timp


#------------------------------------

#RE Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 

#pool <- plm(Gini ~ mmwics, data=data, index=c("Tară", "an"), model="pooling")
#summary(pool)


# Testarea efectelor fixe in timp

#random.time <- plm(Gini ~ mmwics + factor(an), data=data, index=c("Tară","an"), model="within")

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0

#plmtest(pool, type=c("bp")) # p-value < 0.05 => respingem ipoteza nula

# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate a.i.
# exista diferente semnificative intre tari

#------------------------------------


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate
pcdtest(fe, test = 'lm') # p-value < 0.05 =>       dependenta transversala => nu corectam
pcdtest(fe, test = 'cd') # pvalue > 0.05 =>       dependenta temporala => nu corectam
# Nu corectam pt ca avem panel mic


# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # p-value < 0.05 =>         avem autocorelarem => nu corectam


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate 
bptest(Gini ~ mmwics + em_ed_ter +ch_ed + ch_he +
         factor(an), data = data, studentize=F) # pvalue< 0.05 => avem heteroschedasticitate => nu corectam
