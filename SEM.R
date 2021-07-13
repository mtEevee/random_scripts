setwd("E:/MathAb/Data")
data <- read.csv("NoMissNoOutData.csv")
#data <- read.csv("no_outliers_finalData.csv")
library("lavaan")
library("semPlot")
library("psych")
library("nFactors")
library("processR")

##################################################################
################## exploratory factor analysis ###################
##################################################################

##model 1: paradigm RTs, cognitive test Scores
mydata <- data[,c(7:11, 19:21)]
mydata <- mydata[complete.cases(mydata),] #remove NAs
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 2/3 factors
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

##model 2: paradigm RTs, cognitive test RTs
mydata <- data[,c(13:17, 19:21)]
mydata <- mydata[complete.cases(mydata),] #remove NAs
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 2/3 factors
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

##model 3: cognitive test scores only
mydata <- data[,c(7:11)]
mydata <- mydata[complete.cases(mydata),] #remove NAs
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 1/2 factors
fit <- factanal(mydata, 1, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

######################
#obsolete

## case 2: RTs only
mydata <- data[,c(17:19, 28:32)]
mydata <- mydata[complete.cases(mydata),] #remove NAs
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 3/5 factors
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

## case 3: no RTs
mydata <- data[,c(9:14)]
mydata <- mydata[complete.cases(mydata),]
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 2 factors

fit <- factanal(mydata, 1, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

## case 4: + traits
mydata <- data[,c(5:14)]
mydata <- mydata[complete.cases(mydata),]
nofactors <- fa.parallel(mydata, fm = 'ml', fa = 'fa')
sum(nofactors$fa.values > 1.0)
sum(nofactors$fa.values > 0.7)

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # 2 factors

fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)


round1 <- fa(mydata, nfactors = 2, rotate = "oblimin", fm = "ml")
round1

#obsolete
######################

##################################################################
################ confirmatory factor analysis ####################
##################################################################

myvars <- as.data.frame(cbind(scale(data$mr), data$ns, data$pvt))
myvars <- myvars[complete.cases(myvars),]
colnames(myvars) <- colnames(data)[8:10]
cortable <- cor(myvars)
sds <- sapply(myvars, sd)
covtable <- cor2cov(cortable, sds)

#model 1
mathab.model <- 'mathab =~ a*pvt + b*ns + c*mr'
mathab.fit <- cfa(model = mathab.model,
                  sample.cov = covtable,
                  sample.nobs = nrow(mydata),
                  std.lv = FALSE)
inspect(mathab.fit, "cov.lv")
summary(mathab.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)
parameterestimates(mathab.fit, standardized = TRUE)
fitted(mathab.fit)
fitmeasures(mathab.fit, c("rmsea", "srmr", "cfi", "tli", "ecvi", "aic", "chisq", "df"))
residuals(mathab.fit)

semPaths(mathab.fit,
         whatLabels = "std",
         layout = "tree")

#model 2
myvars <- as.data.frame(cbind(scale(data$mr), data$ns, data$pvt, data$sem, data$mean_latency_Ari))
colnames(myvars) <- c(colnames(data)[8:11], colnames(data)[14])
myvars <- myvars[complete.cases(myvars),]
cortable <- cor(myvars)
sds <- sapply(myvars, sd)
covtable <- cor2cov(cortable, sds)
mathab.model <- 'mathab =~ a*pvt + b*ns + c*mr + d*sem + e*mean_latency_Ari'
mathab.fit2 <- cfa(model = mathab.model,
                  sample.cov = covtable,
                  sample.nobs = nrow(mydata),
                  std.lv = FALSE)
summary(mathab.fit2,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)
parameterestimates(mathab.fit2, standardized = TRUE)
fitmeasures(mathab.fit2)
residuals(mathab.fit2)

semPaths(mathab.fit2,
         whatLabels = "std",
         layout = "tree")
#add model fit indices to table!!!!

##################################################################
#################### Structural modeling #########################
##################################################################

##model 1

myvars <- as.data.frame(cbind(scale(data$mr), data$ns, data$pvt, data$iq, scale(data$USE), scale(data$motiv), scale(data$ma), scale(data$ta)))
colnames(myvars) <- c(colnames(data)[8:10], colnames(data)[c(6, 18,2,4,5)])
myvars <- myvars[complete.cases(myvars),]
cortable <- cor(myvars)
sds <- sapply(myvars, sd)
covtable <- cor2cov(cortable, sds)

sem1 <- '
mathab =~ a*pvt + b*ns + c*mr
latentiq =~ iq
latentiq ~~ d*mathab
USE ~ e*mathab + f*motiv + g*mathab:motiv
'

sem1.fit <- sem(sem1, myvars)
summary(sem1.fit, 
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)

semPaths(sem1.fit,
         whatLabels = "std",
         layout = "spring")

##CFA model with IQ controlled

myvars <- as.data.frame(cbind(scale(data$mr), data$ns, data$pvt, data$iq, scale(data$conf), scale(data$USE),scale(data$motiv)))
colnames(myvars) <- c(colnames(data)[8:10], colnames(data)[c(6, 3,18,2)])
myvars <- myvars[complete.cases(myvars),]
cortable <- cor(myvars)
sds <- sapply(myvars, sd)
covtable <- cor2cov(cortable, sds)
  
sem2 <- '
mathab =~ a*pvt + b*ns + c*mr
mathab ~ d*iq
'

sem2.fit <- sem(sem2, myvars)
summary(sem2.fit, 
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)

fitmeasures(sem2.fit, c("rmsea", "srmr", "cfi", "tli", "ecvi", "aic", "chisq", "df"))

semPaths(sem2.fit,
         whatLabels = "std",
         layout = "tree")  

cfa.fit <- cfa(sem2, data = myvars)
pred <- predict(cfa.fit)
head(pred)
myvars$mathab <- pred

hist(pred)


## Moderation model 1

myvars <- myvars %>% mutate(mathab_x_motiv = motiv * mathab)

sem1 <- '
USE ~ a*mathab
USE ~ b*motiv
USE ~ c*mathab_x_motiv
'

sem1.fit <- sem(sem1, myvars)
summary(sem1.fit, 
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)

semPaths(sem1.fit,
         whatLabels = "std",
         layout = "circle")

lmfit <- lm(USE ~ mathab*motiv, data = myvars)
mylabels <- list(X = "mathab", W = "motiv", Y = "USE")
pmacroModel(1,labels=mylabels)
x <- modelsSummary(list(lmfit))
modelsSummaryTable(x)
condPlot(lmfit, mode=2,xpos=0.6)
jnPlot(fit,plot=FALSE)
x
summary(lmfit)
summary(sem1.fit)
statisticalDiagram(1,labels=mylabels,fit=sem1.fit,whatLabel = "est")

## Moderation model 2

myvars <- myvars %>% mutate(mathab_x_motiv = motiv * mathab)

sem2 <- '
conf ~ a*mathab
conf ~ b*motiv
conf ~ c*mathab_x_motiv
'

sem2.fit <- sem(sem2, myvars)
summary(sem2.fit, 
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)

semPaths(sem2.fit,
         whatLabels = "std",
         layout = "circle")

lmfit2 <- lm(conf ~ mathab*motiv, data = myvars)
mylabels2 <- list(X = "mathab", W = "motiv", Y = "conf")
pmacroModel(1,labels=mylabels2)
x2 <- modelsSummary(list(lmfit2))
modelsSummaryTable(x2)
condPlot(lmfit2, mode=2,xpos=0.6)

summary(lmfit2)
summary(sem2.fit)
statisticalDiagram(1,labels=mylabels2,fit=sem2.fit,whatLabel = "est")

## Moderation model 3

myvars <- myvars %>% mutate(mathab_x_ma = motiv * mathab)

sem2 <- '
conf ~ a*mathab
conf ~ b*motiv
conf ~ c*mathab_x_motiv
'

sem2.fit <- sem(sem2, myvars)
summary(sem2.fit, 
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures = TRUE)

semPaths(sem2.fit,
         whatLabels = "std",
         layout = "circle")

lmfit2 <- lm(conf ~ mathab*motiv, data = myvars)
mylabels2 <- list(X = "mathab", W = "motiv", Y = "conf")
pmacroModel(1,labels=mylabels2)
x2 <- modelsSummary(list(lmfit2))
modelsSummaryTable(x2)
condPlot(lmfit2, mode=2,xpos=0.6)

summary(lmfit2)
summary(sem2.fit)
statisticalDiagram(1,labels=mylabels2,fit=sem2.fit,whatLabel = "est")
