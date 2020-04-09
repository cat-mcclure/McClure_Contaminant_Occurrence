rm(list =ls())

## load libraries
library(MASS)
library(coda)
library(Rcpp)
library(RcppArmadillo)
library(HMSC)
library(data.table)
library(dplyr)
library(beanplot)

set.seed(1)

## set directory ##

setwd("~/Desktop/Classwork/CEC_Project/Chapter 1/HMSC")

### community matrix (Y) ###

Y <- fread('HMSC_data_ag.csv')

## remove date columns and discharge columns and site ID columns

Y[,c("V1", "day", "month","year") :=NULL]

Y$`Site ID`<- NULL

#rename E2EqBLYES (ng/L) to estrogenicity 
names(Y)[29]<-"Estrogenicity"

Y <- as.matrix(Y)


str(Y)


### covariates (X) ###

#Column of 1's

#read-in data set in order to ensure the same number of rows as the Y-matrix
X <- fread('HMSC_data_ag.csv')

X[,1] <- 1

X <- X[,1]

X <- as.matrix(X)

dim(X)


#### random effects (Pi) ####

Pi <- fread('HMSC_data_ag.csv')
Pi <- Pi[,c('Site ID', 'year')]

Pi$Observation <- 1:202

Pi <- Pi[,c('Observation', 'Site ID', 'year')]

#rename Site ID to Site and year to Year
names(Pi)[2]<-"Site"
names(Pi)[3]<-"Year"


##convert columns in Pi to factors

Pi <- as.data.frame(Pi)

typeof(Pi)

for(i in 1:ncol(Pi)){
  Pi[,i] <- as.factor(Pi[,i])
}


str(Pi)



## form data

formdata <- as.HMSCdata(Y = Y,
                        X = X,
                        Random = Pi,
                        interceptX = TRUE,
                        scaleX=T)

## set priors

formprior <- as.HMSCprior(formdata)
formparam <- as.HMSCparam(formdata, formprior)

## MCMC

model_3 <- hmsc(formdata,
              family = "probit",
              niter = 20000,
              nburn = 10000)

## save

save(model_3, file = "model_3")

#model = FLOW data set
#model_2 = AG data set
#model_3 = AG data set, intercept-only
#model_4 = FLOW data set, intercept-only

## Mixing object

mixing <- as.mcmc(model, parameters = "paramX")

## draw trace and density plots for all combinations of parameters

par(mar = rep(2, 4))
plot(mixing)

### Convert the mixing object to a matrix 
mixingDF <- as.data.frame(mixing)


#### Draw estimated correlation matrix 

library(corrplot)

plot.new()
corMat <- corRandomEff(model, cor = TRUE) 
averageCor <- apply(corMat[, , , 3], 1:2, mean) 
corrplot(averageCor, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200))



