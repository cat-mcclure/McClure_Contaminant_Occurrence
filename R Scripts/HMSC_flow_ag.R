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

## set working directory ##

setwd("~/Desktop/Classwork/CEC_Project/Chapter 1/HMSC")

### community matrix (Y) ###

Y <- fread('HMSC_data_flow.csv')

## remove date columns and discharge columns and site ID columns

Y[,c("day", "month","year","Discharge (cfs)") :=NULL]

Y$`Site ID`<- NULL
Y$`V1`<- NULL

#rename E2EqBLYES (ng/L) to estrogenicity 
names(Y)[29]<-"Estrogenicity"

Y <- as.matrix(Y)

### covariates (X) ###

X <- fread('HMSC_data_flow.csv')

## add season based on months ##

#spring(month = 3,4,5)

X$spring <- ifelse(X$month == 3|X$month ==4| X$month==5, 1,0)

#summer(month = 6,7,8)

X$summer <- ifelse(X$month == 6|X$month ==7| X$month==8, 1,0)

#fall(month = 9,10,11)

X$fall <- ifelse(X$month == 9|X$month ==10| X$month==11, 1,0)

### grab seasons and discharge ###

X <- X[,c(34,36:38)]


### landuse ###

land_use <- fread('landuse_tw2.csv')
unique(land_use$SiteName)
land_use <- land_use[order(SiteName)]

###  grab only PCTDev, PCTCult, PCTFor for landuse ###

land_use_2 <- land_use[,c('SiteName','PCTdev11', 'PCTfor11', 'PCTcult11')]


X$PCT.AG <- 1:181

X <- as.matrix(X)

X[1:37,5] <- as.numeric(land_use_2[1,4]) #antietam
X[38:67,5] <- as.numeric(land_use_2[2,4]) #chill
X[68:102,5] <- as.numeric(land_use_2[3,4]) #pine
X[103:139,5] <- as.numeric(land_use_2[4,4]) #potomac
X[140:181,5] <- as.numeric(land_use_2[5,4]) #mahantango


dim(X)


#str(X)

#### random effects (Pi) ####

Pi <- fread('HMSC_data_flow.csv')
Pi <- HMSC_flow_estro[,c(2,33)]

## add observation column
Pi$Observation <- 1:181

## grab only columns of interest

Pi <- Pi[,c(3,1,2)]

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

formdata <- as.HMSCdata(Y = Y, X = X,
                        Random = Pi,
                        interceptX = TRUE,
                        scaleX=TRUE)

## set priors

formprior <- as.HMSCprior(formdata)
formparam <- as.HMSCparam(formdata, formprior)

## MCMC

model <- hmsc(formdata,
              family = "probit",
              niter = 20000,
              nburn = 10000)

## save

save(model, file = "model")
save(model, file = "model.rds")

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












