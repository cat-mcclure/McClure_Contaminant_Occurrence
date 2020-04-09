### Install devtools (if you do not have them already)
 install.packages('devtools')

### Install depend packages (if you do not have them already)
 install.packages('Rcpp')
 install.packages('RcppArmadillo')
 install.packages('coda')

### Install suggested packages (if you do not have them already)
 install.packages('beanplot')
 install.packages('circlize')
 install.packages('corrplot')
 install.packages('coda')

# load the package
library(devtools)
library(MASS)
library(coda)
library(Rcpp)
library(RcppArmadillo)

 
remove("/Users/cmm1148/Library/R/3.6/library/00LOCK-HMSC")

# install HMSC from github
install_github('guiblanchet/HMSC')

# and load it
library(HMSC)
