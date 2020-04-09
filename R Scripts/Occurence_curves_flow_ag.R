
#libraries

library(RColorBrewer)
library(dplyr)

#---------------------------------------------------------------------------- 
### Simulating "validation" data  
#---------------------------------------------------------------------------- 


######### use data set with both flow and ag landcover ###########



####### SPRING ########


## create matrix to fill in with simulated data ##
#number of columns must match number of columns in orginal X matrix

X.2 <- matrix(nrow = 20, ncol = 6)

## provide column names ##
#grab columns names from original X-matrix to match simulated data
colnames(X.2) <- colnames(model$data$X) 

## fill in the matrix ##

#set intercept = 1
X.2[, 1] <- 1

#set flow to mean (which is 0 in this case due to scaling)
#flow = not the covariate of interest

#flow
X.2[, 2] <- 0

## assign 1's to season of interest and 0's to the rest

#spring
X.2[, 3] <- 1

#summer
X.2[, 4] <- 0

#fall
X.2[, 5] <- 0

##ag is covariate of interest 
##so allow ag to range from minimum ag landcover to max landcover value
#ag

ag_range <- seq(min(X[,5]),max(X[,5]), length.out = 20)
X.2[, 6] <- seq(min(X[,5]),max(X[,5]), length.out = 20)

X.2 <- as.matrix(X.2)

## sample random effect

Random <- matrix(nrow = 20, ncol = 3)

Random[,1] <- 200
Random[,2] <- 500
Random[,3] <- 700

colnames(Random) <- colnames(model$data$Random)

#random effect matrix -> data frame of factors

Random <- as.data.frame(Random)

for(i in 1:ncol(Random)){
  Random[, i] <- as.factor(as.character(Random[, i])) }
 

dataVal.spring <- as.HMSCdata(X = X.2, Random = Random)



####### SUMMER ########


## create matrix to fill in with simulated data ##
#number of columns must match number of columns in orginal X matrix

X.2 <- matrix(nrow = 20, ncol = 6)

## provide column names ##
#grab columns names from original X-matrix to match simulated data
colnames(X.2) <- colnames(model$data$X) 

## fill in the matrix ##

#set intercept = 1
X.2[, 1] <- 1

#set flow to mean (which is 0 in this case due to scaling)
#flow = not the covariate of interest

#flow
X.2[, 2] <- 0

## assign 1's to season of interest and 0's to the rest

#spring
X.2[, 3] <- 0

#summer
X.2[, 4] <- 1

#fall
X.2[, 5] <- 0

##ag is covariate of interest 
##so allow ag to range from minimum ag landcover to max landcover value
#ag

ag_range <- seq(min(X[,5]),max(X[,5]), length.out = 20)
X.2[, 6] <- seq(min(X[,5]),max(X[,5]), length.out = 20)

X <- as.matrix(X)

## sample random effect

Random <- matrix(nrow = 20, ncol = 3)

Random[,1] <- 200
Random[,2] <- 500
Random[,3] <- 700


colnames(Random) <- colnames(model$data$Random)

#random effect matrix -> data frame of factors

Random <- as.data.frame(Random)

for(i in 1:ncol(Random)){
  Random[, i] <- as.factor(as.character(Random[, i])) }




dataVal.summer <- as.HMSCdata(X = X.2, Random = Random)





####### FALL ########


## create matrix to fill in with simulated data ##
#number of columns must match number of columns in orginal X matrix

X.2 <- matrix(nrow = 20, ncol = 6)

## provide column names ##
#grab columns names from original X-matrix to match simulated data
colnames(X.2) <- colnames(model$data$X) 

## fill in the matrix ##

#set intercept = 1
X.2[, 1] <- 1

#set flow to mean (which is 0 in this case due to scaling)
#flow = not the covariate of interest

#flow
X.2[, 2] <- 0

## assign 1's to season of interest and 0's to the rest

#spring
X.2[, 3] <- 0

#summer
X.2[, 4] <- 0

#fall
X.2[, 5] <- 1

##ag is covariate of interest 
##so allow ag to range from minimum ag landcover to max landcover value
#ag

ag_range <- seq(min(X[,5]),max(X[,5]), length.out = 20)
X.2[, 6] <- seq(min(X[,5]),max(X[,5]), length.out = 20)

X <- as.matrix(X)

## sample random effect

Random <- matrix(nrow = 20, ncol = 3)

Random[,1] <- 200
Random[,2] <- 500
Random[,3] <- 700

colnames(Random) <- colnames(model$data$Random)

#random effect matrix -> data frame of factors

Random <- as.data.frame(Random)

for(i in 1:ncol(Random)){
  Random[, i] <- as.factor(as.character(Random[, i])) }




dataVal.fall <- as.HMSCdata(X = X.2, Random = Random)





####### WINTER ########


## create matrix to fill in with simulated data ##
#number of columns must match number of columns in orginal X matrix

X.2 <- matrix(nrow = 20, ncol = 6)

## provide column names ##
#grab columns names from original X-matrix to match simulated data
colnames(X.2) <- colnames(model$data$X) 

## fill in the matrix ##

#set intercept = 1
X.2[, 1] <- 1

#set flow to mean (which is 0 in this case due to scaling)
#flow = not the covariate of interest

#flow
X.2[, 2] <- 0

## assign 1's to season of interest and 0's to the rest

#spring
X.2[, 3] <- 0

#summer
X.2[, 4] <- 0

#fall
X.2[, 5] <- 0

##ag is covariate of interest 
##so allow ag to range from minimum ag landcover to max landcover value
#ag

ag_range <- seq(min(X[,5]),max(X[,5]), length.out = 20)
X.2[, 6] <- seq(min(X[,5]),max(X[,5]), length.out = 20)

X <- as.matrix(X)

## sample random effect

Random <- matrix(nrow = 20, ncol = 3)

Random[,1] <- 200
Random[,2] <- 500
Random[,3] <- 700

colnames(Random) <- colnames(model$data$Random)

#random effect matrix -> data frame of factors

Random <- as.data.frame(Random)

for(i in 1:ncol(Random)){
  Random[, i] <- as.factor(as.character(Random[, i])) }




dataVal.winter <- as.HMSCdata(X = X.2, Random = Random)
#---------------------------------------------------------------------------- 


### Prediction for a new set of values 


#---------------------------------------------------------------------------- 

##### seasonal predictions #####

# spring
predVal.spring <- predict(model, dataVal.spring)

#summer
predVal.summer <- predict(model, dataVal.summer)

#fall
predVal.fall <- predict(model, dataVal.fall)

#winter
predVal.winter <- predict(model, dataVal.winter)


### confirm data is appropriate ###
head(model$data$X)
summary(predVal.summer)
max(predVal.summer)

## grab means of each contaminant output ##

col.means <- apply(predVal.spring, 2, mean)

y.names <- colnames(Y)
names(col.means) <- colnames(Y)

col.means <- data.frame(col.means)

##look at means that are >0.1 to understand which contaminants have a 
##>10% probability of occurance with increasing ag landcover

cont_of_int <- y.names[col.means>0.1]

##figure out which compound is increasing at the 60-70% ag cover (for SUMMER):

col.means.2 <- apply(predVal.summer, 2, mean)

y.names <- colnames(Y)
names(col.means.2) <- colnames(Y)

col.means.2 <- data.frame(col.means.2)

cont_of_int_2 <- y.names[col.means>0.03]
View(cont_of_int_2)

cont_of_int_2 <- y.names[col.means>0.01]
View(cont_of_int_2)


######### subset each matrix with contaminants of interest ##########


# change column names for easier understanding
colnames(predVal.fall) <- colnames(Y)
colnames(predVal.spring) <- colnames(Y)
colnames(predVal.summer) <- colnames(Y)
colnames(predVal.winter) <- colnames(Y)


## put info above into table (easier to read/interpret) ##


spring <- predVal.spring[,c(29,4,23,11)]


##create dataset to fill in ##
table_spring <- data.frame(matrix(ncol = 2, nrow = 4))

#column names (remember to change ag to agricultural landcover later)
x <- c("Mean (95% CI)", "Range (min-max)")

#apply new column names to new data frame
colnames(table_spring) <- x

#compound names for row names

y <- c("Estrogenicity","Cholesterol","Metolachlor", "Atrazine")

row.names(table_spring) <- y

##attempt at for loop

#mean
for(i in 1:ncol(spring)){
  table_spring$`Mean (95% CI)` <- paste(round(apply(spring,2, mean), digits = 2),"[",round(apply(spring, 2, quantile, 0.025),digits = 2),",",round(apply(spring, 2, quantile, 0.975), digits = 2),"]")
}

#range
for(i in 1:ncol(spring)){
  table_spring$`Range (min-max)` <- paste(round(apply(spring,2, min), digits = 2),"-",round(apply(spring, 2, max),digits = 2))
}


### export to files to use in excel ###


write.csv(table_spring, "Table_spring.csv")





##create dataset to fill in ##

summer <- predVal.summer[,c(29,4,23,11)]

table_summer <- data.frame(matrix(ncol = 2, nrow = 4))

#column names (remember to change ag to agricultural landcover later)
x <- c("Mean (95% CI)", "Range (min-max)")

#apply new column names to new data frame
colnames(table_summer) <- x

#compound names for row names

y <- c("Estrogenicity","Cholesterol","Metolachlor", "Atrazine")

row.names(table_summer) <- y

##attempt at for loop (both Mean + range)

for(i in 1:ncol(summer)){
  table_summer$`Mean (95% CI)` <- paste(round(apply(summer,2, mean), digits = 2),"[",round(apply(summer, 2, quantile, 0.025),digits = 2),",",round(apply(summer, 2, quantile, 0.975), digits = 2),"]")
}

#range
for(i in 1:ncol(summer)){
  table_summer$`Range (min-max)` <- paste(round(apply(summer,2, min), digits = 2),"-",round(apply(summer, 2, max),digits = 2))
}

### export to files to use in excel ###


write.csv(table_summer, "Table_summer.csv")



#### Fall ####


fall <- predVal.fall[,c(29,4,23,11)]


##create dataset to fill in ##
table_fall <- data.frame(matrix(ncol = 2, nrow = 4))

#column names (remember to change ag to agricultural landcover later)
x <- c("Mean (95% CI)", "Range (min-max)")

#apply new column names to new data frame
colnames(table_fall) <- x

#compound names for row names

y <- c("Estrogenicity","Cholesterol","Metolachlor", "Atrazine")

row.names(table_fall) <- y

##attempt at for loop (both Mean + range)

for(i in 1:ncol(fall)){
  table_fall$`Mean (95% CI)` <- paste(round(apply(fall,2, mean), digits = 2),"[",round(apply(fall, 2, quantile, 0.025),digits = 2),",",round(apply(fall, 2, quantile, 0.975), digits = 2),"]")
}

#range
for(i in 1:ncol(fall)){
  table_fall$`Range (min-max)` <- paste(round(apply(fall,2, min), digits = 2),"-",round(apply(fall, 2, max),digits = 2))
}

### export to files to use in excel ###


write.csv(table_fall, "Table_fall.csv")







#### Winter ####

winter <- predVal.winter[,c(29,4,23,11)]


## put info above into table (easier to read/interpret) ##

##create dataset to fill in ##
table_winter <- data.frame(matrix(ncol = 2, nrow = 4))

#column names (remember to change ag to agricultural landcover later)
x <- c("Mean (95% CI)", "Range (min-max)")

#apply new column names to new data frame
colnames(table_winter) <- x

#compound names for row names

y <- c("Estrogenicity","Cholesterol","Metolachlor", "Atrazine")

row.names(table_winter) <- y

##attempt at for loop (both Mean + range)

for(i in 1:ncol(winter)){
  table_winter$`Mean (95% CI)` <- paste(round(apply(winter,2, mean), digits = 2),"[",round(apply(winter, 2, quantile, 0.025),digits = 2),",",round(apply(winter, 2, quantile, 0.975), digits = 2),"]")
}

#range
for(i in 1:ncol(winter)){
  table_winter$`Range (min-max)` <- paste(round(apply(winter,2, min), digits = 2),"-",round(apply(winter, 2, max),digits = 2))
}

### export to files to use in excel ###


write.csv(table_winter, "Table_winter.csv")









################# plot results ####################




##### overall plot of all 38 contaminants (spring) #####

name_figure <- "Occurence_curve_all_spring.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 6.5, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

#nf <- layout(matrix(c(1:5), nrow = 5, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
#layout.show(nf)
par(mar = c(5, 0.0, 2, 0) + 0.1,oma=c(7,4,1,3),mai=c(0.0,0.05,0.05,0)) # Adjust margins
#def.par <- par(no.readonly = TRUE)    


plot(X.2[,6], predVal.spring[,17],type = "n", ylim = c(0,1), xlab = "Agricultural Landcover", ylab = "Probability of Occurence")
for(i  in 1:38){
  lines(X.2[,6], predVal.spring[,i])
  
}

mtext(side = 2, "Probability of occurence", line = 2.5, adj = 0.5, outer = TRUE)
mtext(side = 1, "Agricultural Landcover (%)", line = 2.5, adj = 0.5, outer = TRUE)

par(def.par)
dev.off() # END PLOT



##### overall plot of all 38 contaminants (summer) #####

name_figure <- "Occurence_curve_all_summer.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 6.5, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

#nf <- layout(matrix(c(1:5), nrow = 5, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
#layout.show(nf)
par(mar = c(5, 0.0, 2, 0) + 0.1,oma=c(7,4,1,3),mai=c(0.0,0.05,0.05,0)) # Adjust margins
#def.par <- par(no.readonly = TRUE)    


plot(X.2[,6], predVal.summer[,17],type = "n", ylim = c(0,1), xlab = "Agricultural Landcover", ylab = "Probability of Occurence")
for(i  in 1:38){
  lines(X.2[,6], predVal.summer[,i])
  
}

mtext(side = 2, "Probability of occurence", line = 2.5, adj = 0.5, outer = TRUE)
mtext(side = 1, "Agricultural Landcover (%)", line = 2.5, adj = 0.5, outer = TRUE)

par(def.par)
dev.off() # END PLOT



##### overall plot of all 38 contaminants (fall) #####

name_figure <- "Occurence_curve_all_fall.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 6.5, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

#nf <- layout(matrix(c(1:5), nrow = 5, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
#layout.show(nf)
par(mar = c(5, 0.0, 2, 0) + 0.1,oma=c(7,4,1,3),mai=c(0.0,0.05,0.05,0)) # Adjust margins
#def.par <- par(no.readonly = TRUE)    


plot(X.2[,6], predVal.fall[,17],type = "n", ylim = c(0,1), xlab = "Agricultural Landcover", ylab = "Probability of Occurence")
for(i  in 1:38){
  lines(X.2[,6], predVal.fall[,i])
  
}

mtext(side = 2, "Probability of occurence", line = 2.5, adj = 0.5, outer = TRUE)
mtext(side = 1, "Agricultural Landcover (%)", line = 2.5, adj = 0.5, outer = TRUE)

par(def.par)
dev.off() # END PLOT



##### overall plot of all 38 contaminants (winter) #####

name_figure <- "Occurence_curve_all_winter.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 6.5, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

#nf <- layout(matrix(c(1:5), nrow = 5, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
#layout.show(nf)
par(mar = c(5, 0.0, 2, 0) + 0.1,oma=c(7,4,1,3),mai=c(0.0,0.05,0.05,0)) # Adjust margins
#def.par <- par(no.readonly = TRUE)    


plot(X.2[,6], predVal.winter[,17],type = "n", ylim = c(0,1), xlab = "Agricultural Landcover", ylab = "Probability of Occurence")
for(i  in 1:38){
  lines(X.2[,6], predVal.winter[,i])
  
}

mtext(side = 2, "Probability of occurence", line = 2.5, adj = 0.5, outer = TRUE)
mtext(side = 1, "Agricultural Landcover (%)", line = 2.5, adj = 0.5, outer = TRUE)

par(def.par)
dev.off() # END PLOT











###################################################################################

name_figure <- "Occurence_curve_all_panel_2_ag.tiff" # Name of figure
tiff(filename = name_figure, height = 6, width = 11, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

nf <- layout(matrix(c(1:4), nrow = 4, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
layout.show(nf)

par(op)
par(oma=c(5,5,0,0),mar=c(3,3,2,2),mfrow=c(2,2), mai=c(0.1,0.1,0.1,0.1))

##### overall plot of all 38 contaminants (spring) #####

plot(X.2[,6], predVal.spring[,i],type = "n", ylim = c(0,1), xaxt="n", las=1)
for(i  in 1:29){
  lines(X.2[,6], predVal.spring[,i])
  
}
text(2,0.99, "A")

##### overall plot of all 38 contaminants (summer) #####


plot(X.2[,6], predVal.summer[,i],type = "n", ylim = c(0,1), yaxt="n", xaxt="n")
for(i  in 1:29){
  lines(X.2[,6], predVal.summer[,i])
  
}
text(3,1, "B")

##### overall plot of all 38 contaminants (fall) #####


plot(X.2[,6], predVal.fall[,i],type = "n", ylim = c(0,1), las=1)
for(i  in 1:29){
  lines(X.2[,6], predVal.fall[,i])
  
}
text(2,0.99, "C")

##### overall plot of all 38 contaminants (winter) #####

plot(X.2[,6], predVal.winter[,i],type = "n", ylim = c(0,1), yaxt="n")
for(i  in 1:29){
  lines(X.2[,6], predVal.winter[,i])
  
}
text(2,0.99, "D")

#x-axis for entire plot
#axis(1,round(X.2[,6]))

# x and y labels for entire plot

mtext(side = 2, "Predicted Probability of Occurence", line = 3, adj = 0.5, outer = TRUE)
mtext(side = 1, "Agricultural Landcover (%)", line = 3, adj = 0.5, outer = TRUE)

par(def.par)
dev.off() # END PLOT


##########################################################################