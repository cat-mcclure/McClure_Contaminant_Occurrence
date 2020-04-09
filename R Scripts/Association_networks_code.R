

###################################################################################################
############################################## AG Dataset ######################################
####################################################################################################

#model_2

### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_2,cor=FALSE)



################################### Observation level #########################################



### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values
  
truth <- as.vector(tcrossprod(formparam$param$paramLatent[[1]])[ltri])

### Average
  
average <- as.vector(apply(corMat[, , , 1], 1:2, mean)[ltri])
  
### 95% confidence intervals
  
corMat.025 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)
  
### Plot the results
  
plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_2, cor = TRUE)

averageCor <- apply(corMat[, , , 1], 1:2, mean)
Cor2.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_obs.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



############################## Site level #######################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_2,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[2]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 2], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_2, cor = TRUE)

averageCor <- apply(corMat[, , , 2], 1:2, mean)
Cor2.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_site.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



######################################### Year level ########################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_2,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[3]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 3], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_2, cor = TRUE)

averageCor <- apply(corMat[, , , 3], 1:2, mean)
Cor2.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_year.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##








####################################################################################################
############################## FLOW Dataset ####################################################### 
####################################################################################################

#model

### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model,cor=FALSE)



########################## Observation level #######################################################



### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[1]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 1], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model, cor = TRUE)

averageCor <- apply(corMat[, , , 1], 1:2, mean)
Cor2.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_obs.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



######################################## Site level ################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[2]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 2], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model, cor = TRUE)

averageCor <- apply(corMat[, , , 2], 1:2, mean)
Cor2.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_site.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



########################################## Year level ##################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[3]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 3], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model, cor = TRUE)

averageCor <- apply(corMat[, , , 3], 1:2, mean)
Cor2.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_year.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##








#########################################################################################################
################################ AG Intercept-only Dataset ##############################################
########################################################################################################


#model_3

### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_3,cor=FALSE)



################################### Observation level #########################################



### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[1]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 1], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_3, cor = TRUE)

averageCor <- apply(corMat[, , , 1], 1:2, mean)
Cor2.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_INT_obs.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



############################## Site level #######################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_3,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[2]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 2], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_3, cor = TRUE)

averageCor <- apply(corMat[, , , 2], 1:2, mean)
Cor2.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_INT_site.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



######################################### Year level ########################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_3,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[3]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 3], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_3, cor = TRUE)

averageCor <- apply(corMat[, , , 3], 1:2, mean)
Cor2.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_AG_INT_year.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##







####################################################################################################
############################## FLOW Intercept-only Dataset ####################################################### 
####################################################################################################

#model_4

### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_4,cor=FALSE)



########################## Observation level #######################################################



### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[1]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 1], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 1], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_4, cor = TRUE)

averageCor <- apply(corMat[, , , 1], 1:2, mean)
Cor2.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,1], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_INT_obs.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



######################################## Site level ################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_4,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[2]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 2], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 2], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_4, cor = TRUE)

averageCor <- apply(corMat[, , , 2], 1:2, mean)
Cor2.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,2], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_INT_site.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##



########################################## Year level ##################################################



### Plot random effect estimation through correlation matrix

corMat <- corRandomEff(model_4,cor=FALSE)

#-------------------------------------------------------------------------- 
### Sampling units level 
#---------------------------------------------------------------------------- 
### Isolate the values of interest

ltri <- lower.tri(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025), diag=TRUE)

### True values

truth <- as.vector(tcrossprod(formparam$param$paramLatent[[3]])[ltri])

### Average

average <- as.vector(apply(corMat[, , , 3], 1:2, mean)[ltri])

### 95% confidence intervals

corMat.025 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs = 0.025)[ltri]) 

corMat.975 <- as.vector(apply(corMat[, , , 3], 1:2, quantile, probs=0.975)[ltri])

CI <- cbind(corMat.025, corMat.975)

### Plot the results

plot(0,
     0,
     xlim = c(1, nrow(CI)),
     ylim = range(CI, truth),
     type = "n",
     xlab = "",
     main = "cov(paramLatent[[1, 1]])") 

abline(h = 0, col = "grey")

arrows(x0 = 1:nrow(CI),
       x1 = 1:nrow(CI),
       y0 = CI[, 1],
       y1 = CI[, 2], 
       code = 3,
       angle = 90,
       length = 0.05) 

points(1:nrow(CI), average, pch = 15,cex = 1.5)

points(1:nrow(CI), truth, col = "red", pch=19)

### Draw chord diagram

library(circlize)

corMat <- corRandomEff(model_4, cor = TRUE)

averageCor <- apply(corMat[, , , 3], 1:2, mean)
Cor2.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.025)
Cor97.5 <- apply(corMat[,,,3], 1:2, quantile, probs=0.975)

# Use this to plot only significant cors
sigCors <- Cor2.5 * Cor97.5 > 0


colMat <- matrix(NA, nrow = nrow(averageCor), ncol = ncol(averageCor)) 

## assign colors for significant pos or neg co-occurrence
colMat[which(averageCor > 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "red"
colMat[which(averageCor < 0 & sigCors =='TRUE', arr.ind = TRUE)] <- "blue"


### ben's code for swinging the axis labels outward ###

res<-6
tiff(filename = 'chord_diagram_FLOW_INT_year.tiff', height = 800*res, width = 800*res, res=70*res)
def.par <- par(no.readonly = TRUE) 
chordDiagram(averageCor, symmetric = TRUE,
             annotationTrack = c("grid"), grid.col = "grey",
             col = colMat,
             annotationTrackHeight = c(0.01, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(averageCor))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, cex = 1.2,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

par(def.par)

dev.off()

##

