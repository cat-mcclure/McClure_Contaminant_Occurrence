

setwd("~/Desktop/Classwork/CEC_Project/HMSC")
load("model_2")

# ### Mixing object
mixing<-as.mcmc(model_2,parameters="paramX")

# ### Convert the mixing object to a matrix
mixingDF<-as.data.frame(mixing)
dim(mixingDF)
head(mixingDF)

## posterior means
post.means <- apply(mixingDF,2,mean)

## 95% CIs
lower <- apply(mixingDF,2,quantile, 0.025)
upper <- apply(mixingDF,2,quantile, 0.975)

## create data set for plotting
sum1 <- data.frame(post.means,lower,upper)
names <- row.names(sum1)

## convert to DF
plot.dat1 <- data.frame(names,sum1)
dim(plot.dat1)
row.names(plot.dat1) <- NULL

species.names<- colnames(Y)


## Intercepts plot ##
p.intercept<-plot.dat1[1:32,]
p.intercept

png("Intercept_landcover.png", 490, 350)
par(mar=c(10,4,4,4))
plot(p.intercept$post.means,
     xaxt='n', ann=FALSE,
     ylim=c(-6,5),
     ylab = "Intercept",
     xlab = "",
     pch = 19)
arrows(x0 = c(1:38), x1 = c(1:38), y0 = p.intercept$lower, 
       y1 = p.intercept$upper, code = 3, angle = 90, length = 0)
axis(1,las=2, at= c(1:29))
abline  (h=0, col = "red", lty = 3, lwd = 2)
dev.off()

######################## ######################## ######################## ######################## 
################################## Effects plot #############################################
######################## ######################## ######################## ######################## 


name_figure <- "EffectsFig_AG.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 10, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

nf <- layout(matrix(c(1:4), nrow = 4, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
layout.show(nf)
par(mar = c(12, 0, 2, 5) + 0.1,oma=c(13,6,1,5),mai=c(0,0.05,0.1,0)) # Adjust margins
def.par <- par(no.readonly = TRUE)    

pdat1 <- plot.dat1[30:58,]
plot(pdat1$post.means,
     xaxt='n', ann=FALSE,
     ylim=c(-0.9,1.25),
     ylab = "Percent Agricultural",
     xlab = "",
     pch = 19,
     las=1,
     cex.axis=0.9)
arrows(x0 = c(1:29), x1 = c(1:29), y0 = pdat1$lower, 
       y1 = pdat1$upper, code = 3, angle = 90, length = 0)
abline  (h=0, col = "red", lty = 3, lwd = 1)
text(0.3,1.1, "A")

pdat2 <- plot.dat1[59:87,]
plot(pdat2$post.means,
     xaxt='n', ann=FALSE,
     ylim=c(-1.25,1.3),
     ylab = "Spring",
     xlab = "",
     pch = 19,
     las=1,
     cex.axis=0.9)
arrows(x0 = c(1:29), x1 = c(1:29), y0 = pdat2$lower, 
       y1 = pdat2$upper, code = 3, angle = 90, length = 0)
abline  (h=0, col = "red", lty = 3, lwd = 1)
text(0.3,1.1, "B")

pdat3 <- plot.dat1[88:116,]
plot(pdat3$post.means,
     xaxt='n', ann=FALSE,
     ylim=c(-1.25,1.7),
     ylab = "Summer",
     xlab = "",
     pch = 19,
     las=1,
     cex.axis=0.9)
arrows(x0 = c(1:29), x1 = c(1:29), y0 = pdat3$lower, 
       y1 = pdat3$upper, code = 3, angle = 90, length = 0)
abline  (h=0, col = "red", lty = 3, lwd = 1)
text(0.3,1.5, "C")

pdat4 <- plot.dat1[117:145,]
plot(pdat4$post.means,
     xaxt='n', ann=FALSE,
     ylim=c(-1.1,1.3),
     ylab = "Fall",
     xlab = "",
     pch = 19,
     las=1,
     cex.axis=0.9)
arrows(x0 = c(1:29), x1 = c(1:29), y0 = pdat4$lower, 
       y1 = pdat4$upper, code = 3, angle = 90, length = 0)
abline  (h=0, col = "red", lty = 3, lwd = 1)
text(0.3,1.1, "D")


## Axis for entire plot 
axis(1,las=2,labels = species.names, at= c(1:29),  cex.axis = 1.2)
mtext(side = 2, "Estimated effect", line = 3, adj = 0.5, outer = TRUE, cex = 1)

par(def.par)
dev.off() # END PLOT


######################## ######################## ######################## ######################## 
######################## panel plots of probability of a positive ###################################################
#################### relationship between landcover and contaminant #######################
######################## ######################## ######################## ######################## 

str(mixingDF)

#create function to look at the mean of only the positive relationships of the posterior iterations
probs.fun <- function(x){mean(x>0)}

#apply function and make into dataframe
probs <- data.frame(apply(mixingDF,2,probs.fun))
probs$pos <- probs[,1]

names <- row.names(probs)
plot.dat2 <- data.frame(names,probs)
dim(plot.dat2)
row.names(plot.dat2) <- NULL

species.names<- colnames(Y)

####### Effects plot ############

name_figure <- "EffectsFig_AG_probs.tiff" # Name of figure
tiff(filename = name_figure, height = 5, width = 10, units='in', res=600) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

nf <- layout(matrix(c(1:4), nrow = 4, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
layout.show(nf)
par(mar = c(8, 0, 2, 0) + 0.1,oma=c(13,6,1,5),mai=c(0,0.05,0.1,0)) # Adjust margins
def.par <- par(no.readonly = TRUE)    

pdat5 <- plot.dat2[30:58,]
plot(pdat5$pos,
     xaxt='n', ann=FALSE,
     ylim=c(0,1),
     ylab = "Agricultural Landcover",
     xlab = "",
     pch = 19,
     cex= 0.8,
     las=1,
     cex.axis =0.9)
abline(h=0.5, col = "red", lty = 3, lwd = 1)
text(0.2,0.85, "A")

pdat6 <- plot.dat2[59:87,]
plot(pdat6$pos,
     xaxt='n', ann=FALSE,
     ylim=c(0,1),
     ylab = "Spring",
     xlab = "",
     pch = 19,
     cex= 0.8,
     las=1,
     cex.axis =0.9)
abline(h=0.5, col = "red", lty = 3, lwd = 1)
text(0.2,0.85, "B")

pdat7 <- plot.dat2[88:116,]
plot(pdat7$pos,
     xaxt='n', ann=FALSE,
     ylim=c(0,1),
     ylab = "Summer",
     xlab = "",
     pch = 19,
     cex = 0.8,
     las=1,
     cex.axis =0.9)
abline(h=0.5, col = "red", lty = 3, lwd = 1)
text(0.2,0.85, "C")

pdat8 <- plot.dat2[117:145,]
plot(pdat8$pos,
     xaxt='n', ann=FALSE,
     ylim=c(0,1),
     ylab = "Fall",
     xlab = "",
     pch = 19,
     cex =0.8,
     las=1,
     cex.axis =0.9)
abline(h=0.5, col = "red", lty = 3, lwd = 1)
text(0.2,0.85, "D")

## Axis for entire plot 
axis(1,las=2,labels = species.names, at= c(1:29), cex.axis = 1.2)
mtext(side = 2, "Probability of a positive effect", line = 3, adj = 0.5, outer = TRUE, cex = 1)

par(def.par)
dev.off() # END PLOT
