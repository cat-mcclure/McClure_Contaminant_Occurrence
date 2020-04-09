


#######################################################
##### side by side panel plot of var part figures #####
#######################################################


library(RColorBrewer)


res<-8
tiff(filename = 'varpart_both.tiff', height = 800*res, width = 1200*res, res=75*res) # Figure size, resolution, etc.
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

nf <- layout(matrix(c(2:1), nrow = 2, ncol=1,byrow=TRUE)) # Change the size of this matrix to match the number of predictor variables you want in the panel plot
layout.show(nf)

par(op)
par(oma=c(16,6,1,3),mar=c(16,3,3,1), mfrow=c(2,1), mai=c(0.15,0,0.1,4), xpd=TRUE)


## create object (change second argument to match structure of X matrix)

################################
########## flow and ag ########
##################################

variationPart <- variPart(model, c("Intercept", "Stream Discharge","Spring","Summer","Fall", "Agricultural Landcover"), type = "I",  indSite = TRUE)

variationPart <- variationPart[,2:9]

##### re-order var part df to allign for legend coloring #####
#### i.e., VariationPart = VariationPart_2, except for 'stream discharge' #####

test <- variationPart[,c("Agricultural Landcover", "Spring", "Summer", "Fall", "Stream Discharge", "Observation", "Site", "Year")]

variationPart <- test

barplot(t(variationPart),
        bty='L',
        las=2,
        cex.axis = 1.5,
        col= brewer.pal(n = 8, name = "PRGn"),
        xaxt='n')
legend("topright",
       inset = c(-0.35,0),
       cex = 1.3,
       legend=paste(colnames(variationPart),
                    signif(100*colMeans(variationPart),2),
                    "%"),
       list(y=1,x=nrow(variationPart)/29, xjust=0, horiz=F),
       fill=brewer.pal(n = ncol(variationPart), name = "PRGn"))

##################################
########### ag only ##############
##################################

## create object (change second argument to match structure of X matrix)

variationPart_2 <- variPart(model_2, c("Intercept", "Agricultural Landcover", "Spring","Summer","Fall"), type = "I",  indSite = TRUE)

variationPart_2 <- variationPart_2[,2:8]

#create column of 0's for stream discharge in order to match colors in legends

variationPart_3 <- as.data.frame(variationPart_2)

variationPart_3$streamdischarge <- 0

setnames(variationPart_3, old = "streamdischarge", new = "Stream Discharge")

variationPart_3 <- as.matrix(variationPart_3)

variationPart_2 <- variationPart_3[, c("Agricultural Landcover", "Spring", "Summer", "Fall", "Stream Discharge", "Observation", "Site", "Year")]

## plotting ##

barplot(t(variationPart_2),
        bty='L',
        args.legend=list(y=1,
                         x=nrow(variationPart_2)/29,
                         xjust=0,
                         horiz=F),
        las=2,
        cex.axis = 1.5,
        cex.names = 1.5,
        col= brewer.pal(n = 8, name = "PRGn"))
legend("topright",
       inset = c(-0.35,0),
       cex = 1.3,
       legend=paste(colnames(variationPart_2),
                    signif(100*colMeans(variationPart_2),2),
                    "%"),
       list(y=1,x=nrow(variationPart_2)/29, xjust=0, horiz=F),
       fill = brewer.pal(n = 8, name = "PRGn"))



# x and y labels for entire plot

mtext(side = 2, "Proportion of the total variance", line = 4, adj = 0.6, cex = 1.9, outer = TRUE)

par(def.par)
dev.off()



########################################################################################################
#################################### FLOW data set varpart fig #################################
########################################################################################################



# library needed

library(RColorBrewer)



## format figure
# Figure size, resolution, etc.

res<-8
tiff(filename = 'varpart_FLOW.tiff', height = 700*res, width = 1000*res, res=60*res)
def.par <- par(no.readonly = TRUE)     
par(oma=c(11,2,1,5),mar=c(12,3,3,0),mai=c(0.15,0.4,0,3.2), xpd=TRUE)



#### create object (change second argument to match structure of X matrix)

variationPart <- variPart(model, c("Intercept", "Stream Discharge","Spring","Summer","Fall", "Agricultural Landcover"), type = "I",  indSite = TRUE)

variationPart <- variationPart[,2:9]

##### re-order var part df to allign for legend coloring #####
#### i.e., VariationPart = VariationPart_2, except for 'stream discharge' #####

test <- variationPart[,c("Agricultural Landcover", "Spring", "Summer", "Fall", "Stream Discharge", "Observation", "Site", "Year")]

variationPart <- test

#par(mar = c(8, 3, 1, 13), oma = c(5,2,0,0), xpd=TRUE)
barplot(t(variationPart),
        bty='L',
        las=2,
        args.legend=list(y=1,
                         x=nrow(variationPart_2)/38,
                         xjust=0,
                         horiz=F),
        col= brewer.pal(n = 8, name = "PRGn"))
legend("topright",
       inset = c(-0.26,0),
       cex = 1.3,
       legend=paste(colnames(variationPart),
                    signif(100*colMeans(variationPart),2),
                    "%"),
       list(y=1,x=nrow(variationPart)/38, xjust=0, horiz=F),
       fill=brewer.pal(n = ncol(variationPart), name = "PRGn"))



par(def.par)

dev.off()




mean(variationPart[1:28,8])
