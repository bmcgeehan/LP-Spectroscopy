library(lattice)
library(glmnet)
library(ggplot2)
library(reshape2)
library(psych)
library(plotly)

setwd("/Users/Brendan/Dropbox/Kaggle 2014/Data")
load("labels_train.Rda")
load("FNC_train.Rda")
load("SBM_train.Rda")


LPplot <- function(x, y, m) {
  t <- LPT(x, m)
  LPcorr <- cor(y, t)
  Tsmooth <- LP.smooth(LPcorr, length(y), "AIC")
  reducedT <- t[, which(Tsmooth != 0)]
  reducedsplit <- strsplit(colnames(reducedT), "_")
  tmatsplit <- strsplit(colnames(t), "_")
  tmatnames <- sapply(tmatsplit, "[", 1)
  reducednames <- sapply(reducedsplit, "[", 1)
  sigfeat <- t[,tmatnames %in% reducednames]
  sigcor <- cor(y, sigfeat)
  matcor <- matrix(sigcor^2, nrow=m, ncol=length(unique(reducednames)))
  matcor[is.na(matcor)] <- 0
  colnames(matcor) <- as.numeric(unique(reducednames))
  ##I want to order the matrix
  sorted <- matcor[,order(colSums(matcor), decreasing=TRUE)]
  xa <- list(

    title = "LP Moments"
  )
  ya <- list(
    showticklabels = FALSE,
    title = "Significant Features"
  )
  
  c <- list(c(0, 'azure'),c(.5, '#9ecae1'),c(1,'#3182bd'))
  bar <- list(
    title = "Correlation"
  )
  ###Creates plot for now, the return of the function will be list 
  ###I want to return a ggplot object, which can then be ploted using plotly
  
  #ggplot()
  #plot <- 
  plot_ly(z=t(sorted), x=1:m, y=colnames(x[,as.numeric(colnames(sorted))]),
          type="heatmap", hoverinfo = "x+y+z", colorscale=c, colorbar = bar) %>%
    layout(xaxis = xa, yaxis = ya)
  
  ###output for a matrix with name of features and corresponding lpinfor
  ###This 'values' variable can be plotted to see the "jumps" in the LPinform values
#  lpinfor <- as.vector(sort(colSums(sorted), decreasing=TRUE))
#  values <- as.data.frame(cbind(colnames(x[,as.numeric(colnames(sorted))]), lpinfor))
#  colnames(values) <- c("Features", "LPInform")
#  values$LPInform <- sort(as.numeric(levels(values$LPInform)), decreasing=TRUE)
#  values$Features <- factor(values$Features, levels=unique(values$Features))
#  return(list(values, plot))
  #reshapedcorr <- melt(sortCorr)
  #ggplot(reshapedcorr, aes(x=Var1, y=Var2)) + geom_raster(aes(fill=value)) + scale_fill_gradient( low= "azure", high="steelblue2") 
  
  #  cp <- colorRampPalette(c("azure", "steelblue2"))
  #  levelplot(sorted, ylab = "Significant Features", xlab = "LP Moments", 
  #           col.regions=cp, main="LP Feature Correlations", aspect="fill", scales=list(
  #              y=list(
  #                at=seq(1:length(unique(reducednames))),
  #labels=colnames(x[,as.numeric(unique(reducednames))])[order(sorted)],
  #                labels=colnames(x[,reducednames[order(colSums(matcor))]]),
  #                cex=.5
  #              ),
  #              x=list(
  #                at=seq(1:m),
  #                labels= c(1:m)
  #              )
  #            )
  #  )
}


#####This is exploratory####
##Do NOT run####
xa <- list(
  nticks = m,
  title = "LP Moments"
)
ya <- list(
  showticklabels = FALSE,
  title = "Significant Features"
)

c <- list(c(0, 'azure'),c(.5, '#9ecae1'),c(1,'#3182bd'))
bar <- list(
  title = "Correlation"
)
plot_ly(z=t(sort_mat), x=seq(1:3), y=colnames(SBM_train[,as.numeric(colnames(sort_mat))]),
        type="heatmap", hoverinfo = "x+y+z", colorscale=c, colorbar = bar) %>%
        layout(xaxis = xa, yaxis = ya)

colnames(mat_cor) <- as.numeric(unique(r_names))
sort_mat <- mat_cor[,order(colSums(mat_cor), decreasing=TRUE)]

colnames(SBM_train[,as.numeric(colnames(sort_mat))])
colnames(sort_mat)

reshapedcorr <- melt(sort_mat)

ggplot(reshapedcorr, aes(x=Var1, y=Var2)) + geom_raster(aes(fill=value)) + scale_fill_gradient( low= "azure", high="steelblue2") 


reducedfeat <- cbind(labels_train$Class, FNC_train$FNC183, FNC_train$FNC244, 
                     SBM_train$SBM_map36, SBM_train$SBM_map67,
                     SBM_train$SBM_map61)


#######################

t <- LPT(FNC_train[,-1], 3)
LPcorr <- cor(labels_train$Class, t)
Tsmooth <- LP.smooth(LPcorr, length(labels_train$Class), "AIC")
reducedT <- t[, which(Tsmooth != 0)]
reducedsplit <- strsplit(colnames(reducedT), "_")
tmatsplit <- strsplit(colnames(t), "_")
tmatnames <- sapply(tmatsplit, "[", 1)
reducednames <- sapply(reducedsplit, "[", 1)
sigfeat <- t[,tmatnames %in% reducednames]
sigcor <- cor(labels_train$Class, sigfeat)
matcor <- matrix(sigcor^2, nrow=3, ncol=length(unique(reducednames)))
matcor[is.na(matcor)] <- 0
colnames(matcor) <- as.numeric(unique(reducednames))
##I want to order the matrix

load <- as.matrix(t(matcor))
nitems <- NROW(load)
nfactors <- NCOL(load)
loads <- data.frame(item = seq(1:nitems), cluster = rep(0,nitems), unclass(load))
loads$cluster <- apply(abs(load), 1, which.max)
ord <- sort(loads$cluster, index.return = TRUE)
loads[1:nitems, ] <- loads[ord$ix, ]
rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
items <- table(loads$cluster)
first <- 1
item <- loads$item
for (i in 1:length(items)) {
  if (items[i] > 0) {
    last <- first + items[i] - 1
    ord <- sort(abs(loads[first:last, i + 2]), decreasing = TRUE,index.return = TRUE)
    loads[first:last, 3:(nfactors + 2)] <- load[item[ord$ix + first - 1], ]
    loads[first:last, 1] <- item[ord$ix + first - 1]
    rownames(loads)[first:last] <- rownames(loads)[ord$ix + first - 1]
    first <- first + items[i]
  }
}
item.order <- loads[, 1]
order1<-item.order[length(item.order):1]
sorted <-t(t(matcor)[order1,])
colnames(sorted) <- colnames(FNCtrain[,as.numeric(colnames(sorted))])

sortmelt <- melt(sorted)
colnames(sortmelt) <- c("LPMoment", "SelectedFeatures", "LPInformation")
sortmelt[,1] <- as.factor(sortmelt[,1])
sortmelt[,2] <- as.factor(sortmelt[,2])
###Make an option for sorting the results


sorted <- matcor[,order(colSums(matcor), decreasing=TRUE)]
SBMtrain <- SBM_train[,-1]
FNCtrain <- FNC_train[,-1]
colnames(sorted) <- colnames(SBMtrain[,as.numeric(colnames(sorted))])
sortmelt <- melt(sorted)
colnames(sortmelt) <- c("LPMoment", "SelectedFeatures", "LPInformation")
sortmelt[,1] <- as.factor(sortmelt[,1])
sortmelt[,2] <- as.factor(sortmelt[,2])
ggplot(sortmelt, aes(Var2, fill=Var1)) + geom_bar()
##might have to rename the variables using rename()
#stackedbar <- ggplot(data = sortmelt, aes(x=`Selected Features`,y=`LP Information`, fill=`LP Moment`)) +  geom_bar(stat="identity") + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
stackedbar <- ggplot(data = sortmelt, aes(x=SelectedFeatures, text = paste("Feature:", SelectedFeatures))) +  geom_bar( stat="identity", aes(y=LPInformation, fill=LPMoment)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
##plotly_build
ggplotly(stackedbar)
ggplotly()


heat <- ggplot(data=sortmelt, aes(LPMoment, SelectedFeatures, fill=LPInformation)) +
  geom_tile() + scale_fill_continuous() +
  theme(axis.ticks.y = element_blank()) +
  labs(x="LP Moments", y="Features")
p <- plotly_build(heat)
p$layout$yaxis$showticklabels <- FALSE
p$data[[1]]$z <- t(sorted)
p$data[[1]]$hoverinfo <- "x+y+z"
p$data[[1]]$colorscale = c
p$data[[2]]$marker$colorscale =c
plotly_build(p)

plxa <- list(
  
  title = "LP Moments"
)
ya <- list(
  showticklabels = FALSE,
  title = "Significant Features"
)

c <- list(c(0, 'azure'),c(.5, '#9ecae1'),c(1,'#3182bd'))
bar <- list(
  title = "Correlation"
)
RColorBrewer::display.brewer.all()


LPplot <- function(x, y, m) {
  t <- LPT(x, m)
  LPcorr <- cor(y, t)
  Tsmooth <- LP.smooth(LPcorr, length(y), "AIC")
  reducedT <- t[, which(Tsmooth != 0)]
  reducedsplit <- strsplit(colnames(reducedT), "_")
  tmatsplit <- strsplit(colnames(t), "_")
  tmatnames <- sapply(tmatsplit, "[", 1)
  reducednames <- sapply(reducedsplit, "[", 1)
  sigfeat <- t[,tmatnames %in% reducednames]
  sigcor <- cor(y, sigfeat)
  matcor <- matrix(sigcor^2, nrow=m, ncol=length(unique(reducednames)))
  matcor[is.na(matcor)] <- 0
  colnames(matcor) <- as.numeric(unique(reducednames))
  ##I want to order the matrix
  sorted <- matcor[,order(colSums(matcor), decreasing=TRUE)]
  colnames(sorted) <- colnames(x[,as.numeric(colnames(sorted))])
  sortmelt <- melt(sorted)
  colnames(sortmelt) <- c("LPMoment", "SelectedFeatures", "LPInformation")
  
  ###If type equals heat map 
 # heat <- ggplot(data=sortmelt, aes(LPMoment, SelectedFeatures, fill=LPInformation)) +
#    geom_tile() + scale_fill_continuous() +
#    theme(axis.ticks.y = element_blank()) +
#    labs(x="LP Moments", y="Features")
 # p <- plotly_build(heat)
 # p$layout$yaxis$showticklabels <- FALSE
#  p$data[[1]]$z <- t(sorted)
 # p$data[[1]]$hoverinfo <- "x+y+z"
#  p$data[[1]]$colorscale = c
#  p$data[[2]]$marker$colorscale =c
#  plotly_build(p)
  ###Creates plot for now, the return of the function will be list 
  ###I want to return a ggplot object, which can then be ploted using plotly
  
  #plot_ly(z=t(sorted), x=1:m, y=colnames(x[,as.numeric(colnames(sorted))]),
  #        type="heatmap", hoverinfo = "x+y+z", colorscale=c, colorbar = bar) %>%
  #  layout(xaxis = xa, yaxis = ya)
  
  ###output for a matrix with name of features and corresponding lpinfor
  ###This 'values' variable can be plotted to see the "jumps" in the LPinform values
  # lpinfor <- as.vector(sort(colSums(sorted), decreasing=TRUE))
  # values <- as.data.frame(cbind(colnames(x[,as.numeric(colnames(sorted))]), lpinfor))
  # colnames(values) <- c("Features", "LPInform")
  #  values$LPInform <- sort(as.numeric(levels(values$LPInform)), decreasing=TRUE)
  #  values$Features <- factor(values$Features, levels=unique(values$Features))
  #  return(list(values, plot))
  #reshapedcorr <- melt(sortCorr)
  #ggplot(reshapedcorr, aes(x=Var1, y=Var2)) + geom_raster(aes(fill=value)) + scale_fill_gradient( low= "azure", high="steelblue2") 
  
  ggplot(data = sortmelt, aes(x=SelectedFeatures, text = paste("Feature:", SelectedFeatures))) +  geom_bar( stat="identity", aes(y=LPInformation, fill=LPMoment)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  
}

