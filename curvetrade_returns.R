library(quantmod)
library(downloader)
library(ggplot2)

terms = c(1, 2, 3, 4, 5, 7, 10, 30)
for (term in terms) {
  getSymbols(paste('DSWP', term, sep=''), src='FRED')
}

DSWP1 = DSWP1[!is.na(DSWP1)]
DSWP2 = DSWP2[!is.na(DSWP2)]
DSWP3 = DSWP3[!is.na(DSWP3)]
DSWP4 = DSWP4[!is.na(DSWP4)]
DSWP5 = DSWP5[!is.na(DSWP5)]
DSWP7 = DSWP7[!is.na(DSWP7)]
DSWP10 = DSWP10[!is.na(DSWP10)]
DSWP30 = DSWP30[!is.na(DSWP30)]

# 2-1, 3-1, 4-1, 5-1, 7-1, 10-2, 10-5, 30-10
curve2_1 = last(DSWP2 - DSWP1, 250)
curve3_1 = last(DSWP3 - DSWP1, 250)
curve4_1 = last(DSWP4 - DSWP1, 250)
curve5_1 = last(DSWP5 - DSWP1, 250)
curve7_1 = last(DSWP7 - DSWP1, 250)
curve10_2 = last(DSWP10 - DSWP2, 250)
curve10_5 = last(DSWP10 - DSWP5, 250)
curve30_10 = last(DSWP30 - DSWP10, 250)

pccurve2_1 = dailyReturn(curve2_1)
pccurve3_1 = dailyReturn(curve3_1)
pccurve4_1 = dailyReturn(curve4_1)
pccurve5_1 = dailyReturn(curve5_1)
pccurve7_1 = dailyReturn(curve7_1)
pccurve10_2 = dailyReturn(curve10_2)
pccurve10_5 = dailyReturn(curve10_5)
pccurve30_10 = dailyReturn(curve30_10)

acfcurve2_1 = acf(curve2_1, plot=FALSE)
acfcurve2_1_dataframe = with(acfcurve2_1, data.frame(lag, acf))
q_curve2_1 = ggplot(data = acfcurve2_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 2s1s Swap Rate")
q_curve2_1

acfcurve3_1 = acf(curve3_1, plot=FALSE)
acfcurve3_1_dataframe = with(acfcurve3_1, data.frame(lag, acf))
q_curve3_1 = ggplot(data = acfcurve3_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 3s1s Swap Rate")
q_curve3_1

acfcurve4_1 = acf(curve4_1, plot=FALSE)
acfcurve4_1_dataframe = with(acfcurve4_1, data.frame(lag, acf))
q_curve4_1 = ggplot(data = acfcurve4_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 4s1s Swap Rate")
q_curve4_1

acfcurve5_1 = acf(curve5_1, plot=FALSE)
acfcurve5_1_dataframe = with(acfcurve5_1, data.frame(lag, acf))
q_curve5_1 = ggplot(data = acfcurve5_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 5s1s Swap Rate")
q_curve5_1

acfcurve7_1 = acf(curve7_1, plot=FALSE)
acfcurve7_1_dataframe = with(acfcurve7_1, data.frame(lag, acf))
q_curve7_1 = ggplot(data = acfcurve7_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 7s1s Swap Rate")
q_curve7_1

acfcurve10_2 = acf(curve10_2, plot=FALSE)
acfcurve10_2_dataframe = with(acfcurve10_2, data.frame(lag, acf))
q_curve10_2 = ggplot(data = acfcurve10_2_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 10s2s Swap Rate")
q_curve10_2

acfcurve10_5 = acf(curve10_5, plot=FALSE)
acfcurve10_5_dataframe = with(acfcurve10_5, data.frame(lag, acf))
q_curve10_5 = ggplot(data = acfcurve10_5_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 10s5s Swap Rate")
q_curve10_5

acfcurve30_10 = acf(curve30_10, plot=FALSE)
acfcurve30_10_dataframe = with(acfcurve30_10, data.frame(lag, acf))
q_curve30_10 = ggplot(data = acfcurve30_10_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 30s10s Swap Rate")
q_curve30_10

source("multiplot.R")
pdf("acf-curve.pdf",width=10,height=5)
multiplot(q_curve2_1, q_curve3_1, q_curve4_1, q_curve5_1, q_curve7_1, q_curve10_2, q_curve10_5, q_curve30_10, cols=4)
dev.off()

acfpccurve2_1 = acf(pccurve2_1, plot=FALSE)
acfpccurve2_1_dataframe = with(acfpccurve2_1, data.frame(lag, acf))
q_pccurve2_1 = ggplot(data = acfpccurve2_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 2s1s (% Change)")
q_pccurve2_1

acfpccurve3_1 = acf(pccurve3_1, plot=FALSE)
acfpccurve3_1_dataframe = with(acfpccurve3_1, data.frame(lag, acf))
q_pccurve3_1 = ggplot(data = acfpccurve3_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 3s1s (% Change)")
q_pccurve3_1

acfpccurve4_1 = acf(pccurve4_1, plot=FALSE)
acfpccurve4_1_dataframe = with(acfpccurve4_1, data.frame(lag, acf))
q_pccurve4_1 = ggplot(data = acfpccurve4_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 4s1s (% Change)")
q_pccurve4_1

acfpccurve5_1 = acf(pccurve5_1, plot=FALSE)
acfpccurve5_1_dataframe = with(acfpccurve5_1, data.frame(lag, acf))
q_pccurve5_1 = ggplot(data = acfpccurve5_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 5s1s (% Change)")
q_pccurve5_1

acfpccurve7_1 = acf(pccurve7_1, plot=FALSE)
acfpccurve7_1_dataframe = with(acfpccurve7_1, data.frame(lag, acf))
q_pccurve7_1 = ggplot(data = acfpccurve7_1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 7s1s (% Change)")
q_pccurve7_1

acfpccurve10_2 = acf(pccurve10_2, plot=FALSE)
acfpccurve10_2_dataframe = with(acfpccurve10_2, data.frame(lag, acf))
q_pccurve10_2 = ggplot(data = acfpccurve10_2_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 10s2s (% Change)")
q_pccurve10_2

acfpccurve10_5 = acf(pccurve10_5, plot=FALSE)
acfpccurve10_5_dataframe = with(acfpccurve10_5, data.frame(lag, acf))
q_pccurve10_5 = ggplot(data = acfpccurve10_5_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 10s5s (% Change)")
q_pccurve10_5

acfpccurve30_10 = acf(pccurve30_10, plot=FALSE)
acfpccurve30_10_dataframe = with(acfpccurve30_10, data.frame(lag, acf))
q_pccurve30_10 = ggplot(data = acfpccurve30_10_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("Curve 30s10s (% Change)")
q_pccurve30_10

pdf("acf-curve-returns.pdf",width=10,height=5)
multiplot(q_pccurve2_1, q_pccurve3_1, q_pccurve4_1, q_pccurve5_1, q_pccurve7_1, q_pccurve10_2, q_pccurve10_5, q_pccurve30_10, cols=4)
dev.off()

rates = cbind(curve2_1, curve3_1, curve4_1, curve5_1, curve7_1, curve10_2, curve10_5, curve30_10)
colnames(rates) = c('curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')

pc_rates = cbind(pccurve2_1, pccurve3_1, pccurve4_1, pccurve5_1, pccurve7_1, pccurve10_2, pccurve10_5, pccurve30_10)
colnames(pc_rates) = c('curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')

library(ggplot2)
library(reshape)
dataframe = data.frame(index(rates), rates)
colnames(dataframe) = c('date', 'curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Curve Rates') + ylab('Date')
pdf("descriptive-curve-rates.pdf",width=10,height=5)
plot
dev.off()

dataframe = data.frame(index(pc_rates), pc_rates)
colnames(dataframe) = c('date', 'curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Curve Rates') + ylab('Date')
pdf("descriptive-curve-rates-returns.pdf",width=10,height=5)
plot
dev.off()

pcadata = pc_rates
fit = princomp(pcadata, cor=FALSE, scores=TRUE)

covariance_matrix = cor(pcadata)
require(corrplot)
pdf("corrplot-curve-returns.pdf", width=5, height=5)
corrplot(covariance_matrix, method='shade', type='full', shade.col=NA, tl.col='black')
dev.off()

library(ggbiplot)
pdf("biplot-curve-returns.pdf", width=5, height=5)
ggbiplot(fit, obs.scale=1, var.scale=1)
dev.off()
pdf("screeplot-curve-returns.pdf", width=5, height=5)
ggscreeplot(fit)
dev.off()

scores = fit$scores
scores_dataframe = data.frame(index(pc_rates), scores)
colnames(scores_dataframe) = c('date', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('date', 'pc1', 'pc2', 'pc3')
scores_dataframe = scores_dataframe[keeps]
scores_melted = melt(scores_dataframe, id.vars='date')
plot = ggplot(data=scores_melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Date') + ylab('Principal Component Score')
pdf("pca-scores-curve.pdf", width=10, height=5)
plot
dev.off()

loadings = with(fit, unclass(loadings))
loadings

loadings_dataframe = data.frame(index(loadings), loadings)
colnames(loadings_dataframe) = c('tenor', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('tenor', 'pc1', 'pc2', 'pc3')
loadings_dataframe = loadings_dataframe[keeps]
loadings_melted = melt(loadings_dataframe, id.vars='tenor')
plot = ggplot() + geom_line(data=loadings_melted, aes(x=tenor, y=value, color=variable)) + scale_x_discrete(labels=c('2y1y', '3y1y', '4y1y', '5y1y', '7y1y', '10y2y', '10y5y', '30y10y')) + xlab('Tenor') + ylab('Loading of First 3 PCs')
pdf("pca-loadings-curve.pdf", width=10, height=5)
plot
dev.off()

# first factor analysis
source('graphing_utility.R')
y10 = diff(last(DSWP10, 251), lag=1)
y10 = y10[!is.na(y10)]
first_factor_10y = data.frame(rownames(scores), scores_dataframe$pc1, y10)
colnames(first_factor_10y) = c('date', 'pc1', 'y10')
first_factor_10y$pc1 = -first_factor_10y$pc1

p1 = ggplot() + geom_line(data=first_factor_10y, aes(x=as.Date(date), y=pc1), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC1\nright/blue: 10y Yield')
p2 = ggplot() + geom_line(data=first_factor_10y, aes(x=as.Date(date), y=y10), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)
first_factor_cor = cor(first_factor_10y$pc1, first_factor_10y$y10)
first_factor_cor

# second factor analysis
second_factor_10y2y = data.frame(rownames(scores), scores_dataframe$pc2, (last(pccurve10_2, 250)))
colnames(second_factor_10y2y) = c('date', 'pc2', 'curve10y2y')
# second_factor_10y2y$pc2 = -second_factor_10y2y$pc2

p1 = ggplot() + geom_line(data=second_factor_10y2y, aes(x=as.Date(date), y=pc2), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC2\nright/blue: 10s2s Yield')
p2 = ggplot() + geom_line(data=second_factor_10y2y, aes(x=as.Date(date), y=curve10y2y), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)
second_factor_cor = cor(second_factor_10y2y$pc2, second_factor_10y2y$curve10y2y)
second_factor_cor

# third factor analysis
butterfly2_10_30 = (DSWP10 - DSWP5) - (DSWP30 - DSWP10)
butterfly2_10_30 = diff(butterfly2_10_30, lag=1)
butterfly2_10_30 = butterfly2_10_30[!is.na(butterfly2_10_30)]
butterfly2_10_30 = last(butterfly2_10_30, 250)
third_factor_butterfly = data.frame(rownames(scores), scores_dataframe$pc3, butterfly2_10_30)
colnames(third_factor_butterfly) = c('date', 'pc3', 'butterfly2y10y30y')
third_factor_butterfly$pc3 = -third_factor_butterfly$pc3

p1 = ggplot() + geom_line(data=third_factor_butterfly, aes(x=as.Date(date), y=pc3), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC3\nright/blue: 2s10s30s Yield')
p2 = ggplot() + geom_line(data=third_factor_butterfly, aes(x=as.Date(date), y=butterfly2y10y30y), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)
third_factor_cor = cor(third_factor_butterfly$pc3, third_factor_butterfly$butterfly2y10y30y)
third_factor_cor
