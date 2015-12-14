library(quantmod)
library(downloader)

terms = c(1, 2, 3, 4, 5, 7, 10, 30)
for (term in terms) {
  getSymbols(paste('DSWP', term, sep=''), src='FRED')
}

DSWP1 = last(DSWP1[!is.na(DSWP1)], 250)
DSWP2 = last(DSWP2[!is.na(DSWP2)], 250)
DSWP3 = last(DSWP3[!is.na(DSWP3)], 250)
DSWP4 = last(DSWP4[!is.na(DSWP4)], 250)
DSWP5 = last(DSWP5[!is.na(DSWP5)], 250)
DSWP7 = last(DSWP7[!is.na(DSWP7)], 250)
DSWP10 = last(DSWP10[!is.na(DSWP10)], 250)
DSWP30 = last(DSWP30[!is.na(DSWP30)], 250)

pcDSWP1 = dailyReturn(DSWP1)
pcDSWP2 = dailyReturn(DSWP2)
pcDSWP3 = dailyReturn(DSWP3)
pcDSWP4 = dailyReturn(DSWP4)
pcDSWP5 = dailyReturn(DSWP5)
pcDSWP7 = dailyReturn(DSWP7)
pcDSWP10 = dailyReturn(DSWP10)
pcDSWP30 = dailyReturn(DSWP30)

# pcDSWP1 = diff(DSWP1, lag=1)
# pcDSWP2 = diff(DSWP2, lag=1)
# pcDSWP3 = diff(DSWP3, lag=1)
# pcDSWP4 = diff(DSWP4, lag=1)
# pcDSWP5 = diff(DSWP5, lag=1)
# pcDSWP7 = diff(DSWP7, lag=1)
# pcDSWP10 = diff(DSWP10, lag=1)
# pcDSWP30 = diff(DSWP30, lag=1)
# 
# pcDSWP1 = pcDSWP1[!is.na(pcDSWP1)]
# pcDSWP2 = pcDSWP2[!is.na(pcDSWP2)]
# pcDSWP3 = pcDSWP3[!is.na(pcDSWP3)]
# pcDSWP4 = pcDSWP4[!is.na(pcDSWP4)]
# pcDSWP5 = pcDSWP5[!is.na(pcDSWP5)]
# pcDSWP7 = pcDSWP7[!is.na(pcDSWP7)]
# pcDSWP10 = pcDSWP10[!is.na(pcDSWP10)]
# pcDSWP30 = pcDSWP30[!is.na(pcDSWP30)]

rates = cbind(DSWP1, DSWP2, DSWP3, DSWP4, DSWP5, DSWP7, DSWP10, DSWP30)

pc_rates = cbind(pcDSWP1, pcDSWP2, pcDSWP3, pcDSWP4, pcDSWP5, pcDSWP7, pcDSWP10, pcDSWP30)
colnames(pc_rates) = c('y1', 'y2', 'y3', 'y4', 'y5', 'y7', 'y10', 'y30')

# autocorrelation
acfDSWP1 = acf(DSWP1, plot=FALSE)
acfDSWP1_dataframe = with(acfDSWP1, data.frame(lag, acf))
q_DSWP1 = ggplot(data = acfDSWP1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("1 Year Swap Rate")
q_DSWP1

acfDSWP2 = acf(DSWP2, plot=FALSE)
acfDSWP2_dataframe = with(acfDSWP2, data.frame(lag, acf))
q_DSWP2 = ggplot(data = acfDSWP2_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("2 Year Swap Rate")
q_DSWP2

acfDSWP3 = acf(DSWP3, plot=FALSE)
acfDSWP3_dataframe = with(acfDSWP3, data.frame(lag, acf))
q_DSWP3 = ggplot(data = acfDSWP3_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("3 Year Swap Rate")
q_DSWP3

acfDSWP4 = acf(DSWP4, plot=FALSE)
acfDSWP4_dataframe = with(acfDSWP4, data.frame(lag, acf))
q_DSWP4 = ggplot(data = acfDSWP4_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("4 Year Swap Rate")
q_DSWP4

acfDSWP5 = acf(DSWP5, plot=FALSE)
acfDSWP5_dataframe = with(acfDSWP5, data.frame(lag, acf))
q_DSWP5 = ggplot(data = acfDSWP5_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("5 Year Swap Rate")
q_DSWP5

acfDSWP7 = acf(DSWP7, plot=FALSE)
acfDSWP7_dataframe = with(acfDSWP7, data.frame(lag, acf))
q_DSWP7 = ggplot(data = acfDSWP7_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("7 Year Swap Rate")
q_DSWP7

acfDSWP10 = acf(DSWP10, plot=FALSE)
acfDSWP10_dataframe = with(acfDSWP10, data.frame(lag, acf))
q_DSWP10 = ggplot(data = acfDSWP10_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("10 Year Swap Rate")
q_DSWP10

acfDSWP30 = acf(DSWP30, plot=FALSE)
acfDSWP30_dataframe = with(acfDSWP30, data.frame(lag, acf))
q_DSWP30 = ggplot(data = acfDSWP30_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("30 Year Swap Rate")
q_DSWP30

source("multiplot.R")
pdf("acf-yields.pdf",width=10,height=5)
multiplot(q_DSWP1, q_DSWP2, q_DSWP3, q_DSWP4, q_DSWP5, q_DSWP7, q_DSWP10, q_DSWP30, cols=4)
dev.off()

acfpcDSWP1 = acf(pcDSWP1, plot=FALSE)
acfpcDSWP1_dataframe = with(acfpcDSWP1, data.frame(lag, acf))
q_pcDSWP1 = ggplot(data = acfpcDSWP1_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("1 Year (% Change)")
q_pcDSWP1

acfpcDSWP2 = acf(pcDSWP2, plot=FALSE)
acfpcDSWP2_dataframe = with(acfpcDSWP2, data.frame(lag, acf))
q_pcDSWP2 = ggplot(data = acfpcDSWP2_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("2 Year (% Change)")
q_pcDSWP2

acfpcDSWP3 = acf(pcDSWP3, plot=FALSE)
acfpcDSWP3_dataframe = with(acfpcDSWP3, data.frame(lag, acf))
q_pcDSWP3 = ggplot(data = acfpcDSWP3_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("3 Year (% Change)")
q_pcDSWP3

acfpcDSWP4 = acf(pcDSWP4, plot=FALSE)
acfpcDSWP4_dataframe = with(acfpcDSWP4, data.frame(lag, acf))
q_pcDSWP4 = ggplot(data = acfpcDSWP4_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("4 Year (% Change)")
q_pcDSWP4

acfpcDSWP5 = acf(pcDSWP5, plot=FALSE)
acfpcDSWP5_dataframe = with(acfpcDSWP5, data.frame(lag, acf))
q_pcDSWP5 = ggplot(data = acfpcDSWP5_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("5 Year (% Change)")
q_pcDSWP5

acfpcDSWP7 = acf(pcDSWP7, plot=FALSE)
acfpcDSWP7_dataframe = with(acfpcDSWP7, data.frame(lag, acf))
q_pcDSWP7 = ggplot(data = acfpcDSWP7_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("7 Year (% Change)")
q_pcDSWP7

acfpcDSWP10 = acf(pcDSWP10, plot=FALSE)
acfpcDSWP10_dataframe = with(acfpcDSWP10, data.frame(lag, acf))
q_pcDSWP10 = ggplot(data = acfpcDSWP10_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("10 Year (% Change)")
q_pcDSWP10

acfpcDSWP30 = acf(pcDSWP30, plot=FALSE)
acfpcDSWP30_dataframe = with(acfpcDSWP30, data.frame(lag, acf))
q_pcDSWP30 = ggplot(data = acfpcDSWP30_dataframe, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("Lag") + ylab("Correlation") + ggtitle("30 Year (% Change)")
q_pcDSWP30

pdf("acf-yields-returns.pdf",width=10,height=5)
multiplot(q_pcDSWP1, q_pcDSWP2, q_pcDSWP3, q_pcDSWP4, q_pcDSWP5, q_pcDSWP7, q_pcDSWP10, q_pcDSWP30, cols=4)
dev.off()

library(ggplot2)
library(reshape)
dataframe = data.frame(index(rates), rates)
colnames(dataframe) = c('date', 'y1', 'y2', 'y3', 'y4', 'y5', 'y7', 'y10', 'y30')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=date, y=value, color=variable)) + geom_line() + xlab("Date") + ylab("IRS Rates")
pdf("descriptive-rates.pdf",width=10,height=5)
plot
dev.off()

dataframe = data.frame(index(pc_rates), pc_rates)
colnames(dataframe) = c('date', 'y1', 'y2', 'y3', 'y4', 'y5', 'y7', 'y10', 'y30')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=date, y=value, color=variable)) + geom_line() + xlab("Date") + ylab("IRS Rates")
pdf("descriptive-rates-returns.pdf",width=10,height=5)
plot
dev.off()

pcadata = pc_rates
fit = princomp(pcadata, cor=FALSE, scores=TRUE)

covariance_matrix = cor(pcadata)
require(corrplot)
pdf("corrplot-vanilla-returns.pdf", width=3.3, height=3)
corrplot(covariance_matrix, method='shade', type='full', shade.col=NA, tl.col='black')
dev.off()

library(ggbiplot)
pdf("biplot-vanilla-returns.pdf", width=3.3, height=3)
ggbiplot(fit, obs.scale=1, var.scale=1)
dev.off()
pdf("screeplot-vanilla-returns.pdf", width=3.3, height=3)
ggscreeplot(fit)
dev.off()

scores = fit$scores
scores_dataframe = data.frame(index(pc_rates), scores)
colnames(scores_dataframe) = c('date', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('date', 'pc1', 'pc2', 'pc3')
scores_dataframe = scores_dataframe[keeps]
scores_melted = melt(scores_dataframe, id.vars='date')
plot = ggplot(data=scores_melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Date') + ylab('Principal Component Score')
pdf("pca-scores-vanilla.pdf", width=10, height=5)
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
pdf("pca-loadings-vanilla.pdf", width=10, height=5)
plot
dev.off()
