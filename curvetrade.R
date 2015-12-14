library(quantmod)
library(downloader)

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
curve2_1 = DSWP2 - DSWP1
curve3_1 = DSWP3 - DSWP1
curve4_1 = DSWP4 - DSWP1
curve5_1 = DSWP5 - DSWP1
curve7_1 = DSWP7 - DSWP1
curve10_2 = DSWP10 - DSWP2
curve10_5 = DSWP10 - DSWP5
curve30_10 = DSWP30 - DSWP10

rates = cbind(curve2_1, curve3_1, curve4_1, curve5_1, curve7_1, curve10_2, curve10_5, curve30_10)
rates = last(rates, 250)
colnames(rates) = c('curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')


library(ggplot2)
library(reshape)
dataframe = data.frame(index(rates), rates)
colnames(dataframe) = c('date', 'curve2y1y', 'curve3y1y', 'curve4y1y', 'curve5y1y', 'curve7y1y', 'curve10y2y', 'curve10y5y', 'curve30y10y')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Curve Rates') + ylab('Date')
plot

pcadata = rates
fit = princomp(pcadata, cor=FALSE, scores=TRUE)

covariance_matrix = cor(pcadata)
require(corrplot)
corrplot(covariance_matrix, method='shade', type='full', shade.col=NA, tl.col='black')

library(ggbiplot)
ggbiplot(fit, obs.scale=1, var.scale=1)
ggscreeplot(fit)

scores = fit$scores
scores_dataframe = data.frame(index(rates), scores)
colnames(scores_dataframe) = c('date', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('date', 'pc1', 'pc2', 'pc3')
scores_dataframe = scores_dataframe[keeps]
scores_melted = melt(scores_dataframe, id.vars='date')
plot = ggplot(data=scores_melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Date') + ylab('Principal Component Score')
plot

loadings = with(fit, unclass(loadings))
loadings

loadings_dataframe = data.frame(index(loadings), loadings)
colnames(loadings_dataframe) = c('tenor', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('tenor', 'pc1', 'pc2', 'pc3')
loadings_dataframe = loadings_dataframe[keeps]
loadings_melted = melt(loadings_dataframe, id.vars='tenor')
plot = ggplot() + geom_line(data=loadings_melted, aes(x=tenor, y=value, color=variable)) + scale_x_discrete(labels=c('2y1y', '3y1y', '4y1y', '5y1y', '7y1y', '10y2y', '10y5y', '30y10y')) + xlab('Tenor') + ylab('Loading of First 3 PCs')
plot

# first factor analysis
source('graphing_utility.R')
first_factor_10y = data.frame(rownames(scores), scores_dataframe$pc1, last(DSWP10, 250))
colnames(first_factor_10y) = c('date', 'pc1', 'y10')
first_factor_10y$pc1 = -first_factor_10y$pc1

p1 = ggplot() + geom_line(data=first_factor_10y, aes(x=as.Date(date), y=pc1), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC1\nright/blue: 10y Yield')
p2 = ggplot() + geom_line(data=first_factor_10y, aes(x=as.Date(date), y=y10), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)

# second factor analysis
second_factor_10y2y = data.frame(rownames(scores), scores_dataframe$pc2, last(curve10_2, 250))
colnames(second_factor_10y2y) = c('date', 'pc2', 'curve10y2y')

p1 = ggplot() + geom_line(data=second_factor_10y2y, aes(x=as.Date(date), y=pc2), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC2\nright/blue: 10s2s Yield')
p2 = ggplot() + geom_line(data=second_factor_10y2y, aes(x=as.Date(date), y=curve10y2y), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)

# third factor analysis
butterfly2_10_30 = (DSWP10 - DSWP5) - (DSWP30 - DSWP10)
third_factor_butterfly = data.frame(rownames(scores), scores_dataframe$pc3, last(butterfly2_10_30, 250))
colnames(third_factor_butterfly) = c('date', 'pc3', 'butterfly2y10y30y')
third_factor_butterfly$pc3 = -third_factor_butterfly$pc3

p1 = ggplot() + geom_line(data=third_factor_butterfly, aes(x=as.Date(date), y=pc3), colour=gg_color_hue(1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('Date') + ylab('left/red: PC3\nright/blue: 2s10s30s Yield')
p2 = ggplot() + geom_line(data=third_factor_butterfly, aes(x=as.Date(date), y=butterfly2y10y30y), colour=gg_color_hue(2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = NA))
g = stack_plot(p1, p2)
grid.draw(g)

