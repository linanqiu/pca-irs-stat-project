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

rates = cbind(DSWP1, DSWP2, DSWP3, DSWP4, DSWP5, DSWP7, DSWP10, DSWP30)
rates = last(rates, 250)

library(ggplot2)
library(reshape)
dataframe = data.frame(index(rates), rates)
colnames(dataframe) = c('date', 'y1', 'y2', 'y3', 'y4', 'y5', 'y7', 'y10', 'y30')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=date, y=value, color=variable)) + geom_line()

pcadata = cbind(DSWP1, DSWP2, DSWP3, DSWP4, DSWP5, DSWP7, DSWP10, DSWP30)
fit = princomp(pcadata, cor=FALSE, scores=TRUE)

covariance_matrix = cor(pcadata)
require(corrplot)
corrplot(covariance_matrix, method='shade', type='full', shade.col=NA, tl.col='black')

library(ggbiplot)
ggbiplot(fit, obs.scale=1, var.scale=1)
ggscreeplot(fit)

scores = fit$scores
scores_dataframe = data.frame(index(scores), scores)
colnames(scores_dataframe) = c('date', 'pc1', 'pc2', 'pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('date', 'pc1', 'pc2', 'pc3')
scores_dataframe = scores_dataframe[keeps]
scores_melted = melt(scores_dataframe, id.vars='date')
plot = ggplot(data=scores_melted, aes(x=date, y=value, color=variable)) + geom_line()
