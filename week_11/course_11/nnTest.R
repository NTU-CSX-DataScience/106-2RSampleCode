library(devtools)
library(DMwR)
library(nnet)
library(reshape)
library(scales)
library(ggplot2)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

#範例使用irisdata 
data(iris)

#(2)分為訓練組和測試組資料集
set.seed(1117)
#取得總筆數
n <- nrow(iris)
#設定訓練樣本數70%
t_size = round(0.7 * n)
#取出樣本數的idx
t_idx <- sample(seq_len(n), size = t_size)
#訓練組樣本
traindata <- iris[t_idx,]
#測試組樣本
testdata <- iris[ - t_idx,]

nnetM <- nnet(formula = Species ~ ., linout = T, size = 3, decay = 0.001, maxit = 1000, trace = T, data = traindata)

#(3)畫圖 
plot.nnet(nnetM, wts.only = F)


#(4)預測
#test組執行預測 
prediction <- predict(nnetM, testdata, type = 'class')

#預測結果 
cm <- table(x = testdata$Species, y = prediction, dnn = c("實際", "預測"))
cm