#載入套件
library(neuralnet)

#整理資料
data <- iris
data$setosa <- ifelse(data$Species == "setosa", 1, 0)
data$versicolor <- ifelse(data$Species == "versicolor", 1, 0)
data$virginica <- ifelse(data$Species == "virginica", 1, 0)

#訓練模型
f1 <- as.formula('setosa + versicolor + virginica  ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width')
bpn <- neuralnet(formula = f1, data = data, hidden = c(2,4),learningrate = 0.01)
print(bpn)

#圖解BP
plot(bpn)