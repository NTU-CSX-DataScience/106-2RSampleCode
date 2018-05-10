rm(list=ls())
load("titanic.raw.rdata")
require(rpart)

set.seed(100)
train.index <- sample(x=1:nrow(titanic.raw), size=ceiling(0.8*nrow(titanic.raw) ))
train <- titanic.raw[train.index, ]
test <- titanic.raw[-train.index, ]

cart.model<- rpart(Survived ~. , 
                   data=train)

require(rpart.plot)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  

pred <- predict(cart.model, newdata=test, type="class")
table(real=test$Survived, predict=pred)
confus.matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)