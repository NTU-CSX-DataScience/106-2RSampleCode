library(NLP)
library(tm)
library(tmcn)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.7.0_79/")
library(rJava)
library(SnowballC)
library(slam)
library(Matrix)

# import data
source("readFromTXT.R")

# corpus to tdm
d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus, 
       control = list(wordLengths = c(2, Inf)))
View(inspect(tdm[1:9, 1:10]))

ass = findAssocs(tdm, "老師", 0.75)


# tf-idf computation
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)


doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

# 可以用 findFreqTerms 
# 看看在所有文件裡出現 200 次以上的關鍵字有哪些。
result = findFreqTerms(tdm, 200)

# 畫出 tf-idf 統計圖
library(plotly)
topID = lapply(rownames(as.data.frame(ass)), function(x) 
  which(rownames(tdm) == x))
topID = unlist(topID)
plot_ly(data = as.data.frame(doc.tfidf),
        x = as.numeric(colnames(doc.tfidf)),
        y = doc.tfidf[topID[10],], 
        name = rownames(doc.tfidf)[topID[10]],
        type = "scatter", mode= "box") %>%
add_trace(y = doc.tfidf[topID[2],],
          name = rownames(doc.tfidf)[topID[2]])

# get short doc matrix
nonzero = (doc.tfidf != rep(0,10))
nonzeroid = which(row_sums(nonzero) != 0)
q <- rownames(doc.tfidf[nonzeroid,])
all.term <- rownames(doc.tfidf)
loc <- which(all.term %in% q)
s.tdm <- doc.tfidf[loc,]
View(s.tdm)

# result : cos similarity ranking
cos.sim <- function(x, y)
{ 
  (as.vector(x) %*% as.vector(y)) / (norm(as.matrix(x)) * norm(y)) 
}
doc.cos <- cos.sim(x=as.matrix(s.tdm[,1]), 
                   y=as.matrix(s.tdm[,2]))
doc.cos <- apply(s.tdm[,2:10], 2, cos.sim,
                 y=as.matrix(s.tdm[,2]))
orderDoc <- doc.cos[order(doc.cos, decreasing = TRUE)]
plot_ly(data = as.data.frame(orderDoc),
        x = rownames(as.data.frame(orderDoc)),
        y = orderDoc, 
        name = rownames(doc.tfidf)[topID[10]],
        type = "bar", mode= "box")

# Kmeans 分群
library(stats)
kmeansOut <- kmeans(doc.tfidf, 5, nstart = 20)
