#讀檔案，資料來自於 TIMSS 2011 年台灣資料
dta <- read.table(file = "data/TIMSS2011TW.txt", 
                  header = TRUE)
#看資料結構與前六筆
str(dta)
head(dta)
#看資料基本統計
summary(dta)
#載進 ggplot2 準備畫圖
require(ggplot2)
#底下的圖都用黑白配色（theme_bw）
old <- theme_set(theme_bw())
#看不同性別數學分數的盒鬚圖
ggplot(data = dta, aes(x = gender, y = math)) +
 geom_boxplot() + coord_flip() +
 labs( y = 'math', x = 'gender', 
       title = 'Mathematical Score Box')
#看信賴區間
with(dta, 
     tapply(math, gender,
     function(x) 
       c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))
#以t檢定比較不同性別的數學差異
#預設作法會做 Welch 校正，處理兩樣本變異數不相同的問題
t.test(math ~ gender, data = dta)
#這是一般假設變異數同值下的 t 檢定
t.test(math ~ gender, data = dta, var.equal = TRUE)

#看不同父母教育背景者的數學成績差異
#先把父母教育各個水準順序定下來
dta$parental.education <- factor(dta$parental.education, 
                       levels = c('elementary school',
                                  'junior high school',
                                  'high school',
                                  'college', 
                                  'university above'))
#看不同父母教育程度下的數學分數平均數
tapply(dta$math, dta$parental.education, mean)

library(Hmisc)
#同父母教育程度下的數學分數平均數，加上信賴區間
ggplot(data = dta, 
       aes(x = parental.education, y = math)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(dta$math) , 
             linetype = 'dotted') +
  labs(x = '父母教育', y = '數學平均分數') +
  coord_flip()
  
# anova檢定 
anova(m1 <- lm(math ~ parental.education, data = dta))

#父母教育的效果或許會是教育資源造成的，畫圖看看
ggplot(data = dta, 
       aes(group = parental.education, 
          y = math, x = educational.resources)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = parental.education, 
          y = math, x = educational.resources), 
          method = 'lm', se = F) + 
  facet_grid( . ~  parental.education) +
  labs(x = '教育資源', y = '數學分數')

#把教育資源加進模型
anova(m2 <- update(m1, . ~ . + 
            educational.resources, data = dta))
#或許不是父母教育而是教育資源造成
anova(m3 <- update(m2, . ~ . - 
            parental.education,  data = dta))
#將結果放在一個list中
res_lm <- lapply(list(m1, m2, m3), summary)
#比較在控制教育資源下，父母教育的效果
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)
#比較在控制父母教育下，教育資源的效果
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)

#畫效果
require(coefplot)
#將截距去除，畫更易懂起來
m2 <- lm(math ~ parental.education+educational.resources- 1, 
         data = dta)
coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = 數學分數')

#把資料與迴歸分析的預測值、殘差與影響度放進資料
fit_m2 <- data.frame(dta[, c(2, 12, 13)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )

#疊合真實觀測值預測值的直方圖，依父母教育
ggplot(data = fit_m2, aes(x = math, group = parental.education )) +
 stat_density(geom = 'path', position = 'identity') +
 stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
 geom_vline(xintercept = c(with(dta, tapply(math,parental.education, mean))), linetype = 'dotted')+
 facet_grid(parental.education ~ .) +
 scale_x_continuous(breaks = seq(200, 900, by = 100))+
 labs(x = '數學分數', y = '機率密度')

#看殘差分配，依父母教育，檢視常態與變異數同質假設
ggplot(data = fit_m2, aes(x = scale(resid)), group = parental.education ) +
 stat_density(geom = 'path', position = 'identity', aes(linetype = parental.education)) +
 scale_linetype_manual(values = 5:1) +
 guides(linetype = guide_legend(reverse = TRUE)) +
 labs(x = '標準化殘差', y = '機率密度') +
 theme(legend.position = c(.15, .8))

#看看殘差的 Q-Q 圖，依父母教育。檢視常態假設
require(lattice)
qqmath(~ scale(resid) | parental.education, data = fit_m2, type = c('p', 'g', 'r'),
       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),
       pch = '.', cex = 2)

#畫預測值與殘差的散佈圖，檢查線性與等分散假設
require(MASS)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = parental.education )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(parental.education ~ .) +
  labs(x = '數學預測值', y = '標準化殘差')

#呈現影響值（影響估計結果過大的值）與標準化殘差
ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group = parental.education)) +
 geom_text(aes(label = rownames(fit_m2)), cex = 2) +
 geom_hline(yintercept = 0, linetype = 'dotted') +
 facet_grid(parental.education ~ .) +
 labs(x = '影響值', y = '標準化殘差')

#看看影響值
summary(influence(m2)$hat)

#改回舊的配色，不然 R 就黑白下去了
theme_set(old)



#底下要呈現多個連續解釋變項時的情形
#看看個人變項的可能效果，把跟數學有關的部分取出來
dta_math <- dta[, c('math', 'math.interest', 
                    'math.evaluation', 'math.input')]

#看看基本統計量
colMeans(dta_math)

#呈現兩兩散佈圖
require(heplots)
scatterplotMatrix(~ math + math.interest + math.evaluation + math.input, data= dta_math,
  pch = '.', cex = 3, smooth = FALSE, reg.line = FALSE, ellipse = TRUE,
  diagonal = 'none', lower.panel = NULL)

#載入corrplot 套件，以圖形顯示相關大小
require(corrplot)
corrplot(cor(dta_math), method = 'ellipse', order = 'hclust', addrect = 4,
         type = 'upper', tl.pos = 'tp')
corrplot(cor(dta_math), add = TRUE, type = 'lower', method = 'number',
         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

#放進三個解釋變項
summary(m4 <- lm(math ~ math.interest + math.evaluation + math.input, data = dta_math))

#看效果
coefplot(m4, predictors = c('math.interest', 'math.evaluation', 
                            'math.input'),
 xlab = '估計值', ylab = '迴歸變項(去除截距)', title = '反應變項是數學分數')

#看效果
require(effects)
plot(allEffects(m4), main = '', ylim = c(550, 670), grid = T)

#載入 lm.beta套件，計算標準化迴歸係數
library(lm.beta)
summary(lm.beta(m4))

#看看控制數學興趣與數學評價後，數學投入的效果
summary(m5 <- update(m4, . ~ . - math.input , data = dta_math))
anova(m5, m4)
