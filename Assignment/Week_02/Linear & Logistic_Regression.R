############### 데이터 분석 과제입니다. ################
# install.packages("UsingR")
# 데이터
library(UsingR)
data("babies")

##### 1.회귀분석하기 #####
### wt를 예측해주세요
##추가적으로, ridge/lasso 써도 됩니다.
str(babies)

#회귀에 관련없는 1,2,3,번째 변수 제외하고
#99, 999 등 알 수 없음을 의미하는 행 제거
babies <- babies[, -c(1, 2, 3)]
babies <- subset(babies, wt!=999&parity!=99&race!=99&age!=99&ed!=9&ht!=99&wt1!=999&drace!=99&dage!=99&ded!=9&dwt!=999&dht!=99&inc!=98)
babies <- subset(babies, inc!=99&smoke!=9&time!=9&time!=99&time!=999&number!=98&number!=99)

babies$race <- as.factor(babies$race)
babies$ed <- as.factor(babies$ed)
babies$drace <- as.factor(babies$drace)
babies$ded <- as.factor(babies$ded)
babies$marital <- as.factor(babies$marital)
babies$inc <- as.factor(babies$inc)
babies$time <- as.factor(babies$time)
babies$number <- as.factor(babies$number)

fit.none <- lm(wt ~ 1, babies)
fit.full <- lm(wt ~ ., babies)
fit.foward <- step(fit.none, list(lower=fit.none, upper=fit.full), direction = "forward")
#drace, time, ht, wt1, parity, number, dwt, date, gestation
fit.backward <- step(fit.full, list(lower=fit.none, upper = fit.full), direction = "backward")
#date, gestation, parity, ht, wt1, drace, dwt, time, number
fit.both <- step(fit.none, list(lower=fit.none, upper=fit.full), direction = "both")
#drace, time, ht, wt1, parity, number, dwt, date, gestation

summary(fit.backward) # R-squared:  0.2157,	Adjusted R-squared:  0.1745 
summary(fit.both) # same as above

par(mfrow=c(2,2))
plot(fit.both)

eps <-resid(fit.both)
shapiro.test(eps) #p-value < 1e-3, 정규성 불만족

plot(predict(fit.both), eps) # 딱히 패턴이 보이진 않는다. 독립성 만족
plot(predict(fit.both), eps, type = 'o')
library(car)
outlierTest(fit.both) #p-value < .05이므로 이상치 존재


##### 2.로지스틱 회귀분석하기 #####
# babies$new_wt <- ifelse(babies$wt > median(babies$wt),1,0)
# babies <- babies[,-which(names(babies)=='wt')] 
### 위 두 줄을 실행시키고 new_wt를 예측(분류)해주세요.
### 추가적으로, ridge/lasso 써도 됩니다.
babies$new_wt <- ifelse(babies$wt > median(babies$wt),1,0)
babies <- babies[,-which(names(babies)=='wt')] 

library(caret)
library(glmnet)
set.seed(123)
#train, test 셋 분류
index <- sample(1:nrow(babies), nrow(babies)*0.8, replace = F)
train <- babies[index, ]
test <- babies[-index, ]

#모든 변수 넣고 일단 돌려보기
model1 <- glm(new_wt~., data=babies, family = binomial(link = "logit"))
summary(model1)
coef(model1)
pred_model1 <- predict(model1, newdata = test, type = "response") #type=response면 확률을 출력
y_hat <- round(pred_model1)
tt <- table(test$new_wt, y_hat)
sum(diag(tt))/sum(tt) #0.68

#foward/backward/stepwise 적용해보기
##선택되는 변수는 세 과정 모두 동
model_none <- glm(new_wt~1, data = babies, family = "binomial")
model_full <- glm(new_wt~., data = babies, family = "binomial")
fit_forward <- step(model_none, list(lower=model_none, upper=model_full), direction = "forward")
#ht, time, drace, wt1, gestation, date, ded, dwt
pred_forward <- predict(fit_forward, newdata = test, type = "response")
tt2 <- table(test$new_wt, round(pred_forward))
sum(diag(tt2))/sum(tt2) #0.632

fit_backward <- step(model_full, list(lower=model_none, upper=model_full), direction = "backward")
summary(fit_backward)
#same as above
pred_backward <- predict(fit_backward, newdata = test, type = "response")
tt3 <- table(test$new_wt, round(pred_backward))
sum(diag(tt3))/sum(tt3)

fit_step <- step(model_none, list(lower=model_none, upper=model_full), direction = "both")
pred_step <- predict(fit_step, newdata = test, type = "response")
tt4 <- table(test$new_wt, round(pred_step))
sum(diag(tt4))/sum(tt4)

#ROC 커브 그리기
library(pROC)
curve <- roc(test$new_wt, pred_step, direction="<")
curve$auc
plot(curve)

  
