rm(list=ls())

# 유방암 진단 결과 데이터 셋
wdbc <- read.csv('C:/Users/ingulbull/Desktop/2018-1/투빅스/180131 세션(3)/knn자료/wisc_bc_data.csv', stringsAsFactors = F)

str(wdbc) # 569 x 32
# radius : 반지름 / texture : 텍스처 / perimeter : 둘레 / area : 면적 / smmothness : 평활도 compactness : 다짐도 / concavity : 요면 / concave points : 요면점 / symmetry : 대칭 / fractal dimension : 프렉탈 차원

# 불필요한 id 변수 제거
wdbc <- wdbc[-1]

# 종속변수(y) 분포 확인
table(wdbc$diagnosis)
prop.table(table(wdbc$diagnosis))
# B: 양성 / M : 악성
# 악성인 것을 잘 맞춰야한다


# 종속변수(y)인 diagnosis 변수 factor형으로 변환
wdbc$diagnosis <- factor(wdbc$diagnosis, level=c("B","M"))

set.seed(1)
idx <- sample(1:nrow(wdbc), 0.7*nrow(wdbc))
wdbc_train <- wdbc[idx,]
wdbc_test <- wdbc[-idx,]


library(caret)
#install.packages("caret") 
library(class)

### 기본 knn (majority voting) ###
# 임의의 홀수 k 를 넣어보자
wdbc_pred <- knn(wdbc_train[-1], wdbc_test[-1], wdbc_train$diagnosis, k=5)
confusionMatrix(wdbc_pred, wdbc_test$diagnosis) # Specificity : 0.8167 
# 실제로 악성인 것 중에 악성이라고 예측 (Specificity)


#install.packages("kknn") 
library(kknn)

### weighted knn (가중치) ###
# distance, kernel(가중치)
wdbc_pred2 <- kknn(diagnosis ~., wdbc_train, wdbc_test, k=5, scale = F) #디폴트 : scale=T, 변수들 표준화 여부
wdbc_pred2 <- wdbc_pred2$fitted.values
confusionMatrix(wdbc_pred2, wdbc_test$diagnosis) # Specificity : 0.8333

install.packages('e1071', dependencies=TRUE)



### Feature scaling ###
summary(wdbc)
# wdbc의 데이터는 데이터의 범주가 제각각
# 변수들의 영향도를 더 정확히 보기 위해서 변수 표준화 혹은 정규화를 한다.

normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

colnames(wdbc)
wdbc_normal <- as.data.frame(lapply(wdbc[-1], normalize))
summary(wdbc_normal) #  0~1 사이의 값으로 바뀜

wdbc_normal$diagnosis <- wdbc$diagnosis

wdbc_train_n <- wdbc_normal[idx,]
wdbc_test_n <- wdbc_normal[-idx,]

wdbc_train_n$diagnosis <- wdbc_train$diagnosis
wdbc_test_n$diagnosis <- wdbc_test$diagnosis

### 기본 knn (majority voting) ###
# 임의의 홀수 k 를 넣어보자
wdbc_pred_n <- knn(wdbc_train_n[-31],wdbc_test_n[-31], wdbc_train_n$diagnosis, k=5)
confusionMatrix(wdbc_pred_n, wdbc_test$diagnosis) # Specificity : 0.9333

### weighted knn (가중치) ###
# distance, kernel(가중치)
wdbc_pred_n2 <- kknn(diagnosis ~., wdbc_train_n, wdbc_test_n, k=5, scale = F)
wdbc_pred_n2 <- wdbc_pred_n2$fitted.values
confusionMatrix(wdbc_pred_n2, wdbc_test$diagnosis) # Specificity : 0.9500



### 최적의 K를 구하는 방법 ###
# Cross-Validation
# LOOCV (leave-one-out cv) -> k-fold 에서 k=데이터 수 인 경우

# LOOCV
wdbc.cv <- train.kknn(diagnosis ~., wdbc_train_n, 
                      ks = seq(1, 50, by=2), scale = F) #ks옵션 : k 개수를 차례대로 실험해봄
best_k <- wdbc.cv$best.parameters$k
best_k

wdbc_pred_cv <- kknn(diagnosis ~., train = wdbc_train_n, test = wdbc_test_n, k = best_k, scale = F)
wdbc_pred_cv <- wdbc_pred_cv$fitted.values

confusionMatrix(wdbc_pred_cv, wdbc_test$diagnosis) # Specificity : 0.9500   

