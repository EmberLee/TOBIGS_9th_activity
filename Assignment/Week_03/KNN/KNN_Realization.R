### KNN 가중치를 주는 함수 : 유사도 = 1/거리 사용해서 
### & 최적의 k 찾기 (5-fold cv)로 구현하기
### 함수 매개변수랑 리턴값은 맘대로 해도됩니다!

rm(list = ls())

setwd("C:/Users/ingulbull/Desktop/2018-1/투빅스/180131 세션(3)/knn자료")
wdbc <- read.csv('wisc_bc_data.csv', stringsAsFactors = F)
wdbc <- wdbc[-1]
wdbc$diagnosis <- factor(wdbc$diagnosis, level=c("B","M"))

set.seed(1)
idx <- sample(1:nrow(wdbc), 0.8*nrow(wdbc))
wdbc_train <- wdbc[idx, ]
wdbc_test <- wdbc[-idx, ]
# 이 윗부분 까지만 동일하게 해주세요!

Weighted_knn <- function(train, test, k){
  rownames(train) <- 1:nrow(train)
  rownames(test) <- 1:nrow(test)
  test_x <- test[, -1]
  train_x <- train[, -1]
  train_y <- train[, 1]
  label <- c()
  nrows <- nrow(train_x)
  distance <- as.matrix(dist(rbind(train_x, test_x)))
  for (i in 1:nrow(test_x)){
    selected_dist <- sort(distance[(nrows+i), 1:nrows])[1:k]
    index <- names(selected_dist)
    sim <- 1/selected_dist
    weight <- sim/sum(sim)
    names(weight) <- train_y[as.numeric(index)]
    B.weight <- sum(weight[names(weight) == "B"])
    M.weight <- sum(weight[names(weight) == "M"])
    label[i] <- ifelse(B.weight > M.weight, "B", "M")
  }
  return(label)
}
Weighted_knn(wdbc_train, wdbc_test, 3)
# [1] "B" "B" "B" "B" "M" "B" "B" "B" "B" "B" "B" "M" "B" "M" "B" "B" "B" "B" "B" "B" "M" "B" "B" "B" "M" "B" "B" "B"
# [29] "B" "B" "M" "B" "B" "M" "B" "M" "B" "M" "B" "M" "B" "B" "M" "M" "B" "M" "M" "B" "B" "B" "B" "M" "B" "B" "B" "M"
# [57] "B" "B" "B" "M" "B" "B" "B" "B" "B" "B" "B" "B" "M" "M" "B" "M" "B" "M" "B" "B" "B" "B" "B" "B" "B" "B" "B" "M"
# [85] "M" "B" "M" "B" "B" "B" "M" "B" "B" "B" "B" "B" "B" "B" "B" "M" "B" "B" "M" "M" "B" "M" "M" "M" "B" "M" "B" "B"
# [113] "B" "B"

library(caret)

CrossValidation <- function(train, k_vec){
  fold_5 <- createFolds(train[, 1], 5)
  accuracy_mean <- vector(mode = "numeric", length = length(k_vec))
  for (i in k_vec){
    accuracy <- vector(mode = "numeric", length = length(fold_5))
    for (j in 1:length(fold_5)){
      validation <- train[fold_5[[j]], ]
      training <- train[-fold_5[[j]], ]
      classified <- Weighted_knn(training, validation, i)
      accuracy[j] <- confusionMatrix(classified, validation[, 1])$overall[1]
    }
    accuracy_mean[k_vec == i] <- mean(accuracy)
  }
  return(k_vec[accuracy_mean == max(accuracy_mean)])
  #accuracy 평균이 동률일 가능성도 고려해 그럴 경우 복수의 best_k를 출력하도록 조정했습니다.
}

best_k <- CrossValidation(wdbc_train, c(3, 5, 7)) #best_k = 5
pred <- Weighted_knn(wdbc_train, wdbc_test, 5)
confusionMatrix(pred, wdbc_test$diagnosis) #Sensitivity = 0.9722, #Spectificity = 0.7857

##############################################
