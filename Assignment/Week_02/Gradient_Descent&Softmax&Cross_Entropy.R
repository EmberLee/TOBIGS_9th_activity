##################### 1.Gradient Descent 구현 과제입니다. ##########################
# 시각화용 코드입니다.
smoothing <- function(vec)
{
  vec1 <- c(vec[-1], vec[length(vec)])
  vec2 <- c(vec[1], vec[-length(vec)])
  return((vec1 + vec2 + vec) / 3)
}

visualize_loss <- function(loss_log)
{
  for(i in 1:100)
  {
    loss_log <- smoothing(loss_log)
    plot(loss_log)
    Sys.sleep(0.01)
  }
}

# 단순회귀 구현
x <- rnorm(1000, 0)
y <- 2 * x + 1
w <- 0.001
b <- 0.001
lr <- 0.01
loss_log <- c()
for(i in 1:length(x)){
  w <- w - lr/1000*sum((w*x + b - y)*x)
  b <- b - lr/1000*sum(w*x + b - y)
  loss <- sum((w*x + b - y)^2)/2000
  loss_log[i] <- loss
}
visualize_loss(loss_log)
if(max(abs(w-2), abs(b-1)) < 0.1){
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}

#다중회귀 구현(변수 11개)
x <- as.data.frame(matrix(rnorm(5000,0), nrow = 500, ncol = 10))
y <- x$V1 * 1 + x$V2 * 2 + x$V3 * 3 + x$V4 * 4 + x$V5 * 5 + x$V6 * 6 + x$V7 * 7 + x$V8 * 8 + x$V9 * 9 + x$V10 * 10 + 11
w <- rnorm(10,0)
b <- rnorm(1,0)
lr <- 0.01
loss_log <- c()
mat <- as.matrix(x)

for(i in 1:nrow(x)){
  w <- w - lr/500*t(mat)%*%(mat%*%w + b - y)
  b <- b - lr/500*sum(mat%*%w + b - y)
  loss <- sum((mat%*%w + b - y)^2)/1000
  loss_log[i] <- loss
}
visualize_loss(loss_log)
if(max(abs(w-1:10), abs(b-11)) < 0.5){
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}

#다중회귀 구현(변수 n개)
linear_regression <- function(n, lr = 0.01){
  x <- as.data.frame(matrix(rnorm(50*n*n,0), nrow = 50*n, ncol = n))
  y <- rep(0, 50*n)
  for(i in 1:(50*n)){
    y[i] <- sum(x[i,]*(1:n)) + (n+1)
  }
  w <- rnorm(n,0)
  b <- rnorm(1,0)
  loss_log <- c()
  mat <- as.matrix(x)
  for(i in 1:nrow(x)){
    w <- w - lr/(50*n)*t(mat)%*%(mat%*%w + b - y)
    b <- b - lr/(50*n)*sum(mat%*%w + b - y)
    loss <- sum((mat%*%w + b - y)^2)/(100*n)
    loss_log[i] <- loss
  }
  visualize_loss(loss_log)
  if(max(abs(w-1:n), abs(b-n-1)) < 0.5)
  {
    print("정답입니다!")
  }else{
    print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
  }
  return(list(w = w, b = b))
}

linear_regression(10)
linear_regression(15)
linear_regression(20)

############# 2.Multinomial logistic에서 배운 softmax 와 cross_entropy 를 함수로 구현하세요 ###############
### 결과는 list(table, beta 계수) 반환하도록 해주세요 (GD 사용하실 경우, learning_rate 유의하세요)
## iris data에 한정적인 함수도 괜찮고, 일반화함수도 좋습니다.
# cross_entropy 함수로 beta를 구하고, softmax 함수에서 cross_entropy 함수를 받아들이면 됩니다.
rm(list=ls())
data("iris")
str(iris)
x<-iris[,-5]
y<-iris[,5]

set.seed(1234)
index <- sort(sample(1:nrow(x), nrow(x)*0.8, replace = F))
train_x <- x[index,]
train_y <- y[index]  
test_x <- x[-index,]
test_y <- y[-index]

#x 데이터 매트릭스, y 이진 매트릭스 만들기
x_mat_train <- as.matrix(cbind(1, train_x))
id_train <- 1:length(index)
y_mat_train <- table(data.frame(id = id_train, y = train_y))

x_mat_test <- as.matrix(cbind(1, test_x))
id_test <- 1:(150-length(index))
y_mat_test <- table(data.frame(id = id_test, y = test_y))

#초기값 설정
beta <- matrix(rnorm(15), 5, 3)
lr <- 0.001
loss_log <- c()

#함수 만들기
softmax <- function(mat, beta){
  sum_exp <- apply(exp(mat%*%beta), 1, sum)
  y_prob <- exp(mat%*%beta)/sum_exp
  return(y_prob)
}

cross_entropy <- function(S, Label){
  entropy <- -apply(Label*log(S), 1, sum)
  return(entropy)
}

#학습시키고 beta계수(가중치) 추정
#원래 cost function으로 cross entropy를 사용해야하지만 미분하면 분수의 꼴로 출력됨
#따라서 분모가 너무 작을 경우엔 가중치가 NaN이 출력되므로 기존에 사용했던 sum of square 공식 사용
for (i in 1:10^3){
  predicted_y <- softmax(x_mat_train, beta)
  cost_diff <- y_mat_train - predicted_y
  beta <- beta + lr*t(x_mat_train)%*%cost_diff
  loss <- sum(cross_entropy(predicted_y, y_mat_train))
  loss_log[i] <- loss
}
visualize_loss(loss_log)

#테스트
#실제 데이터와 y_hat의 0, 1이 일치하지 않으면 table의 비대각성분 (1,2), (2,1)에 각각 1이 등록됨
#따라서 분류에 실패한 obs는 2개
result <- round(softmax(x_mat_test, beta))
table(result, y_mat_test)
