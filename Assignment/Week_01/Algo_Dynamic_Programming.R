#########################################
# 알고리즘 과제 문제 1번-(a)
#########################################
n <- 1e5
RESULT <- vector()
for (i in 1:n){
  first_dice <- sample(1:6, 1)+sample(1:6, 1)
  if (first_dice %in% c(7, 11)){
    RESULT[i] <- 1
  } else if (first_dice %in% c(2, 3, 12)){
    RESULT[i] <- 0
  } else {
    repeat{
      next_dice <- sample(1:6, 1)+sample(1:6, 1)
      if (next_dice == first_dice){
        RESULT[i] <- 1 
        break
      } else if (next_dice == 7){
        RESULT[i] <- 0 
        break
      }
    }
  }
}
mean(RESULT)
#########################################
# 문제 1번-(b)
#########################################
games <- function(ky, sy){
  while (ky != 0&sy != 0){
    first_dice <- sample(1:6, 1)+sample(1:6, 1)
    if (first_dice %in% c(7, 11)){
      ky <- ky+1
      sy <- sy-1
    } else if (first_dice %in% c(2, 3, 12)){
      ky <- ky-1
      sy <- sy+1
    } else {
      repeat{
        next_dice <- sample(1:6, 1)+sample(1:6, 1)
        if (next_dice == first_dice){
          ky <- ky+1
          sy <- sy-1 
          break
        } else if (next_dice == 7){
          ky <- ky-1
          sy <- sy+1 
          break
        }
      }
    }
  }
  return(ifelse(ky != 0, 1, 0))
}
games(12,9)
mean(replicate(1e4, games(12,9)))
mean(replicate(1e4, games(20,9)))
#########################################
# 문제 2번
#########################################
# 메모장은 인풋 매트릭스와 같은 차원의 행렬이며, 
# 각 행.열에 메모하는 정보는 현재 행.열까지 만들어진 정사각형의 한 변의 길이이다.
# |-> 추가설명 : memo[i, j] = 오리지널 행렬의 i, j 성분을 
# 정사각형의 오른쪽 아래 꼭지점 끝으로 삼는 가장 큰 정사각형의 한 변의 길이

# 인풋 매트릭스의 원소가 0일 경우 해당 행.열의 메모장 정보는 즉시 0이 되며, (정사각형을 형성하지 못하므로)
# 메모장 행.열의 바로 왼쪽, 위, 대각선 왼쪽 위가 모두 정사각형이 형성되어 있으면(1 이상) 
# 현재 행.열은 여지껏 형성된 정사각형들 중 최소 변의 길이를 갖는 정사각형의 변+1이다.
Large <- function(mat){
  segment <- matrix(0, nrow(mat), ncol(mat))
  segment[1, ] <- mat[1, ]
  segment[, 1] <- mat[, 1]
  for (i in 2:nrow(mat)){
    for (j in 2:ncol(mat)){
      if (mat[i, j] == 1){
        segment[i, j] <- min(segment[i-1, j], segment[i, j-1], segment[i-1, j-1])+1
      } else{
        segment[i, j] <- 0
      }
    }
  }
  return((max(segment))^2)
}

Mat1 <- matrix(c(1, 0, 1, 1, 1,
                 0, 0, 0, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE)
Mat2 <- matrix(c(1, 0, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1,
                 1, 0, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 1, 1,
                 0, 0, 1, 1, 1, 1, 1), 5, 7, byrow=TRUE)

Large(Mat1); Large(Mat2)
#########################################
# 문제 3번
#########################################
# A(10, 20), B(20, 5), C(5, 30), D(30, 15)
# input : c(10, 20, 5, 30, 15)
# 이 문제는 matrix chain multiplication으로 DP issue 중 하나이다.
# memo[i, j] = i번째 젓가락쌍부터 j번째 젓가락쌍까지만을 가지고 계산한 최소배송비
# ex)memo[1, 3] = A, B, C를 가지고 계산한 최소배송비 -> 이 때 임시 인풋은 c(10, 20, 5, 30)
# 그렇다면 A(B, C, D) = memo[1, 1] + memo[2, 4] + input[1]*input[2]*[5]가 되고
# (A, B)(C, D) = memo[1, 2] + memo[3, 4] + input[1]*input[3]*input[5]가 된다.
# 이런 식으로 메모리 테이블을 채워나간다. 메모리 테이블이 행렬이지만 매우 작은 시간복잡도로 계산해낼 수 있다.

chopchop <- function(x){
  X <- x
  N <- length(x) - 1
  memo <- matrix(0, N, N) # memo[i, i] = 0이다. 왜냐하면 단 한 쌍의 젓가락쌍만으로는 배송을 할 수 없기 때문
  
  for (s in 2:N){
    for (i in 1:(N-s+1)){ 
      j <- i+s-1
      memo[i, j] <- Inf # min함수에서의 비교를 위해 초기화
      for (k in i:(j-1)){
        memo[i, j] <- min(memo[i, j], memo[i, k] + 
                            memo[k+1, j] +
                            X[i]*X[k+1]*X[j+1])
      }
    }
  }
  return(memo[1, N])
}

test1 <- c(10, 20, 5, 30, 15)
test2 <- c(10, 20, 5, 30)
test3 <- c(30, 35, 15, 5, 10, 20, 25)
chopchop(test1); chopchop(test2); chopchop(test3)
#########################################
# 문제 4번
#########################################
solveEquation <- function(A, x = NULL, b = NULL){
  if (is.null(b)){
    b <- A%*%x
    return(as.numeric(b))
  } else if (is.null(x)){
    A.b <- cbind(A, b)            #가우스 소거법, backward substitution
    eps <- 1e-5
    p <- nrow(A)
    for (i in 1:p){
      if ((abs(A.b[i, i]) < eps) & (i < p)){
        temp.row <- A.b[i+1, ]
        A.b[i+1, ] <- A.b[i, ]
        A.b[i, ] <- temp.row
      }
      for (j in (i+1):(p+1)) A.b[i, j] <- A.b[i, j]/A.b[i, i]
      A.b[i, i] <- 1
      if (i < p){
        for (k in (i+1):p) A.b[k, ] <- A.b[k, ]-A.b[k, i]/A.b[i, i]*A.b[i, ]
      }
    }
    x <- numeric(p)
    if (is.infinite(abs(A.b[p, p+1]))){
      print("There is no solution")
    }else if (is.nan(abs(A.b[p, p+1]))){
      print("There are infinitely many solutions")
    }else{
      for (i in p:1){
        if (i < p){
          temp <- 0
          for (j in (i+1):p) temp <- temp + A.b[i, j]*x[j]
          x[i] <- A.b[i, p+1]-temp
        } else x[i] <- A.b[i, p+1]
      }
      return(x)
    }
  }else if (identical(as.numeric(A%*%x), b)==TRUE){
    print("Correct")
  }else print("Incorrect")
}

#input (A, x, b), (A, x), (A, b)에 대해
#and 해가 무수히 많은 경우와 해가 존재하지 않는 경우에 대해 함수 성능 검정
A <- matrix(c(2, -5, 4,
              1, -2, 1,
              1, -4, -6), 3, 3, byrow = TRUE)
b <- c(-3, 5, -362)
true_x <- c(124, 75, 31)
false_x <- c(21, 22, 11)
solveEquation(A, true_x, b); solveEquation(A, false_x, b)
solveEquation(A, true_x)
solveEquation(A, b = b)

non_mat <- matrix(c(1, 3, -2,
                    1, 1, 4,
                    3, 5, 6), 3, 3, byrow = TRUE)
non_b <- c(5, 3, 7)
inf_mat <- matrix(c(1, 3, -2,
                    2, 7, -7,
                    1, 1, 4), 3, 3, byrow = TRUE)
inf_b <- c(5, 12, 1)
solveEquation(inf_mat, b = inf_b)
solveEquation(non_mat, b = non_b)
#########################################
