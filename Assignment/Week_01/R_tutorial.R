############################# 과제 ############################################

# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용",
# 아니면 "식용"을 기입하는 함수를 작성해주세요.
# 함수의 매개변수는 대상 데이터 1개 ex) my_function(cow_data)

# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군 단위로 구해주세요)

# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요

# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 (변수 slaughter_date가 도축된 날짜를 의미함)

# (기존기수만 해당) 
# 5. 소 가격(price)과 상관 관계가 있는 변수들이 있다면 찾아내고 그 관계를 설명해주세요
###########################################################################

### 1번
cow_data <- read.csv("C:/Users/ingulbull/Desktop/2018-1/투빅스/180117 세션(1)/cow_data.csv", header = T, stringsAsFactors = F)
cow_data <- na.omit(cow_data)
str(cow_data)

cow_data$is_edible <- vector(length = 11)
my_function <- function(x){
  x[, ncol(x)] <- ifelse(x[, 8]<50, "식용", ifelse(x[, 9]=="3"|x[, 9]=="등외", "폐기용", "식용"))
  return(x)
}
head(my_function(cow_data))

### 2번
library(dplyr)
library(plyr)
library(stringr)
cow_data$address <- str_split(cow_data$address, pattern =  " ", n = 3, simplify = T)
cow_data$address <- paste(cow_data$address[,1], cow_data$address[,2])
cow2 <- select(subset(cow_data, grade == "1++"), address, grade)
attach(cow2)

top3 <- head(sort(table(address), decreasing = T), 3)
top3
table(address)
detach(cow2)

### 3번
cow_top3 <- subset(cow_data, address %in% names(top3))
cow_top3$price <- gsub(",", "", cow_top3$price)
cow_top3$price <- as.numeric(cow_top3$price)
result <- aggregate(cow_top3$price, list(cow_top3$address, cow_top3$grade), mean)
arrange(result, result[, 1])

### 4번
cow_top3$slaughter_date <- substr(cow_top3$slaughter_date, 5, 6) %>% as.factor()
cow_top3_split <- split(cow_top3, cow_top3$address)
cow_1st <- table(as.factor(cow_top3_split[[1]]$slaughter_date))
cow_2nd <- table(as.factor(cow_top3_split[[2]]$slaughter_date))
cow_3rd <- table(as.factor(cow_top3_split[[3]]$slaughter_date))

plot(cow_1st, type="l", col= "blue", las=1, xlab = "월(2014년)", ylab = "도축 수(마리)", main = "경기도 안성시")
plot(cow_2nd, type="l", col= "blue", las=1, xlab = "월(2014년)", ylab = "도축 수(마리)", main = "전라남도 고흥군")
plot(cow_3rd, type="l", col= "blue", las=1, xlab = "월(2014년)", ylab = "도축 수(마리)", main = "전라북도 정읍시")
