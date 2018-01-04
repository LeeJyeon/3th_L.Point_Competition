library(data.table)
library(progress)

raw_purchase<-fread("C:/Users/JYEON/Desktop/데이터/new_purchase.csv",header=T,sep=",")
#불러오기 끝


##테스트 셋 생성
raw_purchase$구매일자<-as.Date(raw_purchase$구매일자,"%Y%m%d")
before_12_purchase<-raw_purchase[raw_purchase$구매일자<16770,]
before_11_purchase<-before_12_purchase[before_12_purchase$구매일자<16740,]
before_10_purchase<-before_11_purchase[before_11_purchase$구매일자<16709,]
before_09_purchase<-before_10_purchase[before_10_purchase$구매일자<16679,]
before_08_purchase<-before_09_purchase[before_09_purchase$구매일자<16648,]
before_07_purchase<-before_08_purchase[before_08_purchase$구매일자<16617,]
before_06_purchase<-before_07_purchase[before_07_purchase$구매일자<16587,]
  
as.Date(16587,origin="1970-01-01")

##
##상위 3개 값 뽑아내는 함수
third<-function(x){
  a<-max(x)
  b<-max(x[x!=a])
  c<-max(x[x!=a&x!=b])
  
  return(c(a,b,c))
}

##

### 12월 맞추기 ###

temp_table<-as.data.frame(table(before_12_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_12<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_12<-as.data.frame(test_for_12,row.names=customer_code)
write.csv(test_for_12,"test_for_12.csv")
####################

### 11월 맞추기 ###

temp_table<-as.data.frame(table(before_11_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_11<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_11<-as.data.frame(test_for_11,row.names=customer_code)
write.csv(test_for_11,"test_for_11.csv")
####################

### 10월 맞추기 ###

temp_table<-as.data.frame(table(before_10_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_10<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_10<-as.data.frame(test_for_10,row.names=customer_code)
write.csv(test_for_10,"test_for_10.csv")
####################

### 9월 맞추기 ###

temp_table<-as.data.frame(table(before_09_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_09<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_09<-as.data.frame(test_for_09,row.names=customer_code)
write.csv(test_for_09,"test_for_09.csv")
####################

### 08월 맞추기 ###

temp_table<-as.data.frame(table(before_08_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_08<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_08<-as.data.frame(test_for_08,row.names=customer_code)
write.csv(test_for_08,"test_for_08.csv")
####################

### 7월 맞추기 ###

temp_table<-as.data.frame(table(before_07_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_07<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_07<-as.data.frame(test_for_07,row.names=customer_code)
write.csv(test_for_07,"test_for_07.csv")
####################


### 6월 맞추기 ###

temp_table<-as.data.frame(table(before_06_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

test_for_06<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
test_for_06<-as.data.frame(test_for_06,row.names=customer_code)
write.csv(test_for_06,"test_for_06.csv")
####################


###-------------16년 1월 추천------------###
############################################
temp_table<-as.data.frame(table(raw_purchase[,3:4]))
item_code<-levels(temp_table$소분류코드)
customer_code<-levels(temp_table$고객번호)
purchase_vector<-matrix(temp_table$Freq,nrow=length(customer_code),ncol=length(item_code),byrow=T)


#temp_recommand 에 판매빈도 상위 3개에 부합하는 아이템들 저장 -> 이후 3개로 줄임
temp_recommand<-as.list(NA)

pb<-progress_bar$new(total=length(customer_code))
for(num in 1:length(customer_code)){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,item_code[which(purchase_vector[num,]==third(purchase_vector[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  temp_recommand[[num]]<-temp
  pb$tick()
}

bindo_16_01<-matrix(unlist(temp_recommand),ncol=3,byrow=T)
bindo_16_01<-as.data.frame(bindo_16_01,row.names=customer_code)
write.csv(bindo_16_01,"bindo_16_01.csv")
