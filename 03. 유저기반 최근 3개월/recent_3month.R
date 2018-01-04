getwd()
setwd("C:/Users/JYEON/Desktop/데이터")

#progress bar in R
library(progress)
progress_bar

#fread in data.table
library(data.table)

raw_purchase<-fread("new_purchase.csv",header=T,sep=",")
#불러오기 끝


##2015년 9월~11월 3개월 구매데이터 추출
raw_purchase$구매일자<-as.Date(raw_purchase$구매일자,"%Y%m%d")
raw_purchase<-raw_purchase[raw_purchase$구매일자<16770 & raw_purchase$구매일자>16678,]
#고객번호와 소분류코드만 추출
raw_purchase<-raw_purchase[,3:4]


# 고객별 제품 구매 건수 테이블 만들기
memory.limit(40000)

temp_table<-as.data.frame(table(raw_purchase))

levels(temp_table$소분류코드)
levels(temp_table$고객번호)

length(levels(temp_table$소분류코드))
length(levels(temp_table$고객번호))

#고객별 구매 벡터만들기
freq_value<-temp_table$Freq
purchase_vector<-matrix(freq_value,nrow=19305,byrow=TRUE)

########################################################### 유사도 행렬 제작 #####


#15군집 결과 불러오기
cluster_index<-read.csv("C:/Users/JYEON/Desktop/데이터/15_cluster.csv")
colnames(cluster_index)<-c("customer number","cluster number")
str(cluster_index)

cluster_index<-cluster_index[cluster_index$`customer number`%in%as.numeric(levels(temp_table$고객번호)),]
cluster_number<-cluster_index$`cluster number`




#1번군집의 유사도 행렬
cluster_1<-purchase_vector[which(cluster_number==1),]

cos.sim <- function(ix) 
{
  A = cluster_1[ix[1],]
  B = cluster_1[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_1) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_1 <- matrix(apply(cmb,1,cos.sim),n,n)

#2번군집의 유사도 행렬
cluster_2<-purchase_vector[which(cluster_number==2),]

cos.sim <- function(ix) 
{
  A = cluster_2[ix[1],]
  B = cluster_2[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_2) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_2 <- matrix(apply(cmb,1,cos.sim),n,n)

#3번군집의 유사도행렬
cluster_3<-purchase_vector[which(cluster_number==3),]

cos.sim <- function(ix) 
{
  A = cluster_3[ix[1],]
  B = cluster_3[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_3) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_3 <- matrix(apply(cmb,1,cos.sim),n,n)

#4번군집의 유사도행렬
cluster_4<-purchase_vector[which(cluster_number==4),]

cos.sim <- function(ix) 
{
  A = cluster_4[ix[1],]
  B = cluster_4[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_4) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_4 <- matrix(apply(cmb,1,cos.sim),n,n)


#5번군집의 유사도 행렬

cluster_5<-purchase_vector[which(cluster_number==5),]

cos.sim <- function(ix) 
{
  A = cluster_5[ix[1],]
  B = cluster_5[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_5) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_5 <- matrix(apply(cmb,1,cos.sim),n,n)

#6번군집의 유사도 행렬

cluster_6<-purchase_vector[which(cluster_number==6),]

cos.sim <- function(ix) 
{
  A = cluster_6[ix[1],]
  B = cluster_6[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_6) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_6 <- matrix(apply(cmb,1,cos.sim),n,n)

#7번군집의 유사도 행렬

cluster_7<-purchase_vector[which(cluster_number==7),]

cos.sim <- function(ix) 
{
  A = cluster_7[ix[1],]
  B = cluster_7[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_7) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_7 <- matrix(apply(cmb,1,cos.sim),n,n)

#8번군집의 유사도 행렬

cluster_8<-purchase_vector[which(cluster_number==8),]

cos.sim <- function(ix) 
{
  A = cluster_8[ix[1],]
  B = cluster_8[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_8) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_8 <- matrix(apply(cmb,1,cos.sim),n,n)

#9번군집의 유사도 행렬

cluster_9<-purchase_vector[which(cluster_number==9),]

cos.sim <- function(ix) 
{
  A = cluster_9[ix[1],]
  B = cluster_9[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_9) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_9 <- matrix(apply(cmb,1,cos.sim),n,n)

#10번군집의 유사도 행렬

cluster_10<-purchase_vector[which(cluster_number==10),]

cos.sim <- function(ix) 
{
  A = cluster_10[ix[1],]
  B = cluster_10[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_10) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_10 <- matrix(apply(cmb,1,cos.sim),n,n)

#11번군집의 유사도 행렬

cluster_11<-purchase_vector[which(cluster_number==11),]

cos.sim <- function(ix) 
{
  A = cluster_11[ix[1],]
  B = cluster_11[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_11) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_11 <- matrix(apply(cmb,1,cos.sim),n,n)

#12번군집의 유사도 행렬

cluster_12<-purchase_vector[which(cluster_number==12),]

cos.sim <- function(ix) 
{
  A = cluster_12[ix[1],]
  B = cluster_12[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_12) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_12 <- matrix(apply(cmb,1,cos.sim),n,n)

#13번군집의 유사도 행렬

cluster_13<-purchase_vector[which(cluster_number==13),]

cos.sim <- function(ix) 
{
  A = cluster_13[ix[1],]
  B = cluster_13[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_13) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_13 <- matrix(apply(cmb,1,cos.sim),n,n)

#14번군집의 유사도 행렬

cluster_14<-purchase_vector[which(cluster_number==14),]

cos.sim <- function(ix) 
{
  A = cluster_14[ix[1],]
  B = cluster_14[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_14) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_14 <- matrix(apply(cmb,1,cos.sim),n,n)

#15번군집의 유사도 행렬

cluster_15<-purchase_vector[which(cluster_number==15),]

cos.sim <- function(ix) 
{
  A = cluster_15[ix[1],]
  B = cluster_15[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(cluster_15) 
cmb <- expand.grid(i=1:n, j=1:n) 
similar_15 <- matrix(apply(cmb,1,cos.sim),n,n)



#as.data.frame
similar_1<-as.data.frame(similar_1)
similar_2<-as.data.frame(similar_2)
similar_3<-as.data.frame(similar_3)
similar_4<-as.data.frame(similar_4)
similar_5<-as.data.frame(similar_5)
similar_6<-as.data.frame(similar_6)
similar_7<-as.data.frame(similar_7)
similar_8<-as.data.frame(similar_8)
similar_9<-as.data.frame(similar_9)
similar_10<-as.data.frame(similar_10)
similar_11<-as.data.frame(similar_11)
similar_12<-as.data.frame(similar_12)
similar_13<-as.data.frame(similar_13)
similar_14<-as.data.frame(similar_14)
similar_15<-as.data.frame(similar_15)


#고객번호 bind 

head(cluster_index)

row.names(similar_1)<-cluster_index[cluster_index$`cluster number`==1,]$`customer number`
colnames(similar_1)<-cluster_index[cluster_index$`cluster number`==1,]$`customer number`

row.names(similar_2)<-cluster_index[cluster_index$`cluster number`==2,]$`customer number`
colnames(similar_2)<-cluster_index[cluster_index$`cluster number`==2,]$`customer number`

row.names(similar_3)<-cluster_index[cluster_index$`cluster number`==3,]$`customer number`
colnames(similar_3)<-cluster_index[cluster_index$`cluster number`==3,]$`customer number`

row.names(similar_4)<-cluster_index[cluster_index$`cluster number`==4,]$`customer number`
colnames(similar_4)<-cluster_index[cluster_index$`cluster number`==4,]$`customer number`

row.names(similar_5)<-cluster_index[cluster_index$`cluster number`==5,]$`customer number`
colnames(similar_5)<-cluster_index[cluster_index$`cluster number`==5,]$`customer number`

row.names(similar_6)<-cluster_index[cluster_index$`cluster number`==6,]$`customer number`
colnames(similar_6)<-cluster_index[cluster_index$`cluster number`==6,]$`customer number`

row.names(similar_7)<-cluster_index[cluster_index$`cluster number`==7,]$`customer number`
colnames(similar_7)<-cluster_index[cluster_index$`cluster number`==7,]$`customer number`

row.names(similar_8)<-cluster_index[cluster_index$`cluster number`==8,]$`customer number`
colnames(similar_8)<-cluster_index[cluster_index$`cluster number`==8,]$`customer number`

row.names(similar_9)<-cluster_index[cluster_index$`cluster number`==9,]$`customer number`
colnames(similar_9)<-cluster_index[cluster_index$`cluster number`==9,]$`customer number`

row.names(similar_10)<-cluster_index[cluster_index$`cluster number`==10,]$`customer number`
colnames(similar_10)<-cluster_index[cluster_index$`cluster number`==10,]$`customer number`

row.names(similar_11)<-cluster_index[cluster_index$`cluster number`==11,]$`customer number`
colnames(similar_11)<-cluster_index[cluster_index$`cluster number`==11,]$`customer number`

row.names(similar_12)<-cluster_index[cluster_index$`cluster number`==12,]$`customer number`
colnames(similar_12)<-cluster_index[cluster_index$`cluster number`==12,]$`customer number`

row.names(similar_13)<-cluster_index[cluster_index$`cluster number`==13,]$`customer number`
colnames(similar_13)<-cluster_index[cluster_index$`cluster number`==13,]$`customer number`

row.names(similar_14)<-cluster_index[cluster_index$`cluster number`==14,]$`customer number`
colnames(similar_14)<-cluster_index[cluster_index$`cluster number`==14,]$`customer number`

row.names(similar_15)<-cluster_index[cluster_index$`cluster number`==15,]$`customer number`
colnames(similar_15)<-cluster_index[cluster_index$`cluster number`==15,]$`customer number`


##유사도 행렬을 통해 상위 5% 유사 고객 추출

list<-c(1372,1654,651,926,2621,369,2319,1648,1359,908,895,1855,1328,890,510)
round(list*0.05)

######################################################
pb<-progress_bar$new(total=1372)
## 1번 군집 유사고객 추출

list_1<-matrix(0,nrow=1372,ncol=69)

for(i in 1:1372){
  pb$tick()
  list_1[i,]<-colnames(sort(similar_1[i,],decreasing=T)[1:69])
}

list_1<-as.data.frame(list_1)
rownames(list_1)<-rownames(similar_1)

######################################################
pb<-progress_bar$new(total=1654)
## 2번 군집 유사고객 추출

list_2<-matrix(0,nrow=1654,ncol=83)

for(i in 1:1654){
  pb$tick()
  list_2[i,]<-colnames(sort(similar_2[i,],decreasing=T)[1:83])
}

list_2<-as.data.frame(list_2)
rownames(list_2)<-rownames(similar_2)

######################################################
pb<-progress_bar$new(total=651)
## 3번 군집 유사고객 추출

list_3<-matrix(0,nrow=651,ncol=33)

for(i in 1:651){
  pb$tick()
  list_3[i,]<-colnames(sort(similar_3[i,],decreasing=T)[1:33])
}

list_3<-as.data.frame(list_3)
rownames(list_3)<-rownames(similar_3)

######################################################
pb<-progress_bar$new(total=926)
## 4번 군집 유사고객 추출

list_4<-matrix(0,nrow=926,ncol=46)

for(i in 1:926){
  pb$tick()
  list_4[i,]<-colnames(sort(similar_4[i,],decreasing=T)[1:46])
}

list_4<-as.data.frame(list_4)
rownames(list_4)<-rownames(similar_4)

######################################################
pb<-progress_bar$new(total=2621)
## 5번 군집 유사고객 추출

list_5<-matrix(0,nrow=2621,ncol=131)

for(i in 1:2621){
  pb$tick()
  list_5[i,]<-colnames(sort(similar_5[i,],decreasing=T)[1:131])
}

list_5<-as.data.frame(list_5)
rownames(list_5)<-rownames(similar_5)

######################################################
pb<-progress_bar$new(total=369)
## 6번 군집 유사고객 추출

list_6<-matrix(0,nrow=369,ncol=18)

for(i in 1:369){
  pb$tick()
  list_6[i,]<-colnames(sort(similar_6[i,],decreasing=T)[1:18])
}

list_6<-as.data.frame(list_6)
rownames(list_6)<-rownames(similar_6)

######################################################
pb<-progress_bar$new(total=2319)
## 7번 군집 유사고객 추출

list_7<-matrix(0,nrow=2319,ncol=116)

for(i in 1:2319){
  pb$tick()
  list_7[i,]<-colnames(sort(similar_7[i,],decreasing=T)[1:116])
}

list_7<-as.data.frame(list_7)
rownames(list_7)<-rownames(similar_7)

######################################################
pb<-progress_bar$new(total=1648)
## 8번 군집 유사고객 추출

list_8<-matrix(0,nrow=1648,ncol=82)

for(i in 1:1648){
  pb$tick()
  list_8[i,]<-colnames(sort(similar_8[i,],decreasing=T)[1:82])
}

list_8<-as.data.frame(list_8)
rownames(list_8)<-rownames(similar_8)

#########################################################
pb<-progress_bar$new(total=1359)
## 9번 군집 유사고객 추출

list_9<-matrix(0,nrow=1359,ncol=68)

for(i in 1:1359){
  pb$tick()
  list_9[i,]<-colnames(sort(similar_9[i,],decreasing=T)[1:68])
}

list_9<-as.data.frame(list_9)
rownames(list_9)<-rownames(similar_9)

#########################################################
pb<-progress_bar$new(total=908)
## 10번 군집 유사고객 추출

list_10<-matrix(0,nrow=908,ncol=45)

for(i in 1:908){
  pb$tick()
  list_10[i,]<-colnames(sort(similar_10[i,],decreasing=T)[1:45])
}

list_10<-as.data.frame(list_10)
rownames(list_10)<-rownames(similar_10)

#########################################################
pb<-progress_bar$new(total=895)
## 11번 군집 유사고객 추출

list_11<-matrix(0,nrow=895,ncol=45)

for(i in 1:895){
  pb$tick()
  list_11[i,]<-colnames(sort(similar_11[i,],decreasing=T)[1:45])
}

list_11<-as.data.frame(list_11)
rownames(list_11)<-rownames(similar_11)

#########################################################
pb<-progress_bar$new(total=1855)
## 12번 군집 유사고객 추출

list_12<-matrix(0,nrow=1855,ncol=93)

for(i in 1:1855){
  pb$tick()
  list_12[i,]<-colnames(sort(similar_12[i,],decreasing=T)[1:93])
}

list_12<-as.data.frame(list_12)
rownames(list_12)<-rownames(similar_12)

#########################################################
pb<-progress_bar$new(total=1328)
## 13번 군집 유사고객 추출

list_13<-matrix(0,nrow=1328,ncol=66)

for(i in 1:1328){
  pb$tick()
  list_13[i,]<-colnames(sort(similar_13[i,],decreasing=T)[1:66])
}

list_13<-as.data.frame(list_13)
rownames(list_13)<-rownames(similar_13)

#########################################################
pb<-progress_bar$new(total=890)
## 14번 군집 유사고객 추출

list_14<-matrix(0,nrow=890,ncol=44)

for(i in 1:890){
  pb$tick()
  list_14[i,]<-colnames(sort(similar_14[i,],decreasing=T)[1:44])
}

list_14<-as.data.frame(list_14)
rownames(list_14)<-rownames(similar_14)

#########################################################
pb<-progress_bar$new(total=510)
## 15번 군집 유사고객 추출

list_15<-matrix(0,nrow=510,ncol=26)

for(i in 1:510){
  pb$tick()
  list_15[i,]<-colnames(sort(similar_15[i,],decreasing=T)[1:26])
}

list_15<-as.data.frame(list_15)
rownames(list_15)<-rownames(similar_15)

#########################################################
##고객들 구매내역 합집합 및 최종 추천##


#구매내역 합집합 행렬
union_purchase<-matrix(0,nrow=19383,ncol=3144)

rownames(purchase_vector)<-as.numeric(levels(temp_table$고객번호))

####
####
#list1 고객

pb<-progress_bar$new(total=1372)
for(i in 1:1372){
  for(j in 1:69){
    union_purchase[as.numeric(rownames(list_1)[i]),]<-union_purchase[as.numeric(rownames(list_1)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_1[i,j])),]
  }
  pb$tick()
}

#list2 고객
pb<-progress_bar$new(total=1654)
for(i in 1:1654){
  for(j in 1:83){
    union_purchase[as.numeric(rownames(list_2)[i]),]<-union_purchase[as.numeric(rownames(list_2)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_2[i,j])),]
  }
  pb$tick()
}

#list3 고객
pb<-progress_bar$new(total=651)
for(i in 1:651){
  for(j in 1:33){
    union_purchase[as.numeric(rownames(list_3)[i]),]<-union_purchase[as.numeric(rownames(list_3)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_3[i,j])),]
  }
  pb$tick()
}

#list4 고객
pb<-progress_bar$new(total=926)
for(i in 1:926){
  for(j in 1:46){
    union_purchase[as.numeric(rownames(list_4)[i]),]<-union_purchase[as.numeric(rownames(list_4)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_4[i,j])),]
  }
  pb$tick()
}

#list5 고객
pb<-progress_bar$new(total=2621)
for(i in 1:2621){
  for(j in 1:131){
    union_purchase[as.numeric(rownames(list_5)[i]),]<-union_purchase[as.numeric(rownames(list_5)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_5[i,j])),]
  }
  pb$tick()
}

#list6 고객
pb<-progress_bar$new(total=369)
for(i in 1:369){
  for(j in 1:18){
    union_purchase[as.numeric(rownames(list_6)[i]),]<-union_purchase[as.numeric(rownames(list_6)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_6[i,j])),]
  }
  pb$tick()
}

#list7 고객
pb<-progress_bar$new(total=2319)
for(i in 1:2319){
  for(j in 1:116){
    union_purchase[as.numeric(rownames(list_7)[i]),]<-union_purchase[as.numeric(rownames(list_7)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_7[i,j])),]
  }
  pb$tick()
}

#list8 고객
pb<-progress_bar$new(total=1648)
for(i in 1:1648){
  for(j in 1:82){
    union_purchase[as.numeric(rownames(list_8)[i]),]<-union_purchase[as.numeric(rownames(list_8)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_8[i,j])),]
  }
  pb$tick()
}

#list9 고객
pb<-progress_bar$new(total=1359)
for(i in 1:1359){
  for(j in 1:68){
    union_purchase[as.numeric(rownames(list_9)[i]),]<-union_purchase[as.numeric(rownames(list_9)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_9[i,j])),]
  }
  pb$tick()
}

#list10 고객
pb<-progress_bar$new(total=908)
for(i in 1:908){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_10)[i]),]<-union_purchase[as.numeric(rownames(list_10)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_10[i,j])),]
  }
  pb$tick()
}

#list11 고객
pb<-progress_bar$new(total=895)
for(i in 1:895){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_11)[i]),]<-union_purchase[as.numeric(rownames(list_11)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_11[i,j])),]
  }
  pb$tick()
}

#list12 고객
pb<-progress_bar$new(total=1855)
for(i in 1:1855){
  for(j in 1:93){
    union_purchase[as.numeric(rownames(list_12)[i]),]<-union_purchase[as.numeric(rownames(list_12)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_12[i,j])),]
  }
  pb$tick()
}

#list13 고객
pb<-progress_bar$new(total=1328)
for(i in 1:1328){
  for(j in 1:66){
    union_purchase[as.numeric(rownames(list_13)[i]),]<-union_purchase[as.numeric(rownames(list_13)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_13[i,j])),]
  }
  pb$tick()
}

#list14 고객
pb<-progress_bar$new(total=890)
for(i in 1:890){
  for(j in 1:44){
    union_purchase[as.numeric(rownames(list_14)[i]),]<-union_purchase[as.numeric(rownames(list_14)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_14[i,j])),]
  }
  pb$tick()
}

#list15 고객
pb<-progress_bar$new(total=510)
for(i in 1:510){
  for(j in 1:26){
    union_purchase[as.numeric(rownames(list_15)[i]),]<-union_purchase[as.numeric(rownames(list_15)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_15[i,j])),]
  }
  pb$tick()
}


#####################################################################################################################


## 본인이 구매한 내역 제거 - 최종 recommand_matrix
recommand_matrix<-union_purchase

for(i in 1:19305){
  for(j in 1:3144){
    if(purchase_vector[i,j]>0)
      recommand_matrix[as.numeric(row.names(purchase_vector)[i]),j]<-0
  }
}

##


#상위 세개 값 뽑는 함수
third<-function(x){
  a<-max(x)
  b<-max(x[x!=a])
  c<-max(x[x!=a&x!=b])
  return(c(a,b,c))
}


####최종추천 목록
#아이템인덱스


recommand_item<-as.list(NA)


pb<-progress_bar$new(total=19383)
for(num in 1:19383){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,levels(temp_table$소분류코드)[which(recommand_matrix[num,]==third(recommand_matrix[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  recommand_item[[num]]<-temp
  pb$tick()
}


##
recent_3month_recommand_item<-matrix(unlist(recommand_item),nrow=19383,byrow=T)
write.csv(recent_3month_recommand_item,"recent_3month_recommand_item.csv")

