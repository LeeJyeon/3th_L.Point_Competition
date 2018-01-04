#고객별 구매데이터 환경
#purchase_vector - 19383 명 고객의 2015년 11월 까지 4270아이템의 구매 빈도 테이블

##############################################################################################

##############################################################################################

#15군집 결과 불러오기
cluster_index<-read.csv("C:/Users/JYEON/Desktop/데이터/15_cluster.csv")
colnames(cluster_index)<-c("customer number","cluster number")
str(cluster_index)

#군집별 고객수 확인
nrow(cluster_index[cluster_index$`cluster number`==1,])

#1번군집의 유사도 행렬
cluster_1<-purchase_vector[cluster_index[cluster_index$`cluster number`==1,]$`customer number`,]

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
cluster_2<-purchase_vector[cluster_index[cluster_index$`cluster number`==2,]$`customer number`,]

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
cluster_3<-purchase_vector[cluster_index[cluster_index$`cluster number`==3,]$`customer number`,]

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
cluster_4<-purchase_vector[cluster_index[cluster_index$`cluster number`==4,]$`customer number`,]

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

cluster_5<-purchase_vector[cluster_index[cluster_index$`cluster number`==5,]$`customer number`,]

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

cluster_6<-purchase_vector[cluster_index[cluster_index$`cluster number`==6,]$`customer number`,]

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

cluster_7<-purchase_vector[cluster_index[cluster_index$`cluster number`==7,]$`customer number`,]

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

cluster_8<-purchase_vector[cluster_index[cluster_index$`cluster number`==8,]$`customer number`,]

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

cluster_9<-purchase_vector[cluster_index[cluster_index$`cluster number`==9,]$`customer number`,]

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

cluster_10<-purchase_vector[cluster_index[cluster_index$`cluster number`==10,]$`customer number`,]

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

cluster_11<-purchase_vector[cluster_index[cluster_index$`cluster number`==11,]$`customer number`,]

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

cluster_12<-purchase_vector[cluster_index[cluster_index$`cluster number`==12,]$`customer number`,]

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

cluster_13<-purchase_vector[cluster_index[cluster_index$`cluster number`==13,]$`customer number`,]

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

cluster_14<-purchase_vector[cluster_index[cluster_index$`cluster number`==14,]$`customer number`,]

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

cluster_15<-purchase_vector[cluster_index[cluster_index$`cluster number`==15,]$`customer number`,]

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
