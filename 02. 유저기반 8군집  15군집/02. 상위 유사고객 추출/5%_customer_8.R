memory.limit(40000)
#top 5% 추출
list<-c(2320,3877,2529,1568,3386,1899,2452,1352)
round(list*0.05)


##
# 2 , 6 번 군집은 구매내역이 없는 고객이 포함되어 있어 별도의 처리가 필요함

similar_2[is.na(similar_2)]<-0
similar_6[is.na(similar_6)]<-0
##



library(progress)
######################################################
pb<-progress_bar$new(total=2320)
## 1번 군집 유사고객 추출

list_1<-matrix(0,nrow=2320,ncol=116)

for(i in 1:2320){
  pb$tick()
  list_1[i,]<-colnames(sort(similar_1[i,],decreasing=T)[1:116])
}

list_1<-as.data.frame(list_1)
rownames(list_1)<-rownames(similar_1)

######################################################
pb<-progress_bar$new(total=3877)
## 2번 군집 유사고객 추출

list_2<-matrix(0,nrow=3877,ncol=194)

for(i in 1:3877){
  pb$tick()
  list_2[i,]<-colnames(sort(similar_2[i,],decreasing=T)[1:194])
}

list_2<-as.data.frame(list_2)
rownames(list_2)<-rownames(similar_2)

######################################################
pb<-progress_bar$new(total=2529)
## 3번 군집 유사고객 추출

list_3<-matrix(0,nrow=2529,ncol=126)

for(i in 1:2529){
  pb$tick()
  list_3[i,]<-colnames(sort(similar_3[i,],decreasing=T)[1:126])
}

list_3<-as.data.frame(list_3)
rownames(list_3)<-rownames(similar_3)

######################################################
pb<-progress_bar$new(total=1568)
## 4번 군집 유사고객 추출

list_4<-matrix(0,nrow=1568,ncol=78)

for(i in 1:1568){
  pb$tick()
  list_4[i,]<-colnames(sort(similar_4[i,],decreasing=T)[1:78])
}

list_4<-as.data.frame(list_4)
rownames(list_4)<-rownames(similar_4)

######################################################
pb<-progress_bar$new(total=3386)
## 5번 군집 유사고객 추출

list_5<-matrix(0,nrow=3386,ncol=169)

for(i in 1:3386){
  pb$tick()
  list_5[i,]<-colnames(sort(similar_5[i,],decreasing=T)[1:169])
}

list_5<-as.data.frame(list_5)
rownames(list_5)<-rownames(similar_5)

######################################################
pb<-progress_bar$new(total=1899)
## 6번 군집 유사고객 추출

list_6<-matrix(0,nrow=1899,ncol=95)

for(i in 1:1899){
  pb$tick()
  list_6[i,]<-colnames(sort(similar_6[i,],decreasing=T)[1:95])
}

list_6<-as.data.frame(list_6)
rownames(list_6)<-rownames(similar_6)

######################################################
pb<-progress_bar$new(total=2452)
## 7번 군집 유사고객 추출

list_7<-matrix(0,nrow=2452,ncol=123)

for(i in 1:2452){
  pb$tick()
  list_7[i,]<-colnames(sort(similar_7[i,],decreasing=T)[1:123])
}

list_7<-as.data.frame(list_7)
rownames(list_7)<-rownames(similar_7)

######################################################
pb<-progress_bar$new(total=1352)
## 8번 군집 유사고객 추출

list_8<-matrix(0,nrow=1352,ncol=68)

for(i in 1:1352){
  pb$tick()
  list_8[i,]<-colnames(sort(similar_8[i,],decreasing=T)[1:68])
}

list_8<-as.data.frame(list_8)
rownames(list_8)<-rownames(similar_8)


#########################################################

