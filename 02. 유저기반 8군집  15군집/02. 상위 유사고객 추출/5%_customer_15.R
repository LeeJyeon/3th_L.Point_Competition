memory.limit(40000)
#top 5% 추출
list<-c(1380,1657,655,929,2628,370,2324,1657,1365,912,897,1861,1344,892,512)
round(list*0.05)

##
# 12 , 13 번 군집은 구매내역이 없는 고객이 포함되어 있어 별도의 처리가 필요함

similar_12[is.na(similar_12)]<-0
similar_13[is.na(similar_13)]<-0
##



library(progress)
######################################################
pb<-progress_bar$new(total=1380)
## 1번 군집 유사고객 추출

list_1<-matrix(0,nrow=1380,ncol=69)

for(i in 1:1380){
  pb$tick()
  list_1[i,]<-colnames(sort(similar_1[i,],decreasing=T)[1:69])
}

list_1<-as.data.frame(list_1)
rownames(list_1)<-rownames(similar_1)

######################################################
pb<-progress_bar$new(total=1657)
## 2번 군집 유사고객 추출

list_2<-matrix(0,nrow=1657,ncol=83)

for(i in 1:1657){
  pb$tick()
  list_2[i,]<-colnames(sort(similar_2[i,],decreasing=T)[1:83])
}

list_2<-as.data.frame(list_2)
rownames(list_2)<-rownames(similar_2)

######################################################
pb<-progress_bar$new(total=655)
## 3번 군집 유사고객 추출

list_3<-matrix(0,nrow=655,ncol=33)

for(i in 1:655){
  pb$tick()
  list_3[i,]<-colnames(sort(similar_3[i,],decreasing=T)[1:33])
}

list_3<-as.data.frame(list_3)
rownames(list_3)<-rownames(similar_3)

######################################################
pb<-progress_bar$new(total=929)
## 4번 군집 유사고객 추출

list_4<-matrix(0,nrow=929,ncol=46)

for(i in 1:929){
  pb$tick()
  list_4[i,]<-colnames(sort(similar_4[i,],decreasing=T)[1:46])
}

list_4<-as.data.frame(list_4)
rownames(list_4)<-rownames(similar_4)

######################################################
pb<-progress_bar$new(total=2628)
## 5번 군집 유사고객 추출

list_5<-matrix(0,nrow=2628,ncol=131)

for(i in 1:2628){
  pb$tick()
  list_5[i,]<-colnames(sort(similar_5[i,],decreasing=T)[1:131])
}

list_5<-as.data.frame(list_5)
rownames(list_5)<-rownames(similar_5)

######################################################
pb<-progress_bar$new(total=370)
## 6번 군집 유사고객 추출

list_6<-matrix(0,nrow=370,ncol=18)

for(i in 1:370){
  pb$tick()
  list_6[i,]<-colnames(sort(similar_6[i,],decreasing=T)[1:18])
}

list_6<-as.data.frame(list_6)
rownames(list_6)<-rownames(similar_6)

######################################################
pb<-progress_bar$new(total=2324)
## 7번 군집 유사고객 추출

list_7<-matrix(0,nrow=2324,ncol=116)

for(i in 1:2324){
  pb$tick()
  list_7[i,]<-colnames(sort(similar_7[i,],decreasing=T)[1:116])
}

list_7<-as.data.frame(list_7)
rownames(list_7)<-rownames(similar_7)

######################################################
pb<-progress_bar$new(total=1657)
## 8번 군집 유사고객 추출

list_8<-matrix(0,nrow=1657,ncol=83)

for(i in 1:1657){
  pb$tick()
  list_8[i,]<-colnames(sort(similar_8[i,],decreasing=T)[1:83])
}

list_8<-as.data.frame(list_8)
rownames(list_8)<-rownames(similar_8)

#########################################################
pb<-progress_bar$new(total=1365)
## 9번 군집 유사고객 추출

list_9<-matrix(0,nrow=1365,ncol=68)

for(i in 1:1365){
  pb$tick()
  list_9[i,]<-colnames(sort(similar_9[i,],decreasing=T)[1:68])
}

list_9<-as.data.frame(list_9)
rownames(list_9)<-rownames(similar_9)

#########################################################
pb<-progress_bar$new(total=912)
## 10번 군집 유사고객 추출

list_10<-matrix(0,nrow=912,ncol=46)

for(i in 1:912){
  pb$tick()
  list_10[i,]<-colnames(sort(similar_10[i,],decreasing=T)[1:46])
}

list_10<-as.data.frame(list_10)
rownames(list_10)<-rownames(similar_10)

#########################################################
pb<-progress_bar$new(total=897)
## 11번 군집 유사고객 추출

list_11<-matrix(0,nrow=897,ncol=45)

for(i in 1:897){
  pb$tick()
  list_11[i,]<-colnames(sort(similar_11[i,],decreasing=T)[1:45])
}

list_11<-as.data.frame(list_11)
rownames(list_11)<-rownames(similar_11)

#########################################################
pb<-progress_bar$new(total=1861)
## 12번 군집 유사고객 추출

list_12<-matrix(0,nrow=1861,ncol=93)

for(i in 1:1861){
  pb$tick()
  list_12[i,]<-colnames(sort(similar_12[i,],decreasing=T)[1:93])
}

list_12<-as.data.frame(list_12)
rownames(list_12)<-rownames(similar_12)

#########################################################
pb<-progress_bar$new(total=1344)
## 13번 군집 유사고객 추출

list_13<-matrix(0,nrow=1344,ncol=67)

for(i in 1:1344){
  pb$tick()
  list_13[i,]<-colnames(sort(similar_13[i,],decreasing=T)[1:67])
}

list_13<-as.data.frame(list_13)
rownames(list_13)<-rownames(similar_13)

#########################################################
pb<-progress_bar$new(total=892)
## 14번 군집 유사고객 추출

list_14<-matrix(0,nrow=892,ncol=45)

for(i in 1:892){
  pb$tick()
  list_14[i,]<-colnames(sort(similar_14[i,],decreasing=T)[1:45])
}

list_14<-as.data.frame(list_14)
rownames(list_14)<-rownames(similar_14)

#########################################################
pb<-progress_bar$new(total=512)
## 15번 군집 유사고객 추출

list_15<-matrix(0,nrow=512,ncol=26)

for(i in 1:512){
  pb$tick()
  list_15[i,]<-colnames(sort(similar_15[i,],decreasing=T)[1:26])
}

list_15<-as.data.frame(list_15)
rownames(list_15)<-rownames(similar_15)

#########################################################

