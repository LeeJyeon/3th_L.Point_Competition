memory.limit(40000)
#top 5% 추출
list<-c(1372,1655,569,926,2621,369,2428,1651,1483,908,896,1858,1207,888,488)
round(list*0.05)

#최근 3개월간 19319 명 / 3106 item 구매

##
##



library(progress)
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
pb<-progress_bar$new(total=1655)
## 2번 군집 유사고객 추출

list_2<-matrix(0,nrow=1655,ncol=83)

for(i in 1:1655){
  pb$tick()
  list_2[i,]<-colnames(sort(similar_2[i,],decreasing=T)[1:83])
}

list_2<-as.data.frame(list_2)
rownames(list_2)<-rownames(similar_2)

######################################################
pb<-progress_bar$new(total=569)
## 3번 군집 유사고객 추출

list_3<-matrix(0,nrow=569,ncol=28)

for(i in 1:569){
  pb$tick()
  list_3[i,]<-colnames(sort(similar_3[i,],decreasing=T)[1:28])
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
pb<-progress_bar$new(total=2428)
## 7번 군집 유사고객 추출

list_7<-matrix(0,nrow=2428,ncol=121)

for(i in 1:2428){
  pb$tick()
  list_7[i,]<-colnames(sort(similar_7[i,],decreasing=T)[1:121])
}

list_7<-as.data.frame(list_7)
rownames(list_7)<-rownames(similar_7)

######################################################
pb<-progress_bar$new(total=1651)
## 8번 군집 유사고객 추출

list_8<-matrix(0,nrow=1651,ncol=83)

for(i in 1:1651){
  pb$tick()
  list_8[i,]<-colnames(sort(similar_8[i,],decreasing=T)[1:83])
}

list_8<-as.data.frame(list_8)
rownames(list_8)<-rownames(similar_8)

#########################################################
pb<-progress_bar$new(total=1483)
## 9번 군집 유사고객 추출

list_9<-matrix(0,nrow=1483,ncol=74)

for(i in 1:1483){
  pb$tick()
  list_9[i,]<-colnames(sort(similar_9[i,],decreasing=T)[1:74])
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
pb<-progress_bar$new(total=896)
## 11번 군집 유사고객 추출

list_11<-matrix(0,nrow=896,ncol=45)

for(i in 1:896){
  pb$tick()
  list_11[i,]<-colnames(sort(similar_11[i,],decreasing=T)[1:45])
}

list_11<-as.data.frame(list_11)
rownames(list_11)<-rownames(similar_11)

#########################################################
pb<-progress_bar$new(total=1858)
## 12번 군집 유사고객 추출

list_12<-matrix(0,nrow=1858,ncol=93)

for(i in 1:1858){
  pb$tick()
  list_12[i,]<-colnames(sort(similar_12[i,],decreasing=T)[1:93])
}

list_12<-as.data.frame(list_12)
rownames(list_12)<-rownames(similar_12)

#########################################################
pb<-progress_bar$new(total=1207)
## 13번 군집 유사고객 추출

list_13<-matrix(0,nrow=1207,ncol=60)

for(i in 1:1207){
  pb$tick()
  list_13[i,]<-colnames(sort(similar_13[i,],decreasing=T)[1:60])
}

list_13<-as.data.frame(list_13)
rownames(list_13)<-rownames(similar_13)

#########################################################
pb<-progress_bar$new(total=888)
## 14번 군집 유사고객 추출

list_14<-matrix(0,nrow=888,ncol=44)

for(i in 1:888){
  pb$tick()
  list_14[i,]<-colnames(sort(similar_14[i,],decreasing=T)[1:44])
}

list_14<-as.data.frame(list_14)
rownames(list_14)<-rownames(similar_14)

#########################################################
pb<-progress_bar$new(total=488)
## 15번 군집 유사고객 추출

list_15<-matrix(0,nrow=488,ncol=24)

for(i in 1:488){
  pb$tick()
  list_15[i,]<-colnames(sort(similar_15[i,],decreasing=T)[1:24])
}

list_15<-as.data.frame(list_15)
rownames(list_15)<-rownames(similar_15)

#########################################################

