#test함수 코딩

library(data.table)
library(progress)

#구매데이터 불러오기
tr<-fread("C:/Users/JYEON/Desktop/L_Point/Big Data Competition/02purchase.txt",header=T,sep=",")
tr$구매일자<-as.Date(as.character(tr$구매일자),"%Y%m%d")
str(tr)


#12월 구매데이터 가져오기
tr_12<-tr[tr$구매일자>=16770,]
tr_12<-tr_12[order(tr_12$고객번호),]
head(tr_12,10)

#12월에 구매한 고객번호 가져오기
length(unique(tr$고객번호)) #[1] 19383
length(unique(tr_12$고객번호)) #[1] 19119

#12월에 안산 고객수
19383-19119  #264명이 12월에 구매하지 않음

tail(tr_12,10)                                                  

#12월에 구매한 고객ID tr_id_unique 변수명으로 저장
tr_id_unique<-unique(tr_12$고객번호)

#12월 추천 아이템 recommend_item로 가져오기 
recommend_item<-read.csv("C:/Users/JYEON/Desktop/for test/item_12.csv")

head(recommend_item,10)
tail(recommend_item,10)
colnames(recommend_item)<-c("고객번호","아이템1","아이템2","아이템3")

recommend_item$아이템1<-as.character(recommend_item$아이템1)
recommend_item$아이템2<-as.character(recommend_item$아이템2)
recommend_item$아이템3<-as.character(recommend_item$아이템3)

#12월 추천아이템과 실제 12월 구매한 데이터 매칭하기
a<-NA
a<-data.frame()

pb<-progress_bar$new(total=19119)
for(i in 1:19119){
  
  buy12<-tr_12[tr_12$고객번호==tr_id_unique[i],]
  buy12<-buy12[order(buy12$소분류코드),]
  recommend<-recommend_item[recommend_item$고객번호==tr_id_unique[i],]  #고객번호 1번이 추천받은 물품
  a[i,1]<-tr_id_unique[i]
  a[i,2]<-buy12$소분류코드[match(recommend[1,2],buy12$소분류코드)] 
  a[i,3]<-buy12$소분류코드[match(recommend[1,3],buy12$소분류코드)] 
  a[i,4]<-buy12$소분류코드[match(recommend[1,4],buy12$소분류코드)] 
  pb$tick()
}
head(a)


head(a,30)
tail(a,30)
colnames(a)<-c("고객번호","아이템1","아이템2","아이템3")
write.csv(a,"C:/Users/JYEON/Desktop/결과/12_result_item_test.csv")
