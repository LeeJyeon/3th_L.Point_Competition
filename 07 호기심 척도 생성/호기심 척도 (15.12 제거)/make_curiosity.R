#호기심 척도 만들기


getwd()
setwd("C:/Users/JYEON/Desktop/데이터")

demo<-read.csv("edit_demo.csv",header=T,sep=",")

#고객별 채널 이용 횟수
channel<-read.table("06channel.txt",header=T,sep=",")
channel_num<-c(rep(0,19383))
for(i in 1:19383){
  channel_num[i]<-sum(channel[channel$고객번호==i,3])
  
}


#MCR 데이터 불러오기

MCR<-read.csv("MCR.csv",header=T,sep=",")
MCR<-MCR[c(1,5,11),]
colnames(MCR)<-c("index","남자10대","남자20대","남자30대","남자40대","남자50대이상","여자10대","여자20대","여자30대","여자40대","여자50대이상")
row.names(MCR)<-MCR[,1]
MCR<-MCR[,-1]

##자신이 구매했던 상품 품목 갯수##
item_variable<-c(rep(0,19383))

library(data.table)
purchase<-fread("new_purchase.csv",header=T,sep=",")

##2015년 12월 구매데이터 삭제
purchase$구매일자<-as.Date(purchase$구매일자,"%Y%m%d")
purchase<-purchase[purchase$구매일자<16770,]

purchase<-purchase[,3:4]
purchase<-unique(purchase)
purchase$고객번호<-as.numeric(purchase$고객번호)

for(i in 1:19383){
  item_variable[i]<-nrow(purchase[purchase$고객번호==i,])
  pb$tick()
}

##########################################################################################################


#MCR 데모 데이터와 매칭

a<-NA
demo<-cbind(demo,a,a,a)
colnames(demo)[5:7]<-row.names(MCR)

#남자#
#10대 남자
for(i in 1:19383){
  if(demo[i,2]=="M" & demo[i,3]==levels(demo$연령대)[1]){
    demo[i,5]<-MCR[1,1]
    demo[i,6]<-MCR[2,1]
    demo[i,7]<-MCR[3,1]
  }
}

#20대 남자
for(i in 1:19383){
  if(demo[i,2]=="M" & (demo[i,3]==levels(demo$연령대)[2]||demo[i,3]==levels(demo$연령대)[3])){
    demo[i,5]<-MCR[1,2]
    demo[i,6]<-MCR[2,2]
    demo[i,7]<-MCR[3,2]
  }
}

#30대 남자
for(i in 1:19383){
  if(demo[i,2]=="M" & (demo[i,3]==levels(demo$연령대)[4]||demo[i,3]==levels(demo$연령대)[5])){
    demo[i,5]<-MCR[1,3]
    demo[i,6]<-MCR[2,3]
    demo[i,7]<-MCR[3,3]
  }
}

##40대 남자
for(i in 1:19383){
  if(demo[i,2]=="M" & (demo[i,3]==levels(demo$연령대)[6]||demo[i,3]==levels(demo$연령대)[7])){
    demo[i,5]<-MCR[1,4]
    demo[i,6]<-MCR[2,4]
    demo[i,7]<-MCR[3,4]
  }
}

#50대 이상 남자
for(i in 1:19383){
  if(demo[i,2]=="M"){
    if(is.na(demo[i,5]))
      demo[i,5]<-MCR[1,5]
    if(is.na(demo[i,6]))
      demo[i,6]<-MCR[2,5]
    if(is.na(demo[i,7]))
      demo[i,7]<-MCR[3,5]
  }
}


#여자#
#10대 여자
for(i in 1:19383){
  if(demo[i,2]=="F" & demo[i,3]==levels(demo$연령대)[1]){
    demo[i,5]<-MCR[1,6]
    demo[i,6]<-MCR[2,6]
    demo[i,7]<-MCR[3,6]
  }
}

#20대 여자
for(i in 1:19383){
  if(demo[i,2]=="F" & (demo[i,3]==levels(demo$연령대)[2]||demo[i,3]==levels(demo$연령대)[3])){
    demo[i,5]<-MCR[1,7]
    demo[i,6]<-MCR[2,7]
    demo[i,7]<-MCR[3,7]
  }
}

#30대 여자
for(i in 1:19383){
  if(demo[i,2]=="F" & (demo[i,3]==levels(demo$연령대)[4]||demo[i,3]==levels(demo$연령대)[5])){
    demo[i,5]<-MCR[1,8]
    demo[i,6]<-MCR[2,8]
    demo[i,7]<-MCR[3,8]
  }
}

##40대 여자
for(i in 1:19383){
  if(demo[i,2]=="F" & (demo[i,3]==levels(demo$연령대)[6]||demo[i,3]==levels(demo$연령대)[7])){
    demo[i,5]<-MCR[1,9]
    demo[i,6]<-MCR[2,9]
    demo[i,7]<-MCR[3,9]
  }
}

#50대 이상 여자
for(i in 1:19383){
  if(demo[i,2]=="F"){
    if(is.na(demo[i,5]))
      demo[i,5]<-MCR[1,10]
    if(is.na(demo[i,6]))
      demo[i,6]<-MCR[2,10]
    if(is.na(demo[i,7]))
      demo[i,7]<-MCR[3,10]
  }
}

##채널 이용횟수 합치기 

demo<-cbind(demo,channel_num)

##구매 품목 갯수 합치기

demo<-cbind(demo,item_variable)


#---------------------------------------------------#
#최근 6개월간 기존 구매빈도로 아이템을 추천했을때 맞았던 고객 데이터#
recent_6<-read.csv("최근 6개월 빈도추천 결과.csv",header=T,sep=",")
recent_6<-recent_6[order(recent_6$고객번호),]
recent_6<-recent_6[,c(11,13)]
colnames(recent_6)<-c("6개월동안 못맞춘 횟수","6개월 추천동안 못맞춘 갯수")

demo<-cbind(demo,recent_6)

head(demo)

## 호기심 지수 생성 ##
make_curiosity<-demo
make_curiosity<-make_curiosity[,c(1,5:11)]
curiosity<-0
make_curiosity<-cbind(make_curiosity,curiosity)

#-변수들의 표준화-#
make_curiosity[,2]<-make_curiosity[,2]/max(make_curiosity[,2])
make_curiosity[,3]<-make_curiosity[,3]/max(make_curiosity[,3])
make_curiosity[,4]<-make_curiosity[,4]/max(make_curiosity[,4])
make_curiosity[,5]<-make_curiosity[,5]/max(make_curiosity[,5])
make_curiosity[,6]<-make_curiosity[,6]/max(make_curiosity[,6])
make_curiosity[,7]<-make_curiosity[,7]/max(make_curiosity[,7])
make_curiosity[,8]<-make_curiosity[,8]/max(make_curiosity[,8])
###

value<-c(0.1,0.1,0.1,0.1,0.2,0.2,0.2)

for(i in 1:19383){
  make_curiosity[i,9]<-sum(make_curiosity[i,2:8]*value)
}

###
write.csv(make_curiosity,"curiosity.csv")

