getwd()
setwd("C:/Users/JYEON/Desktop/데이터")

#progress bar in R
library(progress)
progress_bar

#fread in data.table
library(data.table)

raw_demo<-read.csv("edit_demo.csv",header=T,sep=",",na.strings="",colClasses=c(rep("factor",4)))
raw_purchase<-fread("new_purchase.csv",header=T,sep=",")
raw_purchase<-raw_purchase[,-1]
raw_competitor<-read.table("04competitor.txt",colClasses=c("integer",rep("factor",2),"character"),header=T,sep=",")
raw_membership<-read.table("05membership.txt",colClasses=c("integer",rep("factor",1),"character"),header=T,sep=",")
raw_channel<-read.table("06channel.txt",colClasses=c("factor","factor","integer"),header=T,sep=",")
#불러오기 끝

##날짜처리 패키지
library(zoo)

raw_competitor$이용년월<-as.Date(as.yearmon(raw_competitor$이용년월,"%Y%m"))
raw_membership$가입년월<-as.Date(as.yearmon(raw_membership$가입년월,"%Y%m"))
raw_purchase$구매일자<-as.Date(raw_purchase$구매일자,"%Y%m%d")
raw_purchase$구매일자<-as.numeric(raw_purchase$구매일자)
#날짜 데이터 변환 년월 데이터는 "1일"로 지정

#반복문을 위한 고객번호 숫자화 & 정렬
raw_purchase$고객번호<-as.integer(raw_purchase$고객번호)
raw_purchase<-raw_purchase[order(raw_purchase$고객번호,raw_purchase$구매일자),]

#충성도 생성

#구매 데이터의 최근 구매 일자 / 구매건수 / 평균 구매 금액 / 경쟁사 이용년월 / 멤버십 가입년월 / 채널 이용 횟수

pb<-progress_bar$new(total=19383)

#최근 구매일자
recent_day<-c(rep(NA,19383))
for(i in 1:19383){
  pb$tick()
  recent_day[i]<-max(raw_purchase[raw_purchase$고객번호==i,4])
}


##

#구매건수
num<-c(rep(0,19383))
temp<-unique(raw_purchase[,c(1,3)])

for(i in 1:19383){
  pb$tick()
  num[i]<-nrow(temp[temp$고객번호==i,])
}

##

#경쟁사 이용년월 
competitor_day<-c(rep(0,19383))
raw_competitor<-raw_competitor[order(raw_competitor$고객번호,raw_competitor$이용년월),]
competitor_customer<-unique(raw_competitor$고객번호)

for(i in competitor_customer){
  pb$tick()
  competitor_day[i]<-last(raw_competitor[raw_competitor$고객번호==i,]$이용년월)
}

##

##멤버십 가입년월
membership_day<-c(rep(0,19383))
raw_membership<-raw_membership[order(raw_membership$고객번호,raw_membership$가입년월),]
membership_customer<-unique(raw_membership$고객번호)
##가입년월은 총 39종류 - 1모두 한달차이
levels(as.factor(raw_membership$가입년월))

temp<-unique(raw_membership$가입년월)
temp<-sort(temp)
temp<-cbind(temp,c(1:39))

for(i in membership_customer){
  pb$tick()
  membership_day[i]<-last(raw_membership[raw_membership$고객번호==i,]$가입년월)
}

for(i in 1:19383){
  for(j in 1:39){
    if(membership_day[i]==temp[j,1])
      membership_day[i]<-temp[j,2]
  }
}
##

##고객별 평균 구매금액 
price_mean<-c(rep(0,19383))

aggre_price<-aggregate(구매금액~고객번호,raw_purchase,mean)
price_mean<-aggre_price$구매금액

##

##채널 이용 횟수
raw_channel$고객번호<-as.integer(as.character(raw_channel$고객번호))
channel_num<-c(rep(0,19383))

for(i in 1:19383){
  channel_num[i]<-sum(raw_channel[raw_channel$고객번호==i,3])
  pb$tick()
}



#######################################################################################################

# recent_day / num / price_mean / competitor_day / membership_day / channel_num
customer_number<-c(1:19383)
customer_loyality<-c(rep(0,19383))

make_loyality<-cbind(customer_number,customer_loyality,recent_day,num,price_mean,competitor_day,membership_day,channel_num)
make_loyality<-as.data.frame(make_loyality)

##충성도 준비 완료! 요소값의 표준화 



####최근구매일자에서 16072 마이너스
sort(recent_day)
make_loyality$recent_day<-make_loyality$recent_day-16072

###최근 경쟁사 이용일자에서 16036 마이너스 -> 경쟁사이용 0 인 고객이 있기에 범위값을 보존하는 선에서 빼줌
min(make_loyality[make_loyality$competitor_day>0,]$competitor_day)
max(make_loyality$competitor_day)

make_loyality$competitor_day<-make_loyality$competitor_day-16036
make_loyality[make_loyality$competitor_day<0,]$competitor_day<-0

#최대값으로 나누어주기
make_loyality$recent_day<-make_loyality$recent_day/max(make_loyality$recent_day)
make_loyality$num<-make_loyality$num/max(make_loyality$num)
make_loyality$price_mean<-make_loyality$price_mean/max(make_loyality$price_mean)
make_loyality$competitor_day<-make_loyality$competitor_day/max(make_loyality$competitor_day)
make_loyality$membership_day<-make_loyality$membership_day/max(make_loyality$membership_day)
make_loyality$channel_num<-make_loyality$channel_num/max(make_loyality$channel_num)

##

make_loyality$customer_loyality<-0.15*make_loyality$recent_day+0.3*make_loyality$num+0.35*make_loyality$price_mean+0.05*(1-make_loyality$competitor_day)+0.05*(1-make_loyality$membership_day)+0.1*make_loyality$channel_num

getwd()
write.csv(make_loyality,"final_make_loyality.csv")
