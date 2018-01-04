#충성도와 회귀분석

#충성도 final_make_loyality 파일가져오기
final_make_loyality<-read.csv("F:\\lpoint\\12.회귀\\final_make_loyality.csv", head=T)

head(final_make_loyality)
colnames(final_make_loyality)<-c("고객번호","customer_number","customer_loyality","recent_day",        "num" ,             
                                 "price_mean"    ,    "competitor_day" ,   "membership_day"  ,  "channel_num"  )

#채널이용횟수 raw_channel 가져오기
raw_channel<-read.csv("F:\\lpoint\\제3회 Big Data Competition-개인화상품추천\\채널이용.txt", head=T)
raw_channel<-raw_channel[order(raw_channel$제휴사,raw_channel$고객번호),]
A_Mobile<-raw_channel[raw_channel$제휴사=="A_MOBILE/APP",]
B_Mobile<-raw_channel[raw_channel$제휴사=="B_MOBILE/APP",]
B_Online<-raw_channel[raw_channel$제휴사=="B_ONLINEMALL",]
C_Mobile<-raw_channel[raw_channel$제휴사=="C_MOBILE/APP",]
C_Online<-raw_channel[raw_channel$제휴사=="C_ONLINEMALL",]
D_Mobile<-raw_channel[raw_channel$제휴사=="D_MOBILE/APP",]

A_Mobile<-A_Mobile[,-2]
B_Mobile<-B_Mobile[,-2]
B_Online<-B_Online[,-2]
C_Online<-C_Online[,-2]
C_Mobile<-C_Mobile[,-2]
D_Mobile<-D_Mobile[,-2]

for_dist_custmoer<-NA
for_dist_custmoer<-merge(for_dist_custmoer,A_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"A_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,B_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"B_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,B_Online,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"B_Online"
for_dist_custmoer<-merge(for_dist_custmoer,C_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"C_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,C_Online,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"C_Online"
for_dist_custmoer<-merge(for_dist_custmoer,D_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="이용횟수"]<-"D_mobile"

View(for_dist_custmoer)
head(final_make_loyality)

#고객번호와 customer_loyality만 가져온 customer_loyality 데이터 만들기 
customer_loyality<-final_make_loyality[,c(1,3)]
head(customer_loyality)


#for_dist_custmoer, customer_loyality
#채널이용횟수 & 충성도 데이터 병합
merge1<-merge(for_dist_custmoer, customer_loyality, by="고객번호", all.x=T)
head(merge1)
merge1<-merge1[,-2]

head(merge1)

#NA값에 0으로 대체
x<-merge1$A_mobile
merge1$A_mobile=ifelse(!is.na(x),x,0)

x<-merge1$B_mobile
merge1$B_mobile=ifelse(!is.na(x),x,0)

x<-merge1$B_Online
merge1$B_Online=ifelse(!is.na(x),x,0)

x<-merge1$C_mobile
merge1$C_mobile=ifelse(!is.na(x),x,0)

x<-merge1$C_Online
merge1$C_Online=ifelse(!is.na(x),x,0)

x<-merge1$D_mobile
merge1$D_mobile=ifelse(!is.na(x),x,0)

head(merge1)

merge2<-merge1[,-1]
head(merge2)

#6개 채널이용횟수와 충성도 회귀분석
lmmodel<-lm(customer_loyality~., data=merge2)
summary(lm(customer_loyality~., data=merge2))

step(lmmodel, direction ="both")

# lm(formula = customer_loyality ~ A_mobile + B_mobile + D_mobile, 
# data = merge2)
# Coefficients:
# (Intercept)     A_mobile     B_mobile     D_mobile  
# 0.3672237    0.0005965    0.0003021   -0.0118654




#경쟁사 이용횟수
competitor<-read.csv("F:\\lpoint\\제3회 Big Data Competition-개인화상품추천\\경쟁사이용.txt", head=T)
head(competitor)
competitor<-competitor[,c(1,3)]

#경쟁사 이용횟수 데이터 ee 생성
library(reshape2)
ee<-dcast(competitor, 고객번호~경쟁사, value.var="경쟁사" , length)
head(ee)

#경쟁사 이용횟수와 충성도 회귀분석
#ee, customer_loyality
merge3<-merge(ee, customer_loyality, by="고객번호", all.x=T)
head(merge3)
merge3_2<-merge3[,-1]
head(merge3_2)

lmmodel2<-lm(customer_loyality~., data=merge3_2)
summary(lmmodel2)
step(lmmodel2, direction ="both")

#Call:
#lm(formula = customer_loyality ~ A01 + B01 + B02 + D01 + D02, 
#   data = merge3_2)

#Coefficients:
#(Intercept)          A01          B01          B02          D01          D02  
#0.3407354    0.0004795   -0.0009176   -0.0006538   -0.0027179   -0.0017792  


#경쟁사 이용횟수 ee 각 열의 최대값으로 나누기
head(competitor)
head(ee)
max(ee$A01) #[1] 12
max(ee$A02) #[1] 12
max(ee$B01) #[1] 12
max(ee$B02) #[1] 12
max(ee$C01) #[1] 12
max(ee$C02) #[1] 12
max(ee$C03) #[1] 12
max(ee$D01) #[1] 8
max(ee$D02) #[1] 12

head(ee)
ee$A01_2<-ee$A01/12
ee$A02_2<-ee$A02/12
ee$B01_2<-ee$B01/12
ee$B02_2<-ee$B02/12
ee$C01_2<-ee$C01/12
ee$C02_2<-ee$C02/12
ee$C02_3<-ee$C03/12
ee$D01_2<-ee$D01/8
ee$D02_2<-ee$D02/12

#경쟁사 이용횟수를 각 최대값으로 나눈 데이터 ee2생성
head(ee)
ee2<-ee[,c(1,11,12,13,14,15,16,17,18,19)]
head(ee2)

#경쟁사이용횟수(최대값으로 나눈)와 충성도의 회귀분석
merge4<-merge(ee2, customer_loyality, by="고객번호", all.x=T)
head(merge4)
merge4_2<-merge4[,-1]
head(merge4_2)

lmmodel3<-lm(customer_loyality~., data=merge4_2)
summary(lmmodel3)
step(lmmodel3, direction ="both")

#Call:
#lm(formula = customer_loyality ~ A01_2 + B01_2 + B02_2 + D01_2 + 
#     D02_2, data = merge4_2)

#Coefficients:
#  (Intercept)        A01_2        B01_2        B02_2        D01_2        D02_2  
#0.340735     0.005753    -0.011011    -0.007846    -0.021743    -0.021351  




#호기심과 충성도 회귀분석
hh<-read.csv("F:\\호기심.csv", head=T)
head(hh)
head(final_make_loyality)
hh_loyality<-final_make_loyality[,c(1,3)]
head(hh_loyality)

hh_merge<-merge(hh, hh_loyality, by="고객번호", all.x=T, all.y=T)
head(hh_merge)
hh_merge2<-hh_merge[,-1]
head(hh_merge2)

lmmodel5<-lm(customer_loyality~., data=hh_merge2)
summary(lmmodel5)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.407556   0.001328   306.9   <2e-16 ***
#  curiosity   -0.096676   0.003128   -30.9   <2e-16 ***

#충성도와 호기심 회귀선 포함한 산점도 그리기  
# -- x,y 산점도 그리기 
plot(formula=customer_loyality ~ curiosity, data=hh_merge2)

# -- 회귀선 
abline(lmmodel5, col='red')

