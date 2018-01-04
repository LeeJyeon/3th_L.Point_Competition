getwd()
setwd("C:/Users/JYEON/Desktop/L_Point/Big Data Competition")

#progress bar in R
install.packages("progress")
library(progress)
progress_bar

#필터 패키지
install.packages("dplyr")
library(dplyr)

#fread in data.table
install.packages("data.table")
library(data.table)

raw_purchase<-fread("02purchase.txt",colClasses=c(rep("factor",7),"character",rep("integer",2)),header=T,sep=",")
raw_classification<-read.table("03classification.txt",colClasses=c(rep("factor",6)),header=T,sep=",")

#필요한 데이터 고르기
new_purchase<-raw_purchase[,c(2,5,6,8,9,10)]
new_purchase<-new_purchase[order(new_purchase$고객번호,new_purchase$구매일자,new_purchase$구매시간),]

##item selling freq & customer buying freq

#1번 판매한 품목을 제거
code_freq<-as.data.frame(table(new_purchase$소분류코드))

summary(code_freq$Freq)
boxplot(code_freq$Freq)

nrow(code_freq[code_freq$Freq==1,])
#113개의 품목 원천제거

temp<-code_freq[code_freq$Freq==1,]$Var1
temp

##temp에 1회 판매된 113개 품목 저장

new_classification<-raw_classification


for(i in 1:length(temp)){
  new_classification<-filter(new_classification,new_classification$소분류코드!=temp[i])
}

#구매데이터에서도 제거

a<-c(rep(0,113))
##a에 113개 아이템의 index 저장

for(i in 1:113){
a[i]<-which(new_purchase$소분류코드==temp[i])
}

new_purchase<-new_purchase[-c(a),]
str(new_purchase)
new_purchase<-droplevels(new_purchase)
str(new_purchase)

##
write.csv(new_purchase,"new_purchase.csv")
write.csv(new_classification,"new_classification.csv")
