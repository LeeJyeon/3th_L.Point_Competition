getwd()
setwd("C:/Users/JYEON/Desktop/데이터")

#progress bar in R
library(progress)
progress_bar

#fread in data.table
library(data.table)

raw_purchase<-fread("new_purchase.csv",header=T,sep=",")
#불러오기 끝


##2015년 12월 구매데이터 삭제
raw_purchase$구매일자<-as.Date(raw_purchase$구매일자,"%Y%m%d")
raw_purchase<-raw_purchase[raw_purchase$구매일자<16770,]
#고객번호와 소분류코드만 추출
raw_purchase<-raw_purchase[,3:4]


# 고객별 제품 구매 건수 테이블 만들기
memory.limit(40000)

temp_table<-as.data.frame(table(raw_purchase))

#18761 19209 고객은 2015년 12월 전까지 구매한 내역이 없음
#4270의 상품 구매

#고객별 구매 벡터만들기
freq_value<-temp_table$Freq
purchase_vector<-matrix(freq_value,nrow=19381,byrow=TRUE)
#엑셀로 수정
write.csv(purchase_vector,"purchase_vector_until_11.csv")
purchase_vector<-read.csv("purchase_vector_until_11.csv",header=F)
purchase_vector<-as.matrix(purchase_vector)
###########################################################