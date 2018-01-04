install.packages("prettyR")
library(prettyR)

##구매내역의 점포를 이용하여 거주지 결측치 할당

raw_demo<-read.table("01demo.txt",header=T,sep=",",na.strings="",colClasses=c("numeric",rep("factor",3)))
raw_purchase<-fread("02purchase.txt",colClasses=c(rep("factor",7),"character",rep("integer",2)),header=T,sep=",")

resident_purchase<-raw_purchase[,6:7]
resident_purchase$고객번호<-as.numeric(resident_purchase$고객번호)
resident_purchase<-resident_purchase[order(resident_purchase$고객번호),]

#####
store_code<-levels(resident_purchase$점포코드)
#####
#고객별 최빈 점포코드 (선호도)

frequnct_table<-as.data.frame(table(resident_purchase))
frequnct_table<-merge(frequnct_table,raw_demo[,c(1,4)],all.x=T)
frequnct_table<-frequnct_table[order(frequnct_table$점포코드,-frequnct_table$Freq),]
head(frequnct_table,100)


##결측치 고객 추출
sum(is.na(raw_demo))
customer_NA<-NA
for(i in 1:19383){
  if(is.na(raw_demo[i,4]))
    customer_NA<-c(customer_NA,raw_demo[i,1])
  pb$tick()
}
customer_NA<-customer_NA[-1]
customer_NA
########################

#결측치 고객의 최빈 점포코드
most_store_of_customer_NA<-NA
for(i in 1:178){
  most_store_of_customer_NA<-c(most_store_of_customer_NA,store_code[which.max(frequnct_table[frequnct_table$고객번호==customer_NA[i],]$Freq)])
}
most_store_of_customer_NA<-most_store_of_customer_NA[-1]
most_store_of_customer_NA

########################
#최빈점포코드가 같은 고객 list
house_list<-as.list(NA)
for(i in 1:178){
  house_list[[i]]<-frequnct_table[frequnct_table$점포코드==most_store_of_customer_NA[i],]
}
########################
#350번이하 이용 고객은 거주지와 상관없다고 판단
house_code_of_NA<-c(rep(NA,178))
for(i in 1:178){
  house_code_of_NA[i]<-Mode(house_list[[i]][house_list[[i]]$Freq>350,]$거주지역)
}
house_code_of_NA

##30번쨰는 최빈값이 겹쳐 못찾기에 직접 "500" 거주지역이라고 할당
house_list[[30]]
house_code_of_NA[30]<-"500"
house_code_of_NA
##########################################################################
# 결측치 178개 채워넣기
num<-1
edit_demo<-raw_demo

for(i in 1:19383){
  if(is.na(raw_demo[i,4])){
    edit_demo[i,4]<-house_code_of_NA[num]
    num<-num+1
  }
}
##완료

write.csv(edit_demo,"edit_demo.csv")
