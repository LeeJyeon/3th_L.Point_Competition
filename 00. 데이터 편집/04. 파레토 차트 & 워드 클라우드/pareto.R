setwd("C:/Users/JYEON/Desktop/데이터")
library(progress)

##자신이 구매했던 상품 품목 갯수##
item_variable<-c(rep(0,19383))

library(data.table)
raw_purchase<-fread("new_purchase.csv",header=T,sep=",")
raw_purchase$고객번호<-as.numeric(raw_purchase$고객번호)



purchase<-raw_purchase[,3:4]
purchase<-unique(purchase)


for(i in 1:19383){
  item_variable[i]<-nrow(purchase[purchase$고객번호==i,])
  pb$tick()
}

#상위 20% 갯수#
top_20_number<-round(item_variable*0.2)


#고객별 총 구매건수#
temp<-as.data.frame(table(raw_purchase$고객번호))
total_purchase_num<-temp$Freq

###############################################################################
purchase_table<-as.data.frame(table(raw_purchase[,3:4]))
purchase_matrix<-matrix(purchase_table$Freq,nrow=19383,byrow=T)


#상위 20% 구매건수 구하기#
top_20_count<-c(rep(0,19383))

pb<-progress_bar$new(total=19383)
for(i in 1:19383){
  top_20_count[i]<-sum(sort(purchase_matrix[i,],decreasing=T)[1:top_20_number[i]])
  pb$tick()
}

pareto<-top_20_count/total_purchase_num
boxplot(pareto)

###########################################################3
#19383 모두 합쳐서 파레토 차트 작성#

plot(sort(purchase_matrix[10000,],decreasing = T))

average_purchase_matrix<-matrix(0,nrow=19383,ncol=4273)

for(i in 1:19383){
  average_purchase_matrix[i,]<-sort(purchase_matrix[i,],decreasing = T)
}

pareto_average<-apply(average_purchase_matrix,2,mean)

plot(pareto_average[pareto_average>2])
