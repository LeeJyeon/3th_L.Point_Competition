getwd()
setwd("C:/Users/JYEON/Desktop/데이터")

#progress bar in R
library(progress)
progress_bar

#fread in data.table
library(data.table)

raw_competitor<-read.table("04competitor.txt",colClasses=c(rep("factor",3),"character"),header=T,sep=",")
raw_membership<-read.table("05membership.txt",colClasses=c(rep("factor",2),"character"),header=T,sep=",")
raw_channel<-read.table("06channel.txt",colClasses=c("factor","factor","integer"),header=T,sep=",")
#불러오기 끝


test_demo<-read.csv("edit_demo.csv",header=T,sep=",",colClass=c(rep("factor",4)))
test_demo<-test_demo[order(test_demo$고객번호),]
#결측치 처리한 데모 불러오기

str(test_demo)
str(raw_competitor)
str(raw_membership)
str(raw_channel)

#경쟁사 처리
new_competitor<-raw_competitor[,1:2]
new_competitor<-new_competitor[order(new_competitor$제휴사,new_competitor$고객번호),]
temp_table<-as.data.frame(table(new_competitor))
temp_table
A_competitor<-temp_table[temp_table$제휴사=="A",]
B_competitor<-temp_table[temp_table$제휴사=="B",]
C_competitor<-temp_table[temp_table$제휴사=="C",]
D_competitor<-temp_table[temp_table$제휴사=="D",]

A_competitor<-A_competitor[,-2]
B_competitor<-B_competitor[,-2]
C_competitor<-C_competitor[,-2]
D_competitor<-D_competitor[,-2]

total_competitor<-cbind(A_competitor,B_competitor$Freq,C_competitor$Freq,D_competitor$Freq)
colnames(total_competitor)<-c("고객번호","경쟁사A이용","경쟁사B이용","경쟁사C이용","경쟁사D이용")
rm(A_competitor,B_competitor,C_competitor,D_competitor)

#채널이용 처리
str(raw_channel)

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


##

#멤버십처리
new_membership<-raw_membership[,-3]
temp_table<-as.data.frame(table(new_membership))

Dadung<-temp_table[temp_table$멤버십명=="다둥이",]
Theyoung<-temp_table[temp_table$멤버십명=="더영",]
Lobs<-temp_table[temp_table$멤버십명=="롭스",]
himart<-temp_table[temp_table$멤버십명=="하이마트",]

Dadung<-Dadung[,-2]
Theyoung<-Theyoung[,-2]
Lobs<-Lobs[,-2]
himart<-himart[,-2]

total_membership<-cbind(Dadung,Theyoung$Freq,Lobs$Freq,himart$Freq)
colnames(total_membership)<-c("고객번호","다둥이","더영","롭스","하이마트")

##

head(test_demo)
getwd()

loyality<-read.csv("C:/Users/JYEON/Desktop/데이터/make_loyality.csv",header=T)
loyality<-loyality[,2:3]

#자료 합치기#
memory.limit(40000)
library(progress)
progress_bar

temp_customer_num<-read.table("C:/Users/JYEON/Desktop/L_Point/Big Data Competition/01demo.txt",sep=",",header=T,colClasses=c(rep("factor",4)))
for_dist_custmoer<-cbind(temp_customer_num$고객번호,test_demo[,-1])
colnames(for_dist_custmoer)<-c("고객번호","성별","연령대","거주지역")

for_dist_custmoer<-cbind(for_dist_custmoer,loyality$customer_loyality)
for_dist_custmoer<-merge(for_dist_custmoer,total_competitor,all.x=T)
for_dist_custmoer<-merge(for_dist_custmoer,total_membership,all.x=T)

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


sum(is.na(for_dist_custmoer))
str(for_dist_custmoer)

#결측치 0 처리
for_dist_custmoer[is.na(for_dist_custmoer)]<-0
sum(is.na(for_dist_custmoer))

#이산화 데이터 합치기
discrete_demo<-read.csv("C:/Users/JYEON/Desktop/데이터/discrete_demo.csv")

for_dist_custmoer<-cbind(for_dist_custmoer,discrete_demo)
for_dist_custmoer<-for_dist_custmoer[,c(-2,-3,-4)]
for_dist_custmoer<-for_dist_custmoer[,-17]
for_dist_custmoer<-for_dist_custmoer[,-17]

str(for_dist_custmoer)

#나머지 변수 표준화
for(i in 3:16){
  for_dist_custmoer[,i]<-for_dist_custmoer[,i]/max(for_dist_custmoer[,i])
}

#클러스터링 시작!

##kmeans #최적 k 값 구하기
wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(for_dist_custmoer[,-1])


#kmeans
result_kmean15<-kmeans(for_dist_custmoer[,-1],15)
result_kmean8<-kmeans(for_dist_custmoer[,-1],8)

result_kmean15
result_kmean8

plot(result_kmean15$cluster)
plot(result_kmean8$cluster)

##

cluster_index15<-result_kmean15$cluster
cluster_index15

cluster_index8<-result_kmean8$cluster
cluster_index8

write.csv(cluster_index15,"C:/Users/JYEON/Desktop/데이터/15_cluster.csv")
write.csv(cluster_index8,"C:/Users/JYEON/Desktop/데이터/8_cluster.csv")


library(cluster)
clusplot(for_dist_custmoer[,-1],result_kmean15$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

