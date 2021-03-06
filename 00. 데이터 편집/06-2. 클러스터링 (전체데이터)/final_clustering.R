getwd()
setwd("C:/Users/JYEON/Desktop/������")

#progress bar in R
library(progress)
progress_bar

#fread in data.table
library(data.table)

raw_competitor<-read.table("04competitor.txt",colClasses=c(rep("factor",3),"character"),header=T,sep=",")
raw_membership<-read.table("05membership.txt",colClasses=c(rep("factor",2),"character"),header=T,sep=",")
raw_channel<-read.table("06channel.txt",colClasses=c("factor","factor","integer"),header=T,sep=",")

test_demo<-read.csv("edit_demo.csv",header=T)

#�ҷ����� ��


#����� ó��
new_competitor<-raw_competitor[,1:2]
new_competitor<-new_competitor[order(new_competitor$���޻�,new_competitor$������ȣ),]
temp_table<-as.data.frame(table(new_competitor))
temp_table
A_competitor<-temp_table[temp_table$���޻�=="A",]
B_competitor<-temp_table[temp_table$���޻�=="B",]
C_competitor<-temp_table[temp_table$���޻�=="C",]
D_competitor<-temp_table[temp_table$���޻�=="D",]

A_competitor<-A_competitor[,-2]
B_competitor<-B_competitor[,-2]
C_competitor<-C_competitor[,-2]
D_competitor<-D_competitor[,-2]

total_competitor<-cbind(A_competitor,B_competitor$Freq,C_competitor$Freq,D_competitor$Freq)
colnames(total_competitor)<-c("������ȣ","�����A�̿�","�����B�̿�","�����C�̿�","�����D�̿�")
rm(A_competitor,B_competitor,C_competitor,D_competitor)

#ä���̿� ó��
str(raw_channel)

raw_channel<-raw_channel[order(raw_channel$���޻�,raw_channel$������ȣ),]
A_Mobile<-raw_channel[raw_channel$���޻�=="A_MOBILE/APP",]
B_Mobile<-raw_channel[raw_channel$���޻�=="B_MOBILE/APP",]
B_Online<-raw_channel[raw_channel$���޻�=="B_ONLINEMALL",]
C_Mobile<-raw_channel[raw_channel$���޻�=="C_MOBILE/APP",]
C_Online<-raw_channel[raw_channel$���޻�=="C_ONLINEMALL",]
D_Mobile<-raw_channel[raw_channel$���޻�=="D_MOBILE/APP",]

A_Mobile<-A_Mobile[,-2]
B_Mobile<-B_Mobile[,-2]
B_Online<-B_Online[,-2]
C_Online<-C_Online[,-2]
C_Mobile<-C_Mobile[,-2]
D_Mobile<-D_Mobile[,-2]


##

#�����ó��
new_membership<-raw_membership[,-3]
temp_table<-as.data.frame(table(new_membership))

Dadung<-temp_table[temp_table$����ʸ�=="�ٵ���",]
Theyoung<-temp_table[temp_table$����ʸ�=="����",]
Lobs<-temp_table[temp_table$����ʸ�=="�ӽ�",]
himart<-temp_table[temp_table$����ʸ�=="���̸�Ʈ",]

Dadung<-Dadung[,-2]
Theyoung<-Theyoung[,-2]
Lobs<-Lobs[,-2]
himart<-himart[,-2]

total_membership<-cbind(Dadung,Theyoung$Freq,Lobs$Freq,himart$Freq)
colnames(total_membership)<-c("������ȣ","�ٵ���","����","�ӽ�","���̸�Ʈ")

##

loyality<-read.csv("C:/Users/JYEON/Desktop/������/final_make_loyality.csv",header=T)
loyality<-loyality[,2:3]

#�ڷ� ��ġ��#
memory.limit(40000)

temp_customer_num<-read.table("C:/Users/JYEON/Desktop/L_Point/Big Data Competition/01demo.txt",sep=",",header=T,colClasses=c(rep("factor",4)))
for_dist_custmoer<-cbind(temp_customer_num$������ȣ,test_demo[,-1])
colnames(for_dist_custmoer)<-c("������ȣ","����","���ɴ�","��������")

for_dist_custmoer<-cbind(for_dist_custmoer,loyality$customer_loyality)
for_dist_custmoer<-merge(for_dist_custmoer,total_competitor,all.x=T)
for_dist_custmoer<-merge(for_dist_custmoer,total_membership,all.x=T)

for_dist_custmoer<-merge(for_dist_custmoer,A_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"A_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,B_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"B_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,B_Online,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"B_Online"
for_dist_custmoer<-merge(for_dist_custmoer,C_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"C_mobile"
for_dist_custmoer<-merge(for_dist_custmoer,C_Online,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"C_Online"
for_dist_custmoer<-merge(for_dist_custmoer,D_Mobile,all.x=T)
names(for_dist_custmoer)[names(for_dist_custmoer)=="�̿�Ƚ��"]<-"D_mobile"


sum(is.na(for_dist_custmoer))
str(for_dist_custmoer)

#����ġ 0 ó��
for_dist_custmoer[is.na(for_dist_custmoer)]<-0
sum(is.na(for_dist_custmoer))

#�̻�ȭ ������ ��ġ��
discrete_demo<-read.csv("C:/Users/JYEON/Desktop/������/discrete_demo.csv")

for_dist_custmoer<-cbind(for_dist_custmoer,discrete_demo)
for_dist_custmoer<-for_dist_custmoer[,c(-2,-3,-4)]
for_dist_custmoer<-for_dist_custmoer[,-17]
for_dist_custmoer<-for_dist_custmoer[,-17]

str(for_dist_custmoer)

#������ ���� ǥ��ȭ
for(i in 3:16){
  for_dist_custmoer[,i]<-for_dist_custmoer[,i]/max(for_dist_custmoer[,i])
}

#Ŭ�����͸� ����!

##kmeans #���� k �� ���ϱ�
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

plot(result_kmean15$cluster)

##

cluster_index15<-result_kmean15$cluster

write.csv(cluster_index15,"C:/Users/JYEON/Desktop/������/final_15_cluster.csv")


library(cluster)
clusplot(for_dist_custmoer[,-1],result_kmean15$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

