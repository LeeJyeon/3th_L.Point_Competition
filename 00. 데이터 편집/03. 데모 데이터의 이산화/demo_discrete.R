test_demo<-read.csv("C:/Users/JYEON/Desktop/데이터/edit_demo.csv",header=T,sep=",",colClass=c(rep("factor",4)))
head(test_demo,100) 
str(test_demo)

#데모데이터의 이산화
discrete_demo<-test_demo

#성별처리
sex<-0
discrete_demo<-cbind(discrete_demo,sex)
discrete_demo$sex[discrete_demo$성별=="M"]<-1

#나이처리

age1<-0
age2<-0
age3<-0
age4<-0
age5<-0
age6<-0
age7<-0
age8<-0
age9<-0

discrete_demo<-cbind(discrete_demo,age1,age2,age3,age4,age5,age6,age7,age8,age9)

list_age<-levels(discrete_demo$연령대)

for(i in 1:9){
  for(j in 1:19383){
    if(discrete_demo[j,3]==list_age[i])
      discrete_demo[j,i+5]<-1
  }
}


#거주지처리
list_place<-levels(discrete_demo$거주지역)
place<-as.data.frame(matrix(0,nrow=19383,ncol=40))

discrete_demo<-cbind(discrete_demo,place)

for(i in 1:40){
  for(j in 1:19383){
    if(discrete_demo[j,4]==list_place[i])
      discrete_demo[j,i+14]<-1
  }
}

discrete_demo<-discrete_demo[,c(-2,-3,-4)]

write.csv(discrete_demo,"C:/Users/JYEON/Desktop/데이터//discrete_demo.csv")
