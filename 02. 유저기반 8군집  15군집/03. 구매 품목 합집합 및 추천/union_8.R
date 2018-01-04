
#구매내역 합집합 행렬
union_purchase<-matrix(0,nrow=19383,ncol=4270)


library(progress)


####
####
#list1 고객

pb<-progress_bar$new(total=2320)
for(i in 1:2320){
  for(j in 1:116){
    union_purchase[as.numeric(rownames(list_1)[i]),]<-union_purchase[as.numeric(rownames(list_1)[i]),]+purchase_vector[as.numeric(as.character(list_1[i,j])),]
  }
  pb$tick()
}

#list2 고객
pb<-progress_bar$new(total=3877)
for(i in 1:3877){
  for(j in 1:194){
    union_purchase[as.numeric(rownames(list_2)[i]),]<-union_purchase[as.numeric(rownames(list_2)[i]),]+purchase_vector[as.numeric(as.character(list_2[i,j])),]
  }
  pb$tick()
}

#list3 고객
pb<-progress_bar$new(total=2529)
for(i in 1:2529){
  for(j in 1:126){
    union_purchase[as.numeric(rownames(list_3)[i]),]<-union_purchase[as.numeric(rownames(list_3)[i]),]+purchase_vector[as.numeric(as.character(list_3[i,j])),]
  }
  pb$tick()
}

#list4 고객
pb<-progress_bar$new(total=1568)
for(i in 1:1568){
  for(j in 1:78){
    union_purchase[as.numeric(rownames(list_4)[i]),]<-union_purchase[as.numeric(rownames(list_4)[i]),]+purchase_vector[as.numeric(as.character(list_4[i,j])),]
  }
  pb$tick()
}

#list5 고객
pb<-progress_bar$new(total=3386)
for(i in 1:3386){
  for(j in 1:169){
    union_purchase[as.numeric(rownames(list_5)[i]),]<-union_purchase[as.numeric(rownames(list_5)[i]),]+purchase_vector[as.numeric(as.character(list_5[i,j])),]
  }
  pb$tick()
}

#list6 고객
pb<-progress_bar$new(total=1899)
for(i in 1:1899){
  for(j in 1:95){
    union_purchase[as.numeric(rownames(list_6)[i]),]<-union_purchase[as.numeric(rownames(list_6)[i]),]+purchase_vector[as.numeric(as.character(list_6[i,j])),]
  }
  pb$tick()
}

#list7 고객
pb<-progress_bar$new(total=2452)
for(i in 1:2452){
  for(j in 1:123){
    union_purchase[as.numeric(rownames(list_7)[i]),]<-union_purchase[as.numeric(rownames(list_7)[i]),]+purchase_vector[as.numeric(as.character(list_7[i,j])),]
  }
  pb$tick()
}

#list8 고객
pb<-progress_bar$new(total=1352)
for(i in 1:1352){
  for(j in 1:68){
    union_purchase[as.numeric(rownames(list_8)[i]),]<-union_purchase[as.numeric(rownames(list_8)[i]),]+purchase_vector[as.numeric(as.character(list_8[i,j])),]
  }
  pb$tick()
}



## 본인이 구매한 내역 제거 - 최종 recommand_matrix

recommand_matrix<-union_purchase

pb<-progress_bar$new(total=19383)
for(i in 1:19383){
  for(j in 1:4270){
    if(purchase_vector[i,j]>0)
      recommand_matrix[i,j]<-0
  }
  pb$tick()
}

##



#상위 다섯개 값 뽑는 함수
fifth<-function(x){
  a<-max(x)
  b<-max(x[x!=a])
  c<-max(x[x!=a&x!=b])
  d<-max(x[x!=a&x!=b&x!=c])
  e<-max(x[x!=a&x!=b&x!=c&x!=d])
  return(c(a,b,c,d,e))
}


####최종추천 목록
#아이템인덱스
temp_table<-as.data.frame(table(raw_purchase))
levels(temp_table$소분류코드)
##

recommand_item<-as.list(NA)


for(num in 1:19383){
  temp<-NA
  for(i in 1:5){
    temp<-c(temp,levels(temp_table$소분류코드)[which(recommand_matrix[num,]==fifth(recommand_matrix[num,])[i])])
  }
  temp<-temp[-1]
  recommand_item[[num]]<-temp
  pb$tick()
}

## 유사 고객들이 많이 샀던 (상위5) 품목들 

##상위 다섯개 자르기
recommand_item_top5<-recommand_item
for(i in 1:19383){
  if(length(recommand_item[[i]])>5)
    recommand_item_top5[[i]]<-recommand_item[[i]][1:5]
  pb$tick()
}

##
for_excel_recommand_item<-matrix(unlist(recommand_item_top5),nrow=19383,byrow=T)
write.csv(for_excel_recommand_item,"test_8cluster.csv")
