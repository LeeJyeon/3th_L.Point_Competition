
#구매내역 합집합 행렬
union_purchase<-matrix(0,nrow=19383,ncol=4270)


library(progress)


####
####
#list1 고객

pb<-progress_bar$new(total=1380)
for(i in 1:1380){
  for(j in 1:69){
    union_purchase[as.numeric(rownames(list_1)[i]),]<-union_purchase[as.numeric(rownames(list_1)[i]),]+purchase_vector[as.numeric(as.character(list_1[i,j])),]
  }
  pb$tick()
}

#list2 고객
pb<-progress_bar$new(total=1657)
for(i in 1:1657){
  for(j in 1:83){
    union_purchase[as.numeric(rownames(list_2)[i]),]<-union_purchase[as.numeric(rownames(list_2)[i]),]+purchase_vector[as.numeric(as.character(list_2[i,j])),]
  }
  pb$tick()
}

#list3 고객
pb<-progress_bar$new(total=655)
for(i in 1:655){
  for(j in 1:33){
    union_purchase[as.numeric(rownames(list_3)[i]),]<-union_purchase[as.numeric(rownames(list_3)[i]),]+purchase_vector[as.numeric(as.character(list_3[i,j])),]
  }
  pb$tick()
}

#list4 고객
pb<-progress_bar$new(total=929)
for(i in 1:929){
  for(j in 1:46){
    union_purchase[as.numeric(rownames(list_4)[i]),]<-union_purchase[as.numeric(rownames(list_4)[i]),]+purchase_vector[as.numeric(as.character(list_4[i,j])),]
  }
  pb$tick()
}

#list5 고객
pb<-progress_bar$new(total=2628)
for(i in 1:2628){
  for(j in 1:131){
    union_purchase[as.numeric(rownames(list_5)[i]),]<-union_purchase[as.numeric(rownames(list_5)[i]),]+purchase_vector[as.numeric(as.character(list_5[i,j])),]
  }
  pb$tick()
}

#list6 고객
pb<-progress_bar$new(total=370)
for(i in 1:370){
  for(j in 1:18){
    union_purchase[as.numeric(rownames(list_6)[i]),]<-union_purchase[as.numeric(rownames(list_6)[i]),]+purchase_vector[as.numeric(as.character(list_6[i,j])),]
  }
  pb$tick()
}

#list7 고객
pb<-progress_bar$new(total=2324)
for(i in 1:2324){
  for(j in 1:116){
    union_purchase[as.numeric(rownames(list_7)[i]),]<-union_purchase[as.numeric(rownames(list_7)[i]),]+purchase_vector[as.numeric(as.character(list_7[i,j])),]
  }
  pb$tick()
}

#list8 고객
pb<-progress_bar$new(total=1657)
for(i in 1:1657){
  for(j in 1:83){
    union_purchase[as.numeric(rownames(list_8)[i]),]<-union_purchase[as.numeric(rownames(list_8)[i]),]+purchase_vector[as.numeric(as.character(list_8[i,j])),]
  }
  pb$tick()
}

#list9 고객
pb<-progress_bar$new(total=1365)
for(i in 1:1365){
  for(j in 1:68){
    union_purchase[as.numeric(rownames(list_9)[i]),]<-union_purchase[as.numeric(rownames(list_9)[i]),]+purchase_vector[as.numeric(as.character(list_9[i,j])),]
  }
  pb$tick()
}

#list10 고객
pb<-progress_bar$new(total=912)
for(i in 1:912){
  for(j in 1:46){
    union_purchase[as.numeric(rownames(list_10)[i]),]<-union_purchase[as.numeric(rownames(list_10)[i]),]+purchase_vector[as.numeric(as.character(list_10[i,j])),]
  }
  pb$tick()
}

#list11 고객
pb<-progress_bar$new(total=897)
for(i in 1:897){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_11)[i]),]<-union_purchase[as.numeric(rownames(list_11)[i]),]+purchase_vector[as.numeric(as.character(list_11[i,j])),]
  }
  pb$tick()
}

#list12 고객
pb<-progress_bar$new(total=1861)
for(i in 1:1861){
  for(j in 1:93){
    union_purchase[as.numeric(rownames(list_12)[i]),]<-union_purchase[as.numeric(rownames(list_12)[i]),]+purchase_vector[as.numeric(as.character(list_12[i,j])),]
  }
  pb$tick()
}

#list13 고객
pb<-progress_bar$new(total=1344)
for(i in 1:1344){
  for(j in 1:67){
    union_purchase[as.numeric(rownames(list_13)[i]),]<-union_purchase[as.numeric(rownames(list_13)[i]),]+purchase_vector[as.numeric(as.character(list_13[i,j])),]
  }
  pb$tick()
}

#list14 고객
pb<-progress_bar$new(total=892)
for(i in 1:892){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_14)[i]),]<-union_purchase[as.numeric(rownames(list_14)[i]),]+purchase_vector[as.numeric(as.character(list_14[i,j])),]
  }
  pb$tick()
}

#list15 고객
pb<-progress_bar$new(total=512)
for(i in 1:512){
  for(j in 1:26){
    union_purchase[as.numeric(rownames(list_15)[i]),]<-union_purchase[as.numeric(rownames(list_15)[i]),]+purchase_vector[as.numeric(as.character(list_15[i,j])),]
  }
  pb$tick()
}


#####################################################################################################################


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



#상위 열개 값 뽑는 함수
tenth<-function(x){
  a<-max(x)
  b<-max(x[x!=a])
  c<-max(x[x!=a&x!=b])
  d<-max(x[x!=a&x!=b&x!=c])
  e<-max(x[x!=a&x!=b&x!=c&x!=d])
  f<-max(x[x!=a&x!=b&x!=c&x!=d&x!=e])
  g<-max(x[x!=a&x!=b&x!=c&x!=d&x!=e&x!=f])
  h<-max(x[x!=a&x!=b&x!=c&x!=d&x!=e&x!=f&x!=g])
  i<-max(x[x!=a&x!=b&x!=c&x!=d&x!=e&x!=f&x!=g&x!=h])
  j<-max(x[x!=a&x!=b&x!=c&x!=d&x!=e&x!=f&x!=g&x!=h&x!=i])
  return(c(a,b,c,d,e,f,g,h,i,j))
}


####최종추천 목록
#아이템인덱스
temp_table<-as.data.frame(table(raw_purchase))
levels(temp_table$소분류코드)
##

recommand_item<-as.list(NA)


pb<-progress_bar$new(total=19383)
for(num in 1:19383){
  temp<-NA
  for(i in 1:10){
    temp<-c(temp,levels(temp_table$소분류코드)[which(recommand_matrix[num,]==tenth(recommand_matrix[num,])[i])])
  }
  temp<-temp[-1]
  recommand_item[[num]]<-temp
  pb$tick()
}

## 유사 고객들이 많이 샀던 (상위10) 품목들 

##상위 열개 자르기
recommand_item_top10<-recommand_item
for(i in 1:19383){
  if(length(recommand_item[[i]])>10)
    recommand_item_top10[[i]]<-recommand_item[[i]][1:10]
  pb$tick()
}

##
for_excel_recommand_item<-matrix(unlist(recommand_item_top10),nrow=19383,byrow=T)
write.csv(for_excel_recommand_item,"10items_test_15cluster.csv")
