#�ֱ� 3������ 19319 �� / 3106 item ����

##������ ���ų��� ������ �� ���� ��õ##


#���ų��� ������ ���
union_purchase<-matrix(0,nrow=19383,ncol=3106)

rownames(purchase_vector)<-as.numeric(levels(temp_table$������ȣ))

####
####
#list1 ����

pb<-progress_bar$new(total=1372)
for(i in 1:1372){
  for(j in 1:69){
    union_purchase[as.numeric(rownames(list_1)[i]),]<-union_purchase[as.numeric(rownames(list_1)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_1[i,j])),]
  }
  pb$tick()
}

#list2 ����
pb<-progress_bar$new(total=1655)
for(i in 1:1655){
  for(j in 1:83){
    union_purchase[as.numeric(rownames(list_2)[i]),]<-union_purchase[as.numeric(rownames(list_2)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_2[i,j])),]
  }
  pb$tick()
}

#list3 ����
pb<-progress_bar$new(total=569)
for(i in 1:569){
  for(j in 1:28){
    union_purchase[as.numeric(rownames(list_3)[i]),]<-union_purchase[as.numeric(rownames(list_3)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_3[i,j])),]
  }
  pb$tick()
}

#list4 ����
pb<-progress_bar$new(total=926)
for(i in 1:926){
  for(j in 1:46){
    union_purchase[as.numeric(rownames(list_4)[i]),]<-union_purchase[as.numeric(rownames(list_4)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_4[i,j])),]
  }
  pb$tick()
}

#list5 ����
pb<-progress_bar$new(total=2621)
for(i in 1:2621){
  for(j in 1:131){
    union_purchase[as.numeric(rownames(list_5)[i]),]<-union_purchase[as.numeric(rownames(list_5)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_5[i,j])),]
  }
  pb$tick()
}

#list6 ����
pb<-progress_bar$new(total=369)
for(i in 1:369){
  for(j in 1:18){
    union_purchase[as.numeric(rownames(list_6)[i]),]<-union_purchase[as.numeric(rownames(list_6)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_6[i,j])),]
  }
  pb$tick()
}

#list7 ����
pb<-progress_bar$new(total=2428)
for(i in 1:2428){
  for(j in 1:121){
    union_purchase[as.numeric(rownames(list_7)[i]),]<-union_purchase[as.numeric(rownames(list_7)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_7[i,j])),]
  }
  pb$tick()
}

#list8 ����
pb<-progress_bar$new(total=1651)
for(i in 1:1651){
  for(j in 1:83){
    union_purchase[as.numeric(rownames(list_8)[i]),]<-union_purchase[as.numeric(rownames(list_8)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_8[i,j])),]
  }
  pb$tick()
}

#list9 ����
pb<-progress_bar$new(total=1483)
for(i in 1:1483){
  for(j in 1:74){
    union_purchase[as.numeric(rownames(list_9)[i]),]<-union_purchase[as.numeric(rownames(list_9)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_9[i,j])),]
  }
  pb$tick()
}

#list10 ����
pb<-progress_bar$new(total=908)
for(i in 1:908){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_10)[i]),]<-union_purchase[as.numeric(rownames(list_10)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_10[i,j])),]
  }
  pb$tick()
}

#list11 ����
pb<-progress_bar$new(total=896)
for(i in 1:896){
  for(j in 1:45){
    union_purchase[as.numeric(rownames(list_11)[i]),]<-union_purchase[as.numeric(rownames(list_11)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_11[i,j])),]
  }
  pb$tick()
}

#list12 ����
pb<-progress_bar$new(total=1858)
for(i in 1:1858){
  for(j in 1:93){
    union_purchase[as.numeric(rownames(list_12)[i]),]<-union_purchase[as.numeric(rownames(list_12)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_12[i,j])),]
  }
  pb$tick()
}

#list13 ����
pb<-progress_bar$new(total=1207)
for(i in 1:1207){
  for(j in 1:60){
    union_purchase[as.numeric(rownames(list_13)[i]),]<-union_purchase[as.numeric(rownames(list_13)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_13[i,j])),]
  }
  pb$tick()
}

#list14 ����
pb<-progress_bar$new(total=888)
for(i in 1:888){
  for(j in 1:44){
    union_purchase[as.numeric(rownames(list_14)[i]),]<-union_purchase[as.numeric(rownames(list_14)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_14[i,j])),]
  }
  pb$tick()
}

#list15 ����
pb<-progress_bar$new(total=488)
for(i in 1:488){
  for(j in 1:24){
    union_purchase[as.numeric(rownames(list_15)[i]),]<-union_purchase[as.numeric(rownames(list_15)[i]),]+purchase_vector[which(rownames(purchase_vector)==as.character(list_15[i,j])),]
  }
  pb$tick()
}


#####################################################################################################################

#�ֱ� 3������ 19319 �� / 3106 item ����

## ������ ������ ���� ���� - ���� recommand_matrix
recommand_matrix<-union_purchase

for(i in 1:19319){
  for(j in 1:3106){
    if(purchase_vector[i,j]>0)
      recommand_matrix[as.numeric(row.names(purchase_vector)[i]),j]<-0
  }
}

##


#���� ���� �� �̴� �Լ�
third<-function(x){
  a<-max(x)
  b<-max(x[x!=a])
  c<-max(x[x!=a&x!=b])
  return(c(a,b,c))
}


####������õ ���
#�������ε���


recommand_item<-as.list(NA)


pb<-progress_bar$new(total=19383)
for(num in 1:19383){
  temp<-NA
  for(i in 1:3){
    temp<-c(temp,levels(temp_table$�Һз��ڵ�)[which(recommand_matrix[num,]==third(recommand_matrix[num,])[i])])
  }
  temp<-temp[-1]
  if(length(temp)>3){
    temp<-temp[1:3]
  }
  recommand_item[[num]]<-temp
  pb$tick()
}


##
recent_3month_recommand_item<-matrix(unlist(recommand_item),nrow=19383,byrow=T)
recent_3month_recommand_item[-as.numeric(levels(temp_table$������ȣ)),]<-NA

write.csv(recent_3month_recommand_item,"L_Point sumit.csv")

