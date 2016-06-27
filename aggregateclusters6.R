#start script

clusteridx<-unique(dataClustered["clusterid"])
nclusteridx<-dim(clusteridx)[1]
cols<-dim(dataClustered)[2]
nrows<-dim(dataClustered)[1]
cluster_assignment<-(dataClustered[cols-1])[[1]]

column_centroids = matrix(NA,nrow=nrows,ncol=(ncols-2))
distances = matrix(NA,nrow=nrows,ncol=nrows)
clusters_aggregations_of_data<-list()
for(i in 1:nclusteridx){
  #for(i in 3:3){
  idx<-clusteridx[[1]][i]
  
  w<-which(dataClustered["clusterid"]==idx)
  clusters_aggregations_of_data<-c(clusters_aggregations_of_data, list(w))
  #cat("w",w,"\n")
  subcluster<-dataClustered[w,1:(cols-2)]
  cm<-colMeans(subcluster)
  cmv<-as.vector(cm)
  for (j in 1:length(w)){
    column_centroids[w[j],]<-cmv
  }
  #dist(rbind(x1, x2))
}

#length(which(is.element(clusters_aggregations_of_data[[4]],2)))
ncols_cent<-dim(column_centroids)[2]

thr<-25
min_intersections<-2
for(i in 1:nrows){
  a<-column_centroids[i,]
  max<-2*sqrt(sum((a*2)^2))
  for(j in 1:nrows){
    d<-NULL
    b<-column_centroids[j,]
    maxinter = which(data[i,]==1)
    intersection<-length(which((data[i,]==data[j,] & data[j,]==1)==T))
    d<-sqrt(sum((a-b)^2))
    d<-d*100/max
    if (intersection>=min_intersections){
     #exact match 
    } else{ 
      if (length(which((a==b)==T))==ncols_cent){
        d<-0
        distances[i,j]<-d
      }  
    else{
     
      if (d>thr){
        #if (d>thr){
        #cat("index",i,"vs",j,"\n")
        #print(data[i,which(data[i,]!=0)])
        #print("vs")
        #print(data[j,which(data[j,]!=0)])
          distances[i,j]<-d
      }
      else{
        d<-0
        distances[i,j]<-d
      }  
      }
    }
  }
}

View(distances)
cat("outliers detection\n")
min_na<-2
similarSafe<-function(distances,i,exclude){
  
  k=length(which(is.na(distances[i,])))
  if (k<=min_na)
  {
    similar<-which(distances[i,]==0)
    similar<-similar[similar!=i]
    
    
    if (length(similar)>0){
      cat("checking",i," ")
      for(j in similar){
        if (!(j %in% exclude)){
          exclude[length(exclude)+1]<-i
          cat("->againsts",j," ")
          safe<-similarSafe(distances,j,exclude)
          if (safe[1]){
            cat("->SAFE!"," \n")
            return (c(TRUE,j))
          }
          else{cat("->UNSAFE"," \n")}
        }
      }
    }
    #cat(i," \n")
    return (c(FALSE,i))
    
    #print(data[i,which(data[i,]!=0)])
  }
  else{
    a<-c(TRUE,which(is.na(distances[i,])))
    return (a)
  }
}

outliers<-array()
for(i in 1:nrows){
  safe<-similarSafe(distances,i,c())
  if (safe[1]==0){
    cat("detected outlier:",i," \n")
    #print(data[i,which(data[i,]!=0)])
    outliers[length(outliers)]<-i
  }else{
    safeindxs<-safe[2:length(safe)]
    cindxs<-unique(cluster_assignment[safeindxs])
    for (k in cindxs){
      lst<-length(clusters_aggregations_of_data[k][[1]])
      clusters_aggregations_of_data[k][[1]][lst+1]<-i
    }
  }
}

nbag<-length(clusters_aggregations_of_data)
for(i in 1:nbag){
  #for(i in 3:3){
  
    c1<-clusters_aggregations_of_data[i]
    for(j in 1:nbag){
      #for(j in 4:4){
  
        c2<-clusters_aggregations_of_data[j]
        if (length(intersect(c1[[1]],c2[[1]]))>0){
          c3<-c(c1[[1]],c2[[1]])
          c3<-unique(sort(c3))
          clusters_aggregations_of_data[[i]]<-c3
          clusters_aggregations_of_data[[j]]<-c3
        }
    }
  
}


uniqueClusters<-unique(clusters_aggregations_of_data)
print("*************")
print("unique clusters and outliers")
print(uniqueClusters)
cat("number of found clusters",length(uniqueClusters))