total_cols<-dim(data)[2]
total_rows<-dim(data)[1]
columnsWindow=100
n_columns=min(columnsWindow,total_cols-1)

prob_per_row<-array()
for (i in 1:total_rows) {
  row<-data[i,]
  pr<-n_columns*length(row[row==1])/total_cols
  pr<-min(pr,0.9)
  
  prob_per_row<-c(prob_per_row,pr)
}
prob_per_row<-prob_per_row[!is.na(prob_per_row)]

P<-prod(prob_per_row)
#P<-sum(prob_per_row)
#P<-mean(prob_per_row)
#P<-(P^(total_rows/5))

n_trials= round(1/(P^(1/total_rows)))
n_trials = n_trials * 3 #increase the variety

cat("Number of trials",n_trials,"\n")
coln<-colnames(data)

uberMergedClusters<-list()

for (i in 1:n_trials){
#for (i in 1:2){
  samplecolumns<-sample(2:dim(data)[2], n_columns, replace=FALSE)
  columnsToSelect<-c("k",coln[samplecolumns])
  subdata<-data[ , (names(data) %in% columnsToSelect)]
  toremove<-array()
  for (j in 1:total_rows){
    if (length(which(subdata[j,]==0))==n_columns){
      toremove[length(toremove)+1]<-j
    }
  }
  toremove<-sort(toremove[!is.na(toremove)])
  if (length(toremove)==0)
    subdatarem<-subdata
  else
    subdatarem<-subdata[-toremove,]
  
  densityClusters<-densityClustering(data=subdatarem,wps_uri,username,token)
  aggregatedClusters<-aggregateClusters(data=subdatarem,dataClustered=densityClusters)
  mergedClusters<-mergeClusters(clusters_aggregations_of_data=aggregatedClusters)
  mergedClustersString<-mergedClustersToString(uniqueClusters=mergedClusters)
  
  prevk = 1
  correspondences<-matrix(data=NA, ncol = 2,nrow = total_rows)
  for (k in 1:total_rows){
    if (k %in% toremove){
      correspondences[k,]<-c(k,0)
    }
    else {
      correspondences[k,]<-c(k,prevk)
      prevk<-prevk+1
    }
    
  }
  
  for (j in 1:length(mergedClusters)){
    cl <- mergedClusters[[j]]
    for (t in 1: length(cl)){
      el<-cl[t]
      originalidx <- which(correspondences[,2] == el)
      cl[t]<-originalidx
    }
    mergedClusters[[j]]<-cl
  }
  
  uberMergedClusters<-c(uberMergedClusters,mergedClusters)
  
}
cat("\n***FINAL CLUSTERS***\n")
#print(uberMergedClusters)
finalMergedClusters<-mergeClusters(clusters_aggregations_of_data=uberMergedClusters)

nfinalClusters = length(finalMergedClusters)
cat("\n***FINAL VECTORS CHECK***\n")
for (i in 1:nfinalClusters){
  clustersvectors<-finalMergedClusters[[i]]
  if (length(clustersvectors)==1){
    
    idxcol.i<-clustersvectors[1]
    cat("->",idxcol.i," ")
    vector.i<-as.vector(data[idxcol.i,])
    for (j in 1:nfinalClusters){
      
        if (j!=i){
          clustersvectorsj<-finalMergedClusters[[j]]
          nclust.j<-length(clustersvectorsj)
          for (k in 1: nclust.j){
            idxcol.k<-clustersvectorsj[k]
            cat("<-",idxcol.k," ")
            vector.k<-as.vector(data[idxcol.k,])
            intersection<-length(which((vector.i==vector.k & vector.i==1)==T))
            if (intersection>2){
              cat("MATCH! ")
              finalMergedClusters[[i]][length(finalMergedClusters[[i]])+1]<-idxcol.k
              break;
            }
          }
          cat("\n")
        }
      }
  }
}

finalMergedClusters<-mergeClusters(clusters_aggregations_of_data=finalMergedClusters)
