#wps_uri = "http://dataminer1-d-d4s.d4science.org:80/wps/WebProcessingService"
#username="statistical.wps"
#token="45943442-74ef-408b-be64-d26b42cf4c08" 

#inputfiles = c("clusters/arossi.csv")


#inputfile = inputfiles[1]
#cat("\n*****PROCESSING ",inputfile,"\n")
#data<-prepareInput(inputfile = inputfile)

j0<-1
nrows<-dim(data)[1]
ncols<-dim(data)[2]

thrcolumns<-100
uberMergedClusters<-list()
nclustering<-1
while (j0<=nrows){
  tochange=FALSE
  tochangeidx=1
  overallcols<-c("k")
  cat("Processing",j0)
  for (i in j0:nrows){
    colsnam<-colnames(data)[which(data[i,]==1)]
    overallcols<-unique(c(overallcols,colsnam))
    if ((length(overallcols)>=thrcolumns) && ((nrows-i)>10)){
      tochange=TRUE
      tochangeidx=i
      break
    }
    
  }
  
  #cat("changing\n")
    if (!tochange){
      tochangeidx=nrows
    }
  
  cat(" To",tochangeidx,"\n")
  
  cat("****Clustering number",nclustering,"\n")
  dataf<-data.frame(matrix(vector(), 0, length(overallcols),dimnames=list(c(),overallcols)))
  dataf<-transform(dataf, k = as.factor(k))
  
  k=1
  for (j in j0:tochangeidx){
    if (j0<tochangeidx){
      dataf[k,overallcols]<-data[j, overallcols]
      k=k+1
    }
  }
  
  #dataf <- dataf[,sample(ncol(dataf))]
  
  cat("captured\n")
  #View(dataf)
  
  densityClusters<-densityClustering(data=dataf,wps_uri,username,token)
  similarities<-calculateSimilarities(data=dataf,dataClustered=densityClusters)
  aggregatedClusters<-aggregateClusters(distances=similarities,dataClustered=densityClusters)
  mergedClusters<-mergeClusters(clusters_aggregations_of_data=aggregatedClusters)
  
  for (j in 1:length(mergedClusters)){
    cl <- mergedClusters[[j]]
    for (t in 1: length(cl)){
      el<-cl[t]
      originalidx <- el+j0-1
      cl[t]<-originalidx
    }
    mergedClusters[[j]]<-cl
  }
  
  cat("*************CLUSTERS\n")
  print(mergedClusters)
  cat("*************\n")
  
  uberMergedClusters<-c(uberMergedClusters,mergedClusters)
  j0<-tochangeidx+1
  nclustering<-nclustering+1

}



nc<-length(uberMergedClusters)

uberMergedClustersOrig<-uberMergedClusters
uberMergedClusters<-uberMergedClustersOrig
distthreshold<-1
intersectionthr<-0.3
for (i in 1:(nc-1)){
  
  idxx1<-uberMergedClusters[[i]]
  centroidmean1<-colMeans(data[idxx1,2:ncols])
  if (i<nc) {
  for (j in (i+1):nc){

      idxx2<-uberMergedClusters[[j]]
      centroidmean2<-colMeans(data[idxx2,2:ncols])
      
      intersection<-length(which((centroidmean1>0 & centroidmean2>0))==T)/min(length(centroidmean1[centroidmean1>0]),length(centroidmean2[centroidmean2>0]))
      m<-max(sum((centroidmean1)^2),sum((centroidmean2)^2))
      distancecc<-sum((centroidmean1-centroidmean2)^2)/m
      cat("dist",i,"vs",j,"=",distancecc,";",intersection,"\n")
        if (intersection>intersectionthr & distancecc<distthreshold){
          cat("<-merged!","\n")
          lst<-length(uberMergedClusters[i][[1]])
          uberMergedClusters[i][[1]][lst+1]<-idxx2[1]
          break
        }
  }
  }
  
  
}

finalMergedClusters<-mergeClusters(clusters_aggregations_of_data=uberMergedClusters)
