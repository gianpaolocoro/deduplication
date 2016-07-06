rm(list=ls())
require('stringr')
source("WPS4D4Science.r")

prepareInput<-function(inputfile){
  cat("Preparing data\n")
  #INPUT PARAMETERS
  #inputfile<-"corog.csv"
  #inputfile<-"clusters/corog.csv"
  #inputfile<-"manghip.csv"
  
  #PREPARE THE DATA IN A CSV FILE#
  drops <- c("id","name","title")
  data<-read.csv(inputfile, header =T, sep=",")
  data<-data[ , !(names(data) %in% drops)]
  
  coln<-colnames(data)
  coln<-gsub("\\.", "_", coln)
  colnames(data)<-coln
  cnames<-dimnames(data)
  cat("Finished Preparing data\n")
  return (data)
}

densityClustering<-function(data,wps_uri,username,token){
  cat("Step 1 - Density clustering\n")
  featuresString<-""
  maxiterations<-10000
  if (dim(data)[2]<140){
    epsilon<-1
    cat("using epsilon",epsilon,"\n")
  }
  else{
    epsilon<-3
    cat("using epsilon",epsilon,"\n")
  }
  min_points<-1
  
  #wps_uri = "http://dataminer1-d-d4s.d4science.org:80/wps/WebProcessingService"
  #username<-"statistical.wps"
  #token<-"45943442-74ef-408b-be64-d26b42cf4c08" 
  
  options(digits.secs=3)
  
  cnames<-dimnames(data)
  column_names<-toString(as.character(cnames[[2]][2:length(cnames[[2]])]))
  featuresString<-str_replace_all(column_names,", ", "|")
  
  dffile<-paste("dbscan_data_",Sys.time(),".csv",sep="")
  dffile<-gsub(":", "_", dffile)
  dffile<-gsub(" ", "_", dffile)
  write.csv(data,file=dffile, quote = FALSE, eol = "\n", row.names = FALSE,  fileEncoding = "UTF-8")
  #LOAD THE TEMPLATE#    
  templateFile="TemplateDbscanRequest.xml";
  
  #PREPARE THE REQUEST FILE BY ALTERING THE TEMPLATE#    
  sentfile=paste("dbscan_req_",Sys.time(),".xml",sep="")
  sentfile<-gsub(":", "_", sentfile)
  sentfile<-gsub(" ", "_", sentfile)
  filexml<-readChar(templateFile, file.info(templateFile)$size)
  filexml<-gsub("\r\n", "\n", filexml)
  
  #TAKE THE INPUT TABLE CONTENT FROM THE CSV FILE AS A STRING#  
  body<-readChar(dffile, file.info(dffile)$size)
  body<-gsub("\r\n", "\n", body)
  body<-gsub("\n$", "", body)
  
  #SUBSTITUTE INPUTS IN THE TEMPLATE AND EMBED THE FILE#
  filexml<-gsub("#FILE#", body, filexml)
  filexml<-gsub("#FEATURES#", featuresString, filexml)
  filexml<-gsub("#EPSILON#", epsilon, filexml)
  filexml<-gsub("#MIN_POINTS#", min_points, filexml)
  
  #WRITE THE MODIFIED XML TEMPLATE DOCUMENT LOCALLY#
  filehandle <- file(filexml)
  write(filexml, file = sentfile,sep = "")
  close(filehandle)
  cat("Clustering")
  #SEND THE REQUEST#  
  out<-POST(url = wps_uri, config=c(authenticate(username, token, type = "basic")),body = upload_file(sentfile, type="text/xml"),encode = c("multipart"), handle = NULL)
  
  #CHECK IF THE PROCESS HAS ALREADY FINISHED#
  stop_condition_success<-grepl("Process successful",as.character(out))
  stop_condition_fail<-grepl("Exception",as.character(out))
  
  #GET THE STATUS LOCATION FROM THE ACCEPTANCE RESPONSE#
  lout<-as.character(out)
  #print(lout)
  statusLocation='statusLocation=\"'
  endstatusLocation='">\n'
  pos1 = regexpr(statusLocation, lout)
  pos2 = regexpr(endstatusLocation, lout)
  llout<-substr(lout, pos1+nchar(statusLocation), pos2-1)
  #print(llout)
  
  #CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION#
  while (!stop_condition_fail && !stop_condition_success){
    cat(".")
    #CHECK THE STATUS URL#
    out1<-GET(url = llout, config=c(authenticate(username, token, type = "basic")),handle = NULL, timeout(3600))
    #print(as.character(out1))
    stop_condition_success<-grepl("ProcessSucceeded",as.character(out1))
    stop_condition_fail<-grepl("Exception",as.character(out1))
    #SLEEP FOR 10 SECONDS BEFORE THE NEXT CHECK#
    Sys.sleep(2)
  }
  
  #print(as.character(out1))
  if (stop_condition_success){
    cat("finished\n")
    #PARSE THE OUTPUT IN THE CASE OF SUCCESS
    wps.output<-parseResponse(response=out1)
    output<-as.data.frame(wps.output)
    #View(output)
  }
  
  closeAllConnections()
  
  #GET THE INLINE OUTPUT TABLE#
  con <- textConnection(as.character(output$Value)[1])
  dataClustered <- read.csv(con)
  #View(dataClustered)
  #View(cbind(dataClustered["clusterid"],dataClustered["outlier"]))
  
  ncols = dim(dataClustered)[2]
  clustersdistr<-(dataClustered[,ncols-1])
  nclust = length(unique(dataClustered[,ncols-1]))
  
  cat("Clusters:",nclust,"\n")
  cat("Link to output:",as.character(output["Value"][[1]][2]),"\n")
  cat("Computation finished\n")
  
  file.remove(sentfile, showWarnings = FALSE)
  file.remove(dffile, showWarnings = FALSE)
  
  cat("Finished Step 1 - Density clustering\n")
  
  return(dataClustered)
}

calculateSimilarities<-function(data,dataClustered){
  
  
  cat("Step 2 - Aggregating clusters\n")
  ######CLUSTERING AGGREGATION
  ncols = dim(dataClustered)[2]
  clusteridx<-unique(dataClustered["clusterid"])
  nclusteridx<-dim(clusteridx)[1]
  cols<-dim(dataClustered)[2]
  nrows<-dim(dataClustered)[1]
  
  column_centroids = matrix(NA,nrow=nrows,ncol=(ncols-2))
  distances = matrix(NA,nrow=nrows,ncol=nrows)
  
  cat("Preparing clusters means and original clusters\n")
  for(i in 1:nclusteridx){
    #for(i in 3:3){
    idx<-clusteridx[[1]][i]
    
    w<-which(dataClustered["clusterid"]==idx)
  
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
  cat("Calculating similarities between vectors\n")
  for(i in 1:nrows){
    a<-column_centroids[i,]
    max1<-2*sqrt(sum((a*2)^2))
    #cat("\n",i,"vs ")
    for(j in 1:nrows){
      #cat(j)
      d<-NULL
      b<-column_centroids[j,]
      maxinter = which(data[i,]==1)
      intersection<-length(which((data[i,]==data[j,] & data[j,]==1)==T))
      d<-sqrt(sum((a-b)^2))
      #max2<-sqrt(sum((b*2)^2))
      #maxd<-max(max1,max2)
      maxd<-max1
      d<-d*100/maxd
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
  cat("\n")
  
  return (distances)
}

aggregateClusters<-function(distances,dataClustered){
  
  ncols = dim(dataClustered)[2]
  clusteridx<-unique(dataClustered["clusterid"])
  nclusteridx<-dim(clusteridx)[1]
  cols<-dim(dataClustered)[2]
  nrows<-dim(dataClustered)[1]
  cluster_assignment<-(dataClustered[cols-1])[[1]]
  
  clusters_aggregations_of_data<-list()
  cat("Preparing clusters means and original clusters\n")
  for(i in 1:nclusteridx){
    idx<-clusteridx[[1]][i]
    w<-which(dataClustered["clusterid"]==idx)
    clusters_aggregations_of_data<-c(clusters_aggregations_of_data, list(w))
  }
  
  cat("outliers detection\n")
  min_na<-2
  similarSafe<-function(distances,i,exclude){
    
    k=length(which(is.na(distances[i,])))
    if (k<=min_na)
    {
      similar<-which(distances[i,]==0)
      similar<-similar[similar!=i]
      
      
      if (length(similar)>0){
        #cat("checking",i," ")
        for(j in similar){
          if (!(j %in% exclude)){
            exclude[length(exclude)+1]<-i
            exclude[length(exclude)+1]<-j
            #cat("->againsts",j," ")
            safe<-similarSafe(distances,j,exclude)
            if (safe[1]){
              #cat("->SAFE!"," \n")
              return (c(TRUE,j))
            }
            else{
              #cat("->UNSAFE"," \n")
              }
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
  
  cat("non-outliers assignment to the clusters\n")
  outliers<-array()
  for(i in 1:nrows){
    #cat("->check safe",i," \n")
    safe<-similarSafe(distances,i,c())
    #cat("->check ",i,safe[1]," \n")
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
  
  
  
  #output_namefile_random <- paste("clusters_",inputfile,"_.txt",sep="")
  
  #cat("writing output in",output_namefile_random)
  #write(x=output,file=output_namefile_random, append=F)
  
  cat("Finished Step 2 - Aggregating clusters\n")

return(clusters_aggregations_of_data)

}

mergeClusters<-function(clusters_aggregations_of_data){
  cat("Step 3 - Merging clusters\n")
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
  cat("number of found clusters",length(uniqueClusters),"\n")
  
  cat("Finished Step 3 - Merging clusters\n")
  return (uniqueClusters)
}

mergedClustersToString<-function(uniqueClusters){
  
  output<-paste()
  for (i in 1:length(uniqueClusters)){
    output<-paste(output,"Cluster ",i,"\n",sep="")
    a<-paste(as.character(uniqueClusters[[i]]), collapse=",")
    output<-paste(output,a,"\n",sep="")
  }
  
  return (output)
}

wps_uri = "http://dataminer1-d-d4s.d4science.org:80/wps/WebProcessingService"
username="statistical.wps"
token="45943442-74ef-408b-be64-d26b42cf4c08" 
#inputfile="clusters/corog.csv"
#inputfile="clusters/gcoro.csv"
#inputfile="clusters/arossi.csv"
inputfile="clusters/shibatat.csv"
inputfile="clusters/rossia.csv"
inputfile="clusters/aabdul.csv"

#inputfiles = c("clusters/corog.csv","clusters/gcoro.csv","clusters/arossi.csv","clusters/shibatat.csv","clusters/rossia.csv","clusters/aabdul.csv","clusters/ioannidisy.csv","clusters/manolan.csv","clusters/manghip.csv")
#inputfiles = c("clusters/gcoro.csv")
#inputfiles = c("clusters/corog.csv")
#inputfiles = c("clusters/ioannidisy.csv")
inputfiles = c("clusters/corog.csv")

for (i in 1:length(inputfiles)){
  inputfile = inputfiles[i]
  cat("\n*****PROCESSING ",inputfile,"\n")
data<-prepareInput(inputfile = inputfile)
if (dim(data)[2]<250){
  #data<-prepareInput(inputfile = "corog.csv")
  densityClusters<-densityClustering(data=data,wps_uri,username,token)
  similarities<-calculateSimilarities(data=data,dataClustered=densityClusters)
  aggregatedClusters<-aggregateClusters(distances=similarities,dataClustered=densityClusters)
  mergedClusters<-mergeClusters(clusters_aggregations_of_data=aggregatedClusters)
  mergedClustersString<-mergedClustersToString(uniqueClusters=mergedClusters)
  output_namefile_random <- paste("clusters_",strsplit(inputfile, "/")[[1]][2],"_.txt",sep="")
  cat("writing output in",output_namefile_random)
  write(x=mergedClustersString,file=output_namefile_random, append=F)
  
  } else {
  source("aggregateclusters7.R")
}


cat("\n*****FINISHED PROCESSING",inputfile,"\n")
}

#for checks
#data[19,which(data[19,]!=0)]  