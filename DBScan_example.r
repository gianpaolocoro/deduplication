rm(list=ls())
require('stringr')
source("WPS4D4Science.r")

#INPUT PARAMETERS
#inputfile<-"corog.csv"
inputfile<-"samples/rossia_adj.csv"
#inputfile<-"manghip.csv"
featuresString<-""
maxiterations<-10000
epsilon<-3
min_points<-1

wps_uri = "http://dataminer1-d-d4s.d4science.org:80/wps/WebProcessingService"
username<-"statistical.wps"
token<-"45943442-74ef-408b-be64-d26b42cf4c08" 

options(digits.secs=3)

#PREPARE THE DATA IN A CSV FILE#
data<-read.csv(inputfile, header =T, sep=",")
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
write(filexml, file = sentfile,append = FALSE, sep = "")
close(filehandle)

#SEND THE REQUEST#  
out<-POST(url = wps_uri, config=c(authenticate(username, token, type = "basic")),body = upload_file(sentfile, type="text/xml"),encode = c("multipart"), handle = NULL)
  
#CHECK IF THE PROCESS HAS ALREADY FINISHED#
stop_condition_success<-grepl("Process successful",as.character(out))
stop_condition_fail<-grepl("Exception",as.character(out))

#GET THE STATUS LOCATION FROM THE ACCEPTANCE RESPONSE#
lout<-as.character(out)
print(lout)
statusLocation='statusLocation=\"'
endstatusLocation='">\n'
pos1 = regexpr(statusLocation, lout)
pos2 = regexpr(endstatusLocation, lout)
llout<-substr(lout, pos1+nchar(statusLocation), pos2-1)
print(llout)

#CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION#
while (!stop_condition_fail && !stop_condition_success){
  print("Checking...")
  #CHECK THE STATUS URL#
  out1<-GET(url = llout, config=c(authenticate(username, token, type = "basic")),handle = NULL, timeout(3600))
  print(as.character(out1))
  stop_condition_success<-grepl("ProcessSucceeded",as.character(out1))
  stop_condition_fail<-grepl("Exception",as.character(out1))
  #SLEEP FOR 10 SECONDS BEFORE THE NEXT CHECK#
  Sys.sleep(10)
}

print(as.character(out1))
if (stop_condition_success){
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

source("aggregateclusters6.r")

