######################################################
### This is some R code that BUilds a Knowledge graph - rough rough rough 
### The Watson services here http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/services-catalog.html
######################################################

## WATSON
library(RCurl) # General Network Client Interface for R
library(rjson) # JSON for R
library(jsonlite) # JSON parser
library(XML) # XML parser
library(httr) # Tools for URLs and HTTP
library(stringr)
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images
library(splitstackshape) # for split csplit 


######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Project_Thought2Vec2EEG")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings



###############################  ALCHEMY - COMBINED CALL - RETREIVE AND STORE INFO
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetCombinedData"
api_key <- username_password_ALCH #check we got this from keys.R

alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetCombinedData"
output_mode <- "json"
text <- URLencode("limes")
query <- paste(alchemy_url,api_feature,"?text=",text,"&apikey=",api_key,"&outputMode=",output_mode,"&knowledgeGraph=1", sep="")
query
response <- POST(query)
response

fromJSON(paste(response))

### OK LETS GO!

text <- read.csv("data.csv",header=FALSE) # old method - csv input - we'll use tweets we just pulled
text
len <- dim(text)[1] # how tall is our data frame?
output_mode <- "json"
catchers_mitt <- as.data.frame(matrix(ncol=5, nrow=len))
setnames(catchers_mitt,c("index","text","taxonomy","keyword","concept","entity","knowledgegraph"))
catchers_mitt$text <- text[,1]

for (i in 1:len){
  text_to_send <- URLencode(paste(text[i,]))
  query <- paste(alchemy_url,api_feature,"?extract=keyword,doc-sentiment,entity,taxonomy,concept&apikey=",api_key,"&text=",text_to_send,"&outputMode=",output_mode,"&knowledgeGraph=1", sep="")
  query
  response <- POST(query)
  reply <- fromJSON(paste(response))
  reply
  
  catchers_mitt$index[i] <- i
  catchers_mitt$taxonomy[i] <- paste(reply$taxonomy$label[1],"")
  catchers_mitt$keyword[i] <- paste(reply$keywords$text[1],"")
  catchers_mitt$concept[i] <- paste(reply$concepts$text[1],"")
  catchers_mitt$entity[i] <- paste(reply$entities$text[1],"")
  catchers_mitt$knowledgegraph[i] <- paste(reply$keywords$knowledgeGraph[1],"")
  print(catchers_mitt[i,])
}


catchers_mitt <- data.frame(lapply(catchers_mitt, as.character), stringsAsFactors=FALSE)
dim(catchers_mitt)
write.csv(catchers_mitt,"1500_samples_knowledge_graph_raw.csv")





######  THIS SECTION PLOTS ON OUR TAG (PHysical = 1 ; Conceptual = 9) to our FIRST LEVEL Knowledge Graph Data

tag_table <- read.csv("tag_table.csv",header=TRUE)
head(tag_table) # people and food = 1
tail(tag_table) # concepts = 9

data <- data.frame(catchers_mitt$knowledgegraph) # just want knowledge graph from catcher's mitt
data <- cSplit(data, 'catchers_mitt.knowledgegraph', sep="/", type.convert=FALSE) # separate the / and / between levels
head(data)

temp <- rowSums(!is.na(data)) # count depth of trees (actually N+1)

data <- data$catchers_mitt.knowledgegraph_2
data <- data.frame(data)
setnames(data,c("tag"))
data$depth <- temp
head(data)
data$knowledgegraph <- catchers_mitt$knowledgegraph #add back
data$text <- catchers_mitt$text #add back
head(data)
tag_table
data <- merge(data, tag_table, by = c("tag")) # gonna strip out the N/A
head(data)
data <- data[,c(4,3,1,5,2)]


plot(jitter(data[,'physical.conceptual']), jitter(data[,'depth']), 
     main="Conceptual vs. Graph-Depth",
     xlab="Conceptual", ylab="Graph-Depth",
     pch=19, col="darkgreen")
text(jitter(data[,'physical.conceptual']), jitter(data[,'depth']),  data$text,
     cex=0.65, pos=3,col="red") 


###  NEXT UP! THIS SECTION DOES REAL TIME PROCESSING AND PLOTTING

# 1 STT
# 2 Text to alchemy_url
# 3 ENrich and store in table or data frame
# 4 extract 'coordinates' as above
# 5 plot
