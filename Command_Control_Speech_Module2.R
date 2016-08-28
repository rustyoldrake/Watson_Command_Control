## SPEECH MODULE #2 for Verbal Manipulation (and Invoking) of Virtual Objects using 
## IBM Watson Speech to Text and NLC Services from Watson Developer Cloud
## Ryan Anderson August 2016
## 
## https://cran.r-project.org/web/packages/plotrix/plotrix.pdf
## https://en.wikipedia.org/wiki/Electronic_color_code 
## 
## IBM Watson - AR VR Headset Test Bench: \n Command & Control Foundation
## 


library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio) 
library(data.table)
library(dplyr)
library(reshape2)
library(Rtts)
library(splitstackshape)
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(png)
library(rPython)

#closeAllConnections()

setwd("/Users/ryan/Documents/Project_R_Command_Control") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below


## Base URLs for IBM Watson APIs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api"

### GET - Basic Test Tone
getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=hello",userpwd = username_password_TON ) 

getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

# For audio Sampling later
sample_count <- 60000  ## 0k samples
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time

########## FUNCTION DECLARATIONS  #### FUNCTION DECLARATIONS ##################### FUNCTION DECLARATIONS  #### FUNCTION DECLARATIONS ###########

### STT FUNCTION to test connectivity and return models available
watson.STT.getmodels <- function()
{return(GET(url=paste(base_url_STT,"/v1/models",sep=""),
            authenticate(username_STT,password_STT)))} 
### FOR BEST RESULTS - USE USB HEADSET and ensure in MAc > Sys Preferences >Sound it's selected


## STT FUNCTION - Record!  
watson.STT.record <- function(samp_count,samp_rate)
{
  # record 8000 samples at 8000Hz (1 sec), mono (1 channel)
  # record 64k samples at 16kHz (4 sec), mono (1 channel), stereo = 2
  a <- record(samp_count, samp_rate, 2)
  wait(a) # wait for the recording to finish
  x <- a$data # get the result
  x[1:10] # show first ten samples
  close(a); rm(a) # you can close the instance at this point
  # amplify and crop the signal
  audio <- x * 2
  audio[audio < -1] <- -1
  audio[audio > 1] <- 1
  return(audio)
}


#### STT FUNCTION TO TIDY UP the STT response - just export the TRANSCRIPT ONLY
stt_transcript_only <- function(raw) 
{
  data <- as.data.frame(strsplit(as.character(raw),"\\n"))
  data <- data[c(7), ] # for now, grab just what we want
  data <- paste(data) # kill levels, - fyi this nukes confidence % info (may want later)
  data <- gsub("  ","",data) # remove excessive whitespace  0 cannot use ALL [[punct]] here
  data <- gsub("\\\\","",data) # remove punct we dont like
  data <- gsub("\"","",data) # remove punct we dont like
  data <- gsub("transcript","",data) # remove excessive whitespace
  data <- gsub(":","",data) # remove excessive whitespace - later: Improve this tidy step. 
  return(data) 
}


###### STT FUNCTION - ANalyze AUDIO WAV file with IBM Watson Speech to Text service - SESSIONLESS
watson.speech_to_text.recognize <- function(audio_file)
{ return(POST(url=paste(base_url_STT,"/v1/recognize",sep=""),
              authenticate(username_STT,password_STT),
              add_headers("Content-Type"="audio/wav"),
              body = (file = upload_file(audio_file))  
))} #works # hope this helps you with syntax!
## this is SESSIONLESS MODE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/#!/speech-to-text/recognizeSessionless


##### STT FUNCTION FOR SPEECH TO TEXT - RETURNS TRANSCRIPT - SESSIONLESS
watson.speech_to_text.sessionless <- function(file_name)
{ 
  wait(play(sin(1:200/4)))  # recording START tone
  print("RECORDING ------------------ (beep) ")
  the_audio <- watson.STT.record(sample_count,sample_rate)
  wait(play(sin(1:200/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  #print("Saving WAV File")
  save.wave(the_audio,file_name) 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize(file_name)
  return(stt_transcript_only(content(response,"text")))
} 

####### TTS Function to list voices
watson.TTS.listvoices <- function()
{
  voices <- GET(url=paste("https://stream.watsonplatform.net/text-to-speech/api/v1/voices"),authenticate(username_TTS,password_TTS))
  data <- content(voices,"text")
  data <- as.data.frame(strsplit(as.character(data),"name"))
  data <- data[-c(1:2), ] # remove dud first row
  data <- strsplit(as.character(data),",")
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data <- cSplit(data, 'V1', sep="\"", type.convert=FALSE)
  data <- data.frame(data$V1_04)
  data[,1]  <- gsub("\\\\","",data[,1] )
  return(data) }

watson.TTS.listvoices()


########  TTS FUNCTION --- TEXT TO SPEECH
watson.TTS.execute <- function(url1,text1,voice1,filename1)
{
  the_audio = CFILE(filename1, mode="wb") 
  curlPerform(url = paste(url1,"?text=",text1,"&voice=",voice1,sep=""),
              userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system(paste("open",filename1,"-a vlc"))
}

## Function Robot speaks
headset_speaks <- function(voice_transcript)
{
  voice <- "en-US_MichaelVoice"
  the_url <- paste(base_url_TTS,"?text=",URLencode(voice_transcript),"&voice=",voice,sep="")
  the_audio = CFILE("toy_talks.wav", mode="wb")  ## here we receive the audio back
  curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
  close(the_audio)
  system("open toy_talks.wav -a vlc")  # Now - Let's listen 
  wait(6) ## this gives ECHO some time to use the WiFI and not fight for bandwidth
}

###### 
closeAllConnections()
file_name <- "file_name"
#img <- readPNG("ibm_watson.png")
#grid::grid.raster(img)

#python.load("initialize.py", get.exception = TRUE) # for Arduino and AR headset - later 

### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}

### FUNCTION to recieve results of analysis, pull out 5 traits, and show image and plot
display_results <- function(results){
  results <- data.frame(results[1:5,]) # just the gang :)
  results <- results[with(results, order(-signal)),] # reorder
  results <- data.frame(results,stringsAsFactors=FALSE) # stringsAsFactors=FALSE keeps junky factors out
  results
  
  main_trait <- paste(results$trait[1]) # paste kills levels 
  main_signal <- results$signal[1]
  signal_threshold <- 0.75 # this is level that we present 
  
  ## Switch Statement
  # if(main_signal>signal_threshold)
  # { 
  #   
  #   switch(main_trait,
  #          "joy" = image <- python.exec("ser.write('y')"),
  #          "sadness" = image <- python.exec("ser.write('b')"),
  #          "anger" = image <- python.exec("ser.write('r')"),
  #          "disgust" = image <- python.exec("ser.write('g')"),
  #          "fear" = image <- python.exec("ser.write('f')")
  #   )
  #   
  # } #else{python.exec("ser.write('0')")}
  
  ### this from twilio_comms file 
  
  # barplot(results$signal, main="Tone Analysis", 
  #         xlab="", ylab=query, 
  #         names.arg=results$trait,
  #         border="red",las=3, srt=45) 
}


process_data_to_tone("I am certainly going to make this project a success") # not perfect, but good enough for now!
query <- "I am super scared and nervous about bees"
process_data_to_tone(query)



######################## SPEECH SENSE TEST LOOP - COMMAND AND CONTROL
######################## SPEECH SENSE TEST LOOP - COMMAND AND CONTROL
######################## SPEECH SENSE TEST LOOP - COMMAND AND CONTROL

repeat
{
  response <- watson.speech_to_text.sessionless("test.wav")
  print(response)
  
  ## Logic Sequence 
  ##  (1) Is there a Clear Object/Artefact identified (class and individual)
  ##  (2) Is the CHANGE Intent clear (Location, COlor, Size)
  ##  E.g. Move Cat2 up; Change Cat3 to Yellow;  Move Tree2 down and to the left; Move Tree1 to center
  
  objectclass <- NULL
  objectnumber <- 1  # would prefer nul - but need to handle non number more elegantly first
  objectname <- NULL
  move_x <- 0
  move_y <- 0
  color <- NULL
  
  change_desired <- NULL # move X; increase size to X;  increase size BY Y; change color to Z (for now, no relative positions)
  location <- NULL
  size <- NULL
  zone <- NULL
  
  if(grepl("reset", response)){ccm.plot.reset.testbed()}  # 
  
## TESTING THE UTTERANCE (SIMPLE GREP NOW - NLC LATER)
  
  ## Objectclass
  if(grepl("cat", response)){objectclass="cat"} # 
  if(grepl("tree", response)){objectclass="tree"} # 
  if(grepl("dot", response)){objectclass="node"} # 
  if(grepl("circle", response)){objectclass="node"} # 
  if(grepl("node", response)){objectclass="node"} # 
  
  ## INTENT
  if(grepl("create", response)){change_desired="create"} # 
  if(grepl("invoke", response)){change_desired="create"} # 
  if(grepl("size", response)){change_desired="resize_larger"} # 
  if(grepl("resize", response)){change_desired="resize_larger"} #
  if(grepl("larger", response)){change_desired="resize_larger"} # 
  if(grepl("bigger", response)){change_desired="resize_larger"} #
  if(grepl("smaller", response)){change_desired="resize_smaller"} # 
  if(grepl("reduce", response)){change_desired="resize_smaller"} #
  if(grepl("shrink", response)){change_desired="resize_smaller"} #
  
  
  ## MOVEMENT -------------------
  if(grepl("left", response)){move_x=-5}
  if(grepl("right", response)){move_x=5}
  if(grepl("up", response)){move_y=5}
  if(grepl("down", response)){move_y=-5}
  
  ## COLOR
  if(grepl("black", response)){color="black"}
  if(grepl("brown", response)){color="tan3"}
  if(grepl("red", response)){color="red"}
  if(grepl("orange", response)){color="orange"}
  if(grepl("yellow", response)){color="yellow"}
  if(grepl("green", response)){color="green"}
  if(grepl("blue", response)){color="blue"}
  if(grepl("violet", response)){color="violet"}
  if(grepl("grey", response)){color="grey"}
  if(grepl("white", response)){color="black"}
  
  #objectnumber <- 1 # default - if no others - we ASSUME class first is 1
  if(grepl("one", response)){objectnumber=1}
  if(grepl("two", response)){objectnumber=2}
  if(grepl("three", response)){objectnumber=3}
  if(grepl("four", response)){objectnumber=4}
  if(grepl("five", response)){objectnumber=5}
  if(grepl("six", response)){objectnumber=6}
  if(grepl("seven", response)){objectnumber=7}
  if(grepl("eight", response)){objectnumber=8}
  if(grepl("nine", response)){objectnumber=9}

## ObjectNumber (let's keep number talk as ID, and location use up,down, zone x - i.e. to keep simple for now, numbers are ID's - not locations or size
if( (!is.null(objectclass)) * (!is.null(change_desired))) {  # for now this section is NEW Artefacts

  temp <- paste(objectclass,sample(5:10,1),sep="") # name our new friend
  if(objectclass=="cat"){
    if(is.null(color)){color <- "black"}
    ccm.create.cat(temp,color,sample(-100:100,1),sample(-100:100,1))
  }
  if(objectclass=="tree"){
    ccm.create.tree(temp,sample(-100:100,1),sample(-100:100,1))  
  }
  print(paste("COMMAND: invoking new object: ",temp))
  if((objectclass=="node") & ((change_desired=="resize_larger") | (change_desired=="resize_smaller")  )) # "resize orange circle"
  {
    temp <- paste(color,"-node",sep="")
    resize <- 10 # default larger
    if((change_desired=="resize_smaller")){resize <- -10 } 
    ccm.modify.circle.size(temp,resize,0) # node,change, or set size
  }
  ccm.plot.dataframe()
}
else if(!is.null(objectclass)) # nothing here worth doing if we dont have a CLASS > TARGET
 {
  ## Objectname (Objectclass + Number)
  objectname <- paste(objectclass,objectnumber,sep="")
  print(paste("the object's name is:",objectname))

  ## CIRCLE / NODE WORK HAPPENS HERE
  if ( (objectclass=="node") & (!is.null(color)) ){ # WILL OVERRIDE OTHER STUFF
    objectname <- paste(color,"-node",sep="") # simply blends node for next step
    #ccm.modify.artifact.position(objectname,move_x,move_y)
    #ccm.modify.artifact.position("green-node",left,up)
  } else if( (!is.null(objectname)) & (!is.null(color) )) {
    print(paste("modifying artifact color:",objectname,"color",color))
    ccm.modify.artifact(objectname,"color",color)
  }
  
  if(!is.null(objectname) & ((move_x!=0) | (move_y!=0) )){
    print(paste("modifying artifact position:",objectname,move_x,move_y))
    ccm.modify.artifact.position(objectname,move_x,move_y)
  }
 }
}

closeAllConnections()
ccm.plot.reset.testbed()


#ccm.modify.artifact.position("tree1",right,up)
#ccm.modify.artifact("cat1","color","orange") 
#ccm.modify.artifact.position("cat1",left,down)
#if(grepl("speak", response)){headset_speaks("Hello, My name is Jarvis! Let's be friends!")}
