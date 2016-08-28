## GRAPHICS MODULE #1  - Foundation Module for Verbal Manipulation (and Invoking) of Virtual Objects using 
## IBM Watson Speech to Text and NLC Services from Watson Developer Cloud
## Ryan Anderson August 2016
## 
## https://cran.r-project.org/web/packages/plotrix/plotrix.pdf
## https://en.wikipedia.org/wiki/Electronic_color_code 
## 
## IBM Watson - AR VR Headset Test Bench: \n Command & Control Foundation
## 

setwd("/Users/ryan/documents/project_r_command_control")
getwd()
library("plotrix")

## SECTION 1 - GET GRAPHICS WORKING
plot(0, xlim=c(-100,120), ylim=c(-100,120), main="IBM Watson - AR VR Headset Test Bench: \n Command & Control Foundation", xlab="X", ylab="Y", type="n")
draw.circle(-80,-80,5,border="black",col="black")
draw.circle(-60,-60,10,border="black",col="tan3")
draw.circle(-40,-40,15,border="black",col="red")
draw.circle(-20,-20,15,border="black",col="orange")
draw.circle(0,0,20,border="black",col="yellow")
draw.circle(20,20,20,border="black",col="green")
draw.circle(40,40,20,border="black",col="blue")
draw.circle(60,60,15,border="black",col="violet")
draw.circle(80,80,10,border="black",col="grey")
draw.circle(100,100,5,border="black",col="white")




###  SECTION 2 - BUILD TEST MATRIX / INDEXED 
ccm.plot.reset <- function()
{
    map <<- matrix(c(1:50),ncol=5,byrow=TRUE)
    map
    colnames(map) <<- c("objectname","color","size","x","y")
    rownames(map) <<- c(1,2,3,4,5,6,7,8,9,10)
    map <<- data.frame(map)
    map$color <<- c("black","tan3","red","orange","yellow","green","blue","violet","grey","white")
    map$objectname <<- paste(map$color,"-node",sep="") # for now, let's clone colors to objectname
    map$x <<- seq(-90,90,20)
    map$y <<- seq(-90,90,20)
    map$size <<- 10
    map
    ccm.plot.dataframe()
}
ccm.plot.reset()
map

## Builds a fresh template
## plot(0, xlim=c(-120,120), ylim=c(-100,120), main="IBM Watson - AR VR Headset Test Bench: \n Command & Control Foundation", xlab="X", ylab="Y", type="n")


## SECTION 3 - PLOTS THE CONTENTS OF DATA FRAME
ccm.plot.dataframe <- function() {
    plot(0, xlim=c((min(map$x)-20),(max(map$x)+20)), ylim=c((min(map$y)-20),max(map$y)+20),main="IBM Watson - AR VR Headset Test Bench: \n Command & Control Foundation", xlab="X", ylab="Y", type="n")
    l <- dim(map)[1] # pulls the number of elements in list
    for(i in 1:l) {
      #print(map$color[i])
      draw.circle(map$x[i],map$y[i],map$size[i],border="black",col=map$color[i])
    }
}
ccm.plot.dataframe()

## SECTION 4 - Randomize the Locations  (DF is global)
ccm.plot.random <- function() {
  map$x <<- sample(-80:80,10)
  map$y <<- sample(-80:80,10)
  ccm.plot.dataframe()
}
map
ccm.plot.random()

## SECTION 5A  - Recieve one ID and a movement command (vectorshft)
ccm.modify.position <- function(index,shiftx,shifty) {
  # index <- index +1 # because index starts at 1, but black is 0
  map$x[index] <<- map$x[index]+shiftx
  map$y[index] <<- map$y[index]+shifty
  ccm.plot.dataframe()
}

ccm.modify.position(2, 10, 10) # item 3 moves 10 up and 10 over to right
ccm.modify.position(3, 0, 10) # item 3 moves 10 up and 10 over to right

## SECTION 5B - Recieve one NAME of ENTITY and MOVE ALL ELEMENTS 
ccm.modify.artifact.position <- function(objectname,shiftx,shifty) {
  map[map$objectname==objectname,]$x <<- map[map$objectname==objectname,]$x+shiftx  # hamfisty , but works
  map[map$objectname==objectname,]$y <<- map[map$objectname==objectname,]$y+shifty  # hamfisty , but works
  #map
  ccm.plot.dataframe()
}


## SECTION 5C -
ccm.modify.circle.size <- function(objectname,change,size) {
  # can either receive specific size, OR change in size, - if both, will assign former
  if(change!=0){map[map$objectname==objectname,]$size <<- map[map$objectname==objectname,]$size+change } # hamfisty , but works
  if(size!=0){  map[map$objectname==objectname,]$size <<- size }  # hamfisty , but works
  map
  ccm.plot.dataframe()
}
#ccm.modify.circle.size("orange-node",25,0) # node,change, or set size
#map

ccm.modify.size <- function(index,) {
  if(!is.null(size)){map$size[index] <<- size} else{map$size[index] <<- map$size[index]+change}
  ccm.plot.dataframe()
}


## SECTIon 5D - Later - allow to modify.artifact.LOCATION - will call Position, and prob calculate delta 'here to there' ? how to id the 'primary' node

##
map


ccm.plot.reset()
ccm.create.cat("cat1","red",-80,50)
ccm.create.tree("tree1",-25,-50)
ccm.create.tree("tree2",0,-60)
ccm.plot.dataframe()
map






## SECTION 6 - Translate utterance into math and index - MOVEMENT
left <<- -5
right <<- 5
up <<- 5
down <<- -5
same <<- 0
stay <<- 0
hold <<- 0

## ReSISTOR COLOR CODE IS N-1 here (row index made it hard to keep same)
black <<- 1
brown <<- 2
red <<- 3
orange <<- 4
yellow <<- 5
green <<- 6
blue <<- 7
violet <<- 8
grey1 <<- 9 # will not permit use of grey as name
white <<- 10

ccm.modify.position(green, right, hold) # item 3 moves 10 up and 10 over to right
ccm.modify.position(yellow, right, up) # item 3 moves 10 up and 10 over to right
ccm.modify.position(blue,left,down)

## SECTION 7 - Update SIZE of one shape (DOES NOT WORK FOR Array of shapes!!)
ccm.modify.size <- function(index,change,size) {
  # can either receive specific size, OR change in size, - if both, will assign former
  # index <- index + 1 # because index starts at 1, but black is 0 ## NOW NOT NEEDED?
  if(!is.null(size)){map$size[index] <<- size} else{map$size[index] <<- map$size[index]+change}
  ccm.plot.dataframe()
}

map
red
##  TESTING
## Increase or Decrease
ccm.modify.size(red,5,NULL)
ccm.modify.size(yellow,-5,NULL)
ccm.modify.size(green,8,NULL)
ccm.modify.size(white,5,NULL)
ccm.modify.size(white,0,30)
ccm.modify.size(orange,NULL,23)
ccm.modify.size(orange,NULL,-4)
ccm.modify.size(orange,6,NULL)

ccm.plot.random()
ccm.plot.reset()


## SECTION 8 - Translate intent to magnitude changes 
ccm.plot.dataframe()
"Please Increase orange orb by 50% and move up and to the left"
ccm.modify.size(orange,5,NULL)

# five moves up and too the left (vector AMPLITUDE)
for(i in 1:5) {ccm.modify.position(orange, left, up)}

## SECTION 9 - Let's do some Random Motion -
## TO DO - DOES NOT WORK WITH MULTPLE ENTITY ICONS - like CATS - CIRCLES ONLY - SUBSET?
## TO DO - DOES NOT WORK WITH MULTPLE ENTITY ICONS - like CATS - CIRCLES ONLY - SUBSET?
ccm.plot.random()
ccm.scramble.elements <- function(z)
  # receives integer of how many loops
  {
    for(i in 1:z) {
      print(i)
      temp_index <- sample(1:(dim(map)[1]),1) # pick a target for this loop
      temp_size <- sample(-2:2,1) # change the size -bigger or smaller
      temp_x_shift <- sample(-5:5,1) # move the location - X axis
      temp_y_shift <- sample(-5:5,1) # move the location - Y axis
      ccm.modify.size(temp_index,temp_size,NULL)
      ccm.modify.position(temp_index, temp_x_shift, temp_y_shift)
      Sys.sleep(0.1) # slow things down for 200ms
    }
}
map
ccm.plot.reset()
ccm.scramble.elements(29)


## SECTION 10 - Let's INVOKE a new element - color, size, and location 
ccm.plot.reset()

ccm.invoke.artifact <- function(temp_name,temp_color,temp_size,xlocate,ylocate) # function to create and plot a new kid on the block 
{
    # no error checking here - but maybe later.
    # Tried this - but caused problems elsewhere newrow <- c(temp_name,temp_color,temp_size,xlocate,ylocate) # map <<- rbind(map,newrow)
    temp_index <- dim(map)[1] + 1   # how many elements in the master map PLUS ONE FOR OUR NEW ITEM
    map[temp_index,]$objectname <<- temp_name
    print(map)
    map[temp_index,]$color <<- temp_color
    map[temp_index,]$size <<- temp_size
    map[temp_index,]$x <<- xlocate
    map[temp_index,]$y <<- ylocate
    row.names(map)[temp_index] <<- temp_index # not sure why, but row N+1 (but not N+2) gives N/A s- addded this to take care of 'first add' to N row df (11)
    print(map)
    }

## Test
ccm.plot.reset()
ccm.invoke.artifact("apple","red",6,-50,0)
ccm.invoke.artifact("grape","green",3,50,-50)
ccm.invoke.artifact("sun","yellow",25,-80,80)
map
ccm.plot.dataframe()

ccm.create.tree <- function(objectname,anchorx,anchory)
{
  ## members of the same object should share the same name - even if diff colors
  ccm.invoke.artifact(objectname,"green",2,anchorx,anchory)
  ccm.invoke.artifact(objectname,"green",2,anchorx-2,anchory-5)
  ccm.invoke.artifact(objectname,"green",2,anchorx+2,anchory-5)
  ccm.invoke.artifact(objectname,"green",2,anchorx-4,anchory-10)
  ccm.invoke.artifact(objectname,"green",2,anchorx,anchory-10)
  ccm.invoke.artifact(objectname,"green",2,anchorx+4,anchory-10)
  ccm.invoke.artifact(objectname,"green",2,anchorx-6,anchory-15)
  ccm.invoke.artifact(objectname,"green",2,anchorx-2,anchory-15)
  ccm.invoke.artifact(objectname,"green",2,anchorx+2,anchory-15)
  ccm.invoke.artifact(objectname,"green",2,anchorx+6,anchory-15)
  ccm.invoke.artifact(objectname,"green",2,anchorx-8,anchory-20)
  ccm.invoke.artifact(objectname,"green",2,anchorx-4,anchory-20)
  ccm.invoke.artifact(objectname,"green",2,anchorx,anchory-20)
  ccm.invoke.artifact(objectname,"green",2,anchorx+4,anchory-20)
  ccm.invoke.artifact(objectname,"green",2,anchorx+8,anchory-20)
  ccm.invoke.artifact(objectname,"brown",2,anchorx,anchory-25)
  ccm.invoke.artifact(objectname,"brown",2,anchorx,anchory-30)
  ccm.invoke.artifact(objectname,"brown",2,anchorx,anchory-35)
}

ccm.plot.reset()
ccm.create.tree("tree1",50,-10)
ccm.create.tree("tree2",80,-5)
ccm.plot.dataframe()
map

ccm.create.cat <- function(objectname,objectcolor,anchorx,anchory)
{
  ccm.invoke.artifact(objectname,objectcolor,10,anchorx,anchory) # BIG CAT HEAD
  
  ccm.invoke.artifact(objectname,"white",2,anchorx+3,anchory+6) # CAT EYE1
  ccm.invoke.artifact(objectname,"black",1,anchorx+3,anchory+5) # CAT EYE1 - Pupil 2
  ccm.invoke.artifact(objectname,"white",2,anchorx-3,anchory+6) # CAT EYE2
  ccm.invoke.artifact(objectname,"black",1,anchorx-3,anchory+5) # CAT EYE2 - Pupil 2
  
  ccm.invoke.artifact(objectname,"white",1,anchorx-1,anchory) # CAT NOSE
  ccm.invoke.artifact(objectname,"white",1,anchorx+1,anchory) # CAT NOSE
  ccm.invoke.artifact(objectname,"white",1,anchorx,anchory-2) # CAT NOSE
  ccm.invoke.artifact(objectname,"white",1,anchorx,anchory-4) # CAT NOSE
  # later, learn arc or lines for cat whiskers
  
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-7,anchory+13) # CAT EAR1 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-5,anchory+14) # CAT EAR1 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-3,anchory+15) # CAT EAR1 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-6,anchory+16) # CAT EAR1 2
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-4,anchory+17) # CAT EAR1 2
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx-5,anchory+19) # CAT EAR1 1
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+7,anchory+13) # CAT EAR2 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+5,anchory+14) # CAT EAR2 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+3,anchory+15) # CAT EAR2 3
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+6,anchory+16) # CAT EAR2 2
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+4,anchory+17) # CAT EAR2 2
  ccm.invoke.artifact(objectname,objectcolor,1,anchorx+5,anchory+19) # CAT EAR2 2
  ccm.invoke.artifact(objectname,"white",1,anchorx-4,anchory-6) # CAT SMILE
  ccm.invoke.artifact(objectname,"white",1,anchorx-2,anchory-7) # CAT SMILE
  ccm.invoke.artifact(objectname,"white",1,anchorx,anchory-8) # CAT SMILE
  ccm.invoke.artifact(objectname,"white",1,anchorx+2,anchory-7) # CAT SMILE
  ccm.invoke.artifact(objectname,"white",1,anchorx+4,anchory-6) # CAT SMILE
}

## SECTION 11 - let's reset our test bed with multiple objects
ccm.plot.reset.testbed <- function ()
{
    ccm.plot.reset()
    ccm.create.cat("cat1","red",-80,50)
    ccm.create.cat("cat2","black",-40,50)
    ccm.create.cat("cat3","blue",40,-50)
    ccm.create.cat("cat4","pink",70,-50)
    ccm.create.tree("tree1",-25,-50)
    ccm.create.tree("tree2",0,-60)
    ccm.plot.dataframe()
    #map
}
ccm.plot.reset.testbed()  ## this plots rainbow line plus 4 cats and 2 trees

## SECTION 12 - MOVE ARTIFACT (CONTAINS MULTIPLE SHAPES) ## TESTING MOVING EVERYTHING - see 5b for fucntion
ccm.modify.artifact.position("tree2",right,up)
ccm.modify.artifact.position("cat1",left,up)
for(i in 1:5){ccm.modify.artifact.position("orange",same,up)} # via loop
ccm.modify.artifact.position("blue",0,20) # via direct x y deltat

## Lets push RED around
ccm.modify.artifact.position("red",0,30) # via direct x y deltat
ccm.modify.size(red,5,NULL) # request change in size +//
ccm.modify.size(red,NULL,10) # assign actual size 

## Reset
ccm.plot.reset.testbed()  ## this plots rainbow line plus 4 cats and 2 trees
map

## SECTION 13 - CHANGE EXISTING ARTEFACT TRAITS - e.g. Color, or NameID or location - for now (more later)
ccm.modify.artifact <- function(objectname,modify_trait,new_attribute) {
  if(modify_trait=="color"){
    map[map$objectname==objectname & map$color!="white",]$color <<- new_attribute  
  }
  
  if(modify_trait=="objectname"){
    map[map$objectname==objectname,]$objectname <<- new_attribute
  }
  ccm.plot.dataframe()
}
ccm.modify.artifact()
map
#ccm.modify.artifact(objectname,modify_trait,new_attribute)
ccm.modify.artifact.position("tree1",right,up)
ccm.modify.artifact("tree2","objectname","tree222")
ccm.modify.artifact.position("tree222",right,up)

ccm.modify.artifact("cat1","color","lightblue")  
ccm.modify.artifact("cat2","color","orange") 
ccm.modify.artifact("cat3","color","green")  
ccm.modify.artifact("cat4","color","red")  
ccm.modify.artifact.position("cat4",right,up)
ccm.modify.artifact("cat4","objectname","cat444")
ccm.modify.artifact("cat444","color","yellow") 
ccm.modify.artifact.position("cat444",right,up)
ccm.modify.artifact.position("orange-node",left,up)

## Reset
ccm.plot.reset.testbed()  ## this plots rainbow line plus 4 cats and 2 trees


map





# The postings on this site are my own and don't necessarily represent IBM. 
# Anyone is free to use, copy, distribute, modify or sell the source code here or on GitHub and use other materials directly 
# linked from dreamtolearn.com and is provided "as is" without warranties. 
# I am not responsible for any harm or damage caused to your computer, software or anything else caused by the material. (So handle with care :)
