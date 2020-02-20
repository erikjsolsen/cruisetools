## Created by Espen Johnsen
## modified by Jennifer - is still pretty rough

depth.contour<-function(depth,col="grey80",lty="solid",lwd=0.5,big=F){ # big uses larger files covering N Atlantic but is slower
  if(big==F) raw.data<-scan(file=paste("C:\\Program Files\\R\\R-3.2.3\\library\\contours\\",as.character(depth),"m.coa",sep=""),what="character",sep="\n")
  else raw.data<-scan(file=paste("C:\\Program Files\\R\\R-3.2.3\\library\\contours-L\\",as.character(depth),"m.coa",sep=""),what="character",sep="\n")
  i<-1
  while(i<length(raw.data)){
    points<-raw.data[i]
    points<-as.numeric(substring(points,1,regexpr(" ",as.character(points))-1))
    temp<-raw.data[(i+1):(i+points)]
    lat<-as.numeric(substring(temp,1,8))
    lon<-as.numeric(substring(temp,10,18))
    lines(lon,lat,col=col,lty=lty,lwd=lwd)
    i<-i+points+1
  }
}



icesmap <- function(xMin=-5,xMax=13,yMin=49,yMax=62,int.x=1, int.y=0.5, Q1.lines=T,ICES.squares=T,Q1.text=F,species=NA){
#  icesmap <- function(xMin=-5,xMax=13,yMin=49,yMax=62,int.x=1, int.y=0.5, Q1.lines=T,ICES.squares=T,Q1.text=T,species='cod'){
#-
library(maps); library(mapdata)
degree.in <- function(n){
    my.names <- NULL 
    for(i in n)
       my.names <- c(my.names,eval(as.expression(substitute(expression(i * degree), list(i=i))))) 
}

degree.axis <- function(side=1,pos=NULL){
	 if (side==1 || side==3) tmp <- par()$xaxp else tmp <- par()$yaxp
	 if(is.null(pos)) pos <- seq(tmp[1], tmp[2], length=tmp[3])
	 else pos <- pos
	 if (side==1 || side==3) tmp <- par()$xaxp else tmp <- par()$yaxp    
	 axis(side=side, at=pos, lab=degree.in(pos),cex.axis=0.7)
}

# Suppress graphics warnings 
#  if(is.null(limX[1]))
#xMin <- -3; xMax <- 10
#yMin <- 56; yMax <- 62
xA <- c(min(seq(xMin,xMax,int.x)),max(seq(xMin,xMax,int.x)),length(seq(xMin,xMax,int.x)))
yA <- c(min(seq(yMin,yMax,int.y)),max(seq(yMin,yMax,int.y)),length(seq(yMin,yMax,int.y)))
par(las=1)

map('worldHires', fill=T, col="gray60",
#    plot(map[,1],map[,2],type="n",
         xlab="lon",ylab="lat",main="",sub="",resolution = 0,
         xlim=c(xMin*0.99,xMax*1.01),
         ylim=c(yMin*0.999,yMax*1.001))          
box()        

par(xaxp=xA)
par(yaxp=yA)
degree.axis(1)
degree.axis(2)
#grid(lty=1)

xA1  <- seq(xMin,xMax,int.x)
yA1  <- seq(yMin,yMax,int.y)

##grid.X
for(i in 1:length(xA1)){
   lines(rep(xA1[i],2),c(yMin*0.9,yMax*1.1),lty="dotted",col="gray50")
   }

##grid.Y
for(i in 1:length(yA1)){
   lines(c(xMin*0.9,xMax*1.1), rep(yA1[i],2),lty="dotted",col="gray50")
   }
## depth contours
#depth.contour(100,col="grey85")
#depth.contour(200,col="grey85")
   
# ICES squares
if(ICES.squares==T) {
    #a3 <- c(paste("E",5:9,sep=""),paste("F",0:9,sep=""),paste("G",0:2,sep=""))
    a3 <- c(paste("E",7:9,sep=""),paste("F",0:9,sep=""))
    #a4 <- 27:52
    a4 <- 41:52
    axis(4,at=seq(par()$yaxp[1],par()$yaxp[2],length=par()$yaxp[3]),labels=rep(" ",par()$yaxp[3]),cex.axis=0.7)
    #axis(4,at=(seq(par()$yaxp[1],par()$yaxp[2],length=par()$yaxp[3])+0.25)[1:26],labels=a4,tck=0,cex.axis=0.7)
    axis(4,at=(seq(par()$yaxp[1],par()$yaxp[2],length=par()$yaxp[3])+0.25)[1:12],labels=a4,tck=0,cex.axis=0.7)
    axis(3,at=seq(par()$xaxp[1],par()$xaxp[2],length=par()$xaxp[3]),labels=rep(" ",par()$xaxp[3]),cex.axis=0.7)
    #axis(3,at=(seq(par()$xaxp[1],par()$xaxp[2],length=par()$xaxp[3])+0.5)[1:18],labels=a3,tck=0,cex.axis=0.7)
    axis(3,at=(seq(par()$xaxp[1],par()$xaxp[2],length=par()$xaxp[3])+0.5)[1:13],labels=a3,tck=0,cex.axis=0.7)
  } 

## Q1 survey area in 2017
if(Q1.lines==T)   {
      lines(c(-1,2),rep(62,2),lwd=3)
      lines(c(-1,-1),c(60.5,60),lwd=3)
      lines(c(-1,-1),c(61.5,62),lwd=3)
      lines(c(-1,0),c(61.5,61.5),lwd=3)
      lines(c(0,0),c(61.5,60.5),lwd=3)
      lines(c(-1,0),c(60.5,60.5),lwd=3)
      lines(c(-1,0),rep(60,2),lwd=3)
      lines(c(2,2),c(61.5,62),lwd=3)
      lines(c(2,3),c(61.5,61.5),lwd=3)
      lines(c(3,3),c(61,61.5),lwd=3)
      lines(c(3,4),c(61,61),lwd=3)
      lines(c(4,4),c(61,58.5),lwd=3)
      lines(c(4,5),rep(58.5,2),lwd=3)
      lines(rep(5,2),c(58.5,58),lwd=3)
      lines(c(5,6),rep(58,2),lwd=3)
      lines(rep(6,2),c(58,57.5),lwd=3)
      lines(c(6,7),rep(57.5,2),lwd=3)
      lines(rep(7,2),c(57.5,57),lwd=3)
      lines(c(2,7),rep(57,2),lwd=3)
      lines(rep(2,2),c(59,57),lwd=3)
      lines(c(0,2),rep(59,2),lwd=3)      
      lines(rep(0,2),c(59,60),lwd=3)
  } 
if(Q1.text==T) {
      text(1.5,61.25,'2')
      text(2.5,61.25,'2')
      text(1.5,60.75,'2')
      text(2.5,60.75,'2')
      text(3.5,60.75,'2')
      text(5.5,57.75,'2')
   }
   

## index areas for species
if(species=='Gadus morhua') {
     lines(c(-4,5),rep(62,2),lwd=3)
     lines(rep(-4,2),c(62,57.5),lwd=3)
     lines(c(-4,-2),rep(60.5,2),lwd=3)
     lines(rep(-2,2),c(60.5,61),lwd=3)
     lines(c(-2,0),rep(61,2),lwd=3)
     lines(rep(0,2),c(61,59.5),lwd=3)
     lines(c(-1,0),rep(59.5,2),lwd=3)
     lines(rep(-1,2),c(59.5,58),lwd=3)
     lines(c(-1,4),rep(58,2),lwd=3)
     lines(rep(4,2),c(58,56),lwd=3)
     lines(c(4,10),rep(57,2),lwd=3) #bottom of red
     lines(c(2,4),rep(56,2),lwd=3)
     lines(rep(2,2),c(55.5,56),lwd=3)
     lines(c(1,2),rep(55.5,2),lwd=3)
     lines(rep(1,2),c(55.5,55),lwd=3)
     lines(c(-1.5,1),rep(55,2),lwd=3)  #bottom of blue-to-green
     lines(rep(-2,2),c(51,49),lwd=3)
     lines(c(-2,0),rep(49,2),lwd=3)
     lines(rep(8,2),c(57,57.5),lwd=3)
     lines(c(7,8),rep(57.5,2),lwd=3)
     lines(rep(7,2),c(57.5,58.2),lwd=3)
     lines(c(10,12),rep(57.5,2),lwd=3)
     lines(rep(10,2),c(57,57.5),lwd=3)
     lines(rep(-1,2),c(55,54),lwd=3)
     lines(c(-2,-1),rep(55,2),lwd=3)
     lines(rep(-2,2),c(55,56),lwd=3)
     lines(c(-2,-3),rep(56,2),lwd=3)
     lines(rep(-3,2),c(57,56),lwd=3)
     lines(c(-2,-3),rep(57,2),lwd=3)
     lines(rep(-2,2),c(57,57.5),lwd=3)
     lines(c(-2,-4),rep(57.5,2),lwd=3)
     lines(rep(10,2),c(57,57.5),lwd=3)
     lines(c(10,8),rep(57,2),lwd=3)
     lines(rep(8,2),c(57,55.5),lwd=3)
     lines(c(8,9),rep(55.5,2),lwd=3)
     lines(rep(9,2),c(55.5,54),lwd=3)
     lines(c(8,9),rep(54,2),lwd=3)
     lines(rep(8,2),c(54,53.5),lwd=3)
     lines(c(8,5),rep(53.5,2),lwd=3)
     lines(rep(5,2),c(52,53.5),lwd=3)
     lines(c(4,5),rep(52,2),lwd=3)
     lines(rep(4,2),c(52,51.5),lwd=3)
     lines(c(4,3),rep(51.5,2),lwd=3)
     lines(rep(3,2),c(51,51.5),lwd=3)
     lines(c(2,3),rep(51,2),lwd=3)
     lines(rep(1,2),c(51,53),lwd=3)
     lines(c(0,1),rep(53,2),lwd=3)
     lines(rep(0,2),c(54,53),lwd=3)
     lines(c(0,-1),rep(54,2),lwd=3)
     lines(rep(2,2),c(50,51),lwd=3)
     lines(c(1,2),rep(50,2),lwd=3)
     lines(rep(1,2),c(50,49.5),lwd=3)
     lines(c(0,1),rep(49.5,2),lwd=3)
     lines(rep(0,2),c(49.5,49),lwd=3)
     lines(c(1,-2),rep(51,2),lwd=3)
     lines(rep(8,2),c(57,58),lwd=3)
     lines(c(8,9),rep(58,2),lwd=3)
     lines(rep(9,2),c(58,58.5),lwd=3)
     lines(c(9,10),rep(58.5,2),lwd=3)
     lines(rep(10,2),c(59,58.5),lwd=3)
     lines(c(10,12),rep(59,2),lwd=3)
     lines(rep(12,2),c(59,57.5),lwd=3)

 }

if(species=='Pleuronectes platessa') {
  ## 3a area
     lines(rep(8,2),c(57,58),lwd=3)
     lines(c(8,9),rep(58,2),lwd=3)
     lines(rep(9,2),c(58,58.5),lwd=3)
     lines(c(9,10),rep(58.5,2),lwd=3)   
     lines(rep(10,2),c(59,58.5),lwd=3)
     lines(c(10,11),rep(59,2),lwd=3)
     lines(rep(11,2),c(59,58.5),lwd=3)
     lines(c(11,12),rep(58.5,2),lwd=3)
     lines(rep(12,2),c(58.5,57.5),lwd=3)
     lines(c(12,10),rep(57.5,2),lwd=3)

     lines(rep(10,2),c(57,57.5),lwd=3)
     lines(c(10,8),rep(57,2),lwd=3)## end 3a area
     lines(rep(8,2),c(57,55.5),lwd=3)
     lines(c(8,9),rep(55.5,2),lwd=3)
     lines(rep(9,2),c(55.5,54),lwd=3)
     lines(c(8,9),rep(54,2),lwd=3)
     lines(rep(8,2),c(54,53.5),lwd=3)
     lines(c(8,5),rep(53.5,2),lwd=3)
     lines(rep(5,2),c(52,53.5),lwd=3)
     lines(c(4,5),rep(52,2),lwd=3)
     lines(rep(4,2),c(52,51.5),lwd=3)
     lines(c(4,3),rep(51.5,2),lwd=3)
     lines(rep(3,2),c(51,51.5),lwd=3)
     lines(c(2,3),rep(51,2),lwd=3)
     lines(rep(1,2),c(51,53),lwd=3)
     lines(c(0,1),rep(53,2),lwd=3)
     lines(rep(0,2),c(54,53),lwd=3)
     lines(c(0,-1),rep(54,2),lwd=3)
     lines(rep(-1,2),c(55,54),lwd=3)
     lines(c(-2,-1),rep(55,2),lwd=3)
     lines(rep(-2,2),c(55,56),lwd=3)
     lines(c(-2,-3),rep(56,2),lwd=3)
     lines(rep(-3,2),c(57,56),lwd=3)
     lines(c(-2,-3),rep(57,2),lwd=3)
     lines(rep(-2,2),c(57,57.5),lwd=3)
     lines(c(-2,-4),rep(57.5,2),lwd=3)

#exclude lon>-0.7 & lon<5 & lat>58.24 & lat<62
# include 4abc, 3a20
     lines(c(5,-0.7),rep(58.24,2),lwd=3)
     lines(rep(5,2),c(58.24,58),lwd=3)
     lines(c(7,8),rep(57.5,2),lwd=3)
     lines(rep(7,2),c(57.5,58),lwd=3)
     lines(c(7,5),rep(58,2),lwd=3)
     lines(rep(-0.7,2),c(58.24,62),lwd=3)
     lines(c(-0.7,-1),rep(62,2),lwd=3) 
     lines(rep(-1,2),c(61.5,62),lwd=3)
     lines(c(-1,-2),rep(61.5,2),lwd=3)
     lines(rep(-2,2),c(61.5,60),lwd=3)
     lines(c(-2,-3),rep(60,2),lwd=3)
     lines(rep(-3,2),c(60,58.5),lwd=3)
     lines(c(-3,-4),rep(58.5,2),lwd=3)
     lines(rep(-4,2),c(58.5,57.5),lwd=3)
     lines(c(1,2),rep(51,2),lwd=3)    
  }

if(species=='Melanogrammus aeglefinus') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\hadstand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Merlangius merlangus') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\whistand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Sprattus sprattus') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\spratstand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Scomber scombrus') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\macstand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Trisopterus esmarkii') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\npoutstand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Pollachius virens') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\saithestand_new.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

if(species=='Clupea harengus') {
   z<-read.csv("C:\\Users\\jenniferd\\Documents\\_2017\\IBTSWG\\data\\index areas\\herringstand.bln",header=F,skip=1)
   for(i in 1:(nrow(z)-1)) {
     lines(c(z[i,1],z[i+1,1]),c(z[i,2],z[i+1,2]),lwd=3)
   }
 }

# add box names to grid


}



## pre 2017 Q1 sampling area
#if(Q1.lines==T)   {
#      lines(c(-1,2),rep(62,2),lwd=3)
#      lines(c(-1,-1),c(60.5,60),lwd=3)
#      lines(c(-1,-1),c(61.5,62),lwd=3)
#      lines(c(-1,0),c(61.5,61.5),lwd=3)
#      lines(c(0,0),c(61.5,60.5),lwd=3)
#      lines(c(-1,0),c(60.5,60.5),lwd=3)
#      lines(c(-1,0),rep(60,2),lwd=3)
#      lines(c(2,2),c(61.5,62),lwd=3)
#      lines(c(2,3),c(61.5,61.5),lwd=3)
#      lines(c(3,3),c(61,61.5),lwd=3)
#      lines(c(3,4),c(61,61),lwd=3)
#      lines(c(4,4),c(61,58.5),lwd=3)
#      lines(c(4,5),rep(58.5,2),lwd=3)
#      lines(rep(5,2),c(58.5,58),lwd=3)
#      lines(c(5,6),rep(58,2),lwd=3)
#      lines(rep(6,2),c(58,57.5),lwd=3)
#      lines(c(6,8),rep(57.5,2),lwd=3)
#      lines(rep(8,2),c(57.5,56.5),lwd=3)
#      lines(c(3,8),rep(56.5,2),lwd=3)
#      lines(rep(3,2),c(57.5,56.5),lwd=3)
#      lines(c(3,2),rep(57.5,2),lwd=3)
#      lines(rep(2,2),c(57.5,59),lwd=3)
#      lines(c(0,2),rep(59,2),lwd=3)
#      lines(rep(0,2),c(60,59),lwd=3)
#  } 