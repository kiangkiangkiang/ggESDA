#' @name ggInterval_2Dhist
#' @title visualize a 2-dimension histogram by symbolic data with ggplot
#' package.
#' @description Visualize the two continuous variable distribution
#' by dividing both the x axis and y axis into bins,and calculating
#' the frequency of observation interval in each bin.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @importFrom dplyr between
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param xBins x axis bins,which mean how many partials
#' x variable will be separate into.
#' @param yBins y axis bins.It is the same as xBins.
#' @param removeZero whether remove data whose frequency is equal to zero
#' @param addFreq where add frequency text in each cells.
#' @return Return a ggplot2 object.
#' @usage ggInterval_2Dhist(data = NULL,mapping = aes(NULL)
#' ,xBins = 14,yBins=16)
#' @examples
#' #a classical data input
#' ggInterval_2Dhist(mtcars,aes(x=disp,y=wt))
#' ggInterval_2Dhist(iris,aes(x=iris$Sepal.Length,y=iris[,3]),xBins=30)
#' ggInterval_2Dhist(mtcars,aes(disp,wt),xBins=23,yBins=35)
#'
#' #you can add and aesthetic like colour and alpha
#' p<-ggInterval_2Dhist(mtcars,aes(x=disp,y=wt,col="black",alpha=0.8))
#' p
#' #adjust fill manual you like
#' p+scale_fill_gradient2(low = "green",mid="grey",high = "red",
#'     midpoint = (max(p$data$freq)+min(p$data$freq))/2)
#'
#' #a symbolic data input (ex.RSDA dataset:Cardiological)
#' #it can also be produce by classic2sym
#' mydata<-RSDA::Cardiological #generate symbolic data
#' ggInterval_2Dhist(mydata,aes(Pulse,Syst))
#' p<-ggInterval_2Dhist(mydata,aes(Pulse,Syst,col="red",lty=2))
#' p
#' p+theme_classic()+labs(title="My 2d plot")
#' p+scale_x_continuous(breaks=c(50,60,80,100),
#'     labels=c("aa","bb","cc","dd"))+geom_point(aes(x=60,y=150),pch=16,size=3)
#'
#' #mark a particular interval
#' p+geom_rect(data=p$data,aes(xmin=p$data[10,"x1"],xmax=p$data[10,"x2"],
#'     ymin=p$data[10,"y1"],ymax=p$data[10,"y2"]),fill="red")
#'
#'
#' @export
ggInterval_2Dhist<- function(data = NULL,mapping = aes(NULL),
                             xBins = 14,yBins=16,removeZero = F,
                             addFreq = T){

  #test big O
  if(xBins+yBins>200) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]

  #test big O
  if(n>=75 & n<200){
    warning("The number of observations are not suggested greater than 75.")
  }else if(n>=200){
    stop("The number of observations are too large that will out of time limit.
         Suggested number should be less than 75.")
  }


  #start process
  with(data,{
    #get attrs
    attr1<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.x))))
    attr1<-names(attr1)
    attr2<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.y))))
    attr2<-names(attr2)
    #if cannot find attr
    if(length(attr1)==0 || length(attr2)==0){
      stop("ERROR : Missing attributes x or y in data frame.")
    }

    #test attribute illegal
    if( all((!is.numeric(data[[attr1]])) ,!RSDA::is.sym.interval(data[[attr1]]) )
        ||  all((!is.numeric(data[[attr2]])), !RSDA::is.sym.interval(data[[attr2]]))){
      stop("ERROR : Variables in Scatter Plot can only be numeric.")
    }


    #calculate each frequency
    x.freq <- as.data.frame(calFreq(attr1,data=ggSymData))
    y.freq <- as.data.frame(calFreq(attr2,data=ggSymData))

    #prepare loop data
    r<-8
    minX<-min(iData[[attr1]]$min)
    maxX<-max(iData[[attr1]]$max)
    minY<-min(iData[[attr2]]$min)
    maxY<-max(iData[[attr2]]$max)
    distX<-(maxX-minX)/r
    distY<-(maxY-minY)/r
    intervalX<-seq(minX,maxX,distX)
    intervalY<-seq(minY,maxY,distY)
    recX <- seq(minX,maxX,(maxX-minX)/xBins)
    recY <- seq(minY,maxY,(maxY-minY)/yBins)
    allObs <- data.frame(
      x1=iData[[attr1]]$min,
      x2=iData[[attr1]]$max,
      y1=iData[[attr2]]$min,
      y2=iData[[attr2]]$max
    )
    freq.Rectangle <- matrix(0,nrow=xBins,ncol=yBins)

    #start loop to calculate frequency values in histogram matrix

    tryCatch({
      for(rx in 1:(length(recX)-1)){
        for(ry in 1:(length(recY)-1)){
          for(obs in 1:nrow(allObs)){
            fx<-0;fy<-0
            for(areaX in 1:(length(x.freq))){
              if(x.freq[obs,areaX]!=0){
                headIn<-intervalX[areaX]%>%between(recX[rx],recX[rx+1])
                tailIn<-intervalX[areaX+1]%>%between(recX[rx],recX[rx+1])
                contain<-recX[rx]%>%between(intervalX[areaX],intervalX[areaX+1])
                if(headIn|tailIn|contain){
                  temp<-sort(c(recX[rx],recX[rx+1],intervalX[areaX],intervalX[areaX+1]))
                  fx<-fx+((temp[3]-temp[2])/(intervalX[areaX+1]-intervalX[areaX]))*x.freq[obs,areaX]
                }
              }
            }
            for(areaY in 1:(length(y.freq))){
              if(y.freq[obs,areaY]!=0){
                headIn<-intervalY[areaY]%>%between(recY[ry],recY[ry+1])
                tailIn<-intervalY[areaY+1]%>%between(recY[ry],recY[ry+1])
                contain<-recY[ry]%>%between(intervalY[areaY],intervalY[areaY+1])
                if(headIn|tailIn|contain){
                  temp<-sort(c(recY[ry],recY[ry+1],intervalY[areaY],intervalY[areaY+1]))
                  fy<-fy+((temp[3]-temp[2])/(intervalY[areaY+1]-intervalY[areaY]))*y.freq[obs,areaY]
                }
              }
            }
            freq.Rectangle[rx,ry] <- freq.Rectangle[rx,ry]+fx*fy
          }
        }
      }
    },error = function(err) {
       print("Please try it again.")
       return()
    })

    #end loop for calculate

    #build data frame to plot (combine margin and values)
    freq.matrix<-(c(as.matrix(freq.Rectangle)))
    freq.matrix<-cbind(freq.matrix,
                       rep(recX[1:length(recX)-1],length(recY)-1),
                       rep(recX[2:length(recX)],length(recY)-1),
                       rep(recY[1:length(recY)-1],each=length(recX)-1),
                       rep(recY[2:length(recY)],each=length(recX)-1)
    )
    colnames(freq.matrix)<-c("freq","x1","x2","y1","y2")
    freq.matrix<-as.data.frame(freq.matrix)
    #freq.matrix$freq <- freq.matrix$freq/dim(iData)[1]
    freq.matrix[,"xmid"] <- (freq.matrix$x1+freq.matrix$x2)/2
    freq.matrix[,"ymid"] <- (freq.matrix$y1+freq.matrix$y2)/2

    #escape sparse matrix
    if(removeZero){
      freq.matrix<-freq.matrix[freq.matrix$freq!=0,]
    }
    m <- (max(freq.matrix$freq)+min(freq.matrix$freq))/2

    #build Aesthetic
    usermapping <- mapping[-c(1,2)] #Aesthetic without x,y
    mymapping <- list(data=freq.matrix,
                      mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                                  fill=freq,alpha=0.5))
    allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))


    #plot
    base <- ggplot(freq.matrix,aes(x1,y1))+
      do.call(geom_rect,allmapping)+
      scale_fill_gradient2(low = "blue",mid="yellow",
                           high = "red",midpoint = m,
                           limits=c(0,max(freq.matrix$freq)))+
      labs(x=attr1,y=attr2,title="2D hist.")+
      guides(alpha = FALSE)
    if(addFreq){
      base<-base+geom_text(aes(x=xmid,y=ymid,label=round(freq,1)))
    }
    return(base)
  })
}

calFreq <- function (attr,data=NULL,r=8){
  iData <- data$intervalData
  minimal<-min(iData[[attr]]$min)
  maximal<-max(iData[[attr]]$max)
  dist<-(maximal-minimal)/r
  interval<-seq(minimal,maximal,dist)

  f <- matrix(nrow=length(iData[[attr]]),ncol=r)
  for (obs in 1:length(iData[[attr]])){
    a<-iData[[attr]][obs]$min
    b<-iData[[attr]][obs]$max
    for(area in 1:r){
      headIn<-dplyr::between(a,interval[area],interval[area+1])
      tailIn<-dplyr::between(b,interval[area],interval[area+1])
      contain<-dplyr::between(interval[area],a,b)
      if(headIn|tailIn|contain){
        temp<-sort(c(a,b,interval[area],interval[area+1]))
        f[obs,area]<-(temp[3]-temp[2])/(b-a)
      }else{
        f[obs,area]<-0
      }
    }
  }
  return(f)
}
