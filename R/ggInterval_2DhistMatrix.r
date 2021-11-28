#' @name ggInterval_2DhistMatrix
#' @title 2-Dimension histogram matrix
#' @description  Visualize the all continuous variable distribution
#' by dividing both the x axis and y axis into bins,and calculating
#' the frequency of observation interval in each bin.Eventually
#' show it by a matrix plot.Note:this function will automatically
#' filter out the discrete variables,and plot all continuous in
#' input data,so it can not be necessary that give the particularly
#' variables in aes such like (aes(x=x,y=y)).It isn't also
#' recommended to deal with too many variables because the
#' big O in calculating full matrix will be too large.
#' @import tidyverse rlang ggplot2 grid
#' @importFrom RSDA is.sym.interval
#' @importFrom dplyr between
#' @importFrom gridExtra marrangeGrob
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param xBins x axis bins,which mean how many bins
#' x variable will be separate into
#' @param yBins y axis bins.It is the same as xBins
#' @param removeZero whether remove data whose frequency is equal to zero
#' @param addFreq where add frequency text in each cells.
#' @return Return a plot with no longer a ggplot2 object,instead
#' of a marrangeGrob object.
#' @usage ggInterval_2DhistMatrix(data = NULL,mapping = aes(NULL)
#' ,xBins = 14,yBins=16)
#'
#' @examples
#' ggInterval_2DhistMatrix(iris,aes(col="black",alpha=0.8))
#'
#' mydata<-RSDA::Cardiological
#' ggInterval_2DhistMatrix(mydata)
#'
#'
#'
#' @export
ggInterval_2DhistMatrix<-function(data = NULL,mapping = aes(NULL),
                          xBins = 8,yBins=8,removeZero = F,
                          addFreq = T){
  #test big O
  if(xBins+yBins>100) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]

  #test clearly visualize
  if(p>6 & p<=15){
    warning("It is not recommended number of variables are greater than 4.")
  }else if(p > 15){
    stop("The number of variables are too large to visualize clearly.
         Suggested input variables less than 4. ex. data[,1:4]")
  }

  #test big O
  if(n>50){
    stop("Out of time limits.Suggested number of observations should be less than 50.")
  }
  if(n*p>200){
    stop("Out of time limits.Suggested dimension should be less than (50 x 4).")
  }



  numericData <- unlist(lapply(iData[,1:p] ,FUN = RSDA::is.sym.interval))
  iData <- iData[,numericData]
  p<-dim(iData)[2]

  #now iData only have numeric data
  if(dim(iData)[2]<dim(data)[2]){
    p<-dim(iData)[2]; n<-dim(iData)[1]
    warning("Ignore non-numeric data.")
  }

  freq.matrix<-NULL
  for(i in 1:p){
    for(u in 1:p){
      freq.matrix <- rbind(freq.matrix,
                       data.frame(hist2d(data=ggSymData,
                                         attr1=i,
                                         attr2=u,
                         xBins=xBins,yBins=yBins,args=args),
                         xv = colnames(iData)[i],
                         yv = colnames(iData)[u]))
    }
  }
  freq.matrix[,"xmid"] <- (freq.matrix$x1+freq.matrix$x2)/2
  freq.matrix[,"ymid"] <- (freq.matrix$y1+freq.matrix$y2)/2
  #escape sparse matrix
  if(removeZero){
    freq.matrix<-freq.matrix[freq.matrix$freq!=0,]
  }
  m <- (max(freq.matrix$freq)+min(freq.matrix$freq))/2

  #build Aesthetic
  usermapping <- args
  mymapping <- list(data=freq.matrix
                    ,mapping=aes(xmin=freq.matrix$x1, xmax=freq.matrix$x2,
                                 ymin=freq.matrix$y1, ymax=freq.matrix$y2,
                                 fill=freq.matrix$freq)
                    , alpha=0.5)
  allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

  #plot
  base <- ggplot(data=freq.matrix, aes(freq.matrix$x1,freq.matrix$y1))+
    do.call(geom_rect,allmapping)+
    facet_grid(yv~xv, scale="free")+
    scale_fill_gradient2(name="frequency",
                       low = "blue",mid="yellow",
                       high = "red",midpoint = m,
                       limits=c(0,max(freq.matrix$freq)))+
    labs(x="",y="")

  if(addFreq){
    base <- base + geom_text(aes(x=xmid,y=ymid,label=round(freq,1)))
  }
  return(base)

}

hist2d <- function(data = NULL,attr1,attr2,xBins,yBins,args){
  #start process
  iData<-data$intervalData

    #calculate each frequency
    x.freq <- as.data.frame(calFreq(attr1,data=data))
    y.freq <- as.data.frame(calFreq(attr2,data=data))

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
    for(rx in 1:(length(recX)-1)){
      for(ry in 1:(length(recY)-1)){
        for(obs in 1:nrow(allObs)){
          fx<-0;fy<-0
          for(areaX in 1:(length(x.freq))){
            if(x.freq[obs,areaX]!=0){
              headIn<-dplyr::between(intervalX[areaX],recX[rx],recX[rx+1])
              tailIn<-dplyr::between(intervalX[areaX+1],recX[rx],recX[rx+1])
              contain<-dplyr::between(recX[rx],intervalX[areaX],intervalX[areaX+1])
              if(headIn|tailIn|contain){
                temp<-sort(c(recX[rx],recX[rx+1],intervalX[areaX],intervalX[areaX+1]))
                fx<-fx+((temp[3]-temp[2])/(intervalX[areaX+1]-intervalX[areaX]))*x.freq[obs,areaX]
              }
            }
          }
          for(areaY in 1:(length(y.freq))){
            if(y.freq[obs,areaY]!=0){
              headIn<-dplyr::between(intervalY[areaY],recY[ry],recY[ry+1])
              tailIn<-dplyr::between(intervalY[areaY+1],recY[ry],recY[ry+1])
              contain<-dplyr::between(recY[ry],intervalY[areaY],intervalY[areaY+1])
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
    freq.matrix$freq <- freq.matrix$freq/dim(iData)[1]
    return(freq.matrix)
}
