#' @name ggInterval_centerRange
#' @title Figure with x-axis=center y-axis=range
#' @description  Visualize the relation between center and range.
#' @import ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @return Return a ggplot2 object.
#' @usage ggInterval_centerRange(data = NULL,mapping = aes(NULL))
#' @examples
#' ggInterval_centerRange(iris,aes(iris$Sepal.Length))
#'
#' mydata<-RSDA::facedata
#' ggInterval_centerRange(mydata,aes(AD,col="blue",pch=2))
#' @export
ggInterval_centerRange<- function(data = NULL,mapping = aes(NULL)){
  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
  p<-dim(iData)[2]

  #start process
  with(data,{
    #get attr
    if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))){
      attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))
      attr<-names(attr)
    }else if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))){
      attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))
      attr<-names(attr)
    }else{
      stop("ERROR : Cannot find variables in aes(...)")
    }
    if(p==1){
      attr = colnames(data)
    }
    #test attribute illegal
    if(all(!is.numeric(data[[attr]]) , !RSDA::is.sym.interval(data[[attr]]))){
      stop("ERROR : Variables in Min-Max Plot can only be numeric.")
    }

    #build data frame for ggplot
    center<-(iData[[attr]]$min+iData[[attr]]$max)/2
    r <- (iData[[attr]]$max-iData[[attr]]$min)
    d<-as.data.frame(cbind(center,r))
    #figure limits
    minCenter <- min(d$center)
    maxCenter <- max(d$center)
    minRange <- min(d$r)
    maxRange <- max(d$r)

    #build Aesthetic (mapping)
    usermapping <- mapping[-1] #Aesthetic without x,y
    mymapping <- list(mapping=aes(size=4))
    allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))


    #plot
    ggplot(d,aes(center,r))+
      do.call(geom_point,allmapping)+
      geom_segment(aes(x=mean(center),y=minRange
                       ,xend=mean(center),yend=maxRange),lty=2)+
      geom_segment(aes(x=minCenter,y=mean(r),xend=maxCenter,yend=mean(r)),lty=2)+
      labs(x="center",y="range")+
      guides(size=FALSE)
  })
}
