#' @name ggInterval_indexImage
#' @title An index plot presented by color image for interval data.
#' @description Visualize the range of the variables of each observations
#' by using color image.The index image replace margin bar by color,thus
#' it will be more visible for data.
#' @import ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping  Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param column_condition Boolean variables,which mean the color
#' present by column condition (if TRUE) or matrix condition (if FALSE)
#' @param full_strip Boolean variables,which mean the strip present
#' in full figure-width (if TRUE) or only in its variable values(if FALSE).
#' @return Return a ggplot2 object.
#' @usage ggInterval_indexImage(data = NULL,mapping = aes(NULL),
#' column_condition=TRUE,full_strip=FALSE)
#' @examples
#' d<-data.frame(qq=rnorm(1000,0,1))
#' ggInterval_indexImage(d,aes(qq))
#'
#' mydata<-RSDA::facedata
#' p<-ggInterval_indexImage(mydata,aes(AD),full_strip=TRUE,column_condition = TRUE)
#' #Recommend to add coord_flip() to make the plot more visible
#' p+coord_flip()
#'
#' myIris<-classic2sym(iris,groupby=Species)
#' myIris<-myIris$intervalData
#' p<-ggInterval_indexImage(myIris,aes(myIris$Petal.Length),full_strip=FALSE,column_condition=TRUE)
#' p
#'
#' ggInterval_indexImage(mtcars,aes(disp))+labs(x="anything")
#'
#' @export
ggInterval_indexImage<-function(data = NULL,mapping = aes(NULL),
                        column_condition=TRUE,
                        full_strip=FALSE){

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
  p<-dim(iData)[2]

  #test big o
  if(full_strip==FALSE & dim(iData)[1]>200){
    stop("Out of time limits")
  }
  if(full_strip==TRUE & dim(iData)[1]>50){
    stop("Out of time limits")
  }

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
    n<-length(iData[[attr]])
    a<-iData[[attr]]$min
    b<-iData[[attr]]$max
    datasetMin<-min(ggSymData$statisticsDF$min)
    datasetMax<-max(ggSymData$statisticsDF$max)

    #whether full strip (if partial strip)
    if(!full_strip){
      val<-mapply(a,b,FUN=function(x,y) seq(x,y,by=0.1))
      y <- unlist(val)
      mid <- rep(1:n, lengths(val))
      d <- data.frame(x = mid - 0.4,
                      xend = mid + 0.4,
                      y = y,
                      yend = y)

      #whether column condition
      if(!column_condition){
        midp<-(datasetMin+datasetMax)/2
        #midp<-(max(d$y)+min(d$y))/2
        NAME <- "Matrix Condition"
        myColScale <- "scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$y),max(d$y)),
                              labels = c(round(min(d$y)),round(max(d$y))))"
        #breaks=c(min(d$y),max(d$y)),
        #labels = c(round(min(d$y)),round(max(d$y))))
      }else{
        midp<-(max(d$y)+min(d$y))/2
        #midp<-(datasetMin+datasetMax)/2
        NAME <- "Column Condition"
        myColScale <- "scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$y),max(d$y)),
                              labels = c(round(min(d$y)),round(max(d$y))))"
        #c(round(datasetMin),round(datasetMax)))
      }

      #plot
      ggplot(d, aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
        geom_segment(size = 3)+eval(parse(text=myColScale)) +
        labs(y=attr,x="",title=paste0("Index Image-",NAME))+
        scale_x_continuous(breaks=c(1:n),labels = rownames(iData))
    }# if full strip
    else{
      #adjust
      #newN<-sort(runif((n)*2,1,n))
      datasetMax<-ceiling(datasetMax)
      adjustCoef<-ifelse(datasetMax*n>2000,1,ceiling(2000/(datasetMax*n)))

      myAdjust <- seq(1,datasetMax,(datasetMax-1)/(datasetMax*adjustCoef-1))
      d2 <- data.frame(x = rep(1:n,each=datasetMax*adjustCoef) - 0.8,
                       xend = rep(1:n,each=datasetMax*adjustCoef) + 0.8,
                       y = rep(myAdjust,n))

      val2<-mapply(a,b,FUN=function(x,y) sort(runif(datasetMax*adjustCoef,x,y)))
      val<-matrix(val2,ncol=1)
      d<-data.frame(d2,value=val)

      #whether column condition
      if(!column_condition){
        midp<-(datasetMin+datasetMax)/2
        NAME <- "Matrix Condition"
        myColScale<-"scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$value),max(d$value)),
                              labels = c(round(min(d$value)),round(max(d$value))))"

      }else{
        midp<-(max(d$value)+min(d$value))/2
        NAME <- "Column Condition"
        myColScale<-"scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$value),max(d$value)),
                              labels = c(round(min(d$value)),round(max(d$value))))"

      }

      #plot
      ggplot(d, aes(x = x, xend = xend, y = y, yend = y,color=value)) +
        geom_segment(size = 3) +
        eval(parse(text=myColScale))+
        labs(y=attr,x="",title=paste0("Index Image-",NAME))+
        scale_x_continuous(breaks=c(1:n),labels = rownames(iData))+
        scale_y_continuous(breaks=NULL)
    }
  })
}
