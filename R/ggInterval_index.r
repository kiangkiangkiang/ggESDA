#' @name ggInterval_index
#' @title Plot the range of each observations
#' @description  Visualize the range of the variables of each observations
#' by using a kind of margin bar that indicate the minimal and maximal of
#' observations.
#' @import rlang ggplot2
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
#' @usage ggInterval_index(data = NULL,mapping = aes(NULL))
#' @examples
#' #the observations show on the y-axis .values on x-axis
#' ggInterval_index(iris,aes(x=iris$Sepal.Length))
#'
#' #change above axis
#' ggInterval_index(mtcars,aes(y=disp,col="red",fill="grey"))
#'
#' #symbolic data
#' mydata <- RSDA::facedata
#' ggInterval_index(mydata,aes(x=3:13,y=AD))
#'
#' @export
ggInterval_index <- function(data = NULL,
                             mapping = aes(NULL)){
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN = rlang::get_expr)
  this.x <- args$x ; this.y <- args$y ; this.fill <- args$fill

  ggSymData <- testData(data)#test if symdata
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
  p<-dim(iData)[2]

  #test big o
  if(dim(iData)[1]>=3000){
    stop("Out of time limits.")
  }else if(dim(iData)[1]>200 & dim(iData)[1]<3000){
    warning("It is not recommended for too many observations.")
  }

  isPlotX <- TRUE
  #temp<-c(which(names(args)=="x"),which(names(args)=="y"))
  #args.noXY<-args[-temp]
  with(data,{
    #autogenerate variable pretent user forget
    if(is.null(this.x)){
      this.x <- 1:dim(iData)[1]
    }else if(is.null(this.y)){
      this.y <- 1:dim(iData)[1]
    }

    if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))){
      attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))
      attr<-names(attr)
      #adjust data number from user set
      iData <- iData[eval(this.y),]
    }else if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))){
      attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))
      attr<-names(attr)
      isPlotX<-FALSE
      iData <- iData[eval(this.x),]
    }else{
      stop("ERROR : Cannot find variables in aes(...)")
    }
    if(p==1){
      attr = colnames(data)
    }
    #test attribute illegal
    if(all(!is.numeric(data[[attr]]) , !RSDA::is.sym.interval(data[[attr]]))){
      stop("ERROR : Variables in index Plot can only be numeric.")
    }

    mid<-(iData[[attr]]$min+iData[[attr]]$max)/2
    mymapping<-mapping

    if(isPlotX){
      mymapping$x <- mid
      mymapping$y <- this.y
      errorBar <- quote(geom_errorbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max),width=0.2))
      crossBar <- quote(geom_crossbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max),width=0.2,fill=this.fill))
      temp<-paste0("scale_y_continuous(breaks=c(",list(this.y),"))")
      scale_xy<-eval(parse(text=temp))
      myLabs <- eval(parse(text=paste0("labs(title='Index Plot',y='Observations',x='",attr,"')")))
    }else{
      mymapping$y <- mid
      mymapping$x <- this.x
      errorBar <- quote(geom_errorbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max),width=0.2))
      crossBar <- quote(geom_crossbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max),width=0.2,fill=this.fill))
      temp<-paste0("scale_x_continuous(breaks=c(",list(this.x),"))")
      scale_xy<-eval(parse(text=temp))
      myLabs <- eval(parse(text=paste0("labs(title='Index Plot',x='Observations',y='",attr,"')")))
    }
    p<-ggplot(iData,mapping=mymapping)+
      geom_point()+
      guides(colour = FALSE, alpha = FALSE,fill=FALSE)+
      scale_xy+myLabs
    if(is.null(this.fill)){
        p+eval(errorBar)
    }else{
        p+eval(crossBar)
    }
  })
}






