#' @name ggInterval_minmax
#' @title A min-max plot for interval data
#' @description  Visualize the range of the variables of each observations
#' by marking minimal and maximal point.
#' @import ggplot2
#' @importFrom dplyr arrange
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param scaleXY default "local", which means limits of x-axis and y-axis
#' depend on their own variable. "global" means limits of them depend on all
#' variables that user input.
#' @param plotAll booleans, if TRUE, plot all variable together
#' @return Return a ggplot2 object.
#' @usage ggInterval_minmax(data = NULL,mapping = aes(NULL),
#'           scaleXY = "local",plotAll=FALSE)
#' @examples
#' ggInterval_minmax(mtcars,aes(disp))
#'
#' mydata2<-ggESDA::Cardiological
#' ggInterval_minmax(mydata2,aes(mydata2$Pulse,size=3))
#'
#' d<-mapply(c(10,20,40,80,160),c(20,40,80,160,320),FUN=runif,n=1000)
#' d<-data.frame(qq=matrix(d,ncol=1))
#' ggInterval_minmax(d,aes(qq))
#'
#' myIris<-classic2sym(iris,groupby=Species)
#' myIris<-myIris$intervalData
#' ggInterval_minmax(myIris,aes(myIris$Petal.Length))+
#'    theme_classic()
#' @export
ggInterval_minmax <- function(data = NULL,mapping = aes(NULL),
                              scaleXY = "local",plotAll=FALSE){
  #@param sort if FALSE, it will not be sort by min data,default TRUE.
  sort=TRUE
  if(!(scaleXY %in% c("local", "global"))){
    warning(paste0("There is no method called scaleXY = ",scaleXY,
                   " , please set 'global' or 'local'."))
    scaleXY <- "local"
  }
  if(plotAll){
    if(scaleXY == "local"){
      warning("In order to fix axis, scaleXY must be global when plotAll = T.")
      scaleXY <- "global" #need to fix xy
    }
  }

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
  p <- dim(iData)[2]
  n <- dim(iData)[1]
  #test big o
  if(dim(iData)[1]>3000){
    stop("Out of time limits")
  }else if(dim(iData)[1]>200){
    warning("It is not recommended for too many observations.")
  }

  #start process
  with(data,{
    if(plotAll){
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) ,FUN = is.sym.interval))
      iData <- iData[,which(numericData)]
      attr <- colnames(iData)
      d <- NULL
      #build data frame
      for(i in 1:length(attr)){
        #build data frame for ggplot
        if(scaleXY == "local"){
          temp <- data.frame(min = iData[[attr[i]]]$min,
                             max = iData[[attr[i]]]$max,
                             var = attr[i],
                             min.limits = min(iData[[attr[i]]]$min),
                             max.limits = max(iData[[attr[i]]]$max))
        }else{
          temp <- data.frame(min = iData[[attr[i]]]$min,
                             max = iData[[attr[i]]]$max,
                             var = attr[i],
                             min.limits = min(ggSymData$statisticsDF$min),
                             max.limits = max(ggSymData$statisticsDF$max))
        }
        d <- rbind(d, temp)
      }
    }else{
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
      if(scaleXY == "local"){
        d <- data.frame(min=iData[[attr]]$min,
                        max=iData[[attr]]$max,
                        var = attr,
                        min.limits = min(iData[[attr]]$min),
                        max.limits = max(iData[[attr]]$max))
      }else{
        d <- data.frame(min=iData[[attr]]$min,
                        max=iData[[attr]]$max,
                        var = attr,
                        min.limits = min(ggSymData$statisticsDF$min),
                        max.limits = max(ggSymData$statisticsDF$max))
      }
    }
    #if(sort){
    newd <- dplyr::arrange(d, min)
    #}else{newd <- d}
    #build Aesthetic (mapping)
    xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
    if(length(xyLocation) != 0){
      usermapping <- mapping[-xyLocation] #Aesthetic without x,y
    }else{
      usermapping <- mapping
    }
    #if(sort){
    mymapping <- list(mapping=aes(x=min,y=max,size=3,col="Max"))
    # }
    # else{
    #   mymapping <- list(mapping=aes(x=1:nrow(newd),y=max,size=4,col="Max"))
    # }
    allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

    mymapping2 <- list(mapping=aes(size=3,col="Min"))
    allmapping2 <-as.list(structure(as.expression(c(usermapping,mymapping2)),class="uneval"))
    #plot

    #always sort = T
    #if(sort){
    base  <- ggplot(newd,aes(x=min,y=min, group = var))+
              do.call(geom_point,allmapping2)+
              do.call(geom_point,allmapping)+
              geom_segment(aes(x=min,y=min,xend=min,yend=max))+
              geom_segment(aes(x=min.limits,y=min.limits,xend=max.limits,yend=max.limits),lty=2)+
              guides(size=FALSE,fill=FALSE)

    if(plotAll){
      base <- base + labs(x="",y="",title = "MM Plot") +
        facet_grid(. ~ var)
    }else{
      base <- base + scale_x_continuous(limits = c(mean(d$min.limits), mean(d$max.limits)))+
        scale_y_continuous(limits = c(mean(d$min.limits), mean(d$max.limits)))
      base <- base + labs(x="",y="",title = paste0("MM Plot - ", attr))
    }
    return(base)
    # }
    # else{
    #   ggplot(newd,aes(x=1:nrow(newd),y=min))+
    #     do.call(geom_point,allmapping2)+
    #     do.call(geom_point,allmapping)+
    #     geom_segment(aes(x=1:nrow(newd),y=min,xend=1:nrow(newd),yend=max))+
    #     labs(x="",y=attr,title = "Min-Max Plot")+
    #     guides(size=FALSE,fill=FALSE)+
    #     scale_x_continuous(breaks=1:nrow(newd),labels=rownames(iData))
    # }

  })
}
