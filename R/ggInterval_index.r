#' @name ggInterval_index
#' @title Plot the range of each observations
#' @description  Visualize the range of the variables of each observations
#' by using a kind of margin bar that indicate the minimal and maximal of
#' observations.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plotAll plot all variables
#' @return Return a ggplot2 object.
#' @usage ggInterval_index(data = NULL,mapping = aes(NULL),
#' plotAll = FALSE)
#' @examples
#' #the observations show on the y-axis .values on x-axis
#' ggInterval_index(iris,aes(x=iris$Sepal.Length))
#'
#' #change above axis
#' ggInterval_index(mtcars,aes(y=disp,col="red",fill="grey"))
#'
#' #symbolic data
#' mydata <- ggESDA::facedata
#' ggInterval_index(mydata,aes(x=3:13,y=AD))
#'
#' @export
ggInterval_index <- function(data = NULL,
                             mapping = aes(NULL),
                             plotAll = FALSE){
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN = rlang::get_expr)
  this.x <- args$x ; this.y <- args$y ; this.fill <- args$fill
  this.group <- args$group

  ggSymData <- testData(data)#test if symdata
  iData <- ggSymData$intervalData
  if(plotAll){
    if(!is.null(this.x) | !is.null(this.y)){
      warning("Using plotAll presentation cannot specify variables.")
    }
  }else{
    testXY(iData,this.x,this.y)
  }

  p <- dim(iData)[2]
  n <- dim(iData)[1]
  n.groups <- 1

  #adjust
  this.fill <- eval(this.fill)
  this.group <- eval(this.group)

  if(!is.null(this.group)){
    if(length(this.group) == n | length(this.group) == 1){
      #yes

      this.group <- as.factor(this.group)
      #test concepts_group illegal
      if(length(unique(table(this.group))) != 1){
        stop("Each group of concepts must be equal length.")
      }

      if(length(this.group) == n){
        n.concepts <- unique(table(this.group))
        n.groups <- length(table(this.group))
      }
    }else{
      #no

      stop(paste0("Length of fill must be equal to data (",n,") or 1."))
    }

  }

  myRowNames <- rownames(iData)


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
    if(plotAll){
      if(length(this.fill) == n){
        this.fill <- rep(this.fill, p)
      }
      #isPlotX = FALSE is OK too
      isPlotX <- TRUE
      d <- NULL
      for(i in 1:p){
        temp <- data.frame(
          myx = (iData[[i]]$min + iData[[i]]$max) / 2,
          myy = 1:n,
          min = iData[[i]]$min,
          max = iData[[i]]$max,
          g = colnames(iData)[[i]])
        d <- rbind(d, temp)
      }
      d$g <- as.factor(d$g)

    }else{
      #autogenerate variable pretent user forget
      if(!is.null(this.group) & length(this.group) == n){
        if(is.null(this.x)){
          this.x <- rep(1:n.concepts, n.groups)
        }else if(is.null(this.y)){
          this.y <- rep(1:n.concepts, n.groups)
        }
      }else{
        if(is.null(this.x)){
          this.x <- 1:dim(iData)[1]
        }else if(is.null(this.y)){
          this.y <- 1:dim(iData)[1]
        }
      }

      if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))){
        attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))
        attr<-names(attr)
        #adjust data number from user set
        if(!is.null(this.fill) & length(this.fill) == n){
          #not change
          iData <- iData
        }else{
          iData <- iData[eval(this.y),]
        }
      }else if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))){
        attr<-which(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))
        attr<-names(attr)
        isPlotX<-FALSE
        if(!is.null(this.fill) & length(this.fill) == n){
          #not change
          iData <- iData
        }else{
          iData <- iData[eval(this.x),]
        }
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
    }

    mymapping<-mapping

    #start plot
    if(plotAll){
      mymapping$x <- d$myx
      mymapping$y <- d$myy
      mymapping$fill <- this.fill
      p <- plotAllFun(d, mymapping, this.fill, this.group)
      return(p + scale_y_continuous(breaks = c(1:n),
                                    labels = myRowNames))
    }


    if(isPlotX){
      mymapping$x <- mid
      mymapping$y <- this.y
      if(!is.null(this.group) & length(this.group) == n){
        errorBar <- quote(geom_errorbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max, fill = this.group),width=0.2))
        crossBar <- quote(geom_crossbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max, fill = this.group),width=0.5))
     }else{
       errorBar <- quote(geom_errorbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max),width=0.2))
       crossBar <- quote(geom_crossbar(aes(xmin=iData[[attr]]$min,xmax=iData[[attr]]$max),width=0.5))
      }
      temp<-paste0("scale_y_continuous(breaks=c(",list(this.y),"))")
      #temp2 <- paste0("scale_x_continuous(labels = myRowNames)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))")
      myLabs <- eval(parse(text=paste0("labs(title='Index Plot',y='Observations',x='",attr,"')")))
    }else{
      mymapping$y <- mid
      mymapping$x <- this.x
      if(!is.null(this.group) & length(this.group) == n){
        errorBar <- quote(geom_errorbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max, fill = this.group),width=0.2))
        crossBar <- quote(geom_crossbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max, fill = this.group),width=0.5))
      }else{
        errorBar <- quote(geom_errorbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max),width=0.2))
        crossBar <- quote(geom_crossbar(aes(ymin=iData[[attr]]$min,ymax=iData[[attr]]$max),width=0.5))
      }
      temp<-paste0("scale_x_continuous(breaks=c(",list(this.x),"))")
      #temp2 <- paste0("scale_y_continuous(labels = myRowNames)")
      myLabs <- eval(parse(text=paste0("labs(title='Index Plot',x='Observations',y='",attr,"')")))
    }
    scale_xy<-eval(parse(text=temp))

    mymapping$fill <- this.fill
    mymapping$group <- this.group
    p<-ggplot(iData,mapping=mymapping)+
      geom_point()+
      guides(alpha = FALSE)+
      scale_xy+myLabs
    if(!is.null(this.fill) | !is.null(this.group)){
      p + eval(crossBar)
    }else{
      p + eval(errorBar)
    }
  })
}

plotAllFun <- function(d = NULL, mymapping = NULL, this.fill = NULL,
                       this.group = NULL){
  if(!is.null(this.fill) | !is.null(this.group)){
    ggplot(data = d, mapping = mymapping) +
      guides(alpha = FALSE) +
      geom_crossbar(aes(xmin = d$min,
                        xmax = d$max), width = 0.5)+
      facet_grid(. ~ g, scales = "free")
  }else{
    ggplot(data = d, mapping = mymapping) +
      guides(alpha = FALSE) +
      geom_errorbar(aes(xmin = d$min,
                        xmax = d$max), width = 0.2)+
      facet_grid(. ~ g, scales = "free")
  }
}




