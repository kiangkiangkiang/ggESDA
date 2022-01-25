#' @name ggInterval_boxplot
#' @title A interval Box plot
#' @description  Visualize the one continuous variable distribution
#' by box represented by multiple rectangles.
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
#' @param plotAll booleans, if TRUE, plot all variable together
#' @return Return a ggplot2 object.
#' @usage ggInterval_boxplot(data = NULL,mapping = aes(NULL),plotAll=FALSE)
#' @examples
#' p<-ggInterval_boxplot(iris,aes(iris$Petal.Length))
#' p
#' p+scale_fill_manual(values = c("red","yellow",
#'     "green","blue","black"),
#'     labels=c("0%","25%","50%","75%","100%"),
#'     name="quantile")
#'
#' mydata<-ggESDA::facedata
#' ggInterval_boxplot(mydata,aes(AD,col="black",alpha=0.5))
#'
#' myMtcars<-classic2sym(mtcars)
#' myMtcars<-myMtcars$intervalData
#' ggInterval_boxplot(myMtcars,aes(disp))
#'
#' @export
ggInterval_boxplot<-function(data = NULL,mapping = aes(NULL),plotAll=FALSE){
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
  p<-dim(iData)[2]

  with(data,{
    #build box
    quantileN <- 5
    x <- runif(quantileN, 0.3, 0.5)
    xmid<-rep(1,quantileN)
    d <- NULL

    if(plotAll){
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) ,FUN = is.sym.interval))
      iData <- iData[,which(numericData)]
      attr <- colnames(iData)

      #build data frame
      for(i in 1:length(attr)){
        y1 <- as.numeric(quantile(iData[[attr[i]]]$min))
        y2 <- as.numeric(quantile(iData[[attr[i]]]$max))
        temp <- data.frame(x1=xmid-x,
                           x2=xmid+x,
                           y1,
                           y2,
                           var = attr[i],
                           group = 1:5)
        d <- rbind(d, temp)
        #xmid <- xmid + 1
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
        stop("ERROR : Variables in Box Plot can only be numeric.")
      }

      #build data frame
      y1 <- as.numeric(quantile(iData[[attr]]$min))
      y2 <- as.numeric(quantile(iData[[attr]]$max))
      d <- data.frame(x1=xmid-x,
                      x2=xmid+x,
                      y1,y2,
                      var = attr,
                      group = 1:5)
    }

    d$group <- as.factor(d$group)
    #plot
    if(is.null(this.x)||is.null(this.y)){
      mymapping <- mapping[-1]
    }else{mymapping <- mapping[-c(1,2)]}

    #buildgeom_rect arguments
    arg1 <- list(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                             fill=group,alpha=0.5),col="black")
    arg2 <- mymapping
    ggArgs<-as.list(structure(as.expression(c(arg2,arg1)),class="uneval"))


    base  <- ggplot(data=d)+do.call(geom_rect,ggArgs)+
              scale_fill_manual(name = "quantile",
                                values=gray.colors(5),
                                labels=c("0%","25%","50%","75%","100%"))+
              guides(colour = FALSE, alpha = FALSE)+
              scale_x_continuous(limits=c(-0.5, 2.5))+
              labs(y = "Values")

    if(plotAll){
      base <- base + facet_grid(. ~ var)
    }else{
      base <- base + labs(x = attr)
    }
    return(base)

  })
}
