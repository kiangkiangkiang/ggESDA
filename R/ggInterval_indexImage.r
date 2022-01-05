#' @name ggInterval_indexImage
#' @title An index plot presented by color image for interval data.
#' @description Visualize the range of the variables of each observations
#' by using color image.The index image replace margin bar by color,thus
#' it will be more visible for data.
#' @import ggplot2 ggthemes
#' @importFrom RSDA is.sym.interval
#' @importFrom ggpubr ggarrange
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
#' @param plotAll Boolean, which determine if the heatmap type for visualizing
#' full variables is used. default FALSE.
#' @return Return a ggplot2 object.
#' @usage ggInterval_indexImage(data = NULL,mapping = aes(NULL),
#' column_condition=TRUE,full_strip=FALSE, plotAll = FALSE)
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
                        full_strip=FALSE, plotAll = FALSE){

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  myHeatMapNames <- rownames(iData)
  if(plotAll){
    if(!is.null(this.x) | !is.null(this.y)){
      warning("Using heatmap presentation cannot specify variables.")
    }
  }else{
    testXY(iData,this.x,this.y)
  }

  p<-dim(iData)[2]
  n<-dim(iData)[1]
  #test big o
  if(full_strip==FALSE & dim(iData)[1]>200){
    stop("Out of time limits")
  }
  if(full_strip==TRUE & dim(iData)[1]>50){
    stop("Out of time limits")
  }
  #start process
  datasetMin<-min(ggSymData$statisticsDF$min)
  datasetMax<-max(ggSymData$statisticsDF$max)
  #allDataMean <- mean(unlist(ggSymData$statisticsDF$min, ggSymData$statisticsDF$max))


  with(data,{
    #add heatmap
    if(plotAll){
      #set adjust ggarrange width scale
      widthScale <- 7

      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) ,FUN = is.sym.interval))
      iData <- iData[,which(numericData)]

      #scale
      if(column_condition){
        iData <- scale_sym_table(iData, n, p)$intervalData
      }

      d <- data.frame(NULL)
      for(i in colnames(iData)){
        a<-iData[[i]]$min
        b<-iData[[i]]$max
        adjustStrip <- min(b-a)/50
        d <- rbind(d, data.frame(buildPlotData(a, b, adjustStrip, n, full_strip, datasetMax), group=factor(i)))
      }
    }else{#end headmap

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
      a<-iData[[attr]]$min
      b<-iData[[attr]]$max

      #whether full strip (if partial strip)
      #debug for separate strip "adjustStrip"
      adjustStrip <- min(b-a)/50
    }

    if(!full_strip){
      if(!plotAll){
        d <- buildPlotData(a, b, adjustStrip, n, full_strip, datasetMax)
      }
      #whether column condition
      if(!column_condition){
        midp<-(datasetMin+datasetMax)/2
        #midp <- allDataMean
        #midp<-(max(d$y)+min(d$y))/2
        NAME <- "Matrix Condition"
        myColScale <- "scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$y),max(d$y)),
                              labels = c(round(min(d$y)),round(max(d$y))))"
        #breaks=c(min(d$y),max(d$y)),
        #labels = c(round(min(d$y)),round(max(d$y))))

        if(plotAll){
          base <- ggplot(d, aes(x = x, xend = xend, y = y, yend = y,color=y)) +
            geom_segment(size = 3) + facet_grid(. ~ group) +
            eval(parse(text=myColScale))+
            coord_flip()+scale_x_continuous(breaks = 1:n, label=myHeatMapNames)+
            labs(x="Concepts",y="Values")+
            theme(legend.position="bottom",
                  legend.title = element_blank())

          return(base)
        }
      }else{#column control
        #midp<-(max(d$y)+min(d$y))/2
        midp <- mean(d$y)
        #midp<-(datasetMin+datasetMax)/2
        NAME <- "Column Condition"
        myColScale <- "scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$y),max(d$y)),
                              labels = c(round(min(d$y)),round(max(d$y))))"
        #c(round(datasetMin),round(datasetMax)))

        if(plotAll){
          #midp<-(max(d$y)+min(d$y))/2
          midp <- 0
          p <- ggplot(d, aes(x = x, xend = xend, y = y, yend = yend, color = y))

          p <- p+geom_segment(size = 3)+
            scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                                  midpoint = midp,
                                  breaks = c(min(d$y),max(d$y)),
                                  labels = c("min","max"))+
            scale_x_continuous(breaks=c(1:n),labels = myHeatMapNames)+
            coord_flip()+
            guides(fill=F,alpha=F)+
            facet_grid(. ~ group)+
            labs(x = "Concepts",y="")+
            theme(legend.position="bottom",
                  legend.title = element_blank())+
            scale_y_continuous(breaks = NULL)
          return(p)

          # base <- ggplot(d, aes(x = x, xend = xend, y = y, yend = y,color=y)) +
          #   geom_segment(size = 3) + facet_grid(. ~ group) +
          #   eval(parse(text=myColScale))+
          #   coord_flip()+scale_x_continuous(breaks = 1:n, label=myHeatMapNames)+
          #   labs(x="Concepts",y="Values")+
          #   theme(legend.position="bottom",
          #         legend.title = element_blank())
          #
          # return(base)

          # u<-1
          # myp <- NULL
          # for(ele in levels(d$group)){
          #   tempData <- dplyr::filter(d, d$group == ele)
          #   myp[[u]] <- myplot(tempData, u, NAME, myHeatMapNames, n, ele,full_strip)
          #   u<-u+1
          # }
          #
          # #v <- seq(min(d$y),max(d$y),(max(d$y)-min(d$y))/(1000-1))
          # #adjust grid
          # maxChar <- max(unlist(lapply(myHeatMapNames, nchar)))
          # adjustWidth <- 1 + sqrt(maxChar)/widthScale
          # base <- ggarrange(plotlist=myp, nrow=1,
          #                   widths = c(adjustWidth,rep(1, p-1)))
          #
          # v <- seq(1,1000)
          # return(ggarrange(base, buildColLegend(v),nrow=2,heights=c(8,1)))
        }
      }

      #plot
        ggplot(d, aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
          geom_segment(size = 3)+eval(parse(text=myColScale)) +
          labs(y=attr,x="",title=paste0("Index Image-",NAME))+
          scale_x_continuous(breaks=c(1:n),labels = rownames(iData))
    }
    else{# if full strip
      #adjust
      #newN<-sort(runif((n)*2,1,n))
      if(!plotAll){
        d <- buildPlotData(a, b, adjustStrip, n, full_strip, datasetMax)
      }

      #whether column condition
      if(!column_condition){
        midp<-(datasetMin+datasetMax)/2
        #midp <- allDataMean
        NAME <- "Matrix Condition"
        myColScale<-"scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$value),max(d$value)),
                              labels = c(round(min(d$value)),round(max(d$value))))"

        if(plotAll){
          base <- ggplot(d, aes(x = x, xend = xend, y = y, yend = y,color=value)) +
            geom_segment(size = 3) + facet_grid(. ~ group) +
            eval(parse(text=myColScale))+
            coord_flip()+scale_x_continuous(breaks = 1:n, label=myHeatMapNames)+
            labs(x="Concepts",y="Values")+
            theme(legend.position="bottom",
                  legend.title = element_blank())
          return(base)
        }

      }else{# column condition
        midp<-(max(d$value)+min(d$value))/2
        NAME <- "Column Condition"
        myColScale<-"scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                              midpoint = midp,
                              name = 'Value',
                              breaks=c(min(d$value),max(d$value)),
                              labels = c(round(min(d$value)),round(max(d$value))))"

        if(plotAll){
          midp <- 0
          p <- ggplot(d, aes(x = x, xend = xend, y = y, yend = y, color = value))

          p <- p+geom_segment(size = 3)+
            scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                                  midpoint = midp,
                                  breaks = c(min(d$value),max(d$value)),
                                  labels = c("min","max"))+
            scale_x_continuous(breaks=c(1:n),labels = myHeatMapNames)+
            coord_flip()+
            guides(fill=F,alpha=F)+
            facet_grid(. ~ group)+
            labs(x = "Concepts",y="")+
            theme(legend.position="bottom",
                  legend.title = element_blank())+
            scale_y_continuous(breaks = NULL)
          return(p)




          # u<-1
          # myp <- NULL
          # for(ele in levels(d$group)){
          #   tempData <- dplyr::filter(d, d$group == ele)
          #   myp[[u]] <- myplot(tempData, u, NAME, myHeatMapNames, n, ele, full_strip)
          #   u<-u+1
          # }
          #
          # #v <- seq(min(d$y),max(d$y),(max(d$y)-min(d$y))/(1000-1))
          # #adjust grid
          # maxChar <- max(unlist(lapply(myHeatMapNames, nchar)))
          # adjustWidth <- 1 + sqrt(maxChar)/widthScale
          # base <- ggarrange(plotlist=myp, nrow=1,
          #                   widths = c(adjustWidth, rep(1, p-1)))
          #
          # v <- seq(1,1000)
          #
          # return(ggarrange(base, buildColLegend(v),nrow=2,heights=c(12,1)))

        }

      }

      #plot
        ggplot(data = d, aes(x = x, xend = xend, y = y, yend = y,color=value)) +
          geom_segment(size = 3) +
          eval(parse(text=myColScale))+
          labs(y=attr,x="",title=paste0("Index Image-",NAME))+
          scale_x_continuous(breaks=c(1:n),labels = rownames(iData))+
          scale_y_continuous(breaks=NULL)
    }
  })
}


buildPlotData <- function(a, b, adjustStrip, n, full_strip, datasetMax){
  if(!full_strip){
    val<-mapply(a,b,FUN=function(x,y) seq(x,y,by=adjustStrip))
    y <- unlist(val)
    mid <- rep(1:n, lengths(val))
    d <- data.frame(x = mid - 0.4,
                    xend = mid + 0.4,
                    y = y,
                    yend = y)
  }else{
    datasetMax<-ceiling(datasetMax)
    adjustCoef<-ifelse(datasetMax*n>2000,1,ceiling(2000/(datasetMax*n)))

    myAdjust <- seq(1,datasetMax,(datasetMax-1)/(datasetMax*adjustCoef-1))
    #debug for unequal bin image
    d2 <- data.frame(x = rep(1:n,each=datasetMax*adjustCoef)-0.5,
                     xend = rep(1.5:(n+0.5),each=datasetMax*adjustCoef),
                     y = rep(myAdjust,n))
    val2<-mapply(a,b,FUN=function(x,y) sort(runif(datasetMax*adjustCoef,x,y)))
    val<-matrix(val2,ncol=1)
    d<-data.frame(d2,value=val)
  }
  return(d)
}


# buildColLegend <- function(v){
#     newd<-data.frame(x = rep(1:1000),
#                      xend = rep(1:1000),
#                      y = rep(1, 1000),
#                      yend = rep(2, 1000),
#                      val = v)
#     p2 <- ggplot(newd,aes(x=x,xend=xend,y=y,yend=yend,col=val))+
#       geom_segment()+
#       scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
#                             midpoint = (min(newd$val)+max(newd$val))/2,
#                             name = 'Value',
#                             breaks=c(min(newd$val),max(newd$val)),
#                             labels = c(round(min(newd$val)),round(max(newd$val))))+
#       guides(color=F,y=F)+theme_tufte()+labs(y=NULL,x=NULL)+
#       scale_x_continuous(breaks=c(0,1000),labels = c("min","max"))
#
#     temp<-ggplot()+theme_void()
#     temp2<-ggarrange(temp,p2,temp,nrow=1,widths = c(2,1,2))
#     return(temp2)
# }


myplot <- function(tempData, u, NAME, myHeatMapNames, n, ele, full_strip){


  if(full_strip){
    midp<-(max(tempData$value)+min(tempData$value))/2
    p <- ggplot(tempData, aes(x = tempData$x, xend = tempData$xend, y = tempData$y, yend = tempData$y, color = tempData$value))
  }else{
    midp<-(max(tempData$y)+min(tempData$y))/2
    p <- ggplot(tempData, aes(x = tempData$x, xend = tempData$xend, y = tempData$y, yend = tempData$yend, color = tempData$y))
  }
  p<-p+geom_segment(size = 3)+
    scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                          midpoint = midp)+
    labs(y="",x="",title=paste0("Index Image-",NAME))+
    scale_x_continuous(breaks=c(1:n),labels = myHeatMapNames)+
    coord_flip()+
    guides(fill=F,col=F,alpha=F)+
    theme(plot.title = element_text(hjust = 0.5))+ggtitle(ele)

  if(u!=1){
    p<-p+theme(axis.text.y = element_blank())
  }

  return(p)
}
scale_sym_table <- function(d, n, p){
  temp1 <- sapply(1:p, FUN = function(x) unlist(data.frame(d[[x]])))
  temp2 <- apply(temp1, 2, scale)
  newd <- data.frame(temp2[1:n, ], temp2[(n+1):(n*2), ])
  myd <- classic2sym(newd, groupby = "customize",
                     minData = temp2[1:n, ],
                     maxData = temp2[(n+1):(n*2), ])
  colnames(myd$intervalData) <- colnames(d)
  return(myd)
}
