#' @name ggInterval_hist
#' @title Histogram for symbolic data with equal-bin or unequal-bin.
#' @description  Visualize the continuous variable distribution
#' by dividing the x axis into bins,and calculating the frequency
#' of observation interval in each bin.
#' @import rlang ggplot2 tidyverse
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param method It can be equal-bin(default) or unequal-bin.Enqual-bin means
#' the width in histogram is equal,which represent all intervals divided
#' have the same range.unequal-bin means the range of intervals are not
#' the same,and it can be more general on data.Thus,the bins of unequal-bin
#' method depends on the data,and the argument "bins" will be unused.
#' @param bins x axis bins,which mean how many partials the variable
#' @param plotAll boolean, whether plot all variables, default FALSE.
#' will be separate into.
#' @return Return a ggplot2 object.
#' @usage ggInterval_hist(data = NULL,mapping = aes(NULL),method="equal-bin",bins=10)
#' @examples
#' ggInterval_hist(mtcars,aes(x=wt))
#'
#' ggInterval_hist(iris,aes(iris$Petal.Length,col="blue",alpha=0.2,
#'    fill="red"),bins=30)
#'
#'
#' d<-data.frame(x=rnorm(1000,0,1))
#' p<-ggInterval_hist(d,aes(x=x),bins=40,method="equal-bin")
#' p
#'
#' p+scale_fill_manual(values=rainbow(40))+labs(title="myNorm")
#'
#' @export
ggInterval_hist<-function(data = NULL,mapping = aes(NULL),method="equal-bin",bins=10,
                          plotAll = FALSE){
  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  if(plotAll){
    if(!is.null(this.x) | !is.null(this.y)){
      warning("Using plotAll presentation cannot specify variables.")
    }
  }else{
    testXY(iData,this.x,this.y)
  }
  p<-dim(iData)[2]
  n<-dim(iData)[1]

  #test figure clearly
  if(method=="equal-bin" & bins>200 & bins<=1000){
    warning("Bins are too large that will lead the figure to be fuzzy.")
  }else if(method=="equal-bin" & bins>1000){
    stop("Bins are too large that will lead the figure to be fuzzy.")
  }


  if("ggESDA" %in% class(data)){data <- iData}
  #start process
  with(data,{
    plotVarNum <- NULL
    mmd <- data.frame(NULL) #min and max data frame
    if(plotAll){
      #get numerical data
      numericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) ,FUN = is.sym.interval))
      iData <- iData[,which(numericData)]
      attr <- colnames(iData)

      for(i in 1:length(attr)){
        mmd[i, "minimal"]<-min(iData[[i]]$min)
        mmd[i, "maximal"]<-max(iData[[i]]$max)
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
        stop("ERROR : Variables in histogram can only be numeric.")
      }
      mmd[1, "minimal"]<-min(iData[[attr]]$min)
      mmd[1, "maximal"]<-max(iData[[attr]]$max)

    }#end plotAll

    if(method=="unequal-bin"){

      plotData <- NULL ; intervalDf = NULL
      for(u in 1:nrow(mmd)){
        d<-as.data.frame(iData[[attr[u]]])
        temp<-seq(mmd[u, "minimal"], mmd[u, "maximal"],0.01)
        f <- matrix(0,nrow=length(temp),ncol=n+1)
        f[,1]<-temp

        #calculate fraquency
        for (i in 1:n){
          #start debug version0825-10
          #raw code :
          #start<-which(round(f[,1],2)==round(d[i,1],2))
          #end<-which(round(f[,1],2)==round(d[i,2],2))
          #new code :
          start<-length(which(round(f[,1],2)<=round(d[i,1],2)))
          end<-length(which(round(f[,1],2)<=round(d[i,2],2)))
          #end debug

          f[start:end,i+1]<-1
        }

        f<-cbind(temp,apply(f[,-1],1,FUN=sum))

        x<-f[1,2]
        y<-1
        for (i in 1:(dim(f)[1]-1)){
          if(f[i,2]!=f[i+1,2]){
            y<-append(y,f[i+1,2])
            x<-append(x,i)
            x<-append(x,i+1)
          }
        }
        x<-append(x,length(temp))
        x<-t(matrix(x,nrow=2))
        x1<-f[x[,1],1]
        x2<-f[x[,2],1]
        plotData<-rbind(plotData,
                        data.frame(x1,x2,y1=0,y2=y/n,group=factor(attr[u])))

        tempDf <- data.frame(start = rep(NA, length(x1)),
                             end = rep(NA, length(x1)))
        tempDf[1:length(x1), "start"] <- round(x1,2)
        tempDf[1:length(x1), "end"] <- round(x2,2)
        intervalDf <- rbind(intervalDf, tempDf)
      }

      intervalDf <- intervalDf[order(intervalDf$start), ]
      temp <- paste(intervalDf$start, intervalDf$end, sep = ":")
      nameList <- paste0("[",temp,"]")

      midP <- (plotData$x1 + plotData$x2)/2
      #build Aesthetic (mapping)
      xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
      if(length(xyLocation) != 0){
        usermapping <- mapping[-xyLocation] #Aesthetic without x,y
      }else{
        usermapping <- mapping
      }
      mymapping <- list(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="grey",col="black")
      allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

      #plot
      base <- ggplot(data=plotData,aes(x1,y1))+
        do.call(geom_rect,allmapping)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

      if(plotAll){
        base <- base + facet_grid(group ~ .) +
          labs(x = "", y="frequency") +
          scale_x_continuous(n.breaks = 8)
      }else{
        base <- base + labs(x=attr,y="frequency")+
          scale_x_continuous(breaks = midP, labels=nameList)
      }
      return(base)

    }else if(method=="equal-bin"){
      myhist <- NULL ; intervalDf <- NULL
      for(i in 1:nrow(mmd)){
        #seperate the attribute into bins
        dist<-(mmd[i, "maximal"]-mmd[i, "minimal"])/bins
        interval<-seq(mmd[i, "minimal"],mmd[i, "maximal"],dist)


        #calculate frequency
        f <- NULL
        f <- matrix(nrow=length(iData[[attr[i]]]),ncol=bins)
        for (obs in 1:length(iData[[attr[i]]])){
          a<-iData[[attr[i]]][obs]$min
          b<-iData[[attr[i]]][obs]$max
          for(area in 1:bins){
            headIn<-a %>% between(interval[area],interval[area+1])
            tailIn<-b %>% between(interval[area],interval[area+1])
            contain<-interval[area] %>% between(a,b)
            if(headIn|tailIn|contain){
              temp<-sort(c(a,b,interval[area],interval[area+1]))
              f[obs,area]<-(temp[3]-temp[2])/(b-a)
            }else{
              f[obs,area]<-0
            }
          }
        }

        nInt <- length(interval)
        interval <- round(interval,2)
        tempDf <- data.frame(start = rep(NA, nInt-1),
                             end = rep(NA, nInt-1))
        tempDf[1:(nInt-1), "start"] <- interval[1:(nInt-1)]
        tempDf[1:(nInt-1), "end"] <- interval[2:nInt]
        intervalDf <- rbind(intervalDf, tempDf)

        #build data frame and plot
        myhist<-rbind(myhist,
                      data.frame(interval=interval[1:bins],
                           frequency=apply(f,2,FUN=sum),
                           group = factor(attr[i])))

      }
      myhist <- cbind(myhist, intervalDf)
      intervalDf <- intervalDf[order(intervalDf$start), ]
      temp <- paste(intervalDf$start, intervalDf$end, sep = ":")
      #temp<-mapply(intervalDf$start,intervalDf$end,FUN=function(x,y) paste(interval[x],interval[y],sep = ":"))
      nameList <- paste0("[",temp,"]")


      myhist$frequency <- myhist$frequency/n


      #build Aesthetic (mapping)
      #
      xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
      if(length(xyLocation) != 0){
        usermapping <- mapping[-xyLocation] #Aesthetic without x,y
      }else{
        usermapping <- mapping
      }
      mymapping <- list(stat="identity",
                        mapping=aes(alpha=0.5,fill=gray.colors(bins*length(attr))),col="black")
      allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))


      #plot
      if(plotAll){
        mymapping <- list(mapping=aes(xmin=start, xmax=end, ymin=0, ymax=frequency),fill="grey",col="black")
        allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))
        base <- ggplot(data=myhist)+
          do.call(geom_rect,allmapping) +
          facet_grid(group ~ .) +
          labs(x = "", y="frequency") +
          scale_x_continuous(n.breaks = 8)

      }else{
        base <- ggplot(data=myhist, aes(x=factor(interval),y=frequency))+
          do.call(geom_histogram,allmapping)+
          scale_fill_manual(values=rep("black",bins*length(attr)))+
          guides(colour = FALSE, alpha = FALSE,fill=FALSE)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(x=attr)+
          scale_x_discrete(labels = nameList)
      }
      return(base)

    }else{
      stop(paste0("ERROR : Unrecognize method : ",method," ."))
    }
  })
}





