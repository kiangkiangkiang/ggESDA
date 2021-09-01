#' @name ggInterval_hist
#' @title Histogram for symbolic data with equal-bin or inequal-bin.
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
#' @param method It can be equal-bin(default) or inequal-bin.Enqual-bin means
#' the width in histogram is equal,which represent all intervals divided
#' have the same range.Inequal-bin means the range of intervals are not
#' the same,and it can be more general on data.Thus,the bins of inequal-bin
#' method depends on the data,and the argument "bins" will be unused.
#' @param bins x axis bins,which mean how many partials the variable
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
ggInterval_hist<-function(data = NULL,mapping = aes(NULL),method="equal-bin",bins=10){
  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  testXY(iData,this.x,this.y)
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

    minimal<-min(iData[[attr]]$min)
    maximal<-max(iData[[attr]]$max)

    if(method=="inequal-bin"){
      d<-as.data.frame(iData[[attr]])
      temp<-seq(minimal,maximal,0.01)
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
      plotData<-data.frame(x1,x2,y1=0,y2=y/n)

      nInt <- dim(x1)[1]
      nameList<-paste(round(x1,2),round(x2,2),sep=":")
      nameList<-paste0("[",nameList,"]")
      midP <- (x1+x2)/2
      #build Aesthetic (mapping)
      usermapping <- mapping[-1] #Aesthetic without x,y
      mymapping <- list(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="grey",col="black")
      allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

      ggplot(data=plotData,aes(x1,y1))+
        do.call(geom_rect,allmapping)+
        labs(x=attr,y="frequency")+
        scale_x_continuous(n.breaks=(nInt-1),breaks=midP,labels=nameList)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


    }else if(method=="equal-bin"){
      #seperate the attribute into bins
      dist<-(maximal-minimal)/bins
      interval<-seq(minimal,maximal,dist)

      #calculate frequency
      f <- matrix(nrow=length(iData[[attr]]),ncol=bins)
      for (obs in 1:length(iData[[attr]])){
        a<-iData[[attr]][obs]$min
        b<-iData[[attr]][obs]$max
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
      nameList<-mapply(1:(nInt-1),2:nInt,FUN=function(x,y) paste(interval[x],interval[y],sep = ":"))
      nameList<-paste0("[",nameList,"]")
      #build Aesthetic (mapping)
      usermapping <- mapping[-1] #Aesthetic without x,y
      mymapping <- list(stat="identity",
                        mapping=aes(alpha=0.5,fill=gray.colors(bins)),col="black")
      allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))


      #build data frame and plot
      myhist<-data.frame(interval=interval[1:bins],
                         frequency=apply(f,2,FUN=sum))
      ggplot(data=myhist, aes(x=interval,y=frequency))+
        do.call(geom_histogram,allmapping)+
        labs(x=attr)+
        scale_fill_manual(values=rep("black",bins))+
        guides(colour = FALSE, alpha = FALSE,fill=FALSE)+
        scale_x_continuous(n.breaks=(nInt-1),breaks=myhist$interval,labels=nameList)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


    }else{
      stop(paste0("ERROR : Unrecognize method : ",method," ."))
    }
  })
}






