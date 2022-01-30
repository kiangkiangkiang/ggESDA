#' @name ggInterval_2Dhist
#' @title visualize a 2-dimension histogram by symbolic data with ggplot
#' package.
#' @description Visualize the two continuous variable distribution
#' by dividing both the x axis and y axis into bins,and calculating
#' the frequency of observation interval in each bin.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @importFrom dplyr between
#' @param data A ggESDA object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param xBins x axis bins, which mean how many partials
#' x variable will be separate into.
#' @param yBins y axis bins.It is the same as xBins.
#' @param removeZero whether remove data whose frequency is equal to zero
#' @param addFreq where add frequency text in each cells.
#' @return Return a ggplot2 object.
#' @usage ggInterval_2Dhist(data = NULL,mapping = aes(NULL)
#' ,xBins = 14,yBins=16,removeZero = FALSE,
#' addFreq = TRUE)
#' @examples
#' ggInterval_2Dhist(oils, aes(x = GRA, y = FRE),
#'   xBins = 5, yBins = 5)
#'
#' @export
ggInterval_2Dhist<- function(data = NULL,mapping = aes(NULL),
                             xBins = 14,yBins=16,removeZero = FALSE,
                             addFreq = TRUE){

  #test big O
  if(xBins+yBins>200) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]

  #test big O
  if(n*xBins*yBins >= 35000 & n*xBins*yBins < 50000){
    warning("The number of observations and bins are not suggested be too large.")
  }else if(n*xBins*yBins >= 50000){
    stop("The number of observations and bins are too large that will out of time limit.")
  }


  #start process
  with(data,{
    #get attrs
    attr1<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.x))))
    attr1<-names(attr1)
    attr2<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.y))))
    attr2<-names(attr2)
    #if cannot find attr
    if(length(attr1)==0 || length(attr2)==0){
      stop("ERROR : Missing attributes x or y in data frame.")
    }

    #test attribute illegal
    if( all((!is.numeric(data[[attr1]])) ,!RSDA::is.sym.interval(data[[attr1]]) )
        ||  all((!is.numeric(data[[attr2]])), !RSDA::is.sym.interval(data[[attr2]]))){
      stop("ERROR : Variables in Scatter Plot can only be numeric.")
    }


    #prepare loop data
    r<-8
    minX<-min(iData[[attr1]]$min)
    maxX<-max(iData[[attr1]]$max)
    minY<-min(iData[[attr2]]$min)
    maxY<-max(iData[[attr2]]$max)
    recX <- seq(minX,maxX,(maxX-minX)/xBins)
    recY <- seq(minY,maxY,(maxY-minY)/yBins)

    freq.Rectangle <- matrix(0,nrow=xBins,ncol=yBins)

    #start loop to calculate frequency values in histogram matrix

    #print(recX)
    #print(recY)


    for(rx in 1:(length(recX)-1)){
      for(ry in 1:(length(recY)-1)){
        for(obs in 1:n){
          fx<-0;fy<-0
          a <- iData[[attr1]][obs]$min
          b <- iData[[attr1]][obs]$max
          headIn<-a %>% between(recX[rx],recX[rx+1])
          tailIn<-b %>% between(recX[rx],recX[rx+1])
          contain <- recX[rx] %>% between(a,b)
          if(headIn|tailIn|contain){
            temp<-sort(c(recX[rx],recX[rx+1],a,b))
            if(b-a == 0){
              fx <- fx + 1
            }else{
              fx<-fx+((temp[3]-temp[2])/(b-a))
            }
          }


          a <- iData[[attr2]][obs]$min
          b <- iData[[attr2]][obs]$max
          headIn<-a %>% between(recY[ry],recY[ry+1])
          tailIn<-b %>% between(recY[ry],recY[ry+1])
          contain <- recY[ry] %>% between(a,b)
          if(headIn|tailIn|contain){
            temp<-sort(c(recY[ry],recY[ry+1],a,b))
            if(b-a == 0){
              fy <- fy + 1
            }else{
              fy<-fy+((temp[3]-temp[2])/(b-a))
            }
          }
          freq.Rectangle[rx,ry] <- freq.Rectangle[rx,ry]+fx*fy
          # if(is.na(fx*fy)){
          #   print(paste0("this is na, rx = ",rx,". ry = ",ry,". obs = ",obs))
          # }
        }
      }
    }


    #return(freq.Rectangle)

    #end loop for calculate

    #build data frame to plot (combine margin and values)
    freq.matrix<-c(as.matrix(freq.Rectangle))

    if(all(round(freq.matrix, 1) == 0)){
      warning("Visualize data by 10 times frequency.")
      freq.matrix <- freq.matrix * 10
    }

    freq.matrix<-cbind(freq.matrix,
                       rep(recX[1:(length(recX)-1)],length(recY)-1),
                       rep(recX[2:length(recX)],length(recY)-1),
                       rep(recY[1:(length(recY)-1)],each=length(recX)-1),
                       rep(recY[2:length(recY)],each=length(recX)-1)
    )

    colnames(freq.matrix)<-c("freq","x1","x2","y1","y2")
    freq.matrix<-as.data.frame(freq.matrix)
    #freq.matrix$freq <- freq.matrix$freq/dim(iData)[1]
    freq.matrix[,"xmid"] <- (freq.matrix$x1+freq.matrix$x2)/2
    freq.matrix[,"ymid"] <- (freq.matrix$y1+freq.matrix$y2)/2

    #escape sparse matrix
    if(removeZero){
      freq.matrix<-freq.matrix[freq.matrix$freq!=0,]
    }
    m <- (max(freq.matrix$freq)+min(freq.matrix$freq))/2

    #build Aesthetic
    usermapping <- mapping[-c(1,2)] #Aesthetic without x,y
    mymapping <- list(data=freq.matrix,
                      mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                                  fill=freq,alpha=0.5))
    allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))


    #plot
    base <- ggplot(freq.matrix,aes(x1,y1))+
      do.call(geom_rect,allmapping)+
      scale_fill_gradient2(low = "blue",mid="yellow",
                           high = "red",midpoint = m,
                           limits=c(0,max(freq.matrix$freq)))+
      labs(x=attr1,y=attr2,title="2D hist.")+
      guides(alpha = FALSE)
    if(addFreq){
      base<-base+geom_text(aes(x=xmid,y=ymid,label=round(freq,1)))
    }
    return(base)
  })
}
