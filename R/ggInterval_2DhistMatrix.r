#' @name ggInterval_2DhistMatrix
#' @title 2-Dimension histogram matrix
#' @description  Visualize the all continuous variable distribution
#' by dividing both the x axis and y axis into bins,and calculating
#' the frequency of observation interval in each bin.Eventually
#' show it by a matrix plot. Note: this function will automatically
#' filter out the discrete variables,and plot all continuous in
#' input data, so it can not be necessary that give the particularly
#' variables in aes such like (aes(x = x, y = y)). It isn't also
#' recommended to deal with too many variables because the
#' big O in calculating full matrix will be too large.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @importFrom dplyr between
#' @importFrom magrittr %>%
#' @param data A ggESDA object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param xBins x axis bins,which mean how many bins
#' x variable will be separate into
#' @param yBins y axis bins. It is the same as xBins
#' @param removeZero whether remove data whose frequency is equal to zero
#' @param addFreq where add frequency text in each cells.
#' @return Return a plot with ggplot2 object
#' @usage ggInterval_2DhistMatrix(data = NULL,mapping = aes(NULL)
#' ,xBins = 8,yBins=8,removeZero = FALSE,
#' addFreq = TRUE)
#'
#' @examples
#' ggInterval_2DhistMatrix(oils, xBins = 5, yBins = 5)
#'
#' @export
ggInterval_2DhistMatrix<-function(data = NULL,mapping = aes(NULL),
                          xBins = 8,yBins=8,removeZero = FALSE,
                          addFreq = TRUE){
  #test big O
  #globalVariables(".", add = F)
  . <- NULL
  if(xBins+yBins>100) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }

  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y
  #remove user's x,y input,remain Aesthetic
  if((!is.null(this.x))&&(!is.null(this.y))){#both have value
    usermapping <- mapping[-c(1,2)]
  }else if((!is.null(this.x))||(!is.null(this.y))){#only one value
    usermapping <- mapping[-1]
  }

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]

  #test clearly visualize
  if(p*p*n*xBins*yBins > 200000 & p*p*n*xBins*yBins <= 200000){
    warning("It is not recommended number of variables and xy Bins be too large.")
  }else if(p*p*n*xBins*yBins > 200000){
    stop(paste0("Out of time limits. The sample size and xy bins ",
                p*p*n*xBins*yBins," are too large."))
  }

  #test big O
  if(n>50){
    stop("Out of time limits.Suggested number of observations should be less than 50.")
  }
  if(n*p>200){
    stop("Out of time limits.Suggested dimension should be less than (50 x 4).")
  }



  numericData <- unlist(lapply(iData[,1:p] ,FUN = RSDA::is.sym.interval))
  iData <- iData[,numericData]
  p<-dim(iData)[2]

  #now iData only have numeric data
  if(dim(iData)[2]<dim(data)[2]){
    p<-dim(iData)[2]; n<-dim(iData)[1]
    warning("Ignore non-numeric data.")
  }

  freq.matrix<-NULL
  for(i in 1:p){
    for(u in 1:p){
      if(i != u){
        freq.matrix <- rbind(freq.matrix,
                         data.frame(hist2d(data=ggSymData,
                                           attr1=i,
                                           attr2=u,
                           xBins=xBins,yBins=yBins,args=args),
                           xv = colnames(iData)[i],
                           yv = colnames(iData)[u],
                           isPlot = T,
                           textXY = 0))
      }
      else{
        freq.matrix <- rbind(freq.matrix,
                             data.frame(freq = 0,
                                        x1 = 0,
                                        x2 = 0,
                                        y1 = 0,
                                        y2 = 0,
                                        xv = colnames(iData)[i],
                                        yv = colnames(iData)[u],
                                        isPlot = F,
                                        textXY = 0))
      }
    }
  }

  for(var in colnames(iData)){
    temp <- dplyr::filter(freq.matrix, freq.matrix$xv == var & freq.matrix$isPlot)
    freq.matrix[!freq.matrix$isPlot & freq.matrix$xv==var, "textXY"] <- (min(temp$x1) + max(temp$x2))/2

  }

  freq.matrix[,"xmid"] <- (freq.matrix$x1+freq.matrix$x2)/2
  freq.matrix[,"ymid"] <- (freq.matrix$y1+freq.matrix$y2)/2
  #escape sparse matrix
  if(removeZero){
    freq.matrix <- rbind(freq.matrix[freq.matrix$freq!=0, ],
                       freq.matrix[!freq.matrix$isPlot, ])
  }
  m <- (max(freq.matrix$freq)+min(freq.matrix$freq))/2

  #build Aesthetic
  usermapping <- args
  mymapping <- list(data=.%>% dplyr::filter(.data$isPlot)
                    ,mapping=aes(xmin=.data$x1, xmax=.data$x2,
                                 ymin=.data$y1, ymax=.data$y2,
                                 fill=.data$freq)
                    , alpha=0.5)
  allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

  #plot
  base <- ggplot(data=freq.matrix, aes(.data$x1, .data$y1))+
    do.call(geom_rect,allmapping)+
    geom_text(data = .%>% dplyr::filter(!.data$isPlot), aes(x = .data$textXY, y = .data$textXY, label = .data$xv),
              size=12)+
    facet_grid(.data$yv~.data$xv, scales="free")+
    scale_fill_gradient2(name="frequency",
                       low = "blue",mid="yellow",
                       high = "red",midpoint = m,
                       limits=c(0,max(freq.matrix$freq)))+
    labs(x="",y="")+
    theme_bw()

  if(addFreq){
    base <- base + geom_text(data = .%>% dplyr::filter(.data$isPlot),
                             aes(x=.data$xmid,y=.data$ymid,label=round(.data$freq,1)))
  }
  return(base)

}

hist2d <- function(data = NULL,attr1,attr2,xBins,yBins,args){
    #start process
    iData<-data$intervalData
    n <- dim(iData)[1]

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
    #end loop for calculate

    #build data frame to plot (combine margin and values)
    freq.matrix<-(c(as.matrix(freq.Rectangle)))
    freq.matrix<-cbind(freq.matrix,
                       rep(recX[1:(length(recX)-1)],length(recY)-1),
                       rep(recX[2:length(recX)],length(recY)-1),
                       rep(recY[1:(length(recY)-1)],each=length(recX)-1),
                       rep(recY[2:length(recY)],each=length(recX)-1)
    )
    colnames(freq.matrix)<-c("freq","x1","x2","y1","y2")
    freq.matrix<-as.data.frame(freq.matrix)

    #freq.matrix$freq <- freq.matrix$freq/dim(iData)[1]
    return(freq.matrix)
}
