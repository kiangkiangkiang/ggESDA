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
#' @param x_bins x axis bins,which mean how many bins
#' x variable will be separate into
#' @param y_bins y axis bins. It is the same as x_bins
#' @param is_zero_remove whether remove data whose frequency is equal to zero
#' @param is_frequency_visible where add frequency text in each cells.
#' @return Return a plot with ggplot2 object
#' @usage ggInterval_2DhistMatrix(data = NULL, mapping = aes(NULL)
#' , x_bins = 8, y_bins = 8, is_zero_remove = FALSE,
#' is_frequency_visible = TRUE)
#' @examples
#' ggInterval_2DhistMatrix(oils, x_bins = 5, y_bins = 5)
#' @export
ggInterval_2DhistMatrix<-function(data = NULL, mapping = aes(NULL),
                          x_bins = 8, y_bins = 8, is_zero_remove = FALSE,
                          is_frequency_visible = TRUE){
  # for warning fixing
  . <- NULL

  # data preparing
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN=rlang::get_expr)
  aes_x <- args$x; aes_y <- args$y

  # remove user's x, y input, remain Aesthetic
  if((!is.null(aes_x)) && (!is.null(aes_y))){ # both have value
    origin_mapping <- mapping[-c(1, 2)]
  }else if((!is.null(aes_x)) || (!is.null(aes_y))){ # only one value
    origin_mapping <- mapping[-1]
  }

  # test data illegal
  iData <- test_data_type(data)$symbolic_data

  # test time consuming
  big_o <- num_of_variables * num_of_variables * num_of_concepts * x_bins * y_bins
  if(x_bins + y_bins > 100) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }
  if(num_of_concepts > 50){
    stop("Out of time limits.Suggested number of observations should be less than 50.")
  }
  if(num_of_concepts * num_of_variables > 200){
    stop("Out of time limits.Suggested dimension should be less than (50 x 4).")
  }
  if(big_o > 200000 & big_o <= 200000){
    warning("It is not recommended number of variables and xy Bins be too large.")
  }else if(big_o > 200000){
    stop(paste0("Out of time limits. The sample size and xy bins ",
                big_o, " are too large."))
  }

  # take only interval_valued data
  is_interval_data <- unlist(lapply(iData[, 1:num_of_variables], FUN = RSDA::is.sym.interval))
  iData <- iData[, is_interval_data]
  num_of_variables <- dim(iData)[2]
  num_of_concepts <- dim(iData)[1]
  if(dim(iData)[2] < dim(data)[2]){
    num_of_variables <- dim(iData)[2]; num_of_concepts <- dim(iData)[1]
    warning("Ignore non-numeric data.")
  }

  freq.matrix<-NULL
  for(i in 1:num_of_variables){
    for(u in 1:num_of_variables){
      if(i != u){
        freq.matrix <- rbind(freq.matrix,
                         data.frame(hist2d(data=ggSymData,
                                           attr1=i,
                                           attr2=u,
                           x_bins=x_bins,y_bins=y_bins,args=args),
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
  if(is_zero_remove){
    freq.matrix <- rbind(freq.matrix[freq.matrix$freq!=0, ],
                       freq.matrix[!freq.matrix$isPlot, ])
  }
  m <- (max(freq.matrix$freq)+min(freq.matrix$freq))/2

  #build Aesthetic
  origin_mapping <- args
  mymapping <- list(data=.%>% dplyr::filter(.data$isPlot)
                    ,mapping=aes(xmin=.data$x1, xmax=.data$x2,
                                 ymin=.data$y1, ymax=.data$y2,
                                 fill=.data$freq)
                    , alpha=0.5)
  allmapping <-as.list(structure(as.expression(c(origin_mapping,mymapping)),class="uneval"))

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

  if(is_frequency_visible){
    base <- base + geom_text(data = .%>% dplyr::filter(.data$isPlot),
                             aes(x=.data$xmid,y=.data$ymid,label=round(.data$freq,1)))
  }
  return(base)

}

hist2d <- function(data = NULL,attr1,attr2,x_bins,y_bins,args){
    #start process
    iData<-data$symbolic_data
    num_of_concepts <- dim(iData)[1]

    #prepare loop data
    r<-8
    minX<-min(iData[[attr1]]$min)
    maxX<-max(iData[[attr1]]$max)
    minY<-min(iData[[attr2]]$min)
    maxY<-max(iData[[attr2]]$max)
    recX <- seq(minX,maxX,(maxX-minX)/x_bins)
    recY <- seq(minY,maxY,(maxY-minY)/y_bins)

    freq.Rectangle <- matrix(0,nrow=x_bins,ncol=y_bins)

    #start loop to calculate frequency values in histogram matrix
    for(rx in 1:(length(recX)-1)){
      for(ry in 1:(length(recY)-1)){
        for(obs in 1:num_of_concepts){
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
