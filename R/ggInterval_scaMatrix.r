#' @name ggInterval_scaMatrix
#' @title scatter plot for all variable by interval data.
#' @description  Visualize the all continuous variable distribution
#' by rectangle for both x-axis and y-axis with a matrix grid.
#' Note:this function will automatically
#' filter out the discrete variables,and plot all continuous in
#' input data,so it can not be necessary that give the particularly
#' variables in aes such like (aes(x=x,y=y)).It isn't also
#' recommended to deal with too many variables because the
#' big O in calculating full matrix will be too large.
#' @import rlang ggplot2 tidyverse
#' @importFrom grDevices gray.colors
#' @importFrom grDevices grey.colors
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param showLegend whether show the legend.
#' @return Return a plot with no longer a ggplot2 object,instead
#' of a marrangeGrob object.
#' @usage ggInterval_scaMatrix(data = NULL,mapping = aes(NULL),showLegend=TRUE)
#' @examples
#' a<-rnorm(1000,0,5)
#' b<-runif(1000,-20,-10)
#' c<-rgamma(1000,10,5)
#' d<-as.data.frame(cbind(norm=a,unif=b,gamma_10_5=c))
#' ggInterval_scaMatrix(d)
#'
#'
#' ggInterval_scaMatrix(mtcars[,c("mpg","wt","qsec")],
#'     aes(col="red",lty=2,fill="blue",alpha=0.3))
#'
#'
#' myIris <- classic2sym(iris,groupby = Species)$intervalData
#' ggInterval_scaMatrix(myIris[,1:3])
#'
#'
#' mydata <- RSDA::Cardiological
#' ggInterval_scaMatrix(mydata[,1:3],aes(fill="black",alpha=0.2))
#' @export
ggInterval_scaMatrix <- function(data = NULL,mapping = aes(NULL),showLegend=TRUE){
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
  myRowNames <- rownames(iData)
  #preparing data
  p<-dim(iData)[2]
  n<-dim(iData)[1]
  numericData <- unlist(lapply(iData[,1:p] ,FUN = RSDA::is.sym.interval))
  iData <- iData[,numericData]

  #test big o
  if(p>8 & p<=20){
    warning("It is not recommended number of variables are greater than 8.")
  }else if(p > 20){
    stop("The number of variables are too large to visualize clearly.
         Suggested input variables less than 4. ex. data[,1:4]")
  }
  if(showLegend==T && n>50){
    stop("Suggest set showLegend to FALSE in the situation
         that your observations are large.")
  }else if(showLegend==F && n>3000){
    stop("Out of time limits.")
  }


  #now iData only have numeric data
  if(dim(iData)[2]<dim(data)[2]){
    p<-dim(iData)[2]; n<-dim(iData)[1]
    warning("Ignore non-numeric data.")
  }

  plotData <- NULL
  for(i in 1:p){
    for(u in 1:p){
      if(i != u){
      plotData <- rbind(plotData, data.frame(x1 = iData[[i]]$min,
                                 x2 = iData[[i]]$max,
                                 y1 = iData[[u]]$min,
                                 y2 = iData[[u]]$max,
                                 xv = colnames(iData)[i],
                                 yv = colnames(iData)[u],
                                 Concepts = myRowNames,
                                 isPlot = T,
                                 textXY = 0))
      }else{
        plotData <- rbind(plotData, data.frame(x1 = 0,
                                               x2 = 0,
                                               y1 = 0,
                                               y2 = 0,
                                               xv = colnames(iData)[i],
                                               yv = colnames(iData)[u],
                                               Concepts = "no concepts",
                                               isPlot = F,
                                               textXY = (max(iData[[i]]$max) + min(iData[[i]]$min))/2))
      }
    }
  }

  #build Aesthetic (mapping)
  xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
  if(length(xyLocation) != 0){
    usermapping <- mapping[-xyLocation] #Aesthetic without x,y
  }else{
    usermapping <- mapping
  }

  mymapping <- list(data=. %>% dplyr::filter(isPlot),
                    mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                                 fill=Concepts,alpha=0.5),col="black")
  allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))



  #start plot
  ggplot(data=plotData, aes(x1,y1))+
    do.call(geom_rect,allmapping)+
    scale_fill_manual(name="Concept",
                      values=gray.colors(n),
                      labels=myRowNames)+
    geom_text(data = .%>% dplyr::filter(!isPlot), aes(x = textXY, y = textXY, label = xv),
              size=12)+
    guides(colour = FALSE, alpha = FALSE)+
    facet_grid(yv~xv, scale="free")+
    labs(x="",y="")
}
