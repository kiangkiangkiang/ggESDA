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
#' @import rlang ggplot2 grid gridExtra
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

  #preparing data
  p<-dim(iData)[2]
  n<-dim(iData)[1]
  numericData <- unlist(lapply(iData[,1:p] ,FUN = RSDA::is.sym.interval))
  iData <- iData[,numericData]

  #test big o
  if(p>4 & p<=6){
    warning("It is not recommended number of variables are greater than 4.")
  }else if(p > 6){
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
  dataSetMin <- min(sapply(iData,min))
  dataSetMax <- max(sapply(iData,max))


  temp<-lapply(1:p,function(x) lapply(1:p,x,FUN=scatterMatrix,
                                      data=iData,args=args,
                                      dmin=dataSetMin,dmax=dataSetMax,showLegend=showLegend))
  temp2<-mapply(1:p,FUN = function(x) gridExtra::marrangeGrob(temp[[x]],nrow=1, ncol=p,top=""))
  gridExtra::marrangeGrob(temp2,nrow=p,ncol=1,top = "Scatter matrix")

}
scatterMatrix<-function(data,attr1,attr2,args,dmin,dmax,showLegend){
  #build data frame for ggplot
  n<-dim(data)[1]
  p<-dim(data)[2]
  d=data.frame(x1=data[[attr1]]$min
               ,x2=data[[attr1]]$max
               ,y1=data[[attr2]]$min
               ,y2=data[[attr2]]$max)

  #build Aesthetic
  usermapping <- args
  mymapping <- list(data=d,
                    mapping=aes(xmin=d$x1, xmax=d$x2, ymin=d$y1, ymax=d$y2,
                                fill=grDevices::gray.colors(n),alpha=0.5),col="black")
  allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

  #plot
  if(attr1!=attr2){
    p<-ggplot(data=d,aes(d$x1,d$y1))+
      do.call(geom_rect,allmapping)+
      geom_text(label=rownames(data),size=5)+
      xlim(dmin,dmax)+ylim(dmin,dmax)+
      theme_bw()+labs(x="",y="")+
      theme(panel.grid = element_blank())+
      scale_fill_manual(name="Concept",
                        values=grDevices::gray.colors(n),
                        labels=rownames(data))+
      guides(colour = FALSE, alpha = FALSE)
    if(!showLegend){
      p<-p+guides(fill=FALSE)
    }
    p
  }else{
    ggplot()+
      annotation_custom(grob=grid::textGrob(colnames(data)[attr1],gp = grid::gpar(fontsize = 35)),
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
      theme_void()
  }
}
