#' @name ggInterval_scatter
#' @title scatter plot for two continuous interval data
#' @description Visualize the twwo continuous variable distribution
#' by rectangle and each of its width and heigth represents a
#' interval of the data.
#' @import rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param ... Others in ggplot2.
#' @return Return a ggplot2 object.
#' @usage ggInterval_scatter(data = NULL,mapping = aes(NULL), ...)
#' @examples
#' a<-rnorm(1000,0,5)
#' b<-runif(1000,-20,-10)
#' d<-as.data.frame(cbind(norm=a,unif=b))
#' ggInterval_scatter(d,aes(a,b))
#'
#'
#' ggInterval_scatter(mtcars[,c("mpg","wt","qsec")],
#'     aes(x=mpg,y=wt,
#'     col="red",lty=2,fill="blue",alpha=0.3))
#'
#'
#' myIris <- classic2sym(iris,groupby = "Species")$intervalData
#' p<-ggInterval_scatter(myIris,aes(myIris$Petal.Length,myIris$Petal.Width))
#' p
#' p+scale_fill_manual(labels=rownames(myIris),
#'                    values=c("red","blue","green"),
#'                    name="Group")
#'
#'
#' mydata <- ggESDA::facedata
#' p<-ggInterval_scatter(mydata[1:10,],aes(AD,BC,alpha=0.2))
#' p+scale_fill_manual(labels=rownames(mydata)[1:10],
#'                    values=rainbow(10),
#'                    name="Group")
#' @export
ggInterval_scatter <- function(data = NULL,mapping = aes(NULL), ...){
  #data preparing
  . <- NULL
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]
  myRowNames<- rownames(iData)

  #test big o
  if(n>150 & n<=2000){
    warning("It is not recommended for too many observations.")
  }else if(n > 2000){
    stop("Out of time limits.")
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

    #build data.frame for ggplot
    d=data.frame(x1=iData[[attr1]]$min
                 ,x2=iData[[attr1]]$max
                 ,y1=iData[[attr2]]$min
                 ,y2=iData[[attr2]]$max)

    #build Aesthetic (mapping)
    xyLocation <- c(which(names(mapping) == "x"), which(names(mapping) == "y"))
    if(length(xyLocation) != 0){
      usermapping <- mapping[-xyLocation] #Aesthetic without x,y
    }else{
      usermapping <- mapping
    }
    #add facets
    d <- addFactor(rawData = data, iData = d)


    fcLocation <- c(which(names(mapping) == "fill"), which(names(mapping) == "col"))
    if(length(fcLocation) != 0){
      usermapping <- mapping[-fcLocation] #Aesthetic without fill, col
    }else{
      usermapping <- mapping
    }
    # if(is.null(args$fill)){
    #   args$fill <- grDevices::gray.colors(n)
    # }
    # if(is.null(args$col)){
    #   args$col <- grDevices::gray.colors(n)
    # }


    mymapping <- list(d, mapping=aes(xmin=d$x1, xmax=d$x2, ymin=d$y1, ymax=d$y2,alpha=0.5,fill = eval(args$fill), col = eval(args$col)), ...)

    #mymapping <- list(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2,alpha=0.5))
    allmapping <-as.list(structure(as.expression(c(mymapping,usermapping)),class="uneval"))

    #ggplot(data=d,aes(x = x1, y = y1))+
    #  do.call(geom_rect,ss)

    #start plot
    ggplot(data=d,aes(x = x1, y = y1))+
      do.call(geom_rect,allmapping)+
      geom_text(label=myRowNames)+
      guides(colour = FALSE, alpha = FALSE)+
      labs(x=attr1,y=attr2,fill="Concepts")

  })
}
