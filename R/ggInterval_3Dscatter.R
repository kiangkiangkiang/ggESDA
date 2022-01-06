#' @name ggInterval_3Dscatter
#' @title 3D scatter plot for interval data
#' @description Visualize the three continuous variable distribution
#' by collecting all vertices in each interval to form a shape of
#' cube.Also show the difference between each group.
#' @param data A ggSDA object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggSDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param scale A boolean variable,TRUE, standardlize data. FALSE, not standardlize.
#' If variance is too large(or small) or the difference between
#' two variables are too large,it will be distortion or unseeable,which may
#' happen in different units or others. So, a standardlize way is necessary.
#' @return Return a ggplot2 object (It will still be 2-Dimension).
#' @usage ggInterval_3Dscatter(data = NULL,mapping = aes(NULL),scale=FALSE)
#' @examples
#' #p<-ggInterval_3Dscatter(iris,aes(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length))
#' #p
#' #p+scale_fill_manual(values = c("red","yellow",
#' #     "green","blue",
#' #    "black"),
#' #     labels=c("group1","group2",
#' #            "group3","group4",
#' #             "group5"),
#' #    name="my fill")
#'
#' #generate symbolic data
#' #mySymData<-classic2sym(iris,Species)
#' #mySymData<-mySymData$intervalData
#' #p<-ggInterval_3Dscatter(mySymData,aes(Sepal.Length,
#' #     Sepal.Width,
#' #    Petal.Length,col="red",lty=2))
#' #p
#' #p+scale_fill_manual(values = c("yellow",
#' #     "green",
#' #    "black"),
#' #     labels=rownames(mySymData))
#'
#'
#'
#'
#' @export
ggInterval_3Dscatter <-function(data = NULL,mapping = aes(NULL),scale=FALSE){
  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  this.x <- args$x ; this.y <- args$y
  if(is.null(args$z)){
    args$z <- args[[3]]
    args<-args[-3]
  }
  this.z <- args$z

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  p<-dim(data)[2]
  n<-dim(iData)[1]

  #test big O
  if(n>1000){
    stop("Out of time limits.")
  }

  with(data,{
    #get attrs
    attr1<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.x))))
    attr1<-names(attr1)
    attr3<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.y))))
    attr3<-names(attr3)
    attr2<-which(unlist(lapply(data[,1:p],FUN=identical,x=eval(this.z))))
    attr2<-names(attr2)
    #if cannot find attr
    if(length(attr1)==0 || length(attr2)==0 || length(attr3)==0){
      stop("ERROR : Missing attributes x,y or z in data frame.")
    }

    #test attribute illegal
    if( all((!is.numeric(data[[attr1]])) ,!RSDA::is.sym.interval(data[[attr1]])) ||
        all((!is.numeric(data[[attr3]])) ,!RSDA::is.sym.interval(data[[attr3]]) )
        ||  all((!is.numeric(data[[attr2]])), !RSDA::is.sym.interval(data[[attr2]]))){
      stop("ERROR : Variables in Scatter Plot can only be numeric.")
    }

    #Test adjust
    if(scale){
      iData<-scaleInterval(iData[,c(attr1,attr2,attr3)])
    }

    #build ggplot data.frame
    d<-data.frame(NULL)
    for(i in 1:dim(iData)[1]){
      tmp<-data.frame(iData[[attr1]][i],iData[[attr2]][i],iData[[attr3]][i])
      d<-rbind(d,buildPolyGroup(tmp,g=i))
    }

    #build vision in plot
    sy<-sd(d$newy);sy<-ifelse(is.na(sy),0,sy);sy<-ifelse(sy>10,10,sy);
    sx<-sd(d$newx);sx<-ifelse(is.na(sx),0,sx);sx<-ifelse(sx>10,10,sx);
    m1<-min(c(min(d$newx),max(d$newx),min(d$newy),max(d$newy)))
    m2<-max(c(min(d$newx),max(d$newx),min(d$newy),max(d$newy)))

    #build Aesthetic (mapping)
    usermapping <- mapping[-c(1,2,3)] #Aesthetic without x,y,z
    mymapping <- list(data=d,
                      mapping=aes(group = group,fill=cluster),alpha=0.2,col="black",lwd=0.8)
    allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

    #plot
    ggplot(d,aes(newx,newy))+
      geom_point()+
      do.call(geom_polygon,allmapping)+
      geom_segment(aes(x=m1-sx,xend=m2,y=m1-sy,yend=m2),arrow=arrow(length = unit(0.3, "cm")))+#z
      geom_segment(aes(x=m1-sx,xend=m2,y=m1-sy,yend=m1-sy),arrow=arrow(length = unit(0.3, "cm")))+#x
      geom_segment(aes(x=m1-sx,xend=m1-sx,y=m1-sy,yend=m2),arrow=arrow(length = unit(0.3, "cm")))+#y
      coord_fixed(ratio=1)+labs(x=attr1,y=attr2,title="3D scatter plot")+
      annotate(geom="text",x=m2-sx/2,y=m2-sy/2,label=attr3)
  })
}
point3Dto2D <- function(data){
  data <- as.data.frame(data)
  z<-data[,3]
  z<-round(((z^2)/2)^(0.5),6)
  data[,4]<-data.frame(newx=z+data[,1])
  data[,5]<-data.frame(newy=z+data[,2])
  return(data)
}
buildPolyGroup <- function(newPoint,g=NULL){
  #Sort data
  vertice<-as.factor(get_vertice(3))
  temp<-as.factor(newPoint)
  levels(vertice) <- as.character(newPoint)
  newPoints<-matrix(as.numeric(as.character(vertice)),nrow=8,ncol=3)

  #3D to 2D
  newPoints<-point3Dto2D(newPoints)

  #make group
  newPoints[,6:11]<-0
  newPoints[c(1,2,3,4),6]<-paste0(g,1)
  newPoints[c(3,4,7,8),7]<-paste0(g,2)
  newPoints[c(5,6,7,8),8]<-paste0(g,3)
  newPoints[c(1,2,5,6),9]<-paste0(g,4)
  newPoints[c(1,3,5,7),10]<-paste0(g,5)
  newPoints[c(2,4,6,8),11]<-paste0(g,6)

  d<-newPoints[,c(4,5)]
  d<-rbind(d[c(1,2,4,3),],
           d[c(3,4,8,7),],
           d[c(5,6,8,7),],
           d[c(1,2,6,5),],
           d[c(1,3,7,5),],
           d[c(2,4,8,6),])
  group<-matrix(c(newPoints[,6],newPoints[,7],
                  newPoints[,8],newPoints[,9],
                  newPoints[,10],newPoints[,11]),ncol=1)
  group<-group[group!=0]
  d<-cbind(d,group)
  d$group <- as.factor(d$group)
  d[,"cluster"] <- as.factor(g)
  return(d)
}

scaleInterval<-function(data){
  if(dim(data)[2]>3){
    stop("ERROR : input data must be 3 attributes (x,y,z)")
  }
  temp<-data.frame(data[[1]],data[[2]],data[[3]])
  size<-dim(temp)[1]*dim(temp)[2]
  tempd <- matrix(0,nrow=size/3,ncol=3)
  odd <- unlist(as.list(matrix(which(gtools::odd(1:nrow(tempd))),nrow=1)))
  even<-odd+1
  tempd[odd,1]<-temp[,1]
  tempd[even,1]<-temp[,2]
  tempd[odd,2]<-temp[,3]
  tempd[even,2]<-temp[,4]
  tempd[odd,3]<-temp[,5]
  tempd[even,3]<-temp[,6]
  tempd<-scale(tempd)
  tempd<-cbind(tempd,rep(1:(size/6),each=2))
  tempd<-as.data.frame(tempd)
  tempd<-RSDA::classic.to.sym(tempd,concept = "V4")
  colnames(tempd)<-colnames(data)
  return(tempd)
}
