#' @name ggInterval_PCA
#' @title Vertice-PCA for interval data
#' @description ggInterval_PCA performs a principal components
#' analysis on the given numeric interval data and returns the
#' results like princomp , ggplot object and a interval scores.
#' @import rlang ggplot2 stats
#' @importFrom RSDA is.sym.interval
#' @importFrom gtools odd
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping  Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param plot Boolean variable,Auto plot (if TRUE).It can also plot by
#' its inner object
#' @return A ggplot object for PC1,PC2,and a interval scores and others.
#' \itemize{
#'   \item scores_interval - The interval scores after PCA.
#'   \item ggplotPCA - a ggplot object with x-axis and y-axis are PC1 and
#'   PC2.
#'   \item others - others are the returns values of princomp.
#' }
#' @usage ggInterval_PCA(data = NULL,mapping = aes(NULL),plot=TRUE)
#' @examples
#' ggInterval_PCA(iris)
#'
#' mydata2<-RSDA::Cardiological
#' ggInterval_PCA(mydata2,aes(col="red",alpha=0.2))
#'
#' d<-mapply(c(10,20,40,80,160),c(20,40,80,160,320),FUN=runif,n=1000)
#' d<-data.frame(qq=matrix(d,ncol=4))
#' ggInterval_PCA(d)
#'
#' myIris<-classic2sym(iris,Species)
#' p<-ggInterval_PCA(myIris,plot=FALSE)
#' p$ggplotPCA
#' p$scores_interval
#'
#' @export
ggInterval_PCA<-function(data = NULL,mapping = aes(NULL),plot=TRUE){
  #data preparing
  argsNum<-length(mapping)
  args<-lapply(mapping[1:argsNum],FUN=rlang::get_expr)
  temp<-c(which(names(args)=="x"),which(names(args)=="y"))
  if(length(temp)>0){
    args.noXY<-args[-temp]
  }else{
    args.noXY<-args
  }

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData
  myRowNames <- rownames(iData)
  #preparing data
  temp.1<-ggSymData$statisticsDF$min
  temp.2<-ggSymData$statisticsDF$max
  n<-dim(temp.1)[1]
  p<-dim(temp.1)[2]

  #test big o
  if(n>2000){
    stop("Out of time limits.")
  }

  if(p<2){
    stop("ERROR : Variables need at least 2.")
  }
  vertice<-cbind(get_vertice(p),2*p+1)

  #build PCA data
  pcaData<-lapply(1:p,FUN=function(x) cbind(temp.1[,x],temp.2[,x]))
  pcaData<-data.frame(pcaData,c(1:n))
  pcaData<-as.matrix(pcaData)

  #Meger all pca data into a large matrix m
  m<-NULL
  for (i in 1:n){
    temp<-t(sapply(1:2^p,FUN=function(x) rbind(NULL,pcaData[i,vertice[x,]])))
    m<-rbind(m,temp)
  }

  #remove repeat value
  m<-m[!duplicated(as.data.frame(m[,1:p])),]
  m[, 1:p]<- apply(m[,1:p], 2, scale)

  #start PCA
  mypca<-stats::princomp(m[,1:p])
  PCscores<-data.frame(mypca$scores,u=m[,p+1])

  #build PCA interval data
  PCscores_interval <- matrix(0,nrow=n,ncol=2*p)
  for(obs in 1:n){
    for(var in 1:p){
      PCscores_interval[obs,2*var-1]<-min(PCscores[PCscores$u==obs,var])
      PCscores_interval[obs,2*var]<-max(PCscores[PCscores$u==obs,var])
    }
  }
  PCscores_interval<-as.data.frame(PCscores_interval)


  myname<-NULL
  for(i in 1:p){
    a<-paste0("PC",i,".min")
    b<-paste0("PC",i,".max")
    myname<-append(myname,append(a,b))
  }
  colnames(PCscores_interval)<-myname
  mypca$scores_interval<-adjustToRSDA(PCscores_interval)
  mypca$scores_interval[,"rowname"]<-rownames(iData)
  mypca$scores_interval<-tibble::column_to_rownames(mypca$scores_interval, var = "rowname")
  #等等從這裡 可以build rownames 因為現在rownames藏起來了
  #build Aesthetic (mapping)
  usermapping <- structure(as.expression(args.noXY),class="uneval") #Aesthetic without x,y
  mymapping <- list(PCscores_interval,
                    mapping=aes(xmin=PCscores_interval$PC1.min,
                                xmax=PCscores_interval$PC1.max,
                                ymin=PCscores_interval$PC2.min,
                                ymax=PCscores_interval$PC2.max,
                                fill=grDevices::gray.colors(n),alpha=0.5),col="black")
  allmapping <-as.list(structure(as.expression(c(usermapping,mymapping)),class="uneval"))

  #plot
  pcPlot<-ggplot(PCscores_interval,aes(PCscores_interval$PC1.min,
                                       PCscores_interval$PC2.min))+
    do.call(geom_rect,allmapping)+
    geom_text(label=myRowNames,size=3)+
    labs(x="PC1",y="PC2")+
    scale_fill_manual(name="Concept",
                      values=gray.colors(n),
                      labels=myRowNames)+
    guides(colour = FALSE, alpha = FALSE,fill=FALSE)
  mypca$ggplotPCA <- pcPlot
  if(plot){
    plot(pcPlot)
  }

  return(mypca)
}

get_vertice <- function(p){
  origin<-matrix(which(gtools::odd(1:(2*p))),nrow=1)
  for(i in p:1){
    temp<-origin
    temp[,i]<-temp[,i]+1
    origin<-rbind(origin,temp)
  }
  return(origin)
}
adjustToRSDA <- function(data){
  size<-dim(data)[1]*dim(data)[2]
  p<-dim(data)[2]/2
  tempd <- matrix(0,nrow=size/p,ncol=p)
  odd <- unlist(as.list(matrix(which(gtools::odd(1:nrow(tempd))),nrow=1)))
  even<-odd+1
  for(i in 1:p){
    tempd[odd,i]<-data[,i*2-1]
    tempd[even,i]<-data[,i*2]
  }
  tempd<-cbind(tempd,rep(1:(size/(p*2)),each=2))
  tempd<-as.data.frame(tempd)
  tempd<-RSDA::classic.to.sym(tempd,concept = colnames(tempd)[dim(tempd)[2]])
  colnames(tempd)<-paste0("PC_",1:p)
  return(tempd)
}



