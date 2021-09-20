#' @name ggInterval_radar
#' @title A interval Radar plot
#' @description  Using ggplot2 package to make a radar plot with multiple
#' variables.Each variables contains min values and max values as a
#' symbolic data.
#' @import ggplot2 ggforce
#' @importFrom gridExtra marrangeGrob
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object.It can also be either RSDA object or
#' classical data frame(not recommended),which will be automatically convert to ggESDA
#' data.
#' @param layerNumber number of layer of a concentric circle,usually
#' to visuallize the reach of a observation in particularly variable.
#' @param inOneFig whether plot all observations in one figure.if not,
#' it will generate a new windows containing distinct observations.
#' @param showLegend whether show the legend.
#' @param showXYLabs whether show the x,y axis labels.
#' @param plotPartial a numeric vector,which is the row index from
#' the data.if it is not null,it will extract the row user deciding
#' to draw a radar plot from original data.Notes : the data must be
#' an interval data if the plotPartial is not null.
#' @param base_lty line type in base figure
#' @param base_circle boolean, if true, it will generate inner circle.
#' @param alpha aesthetic alpha of fill color
#' @param addText add value in figure
#' @usage ggInterval_radar(data=NULL,layerNumber=4,
#' inOneFig=FALSE,showLegend=TRUE,showXYLabs=FALSE,
#' plotPartial=NULL,
#' alpha=0.5,
#' base_circle=FALSE,
#' base_lty="solid",
#' addText=TRUE)
#' @examples
#' mydata<-ggESDA::classic2sym(mtcars,k=4)$intervalData
#' ggInterval_radar(data=mydata[,c("mpg","disp",'drat')])
#' ggInterval_radar(data=mydata[,c("mpg","disp",'drat')],inOneFig = TRUE,plotPartial = c(2,3))
#'
#'
#' mydata<-ggESDA::classic2sym(iris,groupby = Species)$intervalData
#' ggInterval_radar(mydata,inOneFig = TRUE)+geom_text(aes(x=0.6,0.6),label="Add anything you want")
#' @export
ggInterval_radar <- function(data=NULL,layerNumber=4,
                             inOneFig=FALSE,showLegend=TRUE,showXYLabs=FALSE,
                             plotPartial=NULL,fillBetween=TRUE,
                             alpha=0.5,
                             base_circle=TRUE,
                             base_lty=2,
                             addText=TRUE){
  fillBetween=TRUE #not fix complete
  #notes
  if(dim(data)[1]<=1){
    stop("ERROR IN data : data must be full data with observations larger than 1.")
  }
  #setting parameter
  nL <- layerNumber
  if(is.null(plotPartial)){
    #test data to iData
    ggSymData <- testData(data)
    iData <- ggSymData$intervalData
    indNum <-dim(iData)[1]
  }else{
    #checking input format
    if(!"symbolic_tbl"%in%class(data)){
      stop("ERROR : data must be symbolic data,when plotPartial is on.")
    }
    if(!is.numeric(plotPartial)){
      stop("ERROR IN plotPartial : the input data must be numeric vector as the row index in full data.")
    }
    if(!all(plotPartial<=nrow(data)) || !all(plotPartial>0)){
      stop("ERROR IN plotPartial : the input data must be numeric vector as the row index in full data.")
    }
    iData <- data
    indNum <- length(plotPartial)
  }
  #data preprocessing
  numericData <- unlist(lapply(iData[,1:dim(data)[2]] ,FUN = RSDA::is.sym.interval))

  if(indNum==1 || is.null(plotPartial) || !inOneFig){
    rawiData<-iData
    propData <-iData[,!numericData]
    allnP <- dim(rawiData)[2]
  }
  iData <- iData[,numericData]
  nP <- dim(iData)[2]

  #test ERROR
  if(indNum>50 & inOneFig==F){
    stop("Out of time limits")
  }else if(indNum>10 & inOneFig==T){
    stop("Suggest set inOneFig to FALSE when the observations are large.")
  }else if(indNum>5 & inOneFig==T){
    warning("Suggest set inOneFig to FALSE when the observations are large.")
  }

  #setting original plot
  #p<-generateCircle(nL)+coord_fixed(ratio = 1)
  if(base_circle){
    p<-generateCircle(nL)+coord_fixed(ratio = 1)
  }else{
    p<-ggplot()+coord_fixed(ratio = 1)
  }

  if(indNum==1 || is.null(plotPartial) || !inOneFig){
    d<-generatePoint(allnP,nL)
    d<-d[(allnP*nL-allnP+1):(allnP*nL),]
    propData<-propData[plotPartial,]
  }else{
    rawiData<-iData
    d<-generatePoint(nP,nL)
    d<-d[(nP*nL-nP+1):(nP*nL),]
  }
  p<-p+geom_point(data=d,aes(x=d$x,y=d$y))

  #get all variables min max data
  allData<-iData[,1:nP]
  if(!is.null(plotPartial)){
    iData<-iData[plotPartial,1:nP]
  }
  dataList<-lapply(1:nP,FUN=function(x) data.frame(allData[[x]]))
  iDataList<-lapply(1:nP,FUN=function(x) data.frame(iData[[x]]))
  maxList<-lapply(1:nP,FUN=function(x) max(dataList[[x]]))
  minList<-lapply(1:nP,FUN=function(x) min(dataList[[x]]))
  minminList<-minList;  maxmaxList<-maxList

  #normalize data to 0,1
  normData<-lapply(1:nP ,FUN=function(x){
    sapply(iDataList[[x]],FUN=function(elem) (elem-minList[[x]])/(maxList[[x]]-minList[[x]]))
  })
  #rescale dataframe to input form which data2Vec need
  minDF<-sapply(1:nP,FUN=function(x) matrix(normData[[x]],ncol=2)[,1])
  maxDF<-sapply(1:nP,FUN=function(x) matrix(normData[[x]],ncol=2)[,2])

  minDF<-matrix(minDF,ncol=nP)
  maxDF<-matrix(maxDF,ncol=nP)
  transMatrix<-d[1:nP,c(3,4)]

  #convert data to match radar axis scale
  plotMin<-data2Vec(iData=iData,data=minDF,transMat = transMatrix)
  plotMax<-data2Vec(iData=iData,data=maxDF,transMat = transMatrix)
  #making labels
  temp<-lapply(1:nP,FUN=function(x) paste(iDataList[[x]][,1],iDataList[[x]][,2],sep=":"))
  temp<-lapply(1:nP,FUN=function(x) paste0("[",temp[[x]],"]"))
  temp<-matrix(unlist(temp),ncol=indNum,byrow=T)
  groupId<-rep(c(1:indNum),each=nP)
  myLabel=paste(groupId,paste(colnames(iData),temp),sep=" : ")

  #new a labels variable
  plotMin<-data.frame(plotMin,Variables=myLabel)
  plotMax<-data.frame(plotMax,Variables=myLabel)

  #make text in plot
  # minList<-unlist(lapply(1:nP,FUN=function(x) iDataList[[x]][,1]))
  # maxList<-unlist(lapply(1:nP,FUN=function(x) iDataList[[x]][,2]))
  #
  newDf <- data.frame(NULL)
  for(i in 1:indNum){
    for(u in 1:nP){
      newDf<-rbind(newDf,iDataList[[u]][i,])
    }
  }
  #
  textMin<-as.data.frame(cbind(plotMin,min=newDf$min))
  textMax<-as.data.frame(cbind(plotMax,max=newDf$max))
  textShift<-0.05
  textMin$cos<-textMin$cos+textShift;textMin$sin<-textMin$sin+textShift
  textMax$cos<-textMax$cos+textShift;textMax$sin<-textMax$sin+textShift


  #calculate nominal variable
  if(indNum==1 || is.null(plotPartial) || !inOneFig){
    propDf<-data.frame()
    counter<-1
    for(var in colnames(propData)){
      varNum <- 1
      for(ele in 1:dim(propData)[1]){
        varPro<-unlist(propData[ele,var])
        propDf[counter:(counter+(length(varPro)/2)-1),"varName"] <- rep(var,length(varPro)/2)
        propDf[counter:(counter+(length(varPro)/2)-1),"varLevels"]<-paste(varPro[1:(length(varPro)/2)],round(as.numeric(varPro[(length(varPro)/2+1):length(varPro)]),2))
        propDf[counter:(counter+(length(varPro)/2)-1),"prop"]<-as.numeric(varPro[(length(varPro)/2+1):length(varPro)])
        propDf[counter:(counter+(length(varPro)/2)-1),"groupid"] <- as.factor(rep(ele,length(varPro)/2))
        pos<-seq(0,1,1/((length(varPro)+2)/2));pos<-pos[-length(pos)][-1]
        propDf[counter:(counter+(length(varPro)/2)-1),"x"] <- d[nP+varNum,3] * pos
        propDf[counter:(counter+(length(varPro)/2)-1),"y"] <- d[nP+varNum,4] * pos
        counter<-counter+length(varPro)/2
      }
    }
  }

  if(inOneFig){
    if(fillBetween){
      myPolyData<-data.frame(NULL)
      for(i in levels(as.factor(plotMin$group))){
        plotMin.temp <- dplyr::filter(plotMin,group==i)
        plotMax.temp <- dplyr::filter(plotMax,group==i)
        newTemp<-rbind(plotMin.temp,plotMin.temp[1,],plotMax.temp,plotMax.temp[1,])
        myPolyData<-rbind(myPolyData,newTemp)
        myPathData<-data.frame(x1=plotMin.temp$cos,y1=plotMin.temp$sin,
                               x2=plotMax.temp$cos,y2=plotMax.temp$sin)
        #add nominal
        if(indNum==1){
          tmp<-propDf[propDf$prop==max(propDf$prop),]
          tmp<-tmp[1,] #還沒想到一次連兩個點的方法 先只取第一個
          tmp2<-plotMin.temp[1,]
          tmp2[,c("cos","sin")]<-c(tmp$x,tmp$y)
          myPathData<-rbind(myPathData,data.frame(x1=tmp$x,y1=tmp$y,x2=tmp$x,y2=tmp$y))
          myPolyData<-data.frame(NULL)
          newTemp<-rbind(plotMin.temp,tmp2,
                         plotMin.temp[1,], plotMax.temp,tmp2,plotMax.temp[1,])
          myPolyData<-rbind(myPolyData,newTemp)
        }
        p<-p+geom_path(data=myPathData,aes(x=myPathData$x1, y=myPathData$y1),lty=0)+
          geom_path(data=myPathData,aes(x=myPathData$x2, y=myPathData$y2),lty=0)
      }
      p<-p+geom_polygon(data=myPolyData,aes(x=myPolyData$cos,y=myPolyData$sin,fill=group),alpha = alpha,col="black",lty=0)
    }else{
      p<-p+geom_polygon(data=plotMin,aes(x=plotMin$cos,y=plotMin$sin,group=plotMin$group,
                                         fill=plotMin$group,colour=plotMin$group),alpha=alpha,size = 1,
                        show.legend = F)+
        geom_polygon(data=plotMax,aes(x=plotMax$cos,y=plotMax$sin,group=plotMax$group,
                                      fill=plotMax$group,colour=plotMax$group),alpha=0.1,size = 0.2,
                     show.legend = F)
    }
    p<-p+geom_point(data=plotMin,aes(x=plotMin$cos,y=plotMin$sin,col=plotMin$group,fill=plotMin$group,
                                     group=plotMin$group),size=2)+
      geom_segment(data=d,aes(x=0,y=0,xend=d$x,yend=d$y),lty=base_lty,alpha=0.6)+
      geom_point(data=plotMax,aes(x=plotMax$cos,y=plotMax$sin,col=plotMax$group,fill=plotMax$group,
                                  group=plotMax$group),size=2)+
      geom_point(data=plotMax,aes(x=0,y=0,alpha=plotMax$Variables))+
      geom_text(data=d,aes(x=d$x,y=d$y,label=c(colnames(rawiData))),vjust=-0.4,hjust=1)

    if(addText){
      p<-p+geom_text(data=textMin,aes(x=textMin$cos,y=textMin$sin,label=textMin$min))+
        geom_text(data=textMax,aes(x=textMax$cos,y=textMax$sin,label=textMax$max))
    }
    if(indNum==1){
      #add nominal var
      p<-p+geom_point(data=propDf,aes(x=propDf$x,y=propDf$y,size=propDf$prop+0.5),alpha=0.5,
                      show.legend = F,col="black")+
        geom_text(data=propDf,aes(x=propDf$x+textShift,y=propDf$y+textShift,label=varLevels))

    }
    if(!showXYLabs){
      p<-p+scale_x_continuous(labels =NULL,limits = c(-1.25,1.25))+
        scale_y_continuous(labels =NULL,limits = c(-1.25,1.25))+xlab(NULL)+ylab(NULL)
    }else{
      #adds scale y continuous label
      yLabelPos<-seq(-1.25,1.25,(1.25-(-1.25))/(nP+1))
      yLabelPos<-yLabelPos[-1]
      yLabelTitle<-paste0(colnames(iData),": [")
      yLabels<-paste0(yLabelTitle,paste(unlist(minminList),unlist(maxmaxList),sep=","),"]")
      yLabels<-c(yLabels,expression(bold("The range of each variable show as follows :")))
      p<-p+scale_x_continuous(limits = c(-1.25,1.25))+
        scale_y_continuous(breaks=yLabelPos,labels=yLabels,limits = c(-1.25,1.25))+xlab(NULL)+ylab(NULL)+
        scale_x_continuous(labels=NULL,limits = c(-1.25,1.25))
    }#showLegend
    if(showLegend){
      p<-p+guides(fill=F)
    }else{
      p<-p+guides(fill=F,col=F,alpha=F)
    }
    p<-p+labs(title=paste0("Radar : ",paste(sapply(rownames(iData), paste, collapse=":"), collapse=",")))+scale_colour_discrete(name = "Group")+
      scale_alpha_discrete(name="Interval")+theme_bw()



    #make circle interpret
    if(base_circle){
      a<-cos(120*pi/180)
      b<-sin(120*pi/180)

      tmp<-seq(0,1,1/layerNumber)[-1]
      tmp<-data.frame(x=a*tmp,y=b*tmp,label=paste0(as.character(round(tmp*100)),"%"))
      p<-p+geom_text(data=tmp,aes(x=x,y=y,label=label))
    }
  }else{
    plotList <- NULL
    u<-1
    for(i in levels(as.factor(plotMin$group))){
      #print(i)
      plotMin.temp <- dplyr::filter(plotMin,plotMin$group==i)
      plotMax.temp <- dplyr::filter(plotMax,plotMax$group==i)
      #print(paste(u,plotMin.temp))
      base<-plotFun(p,iData,plotMin.temp,plotMax.temp,d,showXYLabs,showLegend,fillBetween,base_circle,layerNumber,textMin,textMax,rawiData,propDf)
      base<-base+labs(title=paste0("Radar : ",rownames(iData)[u]))+labs(title=paste0("Radar : ",rownames(iData)[u]))+scale_colour_discrete(name = "Group")+
        scale_alpha_discrete(name="Interval")

      plotList[[u]]<-base
      u<-u+1
    }
    # print(gridExtra::marrangeGrob(list(u1),ncol=2,nrow=2))
    # print(gridExtra::marrangeGrob(list(u2),ncol=2,nrow=2))
    # print(gridExtra::marrangeGrob(list(u3),ncol=2,nrow=2))
    # plotList<-paste0("u",seq(1,indNum,1))
    # plotList<-paste(sapply(plotList, paste, collapse=","), collapse=",")
    # plotList<-eval(parse(text=paste0("list(",eval(plotList),")")))
    #print(gridExtra::marrangeGrob(plotList,ncol=2,nrow=2))
    p<-gridExtra::marrangeGrob(plotList,ncol=2,nrow=2)
  }
  return(p)
}

plotFun<-function(p,iData,plotMin.temp,plotMax.temp,d,showXYLabs,showLegend,fillBetween,base_circle,layerNumber,textMin,textMax,rawiData,propDf){
  if(fillBetween){
    myPolyData<-data.frame(NULL)
    newTemp<-rbind(plotMin.temp,plotMin.temp[1,],plotMax.temp,plotMax.temp[1,])
    myPolyData<-rbind(myPolyData,newTemp)
    myPathData<-data.frame(x1=plotMin.temp$cos,y1=plotMin.temp$sin,
                           x2=plotMax.temp$cos,y2=plotMax.temp$sin)

    #add nominal data
    tmp<-propDf[propDf$prop==max(propDf$prop),]
    tmp<-tmp[1,] #還沒想到一次連兩個點的方法 先只取第一個
    tmp2<-plotMin.temp[1,]
    tmp2[,c("cos","sin")]<-c(tmp$x,tmp$y)
    myPathData<-rbind(myPathData,data.frame(x1=tmp$x,y1=tmp$y,x2=tmp$x,y2=tmp$y))
    myPolyData<-data.frame(NULL)
    newTemp<-rbind(plotMin.temp,tmp2,
                   plotMin.temp[1,], plotMax.temp,tmp2,plotMax.temp[1,])
    myPolyData<-rbind(myPolyData,newTemp)

    base<-p+geom_path(data=myPathData,aes(x=myPathData$x1, y=myPathData$y1))+
      geom_path(data=myPathData,aes(x=myPathData$x2, y=myPathData$y2))
    base<-base+geom_polygon(data=myPolyData,aes(x=myPolyData$cos,y=myPolyData$sin,fill=group),alpha = alpha,col="black")
  }else{
    base<-p+geom_polygon(data=plotMin.temp,aes(x=plotMin.temp$cos,y=plotMin.temp$sin,group=plotMin.temp$group,
                                               fill=plotMin.temp$group,colour=plotMin.temp$group),alpha=alpha,size = 1,
                         show.legend = F)+
      geom_polygon(data=plotMax.temp,aes(x=plotMax.temp$cos,y=plotMax.temp$sin,group=plotMax.temp$group,
                                         fill=plotMax.temp$group,colour=plotMax.temp$group),alpha=0.1,size = 0.2,
                   show.legend = F)
  }
  base<-base+geom_point(data=plotMin.temp,aes(x=plotMin.temp$cos,y=plotMin.temp$sin,col=plotMin.temp$group,fill=plotMin.temp$group,
                                              group=plotMin.temp$group),size=2)+
    geom_segment(data=d,aes(x=0,y=0,xend=d$x,yend=d$y),lty=base_lty,alpha=0.6)+
    geom_point(data=plotMax.temp,aes(x=plotMax.temp$cos,y=plotMax.temp$sin,col=plotMax.temp$group,fill=plotMax.temp$group,
                                     group=plotMax.temp$group),size=2)+
    geom_point(data=plotMax.temp,aes(x=0,y=0,alpha=plotMax.temp$Variables))+
    geom_text(data=d,aes(x=d$x,y=d$y,label=colnames(rawiData)),vjust=-0.4,hjust=1)+
    geom_text(data=textMin,aes(x=textMin$cos,y=textMin$sin,label=textMin$min))+
    geom_text(data=textMax,aes(x=textMax$cos,y=textMax$sin,label=textMax$max))

  #add nominal var
  base<-base+geom_point(data=propDf,aes(x=propDf$x,y=propDf$y,size=propDf$prop+0.5),alpha=0.5,
                        show.legend = F,col="black")+
    geom_text(data=propDf,aes(x=propDf$x+textShift,y=propDf$y+textShift,label=varLevels))

  if(!showXYLabs){
    base<-base+scale_x_continuous(labels =NULL,limits = c(-1.25,1.25))+
      scale_y_continuous(labels =NULL,limits = c(-1.25,1.25))+xlab(NULL)+ylab(NULL)
  }else{
    base<-base+scale_x_continuous(limits = c(-1.25,1.25))+
      scale_y_continuous(limits = c(-1.25,1.25))
  }#showLegend
  if(showLegend){
    base<-base+guides(fill=F)
  }else{
    base<-base+guides(fill=F,col=F,alpha=F)
  }

  if(base_circle){
    a<-cos(120*pi/180)
    b<-sin(120*pi/180)

    tmp<-seq(0,1,1/layerNumber)[-1]
    tmp<-data.frame(x=a*tmp,y=b*tmp,label=paste0(as.character(round(tmp*100)),"%"))
    base<-base+geom_text(data=tmp,aes(x=x,y=y,label=label))
  }

  base<-base+theme_bw()
  return(base)
}

data2Vec <- function(iData=NULL,data=NULL,transMat=NULL){
  if(dim(data)[2]!=dim(transMat)[1]){
    stop("ERROR : Cannot match data and transMat")
  }
  result=data.frame(NULL)
  for(i in 1:dim(data)[1]){
    result <- rbind(result,data.frame(data[i,]*transMat,
                                      group=paste(paste("Group",as.factor(i)),rownames(iData)[i],sep=" : ")))
  }
  return(result)
}
generateCircle <- function(nLayer=NULL){
  if(is.null(nLayer)||nLayer<=0||nLayer>30){
    stop("Illegal layerNumber input. Recommended value will between 2 and 10.")
  }
  circles <- data.frame(
    x0 = rep(0,nLayer),
    y0 = rep(0,nLayer),
    r = round(seq(0,1,1/nLayer)[2:(nLayer+1)],2)
  )
  p<-ggplot() +
    ggforce::geom_circle(aes(x0 = circles$x0, y0 = circles$y0, r = circles$r), data = circles)
  return(p)
}
generatePoint<-function(nPoly=NULL,nLayer=NULL){
  if(is.null(nPoly)||nPoly<=0||nPoly>360){
    stop("ERROR : Illegal parameter input in nPoly")
  }
  if(is.null(nLayer)||nLayer<=0||nLayer>30){
    stop("ERROR : Illegal parameter input in nLayer")
  }
  rList = round(seq(0,1,1/nLayer)[2:(nLayer+1)],2)
  degreeUnit<-round(360/nPoly)
  allPoint<-data.frame(NULL)
  for(r in rList){
    allPoint <- rbind(allPoint,data.frame(x=0,y=r,cos=0,sin=1))
    for(i in 1:(nPoly-1)){
      degree<-90+degreeUnit*i
      x1<-cos(degree*pi/180)*r
      y1<-sin(degree*pi/180)*r
      allPoint <- rbind(allPoint,data.frame(x=x1,y=y1,
                                            cos=cos(degree*pi/180),
                                            sin=sin(degree*pi/180)))
    }
  }
  return(allPoint)
}
