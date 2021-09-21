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
#' @param type different type of radar,it can be "default","rect"
#' @usage ggInterval_radar(data=NULL,layerNumber=4,
#' inOneFig=FALSE,showLegend=TRUE,showXYLabs=FALSE,
#' plotPartial=NULL,
#' alpha=0.5,
#' base_circle=FALSE,
#' base_lty="solid",
#' addText=TRUE,
#' type="default")
#' @examples
#' mydata<-ggESDA::classic2sym(mtcars,k=4)$intervalData
#' ggInterval_radar(data=mydata[,c("mpg","disp",'drat')])
#' ggInterval_radar(data=mydata[,c("mpg","disp",'drat')],inOneFig = TRUE,plotPartial = c(2,3))
#'
#'
#' mydata<-ggESDA::classic2sym(iris,groupby = Species)$intervalData
#' ggInterval_radar(mydata,inOneFig = TRUE)+geom_text(aes(x=0.6,0.6),label="Add anything you want")
#' @export
ggInterval_radar <-function(data=NULL,layerNumber=4,
                            inOneFig=FALSE,showLegend=TRUE,showXYLabs=FALSE,
                            plotPartial=NULL,
                            alpha=0.5,
                            base_circle=TRUE,
                            base_lty=2,
                            addText=TRUE,
                            type="default"){
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
  if( !(type%in%c("default","rect")) ){
    warning(paste("There is no type called ",type,", automatically set default type."))
    type<-"default"
  }


  #data preprocessing
  numericData <- unlist(lapply(iData[,1:dim(data)[2]] ,FUN = RSDA::is.sym.interval))

  rawiData<-iData
  allnP <- dim(rawiData)[2]
  propData <-iData[,!numericData]
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

  if(allnP!=nP){
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
  textShift<-0.065
  textMin$cos<-textMin$cos+textShift;textMin$sin<-textMin$sin+textShift
  textMax$cos<-textMax$cos+textShift;textMax$sin<-textMax$sin+textShift


  #calculate nominal variable (point)
  if(allnP!=nP){
    # preparing the nominal data set
    tmpDf<-data.frame(NULL)
    for(var in colnames(propData)){
      propDf<-data.frame(NULL)
      counter<-1
      varNum <- 1
      for(ele in 1:indNum){
        varPro<-unlist(propData[ele,var])
        propDf[counter:(counter+(length(varPro)/2)-1),"varName"] <- rep(var,length(varPro)/2)
        propDf[counter:(counter+(length(varPro)/2)-1),"varLevels"]<-paste(varPro[1:(length(varPro)/2)],round(as.numeric(varPro[(length(varPro)/2+1):length(varPro)]),2),sep=":")
        propDf[counter:(counter+(length(varPro)/2)-1),"prop"]<-as.numeric(varPro[(length(varPro)/2+1):length(varPro)])
        propDf[counter:(counter+(length(varPro)/2)-1),"groupid"] <- rep(ele,length(varPro)/2)
        pos<-seq(0,1,1/((length(varPro)+2)/2));pos<-pos[-length(pos)][-1]
        propDf[counter:(counter+(length(varPro)/2)-1),"x"] <- d[nP+varNum,3] * pos
        propDf[counter:(counter+(length(varPro)/2)-1),"y"] <- d[nP+varNum,4] * pos
        counter<-counter+length(varPro)/2
      }
      tmpDf<-rbind(tmpDf,propDf)
      varNum<-varNum+1
    }
    propDf<-as.data.frame(tmpDf)
    propDf$groupid <- as.factor(propDf$groupid)


    # preparing the symbolic(rectangle) to represent nominal data
    totalRectDf <- data.frame(NULL)
    obsG<-1
    #lastRect <- NULL
    heiList <- list(NULL)
    for(i in 1:indNum){
      heiList[[i]]<-0
    }
    for(ele in levels(propDf$groupid)){
      propDf.ele <- dplyr::filter(propDf,groupid==ele)
      varG <-1
      tempRectDf <- data.frame(NULL)
      for(i in levels(as.factor(propDf$varName))){
        rectDf <- data.frame(NULL)
        propDf.temp <- dplyr::filter(propDf.ele,varName==i)
        varL <- 1
        heiVec <- NULL
        for(u in 1:dim(propDf.temp)[1]){
          #re range 0~1 to -0.9~0.5 (1 = 平面)
          #print(paste0(ele,i,u))
          #print(propDf.temp)
          thisHei<-reRange(-0.99,-0.8,propDf.temp[u,"prop"])
          heiVec<-cbind(heiVec,thisHei)
          rect <- build3DRect(-0.95,-0.95,thisHei,g=paste0(i,u,ele))
          #print(rect)
          distx <- mean(rect$newx-propDf.temp[u,"x"])
          disty <- mean(rect$newy[13:16]-propDf.temp[u,"y"])
          rect$newx <- rect$newx - distx
          rect$newy <- rect$newy - disty

          #add in height
          if(obsG>=2){
            rect$newy <- rect$newy + unlist(heiList[[obsG-1]][varG])[u]
          }

          rect[,"varLevels"]<-varL
          rectDf<-rbind(rectDf,rect)
          varL<-varL+1
        }#inner for
        heiList[[obsG]][varG]<-list(1+heiVec)
        if(obsG>=2){
          heiList[[obsG]][varG]<-list(unlist(heiList[[obsG-1]][varG])+unlist(heiList[[obsG]][varG]))
        }
        rectDf[,"varGroup"]<-varG
        tempRectDf<-rbind(tempRectDf,rectDf)
        varG<-varG+1
      }# sec for
      tempRectDf[,"obsGroup"]<-obsG
      obsG<-obsG+1
      # if(!is.null(lastRect)){
      #   tempRectDf$newy <- lastRect$newy+tempRectDf$newy
      # }
      totalRectDf<-rbind(totalRectDf,tempRectDf)
      #lastRect<-tempRectDf
      #print(lastRect)
    }# first for

    plotMin$group<-as.factor(plotMin$group)
    #totalRectDf<-as.data.frame(totalRectDf)
    totalRectDf$varLevels <- as.factor(totalRectDf$varLevels)
    totalRectDf$varGroup <- as.factor(totalRectDf$varGroup)
    totalRectDf$obsGroup <- as.factor(totalRectDf$obsGroup)
    levels(propDf$groupid)<-levels(plotMin$group)
  }
  #print(propDf)
  #print(heiList)
  #print(totalRectDf)


  #generate cut line for type==rect && build nominal rect
  if(type=="rect"){
    cutDf<-c(getCutDf(plotMin,transMatrix,nP,indNum),getCutDf(plotMax,transMatrix,nP,indNum))
  }

  #plot
  if(inOneFig){
    if(type=="default"){
      #if(fillBetween){
      myPolyData<-data.frame(NULL)
      for(i in levels(as.factor(plotMin$group))){
        plotMin.temp <- dplyr::filter(plotMin,group==i)
        plotMax.temp <- dplyr::filter(plotMax,group==i)
        myPathData<-data.frame(x1=plotMin.temp$cos,y1=plotMin.temp$sin,
                               x2=plotMax.temp$cos,y2=plotMax.temp$sin)
        if(allnP==nP){
          newTemp<-rbind(plotMin.temp,plotMin.temp[1,],plotMax.temp,plotMax.temp[1,])
          myPolyData<-rbind(myPolyData,newTemp)
        }else{ #add nominal
          propDf.temp <- dplyr::filter(propDf,groupid==i)
          tmp<-propDf[propDf.temp$prop==max(propDf.temp$prop),]
          tmp<-tmp[1,] #還沒想到一次連兩個點的方法 先只取第一個
          tmp2<-plotMin.temp[1,]
          tmp2[,c("cos","sin")]<-c(tmp$x,tmp$y)
          myPathData<-rbind(myPathData,data.frame(x1=tmp$x,y1=tmp$y,x2=tmp$x,y2=tmp$y))

          newTemp<-rbind(plotMin.temp,tmp2,
                         plotMin.temp[1,], plotMax.temp,tmp2,plotMax.temp[1,])
          myPolyData<-rbind(myPolyData,newTemp)
        }
        p<-p+geom_path(data=myPathData,aes(x=myPathData$x1, y=myPathData$y1),lty=0)+
          geom_path(data=myPathData,aes(x=myPathData$x2, y=myPathData$y2),lty=0)
      }
      p<-p+geom_polygon(data=myPolyData,aes(x=myPolyData$cos,y=myPolyData$sin,fill=group),alpha = alpha,col="black",lty=0)+
        geom_point(data=plotMin,aes(x=plotMin$cos,y=plotMin$sin,fill=plotMin$group,group=plotMin$group),size=2)+
        geom_point(data=plotMax,aes(x=plotMax$cos,y=plotMax$sin,fill=plotMax$group,group=plotMax$group),size=2)
    }
    #else{ #this is else for if(fillbetween)
    else if(type=="rect"){
      #add cut segment
      rectPolyData<-data.frame(NULL)
      gId <- 1
      for(g in levels(as.factor(cutDf[[1]]$obsGroup))){
        cutDf.temp<-lapply(1:4,FUN = function(x){as.data.frame(dplyr::filter(cutDf[[x]],obsGroup==g))})
        #print(cutDf.temp)
        for(i in 1:nP){
          tmpDf<-data.frame(x=c(cutDf.temp[[1]]$x2[i],cutDf.temp[[2]]$x2[i],cutDf.temp[[4]]$x2[i],cutDf.temp[[3]]$x2[i]),
                            y=c(cutDf.temp[[1]]$y2[i],cutDf.temp[[2]]$y2[i],cutDf.temp[[4]]$y2[i],cutDf.temp[[3]]$y2[i]),
                            varGroup=as.factor(rep(gId,4)),
                            obsGroup=as.factor(g))
          rectPolyData<-rbind(rectPolyData,tmpDf)
          gId<-gId+1
        }
      }
      #print(cutDf)
      #print(rectPolyData)
      #numeric
      p<-p+geom_polygon(data=rectPolyData,aes(x=x,y=y,group=varGroup,
                                              fill=obsGroup,col=obsGroup),alpha=alpha)


    }
    #nominal
    if(allnP!=nP){
      p<-p+geom_polygon(data=totalRectDf,aes(x=newx,y=newy,group=group,fill=obsGroup),
                        col="black",
                        alpha=alpha)+
        geom_text(data=propDf,aes(x=propDf$x+textShift,y=propDf$y+textShift,label=varLevels),
                  vjust=2.75)
    }
    p<-p+geom_segment(data=d,aes(x=0,y=0,xend=d$x,yend=d$y),lty=base_lty,alpha=0.6)+
      geom_point(data=plotMax,aes(x=0,y=0,alpha=plotMax$Variables))+
      geom_text(data=d,aes(x=d$x,y=d$y,label=c(colnames(rawiData))),vjust=-0.4,hjust=1)

    if(addText){
      p<-p+geom_text(data=textMin,aes(x=textMin$cos,y=textMin$sin,label=textMin$min))+
        geom_text(data=textMax,aes(x=textMax$cos,y=textMax$sin,label=textMax$max))
    }
    if(indNum==1 && type=="default" && allnP!=nP){
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
      base<-plotFun(p,iData,plotMin.temp,plotMax.temp,d,showXYLabs,showLegend,fillBetween,base_circle,layerNumber,textMin,textMax,rawiData,propDf,allnP,nP,type,cutDf)
      base<-base+labs(title=paste0("Radar : ",rownames(iData)[u]))+labs(title=paste0("Radar : ",rownames(iData)[u]))+scale_colour_discrete(name = "Group")+
        scale_alpha_discrete(name="Interval")

      plotList[[u]]<-base
      u<-u+1
    }
    p<-gridExtra::marrangeGrob(plotList,ncol=2,nrow=2)
  }
  return(p)
}

plotFun<-function(p,iData,plotMin.temp,plotMax.temp,d,showXYLabs,showLegend,fillBetween,base_circle,layerNumber,textMin,textMax,rawiData,propDf,allnP,nP,type,cutDf,totalRectDf){
  #if(fillBetween){
  if(type=="default"){
    myPolyData<-data.frame(NULL)
    newTemp<-rbind(plotMin.temp,plotMin.temp[1,],plotMax.temp,plotMax.temp[1,])
    myPolyData<-rbind(myPolyData,newTemp)
    myPathData<-data.frame(x1=plotMin.temp$cos,y1=plotMin.temp$sin,
                           x2=plotMax.temp$cos,y2=plotMax.temp$sin)

    #add nominal data
    if(indNum==1 && allnP!=nP){
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
    base<-p+geom_path(data=myPathData,aes(x=myPathData$x1, y=myPathData$y1))+
      geom_path(data=myPathData,aes(x=myPathData$x2, y=myPathData$y2))
    base<-base+geom_polygon(data=myPolyData,aes(x=myPolyData$cos,y=myPolyData$sin,fill=group),alpha = alpha,col="black")
  }else if(type=="rect"){
    #add cut segment
    rectPolyData<-data.frame(NULL)
    gId <- 1
    for(g in levels(as.factor(cutDf[[1]]$obsGroup))){
      cutDf.temp<-lapply(1:4,FUN = function(x){as.data.frame(dplyr::filter(cutDf[[x]],obsGroup==g))})
      #print(cutDf.temp)
      for(i in 1:nP){
        tmpDf<-data.frame(x=c(cutDf.temp[[1]]$x2[i],cutDf.temp[[2]]$x2[i],cutDf.temp[[4]]$x2[i],cutDf.temp[[3]]$x2[i]),
                          y=c(cutDf.temp[[1]]$y2[i],cutDf.temp[[2]]$y2[i],cutDf.temp[[4]]$y2[i],cutDf.temp[[3]]$y2[i]),
                          varGroup=as.factor(rep(gId,4)),
                          obsGroup=as.factor(g))
        rectPolyData<-rbind(rectPolyData,tmpDf)
        gId<-gId+1
      }
    }
    #print(cutDf)
    #print(rectPolyData)
    #numeric
    p<-p+geom_polygon(data=rectPolyData,aes(x=x,y=y,group=varGroup,
                                            fill=obsGroup,col=obsGroup),alpha=alpha)

    #nominal
    if(allnP!=nP && indNum==1){
      p<-p+geom_polygon(data=totalRectDf,aes(x=newx,y=newy,group=group),
                        col="black",fill="gray",
                        alpha=alpha)+
        geom_text(data=propDf,aes(x=propDf$x+textShift,y=propDf$y+textShift,label=varLevels),
                  vjust=2.75)
    }
  }#end rect

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
  if(type=="default" && allnP!=nP){
    base<-base+geom_point(data=propDf,aes(x=propDf$x,y=propDf$y,size=propDf$prop+0.5),alpha=0.5,
                          show.legend = F,col="black")+
      geom_text(data=propDf,aes(x=propDf$x+textShift,y=propDf$y+textShift,label=varLevels))
  }
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
getCutLine<-function(startPoint,len,perp){
  if(perp[1]==0&&perp[2]==0){
    stop("(0,0) vector cannot find a Perpendicular")
  }else if(perp[2]==0){
    if(perp[1]<0){
      return(c(startPoint[1]-len,startPoint[2]))
    }
    return(c(startPoint[1]+len,startPoint[2]))
  }else if(perp[1]==0){
    return(c(startPoint[1],startPoint[2]+len))
  }else{
    r<-perp[1]/perp[2]
  }
  #adjust <- abs(len/(perp[1]))
  adjust <- (len^2/((perp[1]^2)*(1+(1/r)^2)))^0.5
  result <-  c(perp[1]*adjust+startPoint[1] , perp[1]*adjust*(1/r)+startPoint[2])
  return(result)
}
getPerpendicular <-function(v=NULL,changeSide=TRUE){
  if(changeSide){
    if(is.vector(v)&&length(v)==2){
      if(v[1]==0&&v[2]==0){
        stop("cannot find perp. with a point")
      }else if(v[1]==0){
        return(c(-1,0))
      }else if(v[2]==0){
        return(c(0,-1))
      }else{
        return(c(-1,v[1]/v[2]))
      }
    }else{
      stop("generate perpendicular vector error, check data type and dimension.")
    }
  }else{
    if(is.vector(v)&&length(v)==2){
      if(v[1]==0&&v[2]==0){
        stop("cannot find perp. with a point")
      }else if(v[1]==0){
        return(c(1,0))
      }else if(v[2]==0){
        return(c(0,1))
      }else{
        return(c(1,-v[1]/v[2]))
      }
    }else{
      stop("generate perpendicular vector error, check data type and dimension.")
    }
  }
}
getCutDf <- function(tempDf,transMatrix,nP,indNum){
  startDf<-data.frame(x=tempDf$cos,y=tempDf$sin,group=tempDf$group)
  perp1<-lapply(1:nP,FUN=function(x) getPerpendicular(unlist(transMatrix[x,]),changeSide=T))
  perp2<-lapply(1:nP,FUN=function(x) getPerpendicular(unlist(transMatrix[x,]),changeSide=F))

  cutPos1<-sapply(1:(nP*indNum),FUN=function(x) {
    u <- x %% nP
    if(u==0){
      u<-nP
    }
    getCutLine(startDf[x,],0.025,perp1[[u]])
  })
  cutPos2<-sapply(1:(nP*indNum),FUN=function(x) {
    u <- x %% nP
    if(u==0){
      u<-nP
    }
    getCutLine(startDf[x,],0.025,perp2[[u]])
  })
  #print(as.data.frame(t(cutPos1)))
  cutDf1<-cbind(startDf,as.data.frame(t(cutPos1)))
  cutDf2<-cbind(startDf,as.data.frame(t(cutPos2)))
  colnames(cutDf1)<-c("x1","y1","obsGroup","x2","y2");colnames(cutDf2)<-c("x1","y1","obsGroup","x2","y2");
  cutDf1$x2<-as.numeric(cutDf1$x2);cutDf1$y2<-as.numeric(cutDf1$y2)
  cutDf2$x2<-as.numeric(cutDf2$x2);cutDf2$y2<-as.numeric(cutDf2$y2)
  return(list(data.frame(cutDf1),data.frame(cutDf2)))
}
build3DRect <- function(len=NULL,wid=NULL,hei=NULL,g=NULL){
  if(is.null(g)){
    g<-as.factor("g")
  }
  vertice<-get_vertice(3)
  vertice[c(3,4,7:8),2]<-vertice[c(3,4,7:8),2]+hei
  vertice[c(5:8),1]<-vertice[c(5:8),1]+wid
  vertice[c(2,4,6,8),3]<-vertice[c(2,4,6,8),3]+len
  vertice<-as.data.frame(vertice)
  vertice <- point3Dto2D(vertice)
  #rearrange
  vertice[,6:11]<-0
  vertice[c(1,2,4,3),6]<-paste0(g,1)
  vertice[c(3,4,8,7),7]<-paste0(g,2)
  vertice[c(5,6,8,7),8]<-paste0(g,3)
  vertice[c(1,2,6,5),9]<-paste0(g,4)
  vertice[c(1,3,7,5),10]<-paste0(g,5)
  vertice[c(2,4,8,6),11]<-paste0(g,6)
  d<-vertice[,c(4,5)]
  d<-rbind(d[c(1,2,4,3),],
           d[c(3,4,8,7),],
           d[c(5,6,8,7),],
           d[c(1,2,6,5),],
           d[c(1,3,7,5),],
           d[c(2,4,8,6),])
  group<-matrix(c(vertice[,6],vertice[,7],
                  vertice[,8],vertice[,9],
                  vertice[,10],vertice[,11]),ncol=1)
  group<-group[group!=0]
  d<-cbind(d,group)
  d$group <- as.factor(d$group)
  return(d)
}
reRange <- function(min,max,data){
  dist<-max-min
  return(data*dist-abs(min))
}


