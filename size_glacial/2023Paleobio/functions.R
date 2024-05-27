unitDates<-function(data){
  #constructs and runs Oxcal script for single phase with begin & end boundaries
  #returns full (parsed) Oxcal output
  if(nrow(data)>1){
    dateString<-paste0(apply(data,1,assembleDateString),collapse="\n")
  } else {
    dateString<-assembleDateString(data[[1]])  
  }
  code<-paste0("Options()\n   {\n    BCAD=FALSE;\n    Curve(\"IntCal20\",\"IntCal20.14c\");\n   };\n   Plot()\n   {\n    Sequence()\n    {\n     Boundary(\"begin\");\n     Phase(\"Pit ", 
               names(data), "\")\n     {",
               dateString, 
               "\n      Date(\"Event\");\n     };\n     Boundary(\"end\");\n    };\n   };\n  ",collapse="")
  execute<-executeOxcalScript(code)
  result<-parseOxcalOutput(readOxcalOutput(execute),only.R_Date = F)
  return(result)
}

unitDates.kde<-function(data){
  #constructs and runs Oxcal script for single phase with begin & end boundaries
  #uses KDE_Model() to compute event posterior probability distribution
  #returns full (parsed) Oxcal output
  ptm<-proc.time()
  if(nrow(data[[1]])>1){
    dateString<-paste0(apply(data[[1]],1,assembleDateString),collapse="\n")
  } else {
    dateString<-assembleDateString(data[[1]][[1,]])  
  }
  code<-paste0("Options()\n   {\n    BCAD=FALSE;\n    Curve(\"IntCal20\",\"IntCal20.14c\");\n   };\n   Plot()\n   {\n    Sequence()\n    {\n     Boundary(\"begin\");\n     Phase(\"Pit ", 
               names(data), 
               "\")\n     {\n     KDE_Model(\"pit\")\n     {\n     ",
               dateString, 
               "\n      Date(\"Event\");\n     };\n     };\n     Boundary(\"end\");\n    };\n   };\n  ",collapse="")
  execute<-executeOxcalScript(code)
  result<-parseOxcalOutput(readOxcalOutput(execute),only.R_Date = F)
  print((proc.time() - ptm)/nrow(data[[1]]))
  return(result)
}

unitDates.sum<-function(data){
  #constructs and runs Oxcal script for single phase with begin & end boundaries
  #uses Sum() to compute event posterior probability distribution
  #returns full (parsed) Oxcal output
  if(nrow(data)>1){
    dateString<-paste0(apply(data,1,assembleDateString),collapse="\n")
  } else {
    dateString<-assembleDateString(data[[1]])  
  }
  code<-paste0("Options()\n   {\n    BCAD=FALSE;\n    Curve(\"IntCal20\",\"IntCal20.14c\");\n   };\n   Plot()\n   {\n    Sequence()\n    {\n     Boundary(\"begin\");\n     Phase(\"Pit ", 
               names(data), 
               "\")\n     {\n     Sum(\"pit\")\n     {\n     ",
               dateString, 
               "\n      Date(\"Event\");\n     };\n     };\n     Boundary(\"end\");\n    };\n   };\n  ",collapse="")
  execute<-executeOxcalScript(code)
  result<-parseOxcalOutput(readOxcalOutput(execute),only.R_Date = F)
  return(result)
}

assembleDateString<-function(x) {
  #constructs string to input dates into Oxcal
  paste0("R_Date('",x["SpecNo"],"[",x["SampleNo"],"]',",x["Uncal14CYBP"],",",x["Uncal14CSE"],");",collapse="")
}

medianProbDist<-function(xy,quantile=0.5){
  #finds median of a probability distribution
  #uses a while loop; might suck efficiency-wise but it works
  #due to oxcAAR BC/AD bug, has been adjusted to subtract 1950 
  #from the returned median date of the probability distribution
  #because the BCAD=FALSE argument does not work in oxcAAR
  pairs<-cbind(xy[-nrow(xy),],xy[-1,])
  areas<-apply(pairs,1,trapezoid)
  a_ap<-a_last<-0
  a<-0.5*length(areas)
  int<-sum(areas[0:a])
  while(int!=quantile){
    if(a_ap == a) {
      a<-(a_ap+a_last)/2;break
    }
    a_ap<-a_last
    a_last<-a
    if(int>quantile){
      a<-a-1
    } else {
      a<-a+1
    }
    int<-sum(areas[0:a])
  }
  result<-(c(floor(xy[a,1]),xy[a,2])+c(-1950,0))*-1
  return(result)
}

trapezoid<-function(p){
  #returns area under a trapezoid
  #p must have arguments in order: x1, y1, x2, y2
  return(0.5*abs(p[2]+p[4])*abs(p[1]-p[3]))
}

extractChronology<-function(full){
  #wrapper for medianProbDist that takes unmodified result of unitDates()
  if (is.null(full[[1]])) {
    return (data.frame(age=NA,older=NA,younger=NA))
  } else {
    begin<-medianProbDist(full[[1]]$begin$posterior_probabilities)
    middle<-medianProbDist(full[[1]]$Event$posterior_probabilities)
    end<-medianProbDist(full[[1]]$end$posterior_probabilities)
    return(data.frame(age=middle$dates,older=begin$dates,younger=end$dates))
  }
}

weighted.se.mean <- function(x, w, na.rm = T){
  ## Remove NAs 
  if (na.rm) {
    i <- !is.na(x)
    w <- w[i]
    x <- x[i]
  }
  
  ## Calculate effective N and correction factor
  n_eff <- (sum(w))^2/(sum(w^2))
  correction = n_eff/(n_eff-1)
  
  ## Get weighted variance 
  numerator = sum(w*(x-weighted.mean(x,w))^2)
  denominator = sum(w)
  
  ## get weighted standard error of the mean 
  se_x = sqrt((correction * (numerator/denominator))/n_eff)
  return(se_x)
}
pcvar<-function(pca){
  return (pca$sdev^2 / sum(pca$sdev^2))
}

resampleElementbyPit<-function(sub,probdists=pitProb,pitbounds=boundaries){
  #return each specimen that comes from the specified pit 
  #with an age sampled from the pit distribution
  pitno<-sub$Pit[1]
  dist<-probdists[names(probdists)==pitno][[1]]
  bounds<-pitbounds[rownames(pitbounds)==pitno,]
  resampled.ages<-sample(dist[,1],nrow(sub),prob=dist[,2],replace = T)
  sub$AssignedAge<-resampled.ages
  return(sub)
}

resampleElementAll<-function(data,probdists=pitProb,pitbounds=boundaries,pits=goodPits){
  #return all specimens from an element dataset with new ages
  #remove lines without pit data
  data<-data[data$Pit%in%goodPits,]
  data<-data[!is.na(data$Pit),]
  #assign row numbers as sort index
  data$index<-row.names(data)
  #
  #resample
  resampled.bypit<-by(data,data$Pit,resampleElementbyPit)
  #make into data frame
  resampled.all<-rlist::list.stack(resampled.bypit)
  #sort by index into original order and return
  a<-"index"
  result<-subset(resampled.all[match(resampled.all$index,data$index),],select=-c(index))
  return(result)
}

bootstrapElement<-function(data,bins=climateBins,nreps=100){
  #return the data with bootstrap replicates
  #with their fake dates assigned to the appropriate climate bins
  #resample nreps times (default=100)
  result<-rlist::list.stack(replicate(nreps,resampleElementAll(data),simplify = F))
  #assign to climate bins
  result$Bin<-NA
  result[result$AssignedAge<bins$End[1],"Bin"]<-as.numeric(rownames(bins)[1])
  for(i in 2:(nrow(bins)-1)){
    result[result$AssignedAge<bins$End[i] & result$AssignedAge>bins$Start[i],"Bin"]<-rownames(bins)[i]
  }
  result[result$AssignedAge>bins$Start[nrow(bins)],"Bin"]<-rownames(bins)[nrow(bins)]
  return(result)
}
getRepStat<-function(data,index,statistic,nreps){
  #return indicated statistic for each replication
  samplestat <- rep(NA,nreps)
  noNA <- !is.na(data[,index]) & !is.na(data$repno)
  repPresent <- !is.na(match(1:nreps,unique(data[noNA,]$repno)))
  samplestat[repPresent]<-as.vector(by(data[noNA,index],data[noNA,]$repno,match.fun(statistic)))
  return(samplestat)
}
sqError<-function(data){
  return(var(data)*(length(data)-1))
}
bootTS<-function(data,reps=1000,timebins=climateBins){
  #return table of values and paleoTS object
  #run bootstrap replication function
  all<-bootstrapElement(data,nreps=reps,bins=timebins)
  #assign replicate numbers in order
  all$repno<-sort(rep(1:reps,nrow(all)/reps))
  #count total number of resampled specimens in each climate bin
  #counts.avg<-as.vector(by(all,all$Bin,nrow))/reps
  #find average age of all specimens in each climate bin
  binNames<-as.vector(by(as.numeric(all$Bin),all$Bin,mean))
  sampages<-rlist::list.cbind(by(all,all$Bin,getRepStat,index=which(colnames(all)=="AssignedAge"),stat="mean",nreps=reps))
  ages<-apply(sampages,2,mean,na.rm=T)[order(binNames)]
  #select columns with measurement data in them
  dimensions<-which(!colnames(all)%in%c("CatNo","Side","Sex","Collection","Pit","AssignedAge","Bin","repno"))
  #assemble result list
  result<-vector(mode="list",length=length(dimensions))
  names(result)<-colnames(all)[dimensions]
  for(i in 1:length(dimensions)){
    #mean of that dimension in each climate bin
    sampmeans<-rlist::list.cbind(by(all,all$Bin,getRepStat,index=dimensions[i],stat="mean",nreps=reps))
    means<-apply(sampmeans,2,mean,na.rm=T)[order(binNames)]
    #count for each bin in each replicate
    sampcounts<-rlist::list.cbind(by(all,all$Bin,getRepStat,index=dimensions[i],stat="length",nreps=reps))
    sampcounts[is.na(sampcounts)]<-0
    counts.avg<-apply(sampcounts,2,mean)[order(binNames)]
    #average variance of that dimension in each climate bin in each replicate
    sampsds<-rlist::list.cbind(by(all,all$Bin,getRepStat,index=dimensions[i],stat="sd",nreps=reps))
    sampsds[is.na(sampsds)]<-0
    vars.avg<-apply(sampsds,2,function(x) mean(x^2,na.rm=T))[order(binNames)]
    #return table of values and paleoTS object
    result[[i]]$table<-data.frame(means,vars.avg,counts.avg,ages)
    result[[i]]$table<-result[[i]]$table[which(vars.avg!=0),]
    result[[i]]$paleoTS<-paleoTS::as.paleoTS(
      mm=result[[i]]$table$means, 
      vv=result[[i]]$table$vars.avg, 
      #nn=rep(1,length(result[[i]]$table$means)), 
      nn=result[[i]]$table$counts.avg,
      tt=result[[i]]$table$ages, 
      label=colnames(all)[dimensions[i]])
    result[[i]]$dist<-table(all[,c("Pit","Bin")])
  }
  return(result)
}

getStat <- function(data,index,statistic){
  #return indicated statistic 
  noNA <- !is.na(data[,index]) & !is.na(data$repno)
  samplestat <- as.vector(by(data[noNA,index],data[noNA,]$repno,match.fun(statistic)))
  return(samplestat)
}

assembleRepTS <- function(repdata,dimensions) {
  result <- vector(mode = "list",length = length(dimensions))
  names(result) <- colnames(all)[dimensions]
  binNames <- as.vector(by(as.numeric(repdata$Bin),repdata$Bin,mean))
  sampages <- as.vector(by(repdata,repdata$Bin,getStat,index = which(colnames(repdata) == "AssignedAge"),statistic = "mean"))
  for (i in 1:length(dimensions)) {
    #mean of that dimension in each climate bin
    sampmeans <- as.numeric(by(repdata, repdata$Bin, getStat, index = dimensions[i], statistic = "mean", simplify = F))
    #count for each bin in each replicate
    sampcounts <- as.numeric(by(repdata, repdata$Bin, getStat, index = dimensions[i], statistic = "length", simplify = F))
    sampcounts[is.na(sampcounts)] <- 0
    #average variance of that dimension in each climate bin in each replicate
    sampsds <- as.numeric(by(repdata, repdata$Bin, getStat, index = dimensions[i],statistic = "sd", simplify = F))
    sampsds[is.na(sampsds)] <- 0
    sampvars <- sapply(sampsds,function(x) mean(x^2,na.rm = T))
    #return table of values and paleoTS object
    result[[i]]$table <- data.frame(mean = sampmeans, var = sampvars, 
                                    count = sampcounts, age = sampages)
    rownames(result[[i]]$table) <- binNames
    #drop any bin with count = 0 from paleoTS
    notempty <- subset(result[[i]]$table,result[[i]]$table$count > 0)
    result[[i]]$paleoTS <- paleoTS::as.paleoTS(
      mm = notempty$mean, 
      vv = notempty$var, 
      nn = notempty$count,
      tt = notempty$age, 
      label = colnames(repdata)[dimensions[i]])
    result[[i]]$dist <- table(repdata[,c("Pit","Bin")])
  }
  return(result)
}

bootReps <- function(data, reps = 1000, timebins = climateBins) {
  #return n = reps paleoTS time series
  #run bootstrap replication function
  all <- bootstrapElement(data,nreps = reps,bins = timebins)
  #assign replicate numbers in order
  all$repno <- sort(rep(1:reps,nrow(all)/reps))
  dimensions <- which(!colnames(all) %in% c("CatNo","Side","Sex","Collection","Pit","AssignedAge","Bin","repno"))
  #return table of values and paleoTS object
  result <- by(all, all$repno, assembleRepTS, dimensions = dimensions)
  #  result <- vector(mode = "list",length = reps)
  #  for (i in 1:reps) {
  #    result[[i]] <- assembleRepTS(subset(all,all$repno == i), 
  #                                 dimensions = dimensions)
  #  }
  return(result)
}

modelRepTS <- function(ts) {
  library(paleoTS)
  ts <- ts[[1]]
  tsLength <- length(ts$tt)
  if (any(as.numeric(ts["vv"]) == 0)) { pool <- TRUE } else { pool <- FALSE } 
  if (tsLength <= 5) { minb <- 2 } else { minb <- 3 }
  cl <- list(fnscale = -1.00000001)
  if (ts["vv"] == 0) { 
    punc <- fitGpunc(pool.var(ts,ret.paleoTS = TRUE),
                     minb = minb,pool = pool, cl = cl)
  } else {
    punc <- fitGpunc(ts,minb = minb,pool = pool,cl = cl)
  }
  grw <- opt.joint.GRW(ts,pool = pool)
  urw <- opt.joint.URW(ts,pool = pool)
  stasis <- opt.joint.Stasis(ts,pool = pool,cl = cl)
  params <- list(GRW = grw$parameters,
                 URW = urw$parameters,
                 Stasis = stasis$parameters,
                 Punc = punc$parameters)
  compare <- compareModels(grw, urw, stasis, punc)
  rownames(compare)[4] <- paste(rownames(compare)[4], ",Shift=",
                             punc$parameters[4], sep = "")
  return(list(compare = compare,params = params))
}
