##deals with .csv file of format [Pit#,dim1,dim2,...]

#library(paleoTS)
#setwd("C:/Users/vjsyvers/Documents/R/RLB/csvs")
#pit.ages<-read.csv("pitages.csv") ##lookup array where [,1] contains pit numbers and [,2] their ages
# rawdata<-read.csv(file="caracara.csv")

make.paleoTS<-function(file,agelookup=TRUE){
	if(class(file)=="string") {rawdata<-read.csv(file)}	else rawdata<-file
	rawdata<-rawdata[!is.na(rawdata[,1]),][order(rawdata[,1]),]
	row.names(rawdata)<-1:nrow(rawdata)
	if(agelookup==TRUE){
		pitnumber<-rawdata[,1]
		spec.ages<-array(dim=dim(rawdata)[1])
		allpits<-levels(factor(sort(rawdata[,1])))
		for(i in 1:length(allpits)){
			in.pit.i<-which(pitnumber==allpits[i])
			if (allpits[i]%in%pit.ages[,1]){
				age<-pit.ages[which(pit.ages[,1]==allpits[i]),2]
				spec.ages[in.pit.i]<-age
				}
			}
		} else {
			spec.ages<-array(dim=dim(rawdata)[1])
			spec.ages<-rawdata[,1]
		}
	allnames<-list(unique(spec.ages),c("mm","vv","nn","tt"),colnames(rawdata)[2:length(colnames(rawdata))])
	output<-array(dim=c(length(unique(spec.ages)),4,dim(rawdata)[2]-1),dimnames=allnames)
	for (i in 1:length(unique(spec.ages))){
		if (!is.na(unique(spec.ages))[i]){
			in.age.i<-which(spec.ages==unique(spec.ages)[i])
			output[i,4,]<-mean(spec.ages[in.age.i])
			for (j in 2:(dim(rawdata)[2])){
				output[i,1,j-1]<-mean(rawdata[in.age.i,j],na.rm=T)
				output[i,2,j-1]<-var(rawdata[in.age.i,j],na.rm=T)
				output[i,3,j-1]<-length(in.age.i)
				}
			}
		}
	result<-list()
	for (j in 1:(dim(rawdata)[2]-1)){
		good<-which(!is.na(output[,1,j])&!is.na(output[,2,j])&!is.na(output[,3,j])&!is.na(output[,4,j]))
		good<-good[names(sort(output[good,4,j]))]
		result[[j]]<-as.paleoTS(mm=output[good,1,j],vv=output[good,2,j],nn=output[good,3,j],tt=output[good,4,j],oldest="last")
		names(result)[j]<-colnames(rawdata)[j+1]
		}
	return(result)
	}

owls.make.paleoTS<-function(rawdata){
	data<-rawdata[,-c(1,2)]
	spec.ages<-rawdata[,2]
	allnames<-list(unique(spec.ages),c("mm","vv","nn","tt"),colnames(data)[1:length(colnames(data))])
	output<-array(dim=c(length(unique(spec.ages)),4,dim(data)[2]),dimnames=allnames)
	for (i in 1:length(unique(spec.ages))){
		in.age.i<-which(spec.ages==unique(spec.ages)[i])
		output[i,4,]<-mean(spec.ages[in.age.i])
		for (j in 1:(dim(data)[2])){
			output[i,1,j]<-mean(data[in.age.i,j],na.rm=T)
			output[i,2,j]<-var(data[in.age.i,j],na.rm=T)
			output[i,3,j]<-length(in.age.i)
			}
		}
	result<-list()
	for (j in 1:(dim(data)[2])){
		good<-which(!is.na(output[,1,j])&!is.na(output[,2,j])&!is.na(output[,3,j])&!is.na(output[,4,j]))
		good<-good[names(sort(output[good,4,j]))]
		result[[j]]<-as.paleoTS(mm=output[good,1,j],vv=output[good,2,j],nn=output[good,3,j],tt=output[good,4,j])
		names(result)[j]<-colnames(data)[j]
		}
	return(result)
	}

fit3models.hacked<-function (y, pool = TRUE, silent = FALSE, method = c("AD", "Joint")) {
    method <- match.arg(method)
    if (method == "AD") {
        m1 <- opt.GRW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
        m2 <- opt.URW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10)))
        m3 <- opt.Stasis(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-9, 1e-9)))
    }
    else if (method == "Joint") {
        m1 <- opt.joint.GRW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
        m2 <- opt.joint.URW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10)))
        m3 <- opt.joint.Stasis(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
    }
    mc <- compareModels(m1, m2, m3, silent = silent)
    invisible(mc)
}	

fit4models.punc<-function (y, pool = TRUE, silent = FALSE, method = c("AD", "Joint"), gg, minb=2) {
    method <- match.arg(method)
    if (method == "AD") {
        m1 <- opt.GRW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
        m2 <- opt.URW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10)))
        m3 <- opt.Stasis(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-9, 1e-9)))
        m4 <- fitGpunc(y, pool = pool, minb=minb,method="AD")
    }
    else if (method == "Joint") {
        m1 <- opt.joint.GRW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
        m2 <- opt.joint.URW(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10)))
        m3 <- opt.joint.Stasis(y, pool = pool, cl=list(fnscale=-1, ndeps=c(1e-10, 1e-10)))
        m4 <- fitGpunc(y, pool = pool, minb=minb,method="Joint")
    }
    mc <- compareModels(m1, m2, m3, m4, silent = silent)
    invisible(mc)
}	
	
test3.paleoTS<-function(data){
	result<-array(dim=c(length(data),3))
	dimnames(result)<-list(names(data),c("GRW","URW","Stasis"))
	for(i in 1:length(data)){
#		print(names(data)[i])
		sink("NUL")
#		fits<-fit3models(data[[i]])
		fits<-fit3models.hacked(data[[i]])
		sink()
		result[i,]<-fits$Akaike.wt
		}
	return(result)
	}
