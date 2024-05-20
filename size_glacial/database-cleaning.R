#scripts
source("functions.R")
#get all bone measurements data from xlsx
taxonList<-read.csv("/home/vsyverson/Github/RLB/size_glacial/taxa.csv")
measurements<-vector(mode="list",nrow(taxonList))
allColNames.ij<-vector()
for (i in 1:nrow(taxonList)){
  path<-paste("/home/vsyverson/Github/RLB/size_glacial",taxonList$Folder[i],taxonList$Genus[i],taxonList$Filename[i],sep="/")
  sheetNames<-getSheetNames(path)
  names(measurements)[i]<-paste(taxonList$Genus[i],taxonList$Species[i],sep="_")
  if(length(sheetNames)==1){
    measurements[[i]]<-data.frame(read.xlsx(path,sheet=sheetNames))
    names(measurements)[i]<-paste(names(measurements)[i],sheetNames,sep="_")
    allColNames.ij<-append(allColNames.ij,colnames(measurements[[i]]))
  } else {
    measurements[[i]]<-vector(mode="list",length(sheetNames))
    for (j in 1:length(sheetNames)){
      measurements[[i]][[j]]<-data.frame(read.xlsx(path,sheet=sheetNames[j]))
      allColNames.ij<-append(allColNames.ij,colnames(measurements[[i]][[j]]))
    }
    names(measurements[[i]])<-sheetNames
  }
}

#rename fucking everything
allColNames<-unique(unlist(allColNames.ij))
usableColNames<-c("Collection","CatNo","Pit","Taxon","Element","Length","Width","Depth","ProxWidth","ProxDepth","MidWidth","MidDepth","DistWidth","DistDepth")
cleaned<-measurements
#P. atrox & Smilodon astragali (4.1, 5.3): measurement names?
i<-5;j<-3
c(names(cleaned[i]),names(cleaned[[i]][j]))
#fix error in list structure
#temp<-cleaned[[i]];elemName<-unlist(strsplit(names(cleaned[i]),"_"))[length(unlist(strsplit(names(cleaned[i]),"_")))]
#cleaned[[i]]<-list(temp);names(cleaned[[i]])<-elemName;names(cleaned)[i]<-paste(unlist(strsplit(names(cleaned[i]),"_"))[-length(unlist(strsplit(names(cleaned[i]),"_")))],collapse="_")
#rename columns
head(cleaned[[i]][[j]])
for (i in 1:length(cleaned)){
  for (j in 1:length(cleaned[[i]])){
    names(cleaned[[i]][[j]])<-unlist(lapply(names(cleaned[[i]][[j]]),function(x) {gsub("[.]","_",x)}))
  }
}
#cleaned[[i]][[j]]<-cleaned[[i]][[j]][,-c(2)]
#cleaned[[i]][[j]]$Pit<-"NA"
#cleaned[[i]][[j]]$Pit<-gsub("Modern","NA",cleaned[[i]][[j]]$Pit)
#colnames(cleaned[[i]][[j]])<-c("Pit","Length","Width","Depth","L4","L5","L6")
#colnames(cleaned[[i]][[j]])[11:14]<-c("OccLengthL","OccLengthR","OccWidthL","OccWidthR")
colnames(cleaned[[i]][[j]])[1]<-"CatNo"
head(cleaned[[1]][[1]])

allPitNames<-array();for (i in 1:length(cleaned)){for(j in 1:length(cleaned[i])){allPitNames<-c(allPitNames,cleaned[[i]][[j]]$Pit)}}
sort(as.numeric(unique(allPitNames)))
table(allPitNames)
Table2<-data.frame(Pits=as.numeric(subset(names(table(allPitNames)),table(allPitNames)>10)),
                   Counts=as.numeric(subset(table(allPitNames),table(allPitNames)>10)))
Table2<-Table2[order(Table2$Pits),]

testword<-"77"
for (i in 1:length(cleaned)){
  for (j in 1:length(cleaned[i])){
    test<-which(cleaned[[i]][[j]]$Pit==testword)
    if(length(test)>0) print(c(i,j))
  }
}
i<-23;j<-1
c(names(cleaned[i]),names(cleaned[[i]][j]))
subset(cleaned[[i]][[j]],cleaned[[i]][[j]]$Pit==testword)
#cleaned[[i]][[j]][which(cleaned[[i]][[j]]$Pit==testword),]$CatNo<-testword
cleaned[[i]][[j]][which(cleaned[[i]][[j]]$Pit==testword),]$Pit<-"67"
#cleaned[[i]][[j]]<-cleaned[[i]][[j]][-which(cleaned[[i]][[j]]$Pit==testword),]

all.elements<-vector();
for (i in 1:length(cleaned)){
  elementnames<-unlist(names(cleaned[[i]]))
  all.elements<-append(all.elements,elementnames)
}
unique(all.elements)
for (i in 1:length(cleaned)){names(cleaned[[i]])<-gsub("patella","Patella",names(cleaned[[i]]))}

for (i in 1:length(cleaned)) {
  for (j in 1:length(cleaned[[i]])) {
    colnames(cleaned[[i]][[j]])<-gsub("[.]","_",colnames(cleaned[[i]][[j]]))
    }
  }

#construct measurements database
#first the species lookup table
dbSendQuery(con,"drop table species;")
dbSendQuery(con,"create table species (
            Name varchar(250),
            Elements text ARRAY)")
for (i in 1:length(cleaned)){
  speciesname<-names(cleaned)[i]
  elementnames<-lapply(lapply(names(cleaned[[i]]), function(x){gsub("$.*","'",x)}), 
    function(x){gsub(".*^","'",x)})
  elementnames.sql<-paste0("ARRAY[",paste0(elementnames,collapse=","),"]",collapse="")
  dbSendQuery(con,paste0("insert into species(Name, Elements) values ('",speciesname,"',",elementnames.sql,");",collapse = ""))
}
#how to select with value in array
test<-dbGetQuery(con,"select * from species where 'm1'=any(Elements)");test;rm(test)

#now the individual measurements
allcolnames<-vector()
for (i in 1:length(cleaned)) {
  allcolnames<-append(allcolnames,unlist(lapply(cleaned[[i]],colnames)))
  }
typeLookup<-data.frame(Name=unique(allcolnames),Type=NA)
typeLookup[c(1,2,3,12),2]<-"varchar(100)"
typeLookup[-c(1,2,3,12),2]<-"numeric(100)"
tablenames<-vector()
for (i in 1:length(cleaned)){
  for (j in 1:length(cleaned[[i]])){
    tablename<-tolower(paste0(names(cleaned[i]),"_",names(cleaned[[i]][j])))
    tablenames<-append(tablenames,tablename)
    tablecols<-data.frame(colnames(cleaned[[i]][[j]]),colnames(cleaned[[i]][[j]]))
    for (k in 1:nrow(tablecols)){
      tablecols[k,2]<-typeLookup[which(typeLookup[,1]==tablecols[k,1]),2]
    }
    createquery<-paste(apply(tablecols,1,paste0,collapse=" "),collapse=", ")
    dbSendQuery(con,paste("drop table",tablename,collapse=" ;"))
    dbSendQuery(con,paste0("create table ",tablename," (",createquery,");",collapse =""))
    dbSendQuery(con,paste0("copy ",tablename," from stdin;"))
    postgresqlCopyInDataframe(con,cleaned[[i]][[j]])
  }
}
dbSendQuery(con,paste("drop table tablenames;"))
dbWriteTable(con,"tablenames",data.frame(tablenames),row.names=F)

save(cleaned,file="/home/vsyverson/Github/RLB/size_glacial/2023Paleobio/measurements.RData")
load("~/Github/RLB/size_glacial/2023Paleobio/measurements.RData")
cleaned<-meas

#put all element names into table
SuppTable1<-data.frame(Taxa=NA,Elements=NA,Dimensions=NA,Count=NA)
for (i in 1:length(cleaned)) {
  for (j in 1:length(cleaned[[i]])) {
    xx<-which(!colnames(cleaned[[i]][[j]])%in%c("Collection","Catalog..","CatNo","Pit","Taxon","Element"))
    measurements<-data.frame(cleaned[[i]][[j]][,xx])
    rows<-data.frame(array(dim=c(ncol(measurements),4)))
    names(rows)<-names(SuppTable1)
    rows[,1]<-names(cleaned[i])
    rows[,2]<-names(cleaned[[i]][j])
    rows[,3]<-colnames(cleaned[[i]][[j]])[xx]
    rows[,4]<-apply(!is.na(measurements),2,sum)
    SuppTable1<-rbind(SuppTable1,rows)
    }
  }
SuppTable1$Taxa<-gsub("_"," ",SuppTable1$Taxa)
SuppTable1$Elements<-gsub("TMT","Tarsometatarsus",SuppTable1$Elements)
write.csv(SuppTable1,file="SuppTable1",row.names = F)


