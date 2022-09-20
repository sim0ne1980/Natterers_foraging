require(rgdal)
require(raster)
require(maptools)
require(adehabitatLT)
require(adehabitatHR)
require(SDMTools)
require(rgeos)

options(stringsAsFactors=F)
setwd("C:/Users/simonemordue/THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Drivers update - Documents/Natterers_foraging")


bats.polys1<-readOGR("./data/All_stHildas.shp", layer="All_stHildas")
iter<-5# need to change to 5
pts_per_poly<-10 #need to change to 1
fit<-95# need to change to 95%

bats.points<-list()
bats.clusters<-list()
bats.intersections<-list()
bats<-unique(bats.polys1@data[,"MERGE_SRC"])
for(bat in 1:length(bats)){
  bat.points<-list()
  bat.clusters<-list()
  for(i in 1:iter){
    iter.points<-list()
    iter.clusters<-list()
    bat.polys<-bats.polys1[which(bats.polys1@data[,"MERGE_SRC"]==bats[bat]),]
    for(poly in 1:nrow(bat.polys)){
      poly.date<-bat.polys[poly,]@data[,"Date_new"]
      poly.time<-bat.polys[poly,]@data[,"Time"]
      poly.points<-spsample(bat.polys[poly,],n=pts_per_poly,type="random")
      poly.data<-data.frame("Bat_name"=rep(bats[bat],pts_per_poly),"Date"=rep(poly.date,pts_per_poly),"Time"=rep(poly.time,pts_per_poly),"RepID"=rep(i,pts_per_poly),"PtID"=seq(1,pts_per_poly))
      poly.data[,"ClustID"]<-paste(bats[bat],"_",i,sep="")
      iter.points[[poly]]<-SpatialPointsDataFrame(poly.points,poly.data)
    }
    bat.points[[i]]<-do.call("rbind",iter.points)
    iter.clusters<-clusthr(bat.points[[i]][,"ClustID"])
    iter.clusters<-getverticeshr(iter.clusters,percent=fit)
    iter.clusters<-gBuffer(iter.clusters,byid=T,width=4)#changed from 1.0
    iter.clusters<-gBuffer(iter.clusters,byid=T,width=-4)
    bat.clusters[[i]]<-iter.clusters[rev(order(iter.clusters$area)),]
    if(i==2){bats.intersections[[bat]]<-gIntersection(bat.clusters[[1]],bat.clusters[[2]],byid=T)}
    if(i>2){bats.intersections[[bat]]<-gIntersection(bats.intersections[[bat]],bat.clusters[[i]])}
  }
  bats.points[[bat]]<-do.call("rbind",bat.points)
  bats.clusters[[bat]]<-do.call("rbind",bat.clusters)
  plot(bat.polys)
  plot(bats.points[[bat]],add=T)
  plot(bats.clusters[[bat]],col="green",add=T)
  plot(bats.intersections[[bat]],col="red", add=T)
}
bats.points<-do.call("rbind",bats.points)
bats.clusters<-do.call("rbind",bats.clusters)
bats.intersections<-lapply(1:length(bats.intersections),function(x,bats.intersections) spChFIDs(bats.intersections[[x]],as.character(x)),bats.intersection=bats.intersections)
bats.intersections<-do.call("rbind",bats.intersections)
bats.intersections<-SpatialPolygonsDataFrame(bats.intersections,data.frame("Bat_name"=bats))
plot(bats.polys1)
plot(bats.points,add=T)
plot(bats.clusters,col="green",add=T)
plot(bats.intersections,col="red",add=T)
writeOGR(bats.points,"./output/SH_points.shp",layer="points",driver="ESRI Shapefile")
writeOGR(bats.clusters,"./output/SH_clusters.shp",layer="clusters",driver="ESRI Shapefile")
writeOGR(bats.intersections,"./output/SH_intersections.shp",layer="intersections",driver="ESRI Shapefile")
