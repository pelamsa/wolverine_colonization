library(landscapemetrics)
library(terra)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggrepel)

## INPUT DATA
points = read.csv("") # path for your file here, snow-track data
landcover.rast = rast("") # path for your file here, raster with forested areas

# Chosen buffers
buffer.list=c(3131, 20000)

# Calculate patch density for each triangle within selected buffer
metrics.tab.list=list()
for(b in 1:length(buffer.list)){
  buffer.value=buffer.list[b]
  buffer.value
  buff.points=terra::buffer(points,width=buffer.value)
  metrics.list=list()
  for(j in 1:length(buff.points$triangle_ID)){
    print(paste0(j,"/",length(buff.points$triangle_ID)))
    sample = subset(buff.points,buff.points$triangle_ID==buff.points$triangle_ID[j])
    landcover.sample=crop(landcover.rast,sample,mask=T)
    my_metric1 = lsm_c_pd(landcover.sample)
    my_metric1$id=buff.points$triangle_ID[j]
    metrics.list[[j]]=my_metric1
  }
  metrics.tab=do.call(rbind,metrics.list)
  metrics.tab$buffer=buffer.value
  metrics.tab
  metrics.tab.list[[b]]=metrics.tab
}

# Combine and save results
metricsbuffer.tab=do.call(rbind,metrics.tab.list)
metricsbuffer.tab=metricsbuffer.tab[which(metricsbuffer.tab$class==1),] # select only forested areas
metricsbuffer.tab$id=as.factor(metricsbuffer.tab$id)

fwrite(metricsbuffer.tab,file="") # add path and name
