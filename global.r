source("./script/dependencies.R")
source("./script/helper.R")
source("./script/setting.R")
source("./script/pca_fun.R")
source("./script/drop_exit_func.R")
source("./script/parameter.R")

VDemCY_map<-readRDS("./data/VDemCmap.rds")|>rename(Group=Groupe)|>
  ungroup()

VDemCY_data<-readRDS("./data/VDemCYdata.rds")

WGI_map<-readRDS("./data/WGImap.rds")|>rename(Group=Groupe)|>
  ungroup()
FSI_map<-readRDS("./data/FSImap.rds")|>rename(Group=Groupe)|>
  ungroup()

map <- readRDS("./data/world_map.RDS")

WGI_data<-readRDS("./data/WGIdata.rds")|>
  ungroup()|>
  filter(Country.Territory%in%WGI_map[["Country.Territory"]])
FSI_data<-readRDS("./data/FSIdata.rds")|>
  ungroup()|>
  filter(Country.Territory%in%FSI_map[["Country.Territory"]])

#---------------- Year-----------------
YearFSI<-sort(unlist(unique(FSI_data$Year)))
YearWGI<-sort(unlist(unique(WGI_data$Year)))
YearVDemCY<-sort(unlist(unique(VDemCY_data$Year)))

YearList<-list(FSI=YearFSI, WGI=YearWGI, VDemCY=YearVDemCY)

Map_data<-list(WGI=WGI_map, FSI=FSI_map,VDemCY=VDemCY_map)

Index_data<-list(WGI=WGI_data, FSI=FSI_data,VDemCY=VDemCY_data)
PCA_Varlist<-list(WGI=WGIvariable, FSI=FSIvariable,VDemCY=VDemCYvariable)


rm(WGI_data,FSI_data,VDemCY_data,WGI_map,FSI_map,VDemCY_map)