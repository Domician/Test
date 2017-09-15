# This is a R test code. Exploratory data analysis.

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)


Def_Path<-"C:/Users/Domi/Desktop/Kaggle/zillow_home"
file.path(data, "properties_2016.csv")


properties <- fread(file.path(Def_Path, "properties_2016.csv"))
transactions <- fread(file.path(Def_Path,'train_2016_v2.csv'))
sample_submission <- fread(file.path(Def_Path,'sample_submission.csv'))

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

head(properties)
?mutate

tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))

tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="darkgreen")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=1)
transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))
transactions %>%
  ggplot(aes(x=logerror))+
  geom_histogram(bins=500, fill="darkgreen")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))
  
head(transactions)
tmp2<- transactions %>% mutate(year_y=make_date(year=year(date)))
mpg %>%
  ggplot(aes(class,cty))+
  geom_jitter()+
  geom_boxplot()
head(mpg)
mpg$class =="minivan"
 mpg %>%
   ggplot(aes(x=class=="minivan"))+
   geom_histogram(bins=400,fill="darkgreen")
 head(mpg)
s_mpg<-mpg[mpg$class=="minivan",]

mpg %>%
  ggplot(aes(x=class))+
  geom_bar(fill="darkgreen")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=8))+
  ylab("Count")+
  xlab("Type of vehicle")

p <- mtcars %>% ggplot(aes(x=wt,y=mpg,color=factor(gear))) + geom_point() + facet_wrap(~am)
p+theme_replace()
  
head(mtcars)

tmp %>%
  ggplot(aes(x=year_month))+
  geom_bar(fill="darkred")
transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_logerror)) + 
  geom_line(size=1.5, color="red")+geom_point(size=5, color="red")+theme_bw()
missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values<- missing_values %>% mutate(useful_features = ifelse(missing_pct<0.25,1,0))
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct, fill= factor(useful_features))) +
  geom_bar(stat="identity")+
  coord_flip()+theme_light()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=8))
  
head(missing_values)
good_features<-filter(missing_values,missing_pct<0.25)
(good_features)

vars <- good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
dim(tmp)

corrplot(cor(tmp, use="complete.obs"),type="lower")
apropos("complete")
?complete.cases
tmp[complete.cases(tmp),]

cor_tmp %>% 
  ggplot(aes(x=build_year))+geom_line(stat="density", color='#d14d3c', size=1.2)+theme_bw()
cor_tmp %>%
  ggplot(aes(x=build_year))+
  geom_bar(stat='density',fill = '#d14d3c')+
  geom_line(stat="density", color='#3cb4d8', size=1.2)+
  theme_dark()

cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_abs_logerror = mean(abs(logerror)),n()) %>% 
  ggplot(aes(x=build_year,y=mean_abs_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.25))+theme_bw()

transactions <- transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),include.lowest = TRUE,labels=FALSE))

tmp1 <- transactions %>% 
  filter(percentile == 1) %>% 
  sample_n(1000) %>% 
  left_join(properties, by="id_parcel")
tmp2 <- transactions %>% 
  filter(percentile == 5) %>% 
  sample_n(1000) %>% 
  left_join(properties, by="id_parcel")
tmp3 <- transactions %>% 
  filter(percentile == 3) %>% 
  sample_n(1000) %>% 
  left_join(properties, by="id_parcel")

tmp1 <- tmp1 %>% mutate(type="best_fit")
tmp2 <- tmp2 %>% mutate(type="worst_fit")
tmp3 <- tmp3 %>% mutate(type="typical_fit")


tmp <- bind_rows(tmp1,tmp2,tmp3)
tmp <- tmp %>% mutate(type = factor(type,levels = c("worst_fit", "typical_fit", "best_fit")))
col_pal <- "Set1"

tmp %>% ggplot(aes(x=latitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)


transactions <- transactions %>% mutate(percentile = cut(,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),include.lowest = TRUE,labels=FALSE))
mapped<- transactions %>% 
  left_join(properties, by="id_parcel")



mapped %>% 
  ggplot(aes(x=longitude))+geom_line(stat="density", color='#d14d3c', size=1.2)+theme_bw()

mapped %>% 
  ggplot(aes(x=latitude))+geom_line(stat="density", color='#d14d3c', size=1.2)+theme_bw()

leaflet() %>% 
  addTiles() %>% 
  #fitBounds(-118.5,33.8,-118.25,34.15)  %>% 
  addRectangles(-118.5,33.8,-118.0,34.15) %>%
  addMiniMap()

install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
devtools::install_github('IRkernel/IRkernel')
library(crayon)
IRkernel::installspec()
