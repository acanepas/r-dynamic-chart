library(ggplot2)
library(gganimate)
library(reshape2)
library(sqldf)
library(readstata13)

####Function to download and extract the files from Worldbank.org####
download_csv = function(url) {
  path <- tempfile(fileext = ".zip")
  download.file(url,destfile = path,mode="wb")
  unzip(zipfile = path,exdir=tempdir())
  file = unzip(zipfile = path, list=T)[2,1]
  filepath=paste0(paste0(strsplit(path,"\\\\")[[1]][1:7],collapse="\\"),"\\",file)
  df=read.csv2(filepath,sep=",",skip=4,stringsAsFactors = F)
  colnames(df)= gsub("X","",colnames(df))
  colnames(df)= gsub("\\.","",colnames(df))
  indicator = strsplit(df[1,3]," ")[[1]][1]
  df = df[,c(1:2,5:62)]
  df = melt(data=df,id.vars = c("CountryName","CountryCode"),variable.name = "Year",value.name = indicator,na.rm=T)
  df[df[,4] =="",4]=0
  df= df[df[,4]!=0,]
}

##Data sources urls ####
url_gdp = "http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=csv"

###Execute the gunctions###
gdp = download_csv(url_gdp)
gini = download_csv(url_gini)

##Download continent data from datahub.io#####
continent = read.csv2(file="https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/r/country-and-continent-codes-list-csv.csv",sep=",")
duplicate_countries = c('Georgia','Russian Federation','Armenia, Republic of','Kazakhstan, Republic of','Turkey, Republic of')
colnames(continent)=gsub("_","",colnames(continent))
continent = continent[ !(as.character(continent$CountryName) %in%  duplicate_countries & continent$ContinentName == "Europe"),]

###Data Source from World Income Inequality Database - WIID4 ###
url_gini_db="https://www.wider.unu.edu/sites/default/files/WIID_19Dec2018.zip"

path <- tempfile(fileext = ".zip")
download.file(url_gini_db,destfile = path,mode="wb")
unzip(zipfile = path,exdir=tempdir())
file = unzip(zipfile = path, list=T)[1,1]
filepath=paste0(paste0(strsplit(path,"\\\\")[[1]][1:7],collapse="\\"),"\\",file)
gini_db =read.dta13(filepath)

##Transform data source to get an unique Gini record for each country and year##
gini_db = gini_db[gini_db$resource == "Income (net)",c(2,3,5,6,24)]
gini_db = gini_db[!is.na(gini_db$country)]
gini_db = sqldf("select country,c3,year,min(gini_reported)gini_reported from gini_db group by country,c3,year")
gini_db = gini_db[2:nrow(gini_db),]

##Filter Dataset###
nn = sqldf("select country,min(year)min_year,max(year)max_year,count(country)c,count(distinct year)n_years from gini_db group by country order by c desc")
nn = nn[nn$n_years >=20,]
gini_db = gini_db[gini_db$country %in% nn$country,]

##Mix the two datasources##
 gdp_gini = sqldf("select ContinentName,g.country as CountryName,g.year Year,gini_reported GINI,GDP from gini_db g 
                  inner join gdp on gdp.CountryCode=g.c3 and gdp.Year=g.year 
                  left join continent c on c.ThreeLetterCountryCode = g.c3")
 gdp_gini$Year = as.integer(as.character(gdp_gini$Year))
 gdp_gini$GINI = as.numeric(as.numeric(gdp_gini$GINI))
 gdp_gini$GDP = as.numeric(as.numeric(gdp_gini$GDP))


p=ggplot(gdp_gini[gdp_gini$Year>=1980,],aes(x=GDP,y=GINI,size=2.5,label=CountryName,colour=ContinentName))+
  geom_point(alpha=0.7)+  theme_fivethirtyeight()+scale_x_continuous(labels = scales::dollar_format())+
  geom_label(size=3.5,aes(fill=factor(ContinentName)),colour="white",fontface="bold")+
  theme(legend.position="bottom",
        plot.title = element_text(size=14,hjust =0,margin=margin(t=0,r=0,b=0,l=0)), 
        plot.subtitle = element_text(hjust =0,size=12,margin=margin(t=5,r=0,b=14,l=0)),
        plot.caption = element_text(size=8.5,face="italic",margin=margin(t=0,r=0,b=0,l=0),hjust=1.051),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.margin =margin(0,0,8,0),
        legend.text=element_text(size=11),
        legend.title=element_text(size=11.5,face="bold"),
        axis.text = element_text(size=11),
        axis.title = element_text(size=10,colour = "#3c3c3c"))+
  guides(size=FALSE,colour = guide_legend(override.aes = list(size=5)),label=F,fill=FALSE)+scale_size_area(max_size = 5,guide="none")+
  labs(title="Relation of GINI with GDP per capita",x="GDP per capita",subtitle="Year: 2000",y="GINI", caption="Source: WorldBank.org & World Income Inequality Database")+
  scale_colour_discrete(name = "Continent")+ylab("GINI")+xlab("GDP per capita")


a = p+labs(subtitle="Year: {frame_time}",x="GDP per capita",y="GINI")+
  #transition_states(Year,transition_length = 1,state_length = 1)+ ease_aes('linear') + enter_fade() + 
  transition_time(Year)

animate(a, 
        duration = 74, 
        fps  =  4,width  = 550, height=320)
