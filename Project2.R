library(readxl)
PopulationEmploymentDatabase<- read_excel("/Volumes/124GB USB/Job Applications/data incubator_project/DatabaseProject.xlsx", 
                                                       sheet = "1-Population and Employment")
View(PopulationEmploymentDatabase)
dim(PopulationEmploymentDatabase)
PopulationEmploymentDataset<-as.data.frame(PopulationEmploymentDatabase)

#For Parsing
NamesCongregateEU<-unique(PopulationEmploymentDataset$COUNTRY)[1:7]
NamesEuropeanContinent<-unique(PopulationEmploymentDataset$COUNTRY)[8:44]

NamesEuroCountries1<-unique(PopulationEmploymentDataset$COUNTRY)[ 8:12 ]
NamesEuroCountries2<-unique(PopulationEmploymentDataset$COUNTRY)[ 14:35 ]
NamesEuroCountries<-c(NamesEuroCountries1,NamesEuroCountries2)
length(NamesEuroCountries)

NamesNoEuroCountries<-unique(PopulationEmploymentDataset$COUNTRY)[ 36:44]

NamesWorld<-unique(PopulationEmploymentDataset$COUNTRY)[45:51]

NewDataset<-PopulationEmploymentDataset[ -c(1) ]
#NewDataset[NewDataset$COUNTRY=="West Germany", ] <- NA

library(reshape2)
NewDatasetlong <- melt(NewDataset, id.vars = c("COUNTRY","SUB-CHAPTER" ,"TITLE", "UNIT"), variable.name = "year")
NewDatasetlong$value<-as.numeric(NewDatasetlong$value)
class(NewDatasetlong$value)
#NewDatasetlong$value2<-NewDatasetlong$value
#NewDatasetlong$value2[NewDatasetlong$UNIT== "1000 persons"]<-NewDatasetlong$value*1000



#LabourForcePopulation Over Total Population in EUro Area from 1960 to 2019
EuroareaDataset<-subset(NewDatasetlong, NewDatasetlong$COUNTRY=="Euro area")
DatasetLabourForce<-subset(EuroareaDataset, EuroareaDataset$TITLE=="Total labour force (Labour force statistics)")

Euro27Dataset<-subset(NewDatasetlong, NewDatasetlong$COUNTRY %in% NamesEuroCountries)
DatasetLabourForce<-subset(Euro27Dataset, Euro27Dataset$TITLE=="Total labour force (Labour force statistics)")
DatasetForPlot<-DatasetLabourForce

#Civilian employment, persons,  from 1960 to 2019
DatasetEmployment<-subset(Euro27Dataset, Euro27Dataset$TITLE=="Civilian employment, persons (national)")
DatasetForPlot<-DatasetEmployment


#0-14 years population over total popualtion in EUro Area from 1960 to 2019
DatasetPopulation014years <-subset(Euro27Dataset, Euro27Dataset$TITLE=="Population: 0 to 14 years" )
DatasetForPlot<-DatasetPopulation014years

#unemplyment Rate,  from 1960 to 2019
DatasetEmploymentRate<-subset(Euro27Dataset, Euro27Dataset$TITLE=="Unemployment rate: total :- Member States: definition EUROSTAT")
DatasetForPlot<-DatasetEmploymentRate

#Employment, persons: all domestic industries (National accounts)  from 1960 to 2019
DatasetDomesticIndustries<-subset(Euro27Dataset, Euro27Dataset$TITLE=="Employment, persons: all domestic industries (National accounts)")
DatasetForPlot<-DatasetDomesticIndustries


getOption("max.print")
options(max.print = 99999999)

dev.off() 
cc <- scales::seq_gradient_pal(low = "white", high = "black", "Lab")(seq(0,1,length.out=27))
library(ggplot2)
  p <- ggplot(DatasetForPlot, aes(x=as.factor(year), y=value)) +
    geom_point(aes(color=as.factor(COUNTRY)), size=2) +
    #geom_line(aes(color=as.factor(COUNTRY))) +
    scale_y_continuous( name="Population (x1000 Units)") +
    scale_x_discrete( name="Years", breaks=c("1960", "1970", "1980", "1990", "2000", "2010"), labels=c("1960", "1970", "1980", "1990", "2000", "2010"))+
    theme_bw()   + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    #theme(legend.background = element_rect(fill="transparent")) + #legend.position=c(0.75, 0.87),
    theme(legend.position="none") + #legend.position=c(0.75, 0.87),
    theme(axis.title = element_text(face = "bold.italic", color = "black"), axis.text.x = element_text(face = "bold", color = "black", size = 14, angle = 0, hjust = 0.7, vjust = 0.7)) + #angle = 45,
    theme(plot.title = element_text(hjust = 0.5))+
    scale_colour_manual(values=cc) +
    geom_text(data = subset(DatasetForPlot, year == "2019"), aes(label = COUNTRY, colour = COUNTRY, x = Inf, y = value), hjust = -.1) +
    theme(plot.margin = unit(c(1,9,1,1), "lines")) +
  ggtitle("Employment")
  gt <- ggplotGrob(p)
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  library(grid)
  grid.draw(gt)   
      
  
     
#0-14 years population over total popualtion in EUro Area from 1960 to 2019
#Civilian employment, persons,  from 1960 to 2019
#unemplyment Rate,  from 1960 to 2019
#Employment, persons: all domestic industries (National accounts)  from 1960 to 2019

