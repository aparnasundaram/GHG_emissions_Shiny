##Code to first manipulate the WDI data for the RShiny project
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

### Read the GHG file  ###
GHG=read.csv('GHG.csv', stringsAsFactors = F, skip=4)

#Delete the indicator code col and the indicator name col from file.
GHG$Indicator.Code = NULL
GHG$Indicator.Name = NULL

head(GHG)
names(GHG)

##Attach a suffix to all year vars in GHG file

GHG1 <- GHG %>%
  group_by(Country.Code, Country.Name) %>%
  summarise_all(funs(mean(.))) %>% 
  rename_at(vars(-Country.Code, -Country.Name),
            function(x) paste0(x,"_GHG"))

##Get rid of X in front of each year
names(GHG1)<-str_replace_all(names(GHG1), c("X" = ""))

#Remove the _GHG column from file:
GHG1$`_GHG`=NULL

names(GHG1)



#output the file to check it.
write.csv(GHG1, file='GHGdf.csv', row.names=F)


### Read the population file###
pop = read.csv('Pop.csv', stringsAsFactors = F, skip=4)

#Delete the indicator code and name col from file.
pop$Indicator.Code = NULL
pop$Indicator.Name= NULL

head(pop)
names(pop)


#Attach a suffix to all year vars in the Population file.

pop1 <- pop %>%
  group_by(Country.Code, Country.Name) %>%
  summarise_all(funs(mean(.))) %>% 
  rename_at(vars(-Country.Code, -Country.Name),
            function(x) paste0(x,"_pop"))

##Get rid of X in front of each year
names(pop1)<-str_replace_all(names(pop1), c("X" = ""))

#Remove the _pop column from file:
pop1$`_pop`=NULL

names(pop1)
pop1[,"_pop"]

write.csv(pop1, file='POPdf.csv', row.names=F)


### Merge the GHG and Population files
joindat = left_join(GHG1,pop1, by = c("Country.Code", "Country.Name"))
head(joindat)

write.csv(joindat, file='joindf.csv', row.names=F)

##to set up the division, first try to grab the years in the col. into a separate variable. 
yearcol = colnames(joindat)

yearp = substring(yearcol, first = 1, last = 4)

yearpref= yearp[-(1:2)]
yearpref


# Code from David to compute per capita emissions

library(readr)
joindf <- read_csv("joindf.csv")

# Check that the fields are already aligned
fieldnames = names(joindf)[-(1:2)]
n = length(fieldnames)
half_n = n/2
prefixes = substr(x = fieldnames,start = 1,stop = 4)
all.equal(prefixes[1:half_n],prefixes[(half_n+1):n])

# They are aligned. So we can just use vectorized operations
idx_ghg = 1:half_n + 2
idx_pop = (half_n+1):n + 2
perCapGHG = joindf[,idx_ghg] /  joindf[,idx_pop]


#column bind the first two columns from the joinddf dataframe with percapGHG df
countrydf = joindf[,1:2]
percapGHGdf = cbind(countrydf, perCapGHG)
class(percapGHGdf)
names(percapGHGdf)
class(joindf)

GHGPC <- percapGHGdf %>%

    group_by(Country.Code, Country.Name) %>%
  summarise_all(funs(mean(.))) %>% 
  rename_at(vars(-Country.Code, -Country.Name),
            function(x) paste0(x,"_PC"))

write.csv(GHGPC, file='GHGPCdf.csv', row.names=F)

names(GHGPC)


##Do the tidy part here to rotate the GHGPC dataset to long-form

GHGPC2= GHGPC %>%
  gather(key="year", value="emissionsPC", `1961_GHG_PC`:`2018_GHG_PC`, na.rm = TRUE)
names(GHGPC2)

GHGPC2$`1960_GHG_PC`=NULL

##Check the class of the key variables and convert to correct class.
GHGPC2$emissionsPC=as.numeric(GHGPC2$emissionsPC)
GHGPC2$year=parse_number(GHGPC2$year)
GHGPC2$year=as.integer(GHGPC2$year)
class(GHGPC2$year)

##Rename the Russian Federation to Russia
GHGPC2$Country.Name[GHGPC2$Country.Name == "Russian Federation"] <- "Russia"


write.csv(GHGPC2, file='GHGPC_df.csv', row.names=F)

head(GHGPC2)

##Identifying the min and max GHG PC
result1 = max(GHGPC2$emissionsPC)
result1
result2 = min(GHGPC2$emissionsPC)
result2

##Identify which countries have the min and max.
GHGPC2 %>% 
  group_by(Country.Name, year) %>% 
  summarise(maxem = max(emissionsPC), minem= min(emissionsPC)) %>% 
  arrange(desc(maxem))

##create the trends over time
GHGPC3= GHGPC2 %>% filter(Country.Name=='United Arab Emirates')
ggplot(GHGPC3,aes(year,emissionsPC))+
  geom_line()+geom_point()+
  scale_colour_gradient(low = "coral", high = "steelblue") +

      #x axis limits
      #xlim(min, max) +
      #y axis limits
      ylim(0.0001005067, 0.1656618)


##merge the population and overall GHG emissions data with the GHG PC data.
##To do this, tidy each dataset separately first and then merge. 

##GHG
names(GHG1)

GHG2= GHG1 %>%
  gather(key="year", value="emissionsTot", `1961_GHG`:`2018_GHG`, na.rm = TRUE)
names(GHG2)

GHG2$`1960_GHG`=NULL

##Check the class of the key variables and convert to correct class.
GHG2$emissionsTot=as.numeric(GHG2$emissionsTot)
GHG2$year=parse_number(GHG2$year)
GHG2$year=as.integer(GHG2$year)
class(GHG2$year)
class(GHG2$emissionsTot)

##Rename the Russian Federation to Russia
GHG2$Country.Name[GHG2$Country.Name == "Russian Federation"] <- "Russia"


##Identifying the min and max GHG PC
result1 = max(GHG2$emissionsTot)
result1
result2 = min(GHG2$emissionsTot)
result2

##Identify which countries have the min and max.
GHG_view=GHG2 %>% 
  group_by(Country.Name, year) %>% 
  summarise(maxem = max(emissionsTot), minem= min(emissionsTot)) %>% 
  arrange(desc(maxem))


write.csv(GHG2, file='GHG2_df.csv', row.names=F)

##Pop
names(pop1)

pop2= pop1 %>%
  gather(key="year", value="population", `1961_pop`:`2018_pop`, na.rm = TRUE)
names(pop2)

pop2$`1960_pop`=NULL

##Check the class of the key variables and convert to correct class.
pop2$population=as.numeric(pop2$population)
pop2$year=parse_number(pop2$year)
pop2$year=as.integer(pop2$year)
class(pop2$year)
class(pop2$population)

##Rename the Russian Federation to Russia
pop2$Country.Name[pop2$Country.Name == "Russian Federation"] <- "Russia"

result1 = max(pop2$population)
result1
result2 = min(pop2$population)
result2

##Identify which countries have the min and max.
pop_view=pop2 %>% 
  group_by(Country.Name, year) %>% 
  summarise(maxem = max(population), minem= min(population)) %>% 
  arrange(desc(maxem))

#convert the population values to millions -- leads to errors in graph.
#pop2$population = pop2$population/1000


write.csv(pop2, file='pop2_df.csv', row.names=F)



##merge the GHG total, GHG PC and pop files on country name and year.
##The files are GHG2, pop2, and GHGPC2
names(GHG2)
names(pop2)
names(GHGPC2)

#merge the 3 datasets in two stages.
GHGpop = left_join(GHG2,pop2, by = c("Country.Code", "Country.Name","year"))
class(GHGpop$year)
class(GHGPC2$year)

class(GHGpop$Country.Code)
class(GHGPC2$Country.Code)

class(GHGpop$Country.Name)
class(GHGPC2$Country.Name)

class(GHGpop$population)
class(pop2$population)

#create the new dataset for analysis and to be read in by R Shiny
GHGdf = left_join(GHGPC2, GHGpop, by = c("Country.Code", "Country.Name","year"))

#arrange the countries by alpha order and by year
GHGdf = GHGdf %>% arrange(Country.Name, year)

write.csv(GHGdf, file='GHG_df.csv', row.names=F)




##create two line graphs in one plot and format it -- code can then moved to server.R

library(ggthemes)
library(mapproj)
library(plotly)

pop3= pop2 %>% filter(Country.Name=='United Arab Emirates')
pop4 = pop2 %>% filter(Country.Name == 'Central African Republic')

g = ggplot() +
  geom_line(data = pop3, aes(x = year, y = population, color=Country.Name), 
            size = 1) +
  scale_colour_wsj("colors6", "")+ theme_wsj()+
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=4))) +
  
  
  #geom_point(data = pop3, aes(x = year, y = population)) +
  
  
  geom_line(data = pop4, aes(x = year, y = population, color=Country.Name), 
            size = 1) +
  scale_colour_wsj(palette ="colors6", "")+ theme_wsj()+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  
  labs(title = "Greenhouse Gas Emissions Per Capita by Year",
       x='\nYears', 
       y='Pop\n') +
  
  theme(
    axis.ticks.length.y = unit(.25, "cm"),
    axis.ticks.length.x = unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), size=10),
    axis.text.y = element_text(margin = margin(t = .3, unit = "cm"), size=10),
    axis.title  = element_text(margin = margin(t = 20, r=20, b=20, l=20, unit = "cm"), 
                               size=15),
    plot.title =  element_text(margin = margin(t = .3, unit = "cm"), size=20, 
                              face="bold"),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(size = 1))

g = ggplotly(g)
g

#scale_y_continuous(labels = comma)
# t = 0, r = 20, b = 0, l = 0)
  # theme(
  #       ,plot.title = element_text(size = 18)
  #       ,axis.title = element_text(margin(t = 0.5, unit = "cm"),
  #                                  size = 15)
  #       ,axis.title.y = element_text(margin(t = 0.5, unit = "cm"),
  #                                    size = rel(0.8), angle = 90)
  # )
  # 
  
  #geom_point(data = pop4, aes(x = year, y = population)) 
  












##create a map -- ignore this and go to code in the Ui.R and Server.R files.

library(mapproj)

WorldData <- map_data('world')

ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id='Country.Name'),  #,map_id=region
           fill = "purple", colour = "green", size=0.5) +
  # geom_map(data = GHGPC2, map=WorldData,
  #          aes(fill='emissionsPC', map_id='Country.Name'),
  #          colour="blue", size=2.0) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  labs(fill="legend", title="Title", x="", y="") +
  theme_bw()



# dynamic maps code from Stella

# Map tab: Global map #
df_map_glob <- reactive({
  GHG %>%
    
    filter((year >= input$year[1] & year <= input$year[2])) %>% 
    group_by(Country.Name) %>% 
    summarise(n_app = n())
})

output$map_glob <- renderGvis({
  df_map_glob = df_map_glob()
  gvisGeoChart(df_map_glob,
               "Country.Name", 
               ifelse(input$var_map == "emissions_PC", "n_app"),
               options=list(width="auto", height="auto", 
                            gvis.listener.jscode = "var text = data.getValue(chart.getSelection()[0].row,0);Shiny.onInputChange('text', text.toString());"))
})


















  
  



