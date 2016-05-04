# run this code to see the shiny output of how the number of crimes change over the years
# and where the different types of crimes occur in Chicago


library(MASS)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggmap)
library(rgdal)
library(maptools)
gpclibPermit()
library(rgeos)
library(gridExtra)
library(caret)
library(shiny)

#Loading data and Data wrangling

crime2011<-read_csv("Crimes_-_2011.csv")
crime2012<-read_csv("Crimes_-_2012.csv")
crime2013<-read_csv("Crimes_-_2013.csv")
crime2014<-read_csv("Crimes_-_2014.csv")
crime2015<-read_csv("Crimes_-_2015.csv")
crime<-bind_rows(crime2011,crime2012,crime2013,crime2014,crime2015)

#data wrangling: fixing duplicate column names
names(crime)[1]<-"Casenumber1"
names(crime)[24]<-"Casenumber2"
names(crime)[2] <- "ID1"
names(crime)[23] <- "ID2"
crime$Casenumber2<-ifelse(is.na(crime$Casenumber2), crime$Casenumber1,crime$Casenumber2)
crime$ID1<-ifelse(is.na(crime$ID1),crime$ID2,crime$ID1)

#combined dataset
crime<-crime %>% select(-Casenumber1,-ID2)

#removing unnecessary files to save space
rm(crime2011)
rm(crime2012)
rm(crime2013)
rm(crime2014)
rm(crime2015)


#separating Date and Time
crimedata<- crime %>% 
  separate(Date, into = c("Date","Time","AMPM"), sep = " ") %>%
  separate(Time, into = c("Hour","Minute","Second"), sep = ":") %>%
  separate(Date, into = c("Month","Date","year"), sep = "/") %>% 
  select(-Year)
#adding 12 hours to indicate PM
names(crimedata)[4] <-"Year"
crimedata$Hour<-ifelse(crimedata$AMPM == "PM", as.numeric(crimedata$Hour) + 12, crimedata$Hour)

crimedata$Month<-as.numeric(crimedata$Month)
crimedata$Date<-as.numeric(crimedata$Date)
crimedata$Year<-as.numeric(crimedata$Year)
#want to match day of week to the specific date (e.g. is 1/1/2012 a Monday?)
noleap<-c(0,31,28,31,30,31,30,31,31,30,31,30)
leap<-c(0,31,29,31,30,31,30,31,31,30,31,30)
noleap_sum<-rep(1,12)
leap_sum<-rep(1,12)
day<-c("Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu")

for (i in 1:length(noleap)){
  noleap_sum[i] <- sum(noleap[1:i])
  leap_sum[i]<-sum(leap[1:i])
}
#our dataset
#filter out 2011 for consistency since it does not have location data
crimedata <- crimedata %>% 
  mutate(index = ifelse(Year ==2011, noleap_sum[Month]+Date, 
                        ifelse(Year == 2012, leap_sum[Month]+Date+365,
                               ifelse(Year == 2013, noleap_sum[Month]+Date+365+366,
                                      ifelse(Year == 2014, noleap_sum[Month]+Date+365+366+365,
                                             noleap_sum[Month]+Date+365+366+365+365))))) %>%
  mutate(Day = day[(index %% 7)+1]) %>% filter(!Year == 2011)
rm(crime)

#grouping primary types into 5 categories
crimedata$newtype <- ifelse(crimedata$`Primary Type` %in% c("ASSAULT", "BATTERY"), "ASSAULT/BATTERY", crimedata$`Primary Type`)
crimedata$newtype <- ifelse(crimedata$newtype %in% c("BURGLARY", "MOTOR VEHICLE THEFT", "ROBBERY", "THEFT"), "BURGLARY/ROBBERY/THEFT", crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION"), "NARCOTICS", crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype %in% c("ASSAULT/BATTERY", "BURGLARY/ROBBERY/THEFT", "CRIMINAL DAMAGE", "NARCOTICS"), crimedata$newtype, "OTHERS")

unzip("Chicagoshape.zip")
ogrInfo(".", "geo_export_34b5f872-0327-45f0-9099-c87f1f2b8b83")
chicago <- readOGR(".", "geo_export_34b5f872-0327-45f0-9099-c87f1f2b8b83")
chicago <- spTransform(chicago, CRS("+proj=longlat +datum=WGS84"))

location <- unlist(geocode('4135 S Morgan St, Chicago, IL 60609'))+c(0,.02)

gmap <- get_map(location=location, zoom = 10, maptype = "terrain", source = "google", col="bw")

chicagomap <- ggmap(gmap)
chicagomap <- chicagomap + geom_polygon(data=chicago, aes(x=long, y=lat, group=group), color="red", alpha=0) + xlab("Longitude") + ylab("Latitude")
chicagomap <- chicagomap + coord_map()
chicagomap <- chicagomap + theme_bw()

# sample 1000 points from the data
test_crimedata <- sample_n(crimedata,1000)
chicagomap + geom_point(data=test_crimedata, aes(x=`Longitude`, y=`Latitude`))
#change column name for crimedata to match with shapefile
colnames(crimedata)[which(names(crimedata) == "Community Area")] <- "id"
#exclude outliers
crimedata <- crimedata %>% filter(id != 0)

mydata <- crimedata %>% group_by(id) %>% mutate(counts = n()) %>% select(id, counts) %>% unique()
chicago.ch <- fortify(chicago, region="area_numbe")
#merge with coefficients and reorder
merge.shp.coef<-merge(chicago.ch, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
#plotting choropleth
centers <- ddply(final.plot, .(id), summarize, clat = mean(lat), clong = mean(long))

choropleth <- function(x){
  year <- crimedata %>% filter(Year == x)
  colnames(year)[which(names(year) == "Community Area")] <- "id"
  year <- year %>% filter(id != 0)
  mydata <- year %>% group_by(id) %>% mutate(counts = n()) %>% select(id, counts) %>% unique()
  chicago.ch <- fortify(chicago, region="area_numbe")
  merge.shp.coef<-merge(chicago.ch, mydata, by="id", all.x=TRUE)
  final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
  ggplot() + geom_polygon(data = final.plot,
                          aes(x = long, y = lat, 
                              group = group, 
                              fill = counts),
                          color = "black", size = 0.25) + coord_map() + 
    scale_fill_distiller(name="Number of Crimes", palette = "OrRd") + xlab("Longitude") + ylab("Latitude") + theme(panel.background = element_blank())}
#assigning numbers to 5 crime type categories
crimedata$newtype <- ifelse(crimedata$newtype == "ASSAULT/BATTERY", 1, crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype == "BURGLARY/ROBBERY/THEFT", 2, crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype == "CRIMINAL DAMAGE", 3, crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype == "NARCOTICS", 4, crimedata$newtype)
crimedata$newtype <- ifelse(crimedata$newtype == "OTHERS", 5, crimedata$newtype)
choropleth2 <- function(x){
  x=as.character(x)
  type <- crimedata %>% filter(newtype == x)
  colnames(type)[which(names(type) == "Community Area")] <- "id"
  type <- type %>% filter(id != 0)
  mydata <- type %>% group_by(id) %>% mutate(counts = n()) %>% select(id, counts) %>% unique()
  chicago.ch <- fortify(chicago, region="area_numbe")
  merge.shp.coef<-merge(chicago.ch, mydata, by="id", all.x=TRUE)
  final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
  ggplot() + geom_polygon(data = final.plot,
                          aes(x = long, y = lat, 
                              group = group, 
                              fill = counts),
                          color = "black", size = 0.25) + 
    scale_fill_distiller(name="Number of Crimes", palette = "YlGn") +
    coord_map() + xlab("Longitude") + ylab("Latitude") + theme(panel.background = element_blank())}


###################################
############## SHINY ############## 
###################################
ui <- navbarPage(title = "Chicago Choropleth",
  tabPanel("Year",
           fluidRow(
             column(width = 4,
                    sliderInput(inputId = "n",label="Year:", min = 2012, max = 2015, value = 2013, step = 1)),
             column(width = 8,
                    plotOutput(outputId = "map")))
           ),
  tabPanel("Crime Type",
           fluidRow(
             column(width = 4,
                    selectInput("select", label = "Crime types:", 
                                choices = list("ASSAULT/BATTERY" = 1, "BURGLARY/ROBBERY/THEFT" = 2, 
                                               "CRIMINAL DAMAGE" = 3, "NARCOTICS" = 4, "OTHERS" = 5), selected = 1)),
             column(width = 8,
                    plotOutput(outputId = "map2"))))
 )

server <- function(input, output) {
  output$map <- renderPlot({
    choropleth(input$n)
  })
  output$map2 <- renderPlot({
    choropleth2(input$select)
  })
}

shinyApp(ui = ui, server = server)



