library(shiny)
library(sp)
library(plotGoogleMaps)
library(rjson)
countries=read.csv("countries2.csv",header=FALSE)
test=as.data.frame(cbind(as.character(countries[,5]),0:214))
test$V1=factor(test$V1,levels(test$V1)[c(212,1:211,213:215)])


shinyServer(function(input, output) {
  
  cindex=reactive({as.numeric(input$region)+1})
  minlat=reactive({countries[cindex(),2]})
  maxlat=reactive({countries[cindex(),4]})
  minlon=reactive({countries[cindex(),1]})
  maxlon=reactive({countries[cindex(),3]})
  
  data <- reactive({
    
    startyear=input$date_range[1]
    endyear=input$date_range[2]
    minmagnitude=input$min_mag
    
    
    # Fetch json data from USGS website
    if (endyear==2015){
      json_file = paste0("http://earthquake.usgs.gov/fdsnws/event/1/query?starttime=",startyear,
                         "-01-01&minmagnitude=",minmagnitude,
                         "&format=geojson&endtime=",Sys.Date()-1,
                         "&orderby=time")
    } else{
      json_file = paste0("http://earthquake.usgs.gov/fdsnws/event/1/query?starttime=",startyear,
                         "-01-01&minmagnitude=",minmagnitude,
                         "&format=geojson&endtime=",endyear,
                         "-12-31&orderby=time")
    }
    
    if (input$region>0){
      json_file=paste0(json_file,
                       "&minlatitude=" , minlat(),
                       "&maxlatitude=", maxlat(),
                       "&minlongitude=", minlon(),
                       "&maxlongitude=", maxlon())
      
    }
    print(json_file)
    return(fromJSON(file=json_file))
  })
  
  
  output$mymap <- renderUI({
    
    nEQ=as.numeric(summary(data()["features"])[1])
    
    if (nEQ>0){
      
      catalog=as.data.frame(matrix(ncol=5,nrow=nEQ))
      names(catalog)=c("Longitude","Latitude","Magnitude","Location","Time")
      
      catalog$Longitude=sapply(data()$features,function(vec){vec$geometry$coordinates[1]})
      catalog$Latitude=sapply(data()$features,function(vec){vec$geometry$coordinates[2]})
      catalog$Magnitude=sapply(data()$features,function(vec){vec$properties$mag})
      catalog$Location=sapply(data()$features,function(vec){vec$properties$place})
      catalog$Time = sapply(data()$features,function(vec){paste0(format(as.POSIXct(
        vec$properties$time/1000,origin="1970-01-01"),"%Y-%m-%d %H:%M:%S",tz="GMT")," GMT")})
      
      catalogAttributes=as.data.frame(cbind(catalog$Magnitude,catalog$Location,catalog$Time))
      colnames(catalogAttributes)=c("Magnitude","Location","Time")
      
      catalogSPDF<- SpatialPointsDataFrame(cbind(catalog$Longitude,catalog$Latitude), proj4string =CRS("+proj=longlat +datum=WGS84"),
                                           data =  catalogAttributes ,match.ID=FALSE,
                                           bbox=bbox(SpatialPoints(cbind(c(minlon(),maxlon()),c(minlat(),maxlat())))))
      m <- plotGoogleMaps(catalogSPDF , zcol="Magnitude",filename='myMap1.html',openMap=FALSE,legend=FALSE,control=FALSE,mapTypeId='HYBRID',
      )
      
      tags$iframe(
        srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
        width = "100%",
        height = "800px"
      )
    } 
    
    else {
      catalogSPDF<- SpatialPointsDataFrame(cbind(c(rep(minlon(),2),rep(maxlon(),2)),c(minlat(),maxlat(),minlat(),maxlat())), proj4string =CRS("+proj=longlat +datum=WGS84"),
                                           data=as.data.frame(c(rep(0,2),rep(1,2)))) #,bbox=bbox(SpatialPoints(cbind(c(minlon(),maxlon()),c(minlat(),maxlat()))))) #bbox=bbox(SpatialPoints(cbind(c(minlon(),maxlon()),c(minlat(),maxlat()))))
      m <- plotGoogleMaps(catalogSPDF ,filename='myMap1.html',openMap=FALSE,legend=FALSE,control=FALSE,visible=FALSE,mapTypeId='HYBRID')
      tags$iframe(
        srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
        width = "100%",
        height = "800px"
      )
      
    }
    
  })
  output$text1    <- renderText({
    #load('C:\\Users\\cloth\\Documents\\Software\\Shiny\\data_2014.RData')
    #data = fromJSON(file=json_file)
    nEQ=as.numeric(summary(data()["features"])[1])
    paste0(nEQ," earthquake record(s) found")
  })
}
)




