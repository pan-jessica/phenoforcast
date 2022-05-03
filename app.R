install.packages(setdiff("pacman", rownames(installed.packages())))
library(pacman)
p_load(shiny)
p_load(leaflet)
p_load(raster)
p_load(colorRamps)
p_load(tidyverse)
p_load(rgdal)
p_load(lubridate)
p_load(RhpcBLASctl)
p_load(foreach)
p_load(doSNOW)
p_load(shinyscreenshot)
p_load(digest)
p_load(shinyjs)
p_load(mapview)
# p_load(lattice)
# p_load(htmlwidgets) #for onRender
p_load(magrittr) #for svg?

num_cores<-get_num_procs()-1
cl <- makeCluster(num_cores, outfile = "")
registerDoSNOW(cl)

path_app<-""
today<-read_file(paste0(path_app,"today.txt")) %>% as.Date()
date_list<-seq(today-years(1), today+14, by=1)

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

genusoi_list <- c(
  "Quercus", 
  "Betula",
  "Populus",
  "Acer"
)


evi_sta_list<-leaf_sta_list<-flower_sta_list<-pollen_sta_list<-vector(mode="list",length=length(genusoi_list))
names(evi_sta_list)<-names(leaf_sta_list)<-names(flower_sta_list)<-names(pollen_sta_list)<-genusoi_list

for (i in 1:length(genusoi_list)){
  # collecting tif files of evi type for each genusoi
  genusoi<-genusoi_list[i]
  path_evi<-paste0(path_app,"data/",genusoi,"/evi/")
  evi_files<-list.files(path_evi, full.names = T, pattern="\\.tif$") %>% sort()   
  
  # creating the list of rasterlayer objects for current genusoi (i)
  evi_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(evi_files[r])
               print(r)
               ras
             }
  evi_sta<-stack(evi_ras_list)      # create rasterstack object from the list of rasterlayer objects
  evi_sta_list[[i]]<-evi_sta        # have evi_sta_list (vector of genusoi in evi) to hold the rasterstack for current genusoi (i)
}

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_leaf<-paste0(path_app,"data/",genusoi,"/leaf/")
  leaf_files<-list.files(path_leaf, full.names = T, pattern="\\.tif$") %>% sort()
  
  leaf_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(leaf_files[r])
               print(r)
               ras
             }
  leaf_sta<-stack(leaf_ras_list)
  leaf_sta_list[[i]]<-leaf_sta
}

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_flower<-paste0(path_app,"data/",genusoi,"/flower/")
  flower_files<-list.files(path_flower, full.names = T, pattern="\\.tif$") %>% sort()
  
  flower_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(flower_files[r])
               print(r)
               ras
             }
  flower_sta<-stack(flower_ras_list)
  flower_sta_list[[i]]<-flower_sta
}

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_pollen<-paste0(path_app,"data/",genusoi,"/pollen/")
  pollen_files<-list.files(path_pollen, full.names = T, pattern="\\.tif$") %>% sort()
  
  pollen_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(pollen_files[r])
               print(r)
               ras
             }
  pollen_sta<-stack(pollen_ras_list)
  pollen_sta_list[[i]]<-pollen_sta
}


# sta_list will consist of a list of lists that contain vectors of rasterstacks to each genousi for each type
sta_list<-list(EVI=evi_sta_list,Leaf=leaf_sta_list,Flower=flower_sta_list, Pollen=pollen_sta_list)

####PAL####
pal_evi<-colorNumeric(palette = "Greens",  domain = c(0,1), na.color = "transparent")
pal_leaf<-colorNumeric(palette = "Greens",  domain = c(0,1), na.color = "transparent")
pal_flower<-colorNumeric(palette = "Reds",  domain = c(0,1), na.color = "transparent")
pal_pollen<-colorNumeric(palette = "Reds",  domain = c(0,5), na.color = "transparent")
pal<-list(EVI=pal_evi, Leaf=pal_leaf, Flower=pal_flower, Pollen=pal_pollen)
maxlist<-list(EVI=1.0, Leaf=1.0, Flower=1.0, Pollen=5.0)
minlist<-list(EVI=0, Leaf=0, Flower=0, Pollen=0)

variable_list<-list(EVI="Enhanced Vegetation Index",
                    Leaf="Leafing status",
                    Flower="Flowering status",
                    Pollen="Pollen concentration (grains/m^3) fourth root")


###########UI###########
ui<-fillPage(
  shinyjs::useShinyjs(),
  #shinyjs::inlineCSS(appCSS),
  tags$style(type = "text/css", 
             "html, body {width:100%; height:100%;}"
  ),
  
  leafletOutput("raster_map", height="100%",width="100%"),
  
  absolutePanel(id = "controls", 
                class = "panel panel-default", 
                fixed = TRUE,draggable = TRUE, 
                top = 50, right = "auto", left = 60, bottom = "auto",
                width = "auto", height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                h1(id="title","PhenoForecast"),
                selectInput("type", "Type",
                            choices = c("EVI","Leaf", "Flower", "Pollen"),
                            selected =  "EVI"),
                
                selectInput("genus", "Genus",
                            choices = genusoi_list,
                            selected = genusoi_list[1]),
                
                # sliderInput("date", "Date", min=mindate, max=maxdate, timeFormat = "%Y-%m-%d", value=1, ticks=T),
                sliderInput("day", "Day", min=14-length(date_list)+1, max=14, value=0, ticks=T)
                # If not using custom CSS, set height of leafletOutput to a number instead of percent     
  ),
  
  absolutePanel(id = "figure",
                class = "panel panel-default",
                fixed = TRUE,draggable = TRUE,
                top = 60, left = "auto", right = 60, bottom = "auto",
                width = 300, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                # h4("Temporal patterns"),
                plotOutput("lineplot", height = 200)
                
                
  ),
  
  
  # absolutePanel(id = "tweetfeed_shown",
  #               class = "panel panel-default",
  #               fixed = TRUE,draggable = TRUE,
  #               top = "auto", left = 100, right = "auto", bottom = 10,
  #               width = 300, height = 300,
  #               style = "background-color: rgba(255,255,255,0);
  #               border-color: rgba(255,255,255,0);
  #               box-shadow: 0pt 0pt 0pt 0px",
  #               
  #               tags$script(src="https://apps.elfsight.com/p/platform.js",
  #                           defer=NA),
  #               # includeScript("https://apps.elfsight.com/p/platform.js"), # this causes the app to crash
  #               tags$div(class = "elfsight-app-ab030cd9-764d-413a-9cfa-0e630029053f"),
  #               actionButton("hidetweet", "Hide Twitter feed", class = "btn-primary")
  #               
  # )
  # ,
  # shinyjs::hidden(
  #   absolutePanel(id = "tweetfeed_hidden",
  #                 class = "panel panel-default",
  #                 fixed = TRUE,draggable = FALSE,
  #                 top = "auto", left = 100, right = "auto", bottom = 10,
  #                 width = 300, height = "auto",
  #                 style = "background-color: rgba(255,255,255,0);
  #               border-color: rgba(255,255,255,0);
  #               box-shadow: 0pt 0pt 0pt 0px",
  #                 
  #                 actionButton("showtweet", "Show Twitter feed", class = "btn-primary")
  #                 
  #   )
  # ),
  
  absolutePanel(id = "misc",
                class = "panel panel-default",
                fixed = TRUE,draggable = FALSE,
                top = "auto", left = "auto", right = 60, bottom = 30,
                width = 250, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                text-align: right;
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                
                # actionButton("go", "Take a screenshot", class = "btn-primary"),
                downloadButton('map_down', "Take a screenshot", class = 'dwnbttn'),
                tags$head(tags$style(".dwnbttn{background-color:#337ab8; color: #ffffff;} .dwnbttn:focus{background-color:#337ab8; color: #ffffff;}")),
                br(),
                tags$a( href="https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
                        class="twitter-hashtag-button",
                        "data-size"="large",
                        "data-show-count"="false",
                        "Tweet #phenology"),
                tags$script(async=NA,
                            src="https://platform.twitter.com/widgets.js",
                            charset="utf-8"),
                
                # includeScript("http://platform.twitter.com/widgets.js"),
                # https://shiny.rstudio.com/articles/html-tags.html
                # https://community.rstudio.com/t/include-a-button-in-a-shiny-app-to-tweet-the-url-to-the-app/8113/2
                
                tags$div(id="cite",align="right",
                         '', tags$em('"PhenoForecast"'), ' by Yiluan Song'
                ),
                tags$a (id="link",target="_blank",
                        href="http://phenoobservers.ucsc.edu/phenowatch/",
                        tags$div (
                          id="linktext",align="right",
                          'Visit ', tags$em('"PhenoWatch"'), ''
                        )
                ),
                tags$a (id="link",target="_blank",
                        href="http://phenoobservers.ucsc.edu/phenoinfo/",
                        tags$div (
                          id="linktext",align="right",
                          'Visit ', tags$em('"PhenoInfo"'), ''
                        )
                )
  )
  # absolutePanel(id = "figures2", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60+280, left = "auto", right = 60, bottom = "auto",width = 300, height = "auto", 
  #               
  #               # h4("Spatial patterns"),
  #               plotOutput("neighbours",height = 360)
  # )
)

#######SERVER#######
server<-function(input, output,session){
  
  mymap <- reactive({
    leaflet() %>%
      addTiles()%>%
      setView(lng = -98, lat = 38, zoom = 4)
  })
  
  output$raster_map = renderLeaflet({
    mymap()
  })
  
  myfun <- function(raster_map) {
    res <- reactiveInput()
    date_label <- res[1]
    input_type = res[3]
    
    input_type <- unlist(input_type)
    date_label <- tags$div(date_label)
    
    clearImages(raster_map) %>%
      clearControls() %>%
      addRasterImage(reactiveRaster(), colors = pal[[input_type]], opacity = 0.8, layerId = "map") %>%
      addLegend(pal =  pal[[input_type]], values = seq(minlist[[input_type]],maxlist[[input_type]], length.out=6),
                position = "bottomleft", title = "", layerId = "map") %>%
      addControl(date_label, position = "bottomleft")
    
  }
  
  reactiveRaster <- reactive({reactiveInput()$r_type_genusoi_date_lim})
  
  reactiveInput <- reactive({
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    if (input$type=="Pollen") {
      r_type_genusoi_date[r_type_genusoi_date<0]<-0
      r_type_genusoi_date<-r_type_genusoi_date^(1/4)
    }
    r_type_genusoi_date_lim<-r_type_genusoi_date
    r_type_genusoi_date_lim[r_type_genusoi_date_lim>maxlist[[input$type]]]<-maxlist[[input$type]]-1e-5
    r_type_genusoi_date_lim[r_type_genusoi_date_lim<minlist[[input$type]]]<-minlist[[input$type]]+1e-5
    date_label <- date_list[input$day-14+length(date_list)]
    
    outlist <- list(date_label = date_label, r_type_genusoi_date_lim = r_type_genusoi_date_lim, input_type = input$type)
  })
  
  observe({
    leafletProxy("raster_map") %>% myfun()
  })
  
  
  ## To hold the popup locations
  v = reactiveValues()
  v$point = NULL
  
  #######Show popup on click########
  getPop <- reactive({
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    if (input$type=="Pollen") {
      r_type_genusoi_date[r_type_genusoi_date<0]<-0
      r_type_genusoi_date<-r_type_genusoi_date^(1/4)
    }
    variable<-variable_list[[input$type]]
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    text_lat<-paste0("Latitude: ", round(lat,2))
    text_lng<-paste0("Longtitude: ", round(lng,2))
    text_date<-paste0("Date: ", date_list[[input$day-14+length(date_list)]])
    value<-round(raster::extract(r_type_genusoi_date,data.frame(lng,lat)),2)
    text_value<-paste0(variable,": ", value,"")
    
    content <- list(
      text_lat, tags$br(),
      text_lng, tags$br(),
      text_date, tags$br(),
      text_value, tags$br()
    )
    p = data.frame(lng = click$lng, lat = click$lat)
    v$point = NULL
    v$point = rbind(v$point,p)
    
    output <- list(content = content) #, lat = click$lat, lng = click$lng)
    
  })%>% bindEvent(input$raster_map_click)
  
  popups <- function(raster_map){
    # print("content")
    cont <- getPop()
    content <- as.character(tagList(cont[1]))
    # lat <- unlist(cont[2])
    # lng <- unlist(cont[3])
    lng<-lat<-NULL
    if (!is.null(v$point)) {
      lng=v$point[,1]
      lat=v$point[,2]
    }
    
    clearPopups(raster_map) %>%
      addPopups(lng, lat, content)
    
  }
  
  # observe({
  #   leafletProxy("raster_map") %>% popups()
  # }) %>% bindEvent(input$raster_map_click)
  
  ########Show lineplot on click#########
  getLinePlot <- reactive({
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    # r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    variable<-variable_list[[input$type]]
    if(input$type=="EVI" || input$type=="Leaf") {
      col_line<-"dark green"
    }
    if (input$type=="Flower" || input$type=="Pollen") {
      col_line<-"red"
    }
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    
    sp<-SpatialPoints(cbind(lng, lat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    
    ts<-
      foreach(i = 1:length(date_list),
              .packages=(c("raster","tidyverse")),
              .combine="rbind") %dopar% {
                value<-raster::extract(r_type_genusoi[[i]], sp )
                value
              }
    if (input$type=="Pollen") {
      ts[ts<0]<-0
      ts<-ts^(1/4)
    }
    ts_df<-data.frame(ts, date_list)
    colnames(ts_df)<-c("value", "date")
    
    plotty <- NULL
    
    if (!any(is.na(ts_df))) {
      plotty <- ggplot(ts_df)+
        geom_line(aes(x=date, y=value),col=col_line)+
        geom_vline(aes(xintercept=date_list[input$day-14+length(date_list)]))+
        geom_vline(aes(xintercept=date_list[0-14+length(date_list)]), alpha=0.5)+
        # geom_smooth(aes(x=date, y=value))+
        theme_light()+
        ylim(minlist[[input$type]]-0.1, maxlist[[input$type]]+0.1)+
        xlab("date")+
        ylab(variable)+
        ggtitle(paste0("Longitude: ", round(lng,2), ", Latitude: ", round(lat,2))) +
        theme(plot.title = element_text(size = 10))
      
    } else {
      plotty <- ggplot()+
        theme_void ()+
        ggtitle("\n No time series available.\n Contribute by submitting your data.")
    }
    
    output = plotty
    
  })
  
  createLinePlot <- function(raster_map, plotty) {
    # save lineplot as svg and display it as html using addControl
    ggsave(file="plotty.svg", plot=plotty, width=3.5, height=2.5)
    content = as.character(read_file(paste0("plotty.svg")))
    
    removeControl(raster_map, layerId = "lineplot") %>%
      addControl(content, position = "topright", layerId = "lineplot")
  }
  
  toListen <- reactive({list(input$raster_map_click, input$type)})
  
  observeEvent(input$raster_map_click, {
    leafletProxy("raster_map") %>% 
      popups() %>%
      createLinePlot(getLinePlot())
  })
  
  observeEvent(formData(), {
    if (!is.null(v$point)){
      print("yes there was a click")
      leafletProxy("raster_map") %>%
        createLinePlot(getLinePlot())
    }
  })
  
  #######show tweeet########
  # observeEvent(input$showtweet, {
  #   shinyjs::hide("tweetfeed_hidden")
  #   shinyjs::show("tweetfeed_shown")
  # })
  
  #######hide tweeet########
  # observeEvent(input$hidetweet, {
  #   shinyjs::hide("tweetfeed_shown")
  #   shinyjs::show("tweetfeed_hidden")
  # })
  
  #######form data#########
  formData <- reactive({
    data <- c(input$type, input$genus, input$day, as.character(Sys.time()))
    data
  })
  
  ##########screenshoting########
  user_created_map <- reactive({
    m = mymap() %>% 
      setView(lng = input$raster_map_center$lng, lat = input$raster_map_center$lat,
              zoom = input$raster_map_zoom) %>%
      myfun()
    
    if (!is.null(v$point)) {
      m = m %>%
        popups() %>%
        createLinePlot(getLinePlot())
    }
    m
  })
  
  output$map_down <- downloadHandler(
    filename = 'mymap.png',
    
    content = function(file) {
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      on.exit(getwd())
      mapshot(user_created_map(), file = file, cliprect = "viewport")
    })
  
}

#######
shinyApp(ui, server)#, options = list(height=600,width=1200)
