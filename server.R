library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(geosphere)

#setwd("C:/Users/TKW/Documents/R/RentApp/data") # Change if necessary
help <- paste(readLines("data/help.txt"), collapse=' ')
properties <- read.csv("data/properties.csv", stringsAsFactors=FALSE)
districts <- read.csv("data/districts.csv", stringsAsFactors=FALSE)
rentals <- read.csv("data/rentals.csv", stringsAsFactors=FALSE)
constructions <- read.csv("data/constructions.csv", stringsAsFactors=FALSE)
hospitals <- read.csv("data/hospitals.csv", stringsAsFactors=FALSE)
preschools <- read.csv("data/preschools.csv", stringsAsFactors=FALSE)
schools <- read.csv("data/schools.csv", stringsAsFactors=FALSE)
shopping <- read.csv("data/shopping.csv", stringsAsFactors=FALSE)
stations <- read.csv("data/stations.csv", stringsAsFactors=FALSE)

# Get property ID based on name
property.id <- function(property) {
  return(which(properties$Name == property))
}

# District
get.district <- function(n) {
  dist <- properties$District[n]
  neighborhood <- districts$Location[which(districts$District == dist)]
  paste('District ', dist, ' (', neighborhood, ')', sep='')
}

# Past rentals (table)
past.rentals <- function(n) {
  property_name <- properties$Name[n]
  past_rentals <- rentals[grep(property_name, rentals$Name),]
  row.names(past_rentals) <- NULL
  return(past_rentals)
}

# Past rentals (graph)
past.rentals.graph <- function(n) {
  past_rentals <- past.rentals(n)
  ggplot(data=past_rentals, aes(x=Quarter, y=Median, group=1)) +
    geom_line(color='dark red') +
    geom_point(color='red') +
    labs(x=NULL, y=NULL) +
    ggtitle('Past Median Rental (per Sq. Ft.)') +
    theme_economist() +
    theme(title=element_text(size=10, face='bold'),
          axis.text=element_text(size=12))
}

# Nearest hospital
nearest.hospital <- function(n) {
  nearest_hospital <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  hospitals.coords <- hospitals %>% select(X,Y)
  distances <- distm(properties.coords, hospitals.coords, fun=distGeo)[1,]
  
  nearest_hospital[1,]$Property <- properties$Name[n]
  nearest_hospital[1,]$Nearest <- hospitals$Name[which.min(distances)]
  nearest_hospital[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_hospital[1,]$X <- hospitals$X[which.min(distances)]
  nearest_hospital[1,]$Y <- hospitals$Y[which.min(distances)]
  
  return(nearest_hospital)
}

# Nearest school
nearest.school <- function(n) {
  nearest_school <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  schools.coords <- schools %>% select(X,Y)
  distances <- distm(properties.coords, schools.coords, fun=distGeo)[1,]
  
  nearest_school[1,]$Property <- properties$Name[n]
  nearest_school[1,]$Nearest <- schools$Name[which.min(distances)]
  nearest_school[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_school[1,]$X <- schools$X[which.min(distances)]
  nearest_school[1,]$Y <- schools$Y[which.min(distances)]
  
  return(nearest_school)
}

# Nearest preschool
nearest.preschool <- function(n) {
  nearest_preschool <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  preschools.coords <- preschools %>% select(X,Y)
  distances <- distm(properties.coords, preschools.coords, fun=distGeo)[1,]
  
  nearest_preschool[1,]$Property <- properties$Name[n]
  nearest_preschool[1,]$Nearest <- preschools$Name[which.min(distances)]
  nearest_preschool[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_preschool[1,]$X <- preschools$X[which.min(distances)]
  nearest_preschool[1,]$Y <- preschools$Y[which.min(distances)]
  
  return(nearest_preschool)
}

# Nearest preschool
nearest.preschool <- function(n) {
  nearest_preschool <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  preschools.coords <- preschools %>% select(X,Y)
  distances <- distm(properties.coords, preschools.coords, fun=distGeo)[1,]
  
  nearest_preschool[1,]$Property <- properties$Name[n]
  nearest_preschool[1,]$Nearest <- preschools$Name[which.min(distances)]
  nearest_preschool[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_preschool[1,]$X <- preschools$X[which.min(distances)]
  nearest_preschool[1,]$Y <- preschools$Y[which.min(distances)]
  
  return(nearest_preschool)
}

# Nearest shopping
nearest.shopping <- function(n) {
  nearest_shopping <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  shopping.coords <- shopping %>% select(X,Y)
  distances <- distm(properties.coords, shopping.coords, fun=distGeo)[1,]
  
  nearest_shopping[1,]$Property <- properties$Name[n]
  nearest_shopping[1,]$Nearest <- shopping$Name[which.min(distances)]
  nearest_shopping[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_shopping[1,]$X <- shopping$X[which.min(distances)]
  nearest_shopping[1,]$Y <- shopping$Y[which.min(distances)]
  
  return(nearest_shopping)
}

# Nearest station
nearest.station <- function(n) {
  nearest_station <- 
    data.frame(Property=NA,
               Nearest=NA,
               Distance=NA,
               X=NA, Y=NA)
  
  properties.coords <- properties[n,] %>% select(X,Y)
  stations.coords <- stations %>% select(X,Y)
  distances <- distm(properties.coords, stations.coords, fun=distGeo)[1,]
  
  nearest_station[1,]$Property <- properties$Name[n]
  nearest_station[1,]$Nearest <-
    paste(stations$Name[which.min(distances)], stations$Type[which.min(distances)], sep=' ')
  nearest_station[1,]$Distance <- min(distances) %>% round(-1) %>% as.integer() # Round to nearest 10m
  nearest_station[1,]$X <- stations$X[which.min(distances)]
  nearest_station[1,]$Y <- stations$Y[which.min(distances)]
  
  return(nearest_station)
}

# Nearby constructions within noise radius threshold
nearby.constructions <- function(n, start, end, radius) {
  constructions_withinperiod <-
    constructions %>% subset(!(Start_Date > end | End_Date < start))
  
  properties.coords <- properties[n,] %>% select(X,Y)
  
  constructions.output <-
    data.frame(Property=character(),
               Name=character(),
               Distance=numeric(),
               Start=numeric(),
               End=numeric(),
               X=numeric(),
               Y=numeric(),
               stringsAsFactors=FALSE)
  
  if (nrow(constructions_withinperiod) > 0) {
    for (i in 1:nrow(constructions_withinperiod)) {
      constructions_coords <- constructions_withinperiod[i,] %>% select(X,Y)
      constructions_distance <- distm(properties.coords, constructions_coords, fun=distGeo)[1,]
      if (constructions_distance <= radius) {
        constructions.output.temp <-
          data.frame(Property=character(),
                     Name=character(),
                     Distance=numeric(),
                     Start=numeric(),
                     End=numeric(),
                     X=numeric(),
                     Y=numeric(),
                     stringsAsFactors=FALSE)
        constructions.output.temp[1,]$Property <- properties$Name[n]
        constructions.output.temp[1,]$Name <- constructions_withinperiod$Name[i]
        constructions.output.temp[1,]$Distance <- constructions_distance %>% round(0) # nearest integer
        constructions.output.temp[1,]$Start <- constructions_withinperiod$Start_Date[i]
        constructions.output.temp[1,]$End <- constructions_withinperiod$End_Date[i]
        constructions.output.temp[1,]$X <- constructions_coords$X
        constructions.output.temp[1,]$Y <- constructions_coords$Y
        constructions.output <- rbind(constructions.output, constructions.output.temp)
      }
    }
  }
  constructions.output %>% arrange(Distance) %>% return()
}

# Listings on 99.co
generate.listings <- function(n) {
  listings <- data.frame(Name=character(),
                         Bedrooms=character(),
                         Rental=character(),
                         Listings=character(),
                         URL=character(),
                         stringsAsFactors=FALSE)
  
  site_url <- "https://www.99.co/singapore/condos-apartments/"
  site_url_front <- "https://www.99.co/singapore/s/rent/condos-apartments/"
  site_url_back <- "?enquiry_destination=Cluster%20Details%20Widget%20Rent%20Listings"
  
  property_name <- properties$Name[n] %>% tolower() %>%
    gsub(' [[:punct:]] ','',.) %>%
    gsub('[[:punct:]]','',.) %>%
    gsub(' the','&',.) %>%
    gsub('the ','',.) %>%
    gsub('&',' the ',.) %>%
    gsub(' ','',.)
  
  listing.temp <- data.frame(Name=NA,
                             Bedrooms=NA,
                             Rental=NA,
                             Listings=NA,
                             URL=NA,
                             stringsAsFactors=FALSE)
  
  listing_url <- paste(site_url, property_name, sep='')
  listing_url.test <- tryCatch(read_html(listing_url), error=function(x){NA})
  if (is.na(listing_url.test)) {
    return(NA)
  }
  
  listing_doc <- read_html(listing_url)
  listing_data <- html_text(html_nodes(listing_doc, "#listings"))
  
  n_sale <- str_count(str_match(listing_data,"Listings for saleBedroom typePrice rangeNo. of listings(.*?)View listings for sale")[,2], pattern='\\$') - str_count(str_match(listing_data, "Listings for saleBedroom typePrice rangeNo. of listings(.*?)View listings for sale")[,2], pattern='-')
  n_rent <- str_count(str_match(listing_data, "Listings for rentBedroom typePrice rangeNo. of listings(.*?)View listings for rent")[,2], pattern='\\$') - str_count(str_match(listing_data, "Listings for rentBedroom typePrice rangeNo. of listings(.*?)View listings for rent")[,2], pattern='-')
  
  if (isTruthy(!is.na(n_rent) & n_rent>0)) {
    listing_data <- html_text(html_nodes(listing_doc, "._1Rmlo"))
    listing <- listing_data[((n_sale)*3+1):((n_sale+n_rent)*3)]
    for (j in 1:n_rent) {
      listing.temp[j,]$Name <- properties$Name[n]
      listing.temp[j,]$Bedrooms <- listing[3*j-2]
      listing.temp[j,]$Rental <- listing[3*j-1] 
      listing.temp[j,]$Listings <- listing[3*j]
      listing.temp[j,]$URL <- paste(site_url_front, property_name, site_url_back, sep='')
    }
    listings <- rbind(listings, listing.temp)
    listings %>% na.omit() %>% select(Bedrooms, Rental, Listings, URL) %>% arrange(Bedrooms) %>% return()
  } else {
    return(NA)
  }
}

# Generate icons for map
icons <- iconList(
  condo = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/2188/2188540.svg",
    iconWidth=50, iconHeight=50, iconAnchorX=0, iconAnchorY=0
    ),
  preschool = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/2767/2767788.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
    ),
  school = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/2784/2784389.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
    ),
  hospital = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/619/619172.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
    ),
  shopping = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/2080/2080201.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
    ),
  station = makeIcon(
  iconUrl="https://image.flaticon.com/icons/svg/744/744537.svg",
  iconWidth=25, iconHeight=25, iconAnchorX=0, iconAnchorY=0
  ),
  construction = makeIcon(
    iconUrl="https://image.flaticon.com/icons/svg/1971/1971308.svg",
    iconWidth=40, iconHeight=40, iconAnchorX=0, iconAnchorY=0
    )
)

server <- function(input, output, session) {
  
  # [FOR TENANTS] Output data for selected categories
  output$selected.data <- renderUI({
    if (length(input$search_tenant) > 0) {
      # Get ID of property
      id <- property.id(input$search_tenant)
      
      # Generate string to contain results
      result.string <- character()
      
      # District
      districts == properties[id,]$District 
      result.string <- paste(result.string, '<b><i><p style="color:blue">',
                             get.district(id), '</b></i></p>', sep='')
      
      # Nearest preschool
      if ('Preschools' %in% input$layers) {
        result.string <- paste(result.string, '<b>Nearest Preschool</b>: ',
                               nearest.preschool(id)$Nearest,
                               ' (', nearest.preschool(id)$Distance, 'm)|',
                               sep='')
      }
      # Nearest school
      if ('Schools' %in% input$layers) {
        result.string <- paste(result.string, '<b>Nearest School</b>: ',
                               nearest.school(id)$Nearest,
                               ' (', nearest.school(id)$Distance, 'm)|',
                               sep='')
      }
      # Nearest hospital
      if ('Hospitals' %in% input$layers) {
        result.string <- paste(result.string, '<b>Nearest Hospital</b>: ',
                               nearest.hospital(id)$Nearest,
                               ' (', nearest.hospital(id)$Distance, 'm)|',
                               sep='')
      }
      # Nearest shopping mall
      if ('Shopping Malls' %in% input$layers) {
        result.string <- paste(result.string, '<b>Nearest Shopping Mall</b>: ',
                               nearest.shopping(id)$Nearest,
                               ' (', nearest.shopping(id)$Distance, 'm)|',
                               sep='')
      }
      # Nearest station
      if ('Stations' %in% input$layers) {
        result.string <- paste(result.string, '<b>Nearest Station</b>: ',
                               nearest.station(id)$Nearest,
                               ' (', nearest.station(id)$Distance, 'm)|',
                               sep='') 
      }
      result.string <- gsub('\\|', '<br/>', result.string)
      HTML(result.string)
    }
  })
  
  # [FOR TENANTS] Generate table of listings on website
  listings.table_tenant <- reactive({
    # Get ID of property
    id <- property.id(input$search_tenant)
    
    # Generate table of listings
    listings_table <- generate.listings(id)
    if (isTruthy(!is.na(listings_table) & nrow(listings_table) > 0)) {
      listings_final <-
        list('available' = '<br> <b><i><p style="color:green"> Listings available on 99.co! </p></b></i>',
             'table' = listings_table %>% select(Bedrooms, Rental, Listings) %>% as.list(),
             'url' = listings_table[1,]$URL)
      listings_final
    }
  })
  
  # [FOR TENANTS] Output an alert, if listings are available on website
  output$listings.available_tenant <- renderUI({
    HTML(listings.table_tenant()$available)
    })
  
  # [FOR TENANTS] Output hyperlink to view listings on website
  output$listings_tenant <- renderTable(listings.table_tenant()$table)
  output$listings.url_tenant <- renderUI({
    if (isTruthy(!is.na(listings.table_tenant()$url))) {
      actionButton('listings.url', 'See listings on 99.co',
                   onclick = paste("window.open('", listings.table_tenant()$url, "', '_blank')", sep='')
      )
    }
  })
  
  # [FOR TENANTS] Generate map (with selected elements, if any)
  proximity.map_tenant <- reactive({
    # Generate base map
    proximity_map <- leaflet() %>% addTiles()
    
    # Get ID of property
    id <- property.id(input$search_tenant)
    
    if (length(input$search_tenant) > 0) {
      properties_table <- properties[id,] %>% select(Name,X,Y)
      
      # Get start and end dates
      start_date <- as.Date(input$period[1], '%d/%m/%Y')
      end_date <- as.Date(input$period[2], '%d/%m/%Y')
      
      # Preschools layer
      if ('Preschools' %in% input$layers) {
        nearest_preschool <- nearest.preschool(id)
        nearest_preschool.join <-
          full_join(properties_table, nearest_preschool, on=c('X','Y'))
        proximity_map <- proximity_map %>%
          addMarkers(data=nearest_preschool, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['preschool'])
        proximity_map <- proximity_map %>%
          addPolylines(data=nearest_preschool.join, lng=~X, lat=~Y)
      }
      # Schools layer
      if ('Schools' %in% input$layers) {
        nearest_school <- nearest.school(id)
        nearest_school.join <-
          full_join(properties_table, nearest_school, on=c('X','Y'))
        proximity_map <- proximity_map %>%
          addMarkers(data=nearest_school, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['school'])
        proximity_map <- proximity_map %>%
          addPolylines(data=nearest_school.join, lng=~X, lat=~Y)
      }
      # Hospitals layer
      if ('Hospitals' %in% input$layers) {
        nearest_hospital <- nearest.hospital(id)
        nearest_hospital.join <-
          full_join(properties_table, nearest_hospital, on=c('X','Y'))
        proximity_map <- proximity_map %>%
          addMarkers(data=nearest_hospital, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['hospital'])
        proximity_map <- proximity_map %>%
          addPolylines(data=nearest_hospital.join, lng=~X, lat=~Y)
      }
      # Shopping Malls layer
      if ('Shopping Malls' %in% input$layers) {
        nearest_shopping <- nearest.shopping(id)
        nearest_shopping.join <-
          full_join(properties_table, nearest_shopping, on=c('X','Y'))
        proximity_map <- proximity_map %>%
          addMarkers(data=nearest_shopping, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['shopping'])
        proximity_map <- proximity_map %>%
          addPolylines(data=nearest_shopping.join, lng=~X, lat=~Y)
      }
      # Stations layer
      if ('Stations' %in% input$layers) {
        nearest_station <- nearest.station(id)
        nearest_station.join <-
          full_join(properties_table, nearest_station, on=c('X','Y'))
        proximity_map <- proximity_map %>%
          addMarkers(data=nearest_station, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['station'])
        proximity_map <- proximity_map %>%
          addPolylines(data=nearest_station.join, lng=~X, lat=~Y)
      }
      # Constructions layer
      if ('Construction' %in% input$layers) {
        nearby_constructions <- nearby.constructions(id, start_date, end_date, 500) # 500m
        for (i in 1:nrow(nearby_constructions)) {
          proximity_map <- proximity_map %>%
            addMarkers(data=nearby_constructions[i,], lng=~X, lat=~Y,
                       popup = paste('<center><b>',
                                     nearby_constructions[i,]$Name,
                                     '</b> <br> (',
                                     format(as.Date(nearby_constructions[i,]$Start, origin='1970-01-01'), '%Y-%m-%d'),
                                     ' to ',
                                     format(as.Date(nearby_constructions[i,]$End, origin='1970-01-01'), '%Y-%m-%d'),
                                     ')</center>',
                                     sep=''),
                       icon=~icons['construction']
                       )
        }
      }
      # Show selected property on map
      proximity_map <- proximity_map %>%
        addMarkers(data=properties_table, lng=~X, lat=~Y,
                   popup = paste('<center><b>',
                                 properties[id,]$Name,
                                 '<p style="color:navy">',
                                 str_to_title(properties[id,]$Street),
                                 '</p>',
                                 '</center>',
                                 sep=''),
                   icon=~icons['condo']) %>%
        setView(properties_table$X, properties_table$Y, zoom=15)

    } else {
      # Default view (if nothing is selected)
      proximity_map <- setView(proximity_map, 103.84, 1.35, zoom=12) %>%
        addMarkers(data=properties, lng=~X, lat=~Y, label=~Name)
    }
  })
  
  # [FOR TENANTS] Show map
  output$proximity.map_tenant <- renderLeaflet(proximity.map_tenant())
  
  # [FOR TENANTS] Detect if elements on map are clicked
  property.click_tenant <- observeEvent(input$proximity.map_tenant_marker_click, {
      lat <- input$proximity.map_tenant_marker_click$lat
      lng <- input$proximity.map_tenant_marker_click$lng
      property <- properties %>% filter(X==lng & Y==lat)
      
      # Adds clicked property into searchbox
      if (nrow(property) > 0) {
        updateTextInput(session, 'search_tenant', value=property$Name)
        }
      })
  
  # [FOR OWNERS] Output key data related to property
  output$property.info <- renderUI({
    if (length(input$search_owner) > 0) {
      # Get ID of property
      id <- property.id(input$search_owner)
      
      # Generate string to contain results
      result.string <- character()
      
      # District
      result.string <- paste(result.string, '<b><i><p style="color:blue">',
                             get.district(id), '</b></i></p>', sep='')
      same_district <- which(properties$District == properties[id,]$District)
      
      # Nearest preschool
      nearest.preschool.dist <- function(n) {
        nearest.preschool(n)$Distance
      }
      dist_preschool <- nearest.preschool.dist(id)
      if (dist_preschool <= 1000) {
        DIST.preschool <- paste('<p style="display:inline;color:green"><b>', dist_preschool, 'm</b></p>', sep='')
      } else {
        DIST.preschool <- paste('<p style="display:inline;color:firebrick"><b>', dist_preschool, 'm</b></p>', sep='')
      }
      mean.dist_preschool <- sapply(same_district, nearest.preschool.dist) %>% mean()
      if (dist_preschool <= mean.dist_preschool) {
        POS.preschool <- '<p style="display:inline;color:green"><b>closer</b></p>'
      } else {
        POS.preschool <- '<p style="display:inline;color:firebrick"><b>further</b></p>'
      }
      result.string <- paste(result.string, '<b>Nearest Preschool</b>: ',
                             DIST.preschool, ' (', POS.preschool, " than your district's average)",
                             sep='')
      # Nearest school
      nearest.school.dist <- function(n) {
        nearest.school(n)$Distance
      }
      dist_school <- nearest.school.dist(id)
      if (dist_school <= 1000) {
        DIST.school <- paste('<p style="display:inline;color:green"><b>', dist_school, 'm</b></p>', sep='')
      } else {
        DIST.school <- paste('<p style="display:inline;color:firebrick"><b>', dist_school, 'm</b></p>', sep='')
      }
      mean.dist_school <- sapply(same_district, nearest.school.dist) %>% mean()
      if (dist_school <= mean.dist_school) {
        POS.school <- '<p style="display:inline;color:green"><b>closer</b></p>'
      } else {
        POS.school <- '<p style="display:inline;color:firebrick"><b>further</b></p>'
      }
      result.string <- paste(result.string, '<br> <b>Nearest School</b>: ',
                             DIST.school, ' (', POS.school, " than your district's average)",
                             sep='')
      # Nearest hospital
      nearest.hospital.dist <- function(n) {
        nearest.hospital(n)$Distance
      }
      dist_hospital <- nearest.hospital.dist(id)
      if (dist_hospital <= 1000) {
        DIST.hospital <- paste('<p style="display:inline;color:green"><b>', dist_hospital, 'm</b></p>', sep='')
      } else {
        DIST.hospital <- paste('<p style="display:inline;color:firebrick"><b>', dist_hospital, 'm</b></p>', sep='')
      }
      mean.dist_hospital <- sapply(same_district, nearest.hospital.dist) %>% mean()
      if (dist_hospital <= mean.dist_hospital) {
        POS.hospital <- '<p style="display:inline;color:green"><b>closer</b></p>'
      } else {
        POS.hospital <- '<p style="display:inline;color:firebrick"><b>further</b></p>'
      }
      result.string <- paste(result.string, '<br> <b>Nearest Hospital</b>: ',
                             DIST.hospital, ' (', POS.hospital, " than your district's average)",
                             sep='')
      # Nearest shopping mall
      nearest.shopping.dist <- function(n) {
        nearest.shopping(n)$Distance
      }
      dist_shopping <- nearest.shopping.dist(id)
      if (dist_shopping <= 1000) {
        DIST.shopping <- paste('<p style="display:inline;color:green"><b>', dist_shopping, 'm</b></p>', sep='')
      } else {
        DIST.shopping <- paste('<p style="display:inline;color:firebrick"><b>', dist_shopping, 'm</b></p>', sep='')
      }
      mean.dist_shopping <- sapply(same_district, nearest.shopping.dist) %>% mean()
      if (dist_shopping <= mean.dist_shopping) {
        POS.shopping <- '<p style="display:inline;color:green"><b>closer</b></p>'
      } else {
        POS.shopping <- '<p style="display:inline;color:firebrick"><b>further</b></p>'
      }
      result.string <- paste(result.string, '<br> <b>Nearest Shopping Mall</b>: ',
                             DIST.shopping, ' (', POS.shopping, " than your district's average)",
                             sep='')
      # Nearest station
      nearest.station.dist <- function(n) {
        nearest.station(n)$Distance
      }
      dist_station <- nearest.station.dist(id)
      if (dist_station <= 1000) {
        DIST.station <- paste('<p style="display:inline;color:green"><b>', dist_station, 'm</b></p>', sep='')
      } else {
        DIST.station <- paste('<p style="display:inline;color:firebrick"><b>', dist_station, 'm</b></p>', sep='')
      }
      mean.dist_station <- sapply(same_district, nearest.station.dist) %>% mean()
      if (dist_station <= mean.dist_station) {
        POS.station <- '<p style="display:inline;color:green"><b>closer</b></p>'
      } else {
        POS.station <- '<p style="display:inline;color:firebrick"><b>further</b></p>'
      }
      result.string <- paste(result.string, '<br> <b>Nearest Station</b>: ',
                             DIST.station, ' (', POS.station, " than your district's average)",
                             sep='')
      return(HTML(result.string))
    } else {
      return(HTML('<p style="color:gray;font-size:125%"><i>
                  Select a property to see more information about it!</i></p>'))
    }
  })
  
  # [FOR OWNERS] Generate table of listings on website
  listings.table_owner <- reactive({
    # Get ID of property
    id <- property.id(input$search_owner)
    
    # Generate table of listings
    listings_table <- generate.listings(id)
    if (isTruthy(!is.na(listings_table) & nrow(listings_table) > 0)) {
      listings_final <-
        list('available' = '<br> <b><i><p style="color:crimson">
             Other homeowners have already placed listings on 99.co! </p></b></i>',
             'table' = listings_table %>% select(Bedrooms, Rental, Listings) %>% as.list(),
             'url' = listings_table[1,]$URL)
      listings_final
    }
  })
  
  # [FOR OWNERS] Output an alert, if listings are available on website
  output$listings.available_owner <- renderUI({
    HTML(listings.table_owner()$available)
  })
  
  # [FOR OWNERS] Output hyperlink to view listings on website
  output$listings_owner <- renderTable(listings.table_owner()$table)
  output$listings.url_owner <- renderUI({
    if (isTruthy(!is.na(listings.table_owner()$url))) {
      actionButton('listings.url', 'See listings on 99.co',
                   onclick = paste("window.open('", listings.table_owner()$url, "', '_blank')", sep='')
      )
    }
  })
  
  # [FOR OWNERS] Generate map with all key elements
  proximity.map_owner <- reactive({
    # Generate base map
    proximity_map <- leaflet() %>% addTiles()
    
    # Get ID of property
    id <- property.id(input$search_owner)
    
    if (length(input$search_owner) > 0) {
      properties_table <- properties[id,] %>% select(Name,X,Y)
      
      # Preschools layer
      nearest_preschool <- nearest.preschool(id)
      nearest_preschool.join <-
        full_join(properties_table, nearest_preschool, on=c('X','Y'))
      proximity_map <- proximity_map %>%
        addMarkers(data=nearest_preschool, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['preschool'])
      proximity_map <- proximity_map %>%
        addPolylines(data=nearest_preschool.join, lng=~X, lat=~Y)
      # Schools layer
      nearest_school <- nearest.school(id)
      nearest_school.join <-
        full_join(properties_table, nearest_school, on=c('X','Y'))
      proximity_map <- proximity_map %>%
        addMarkers(data=nearest_school, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['school'])
      proximity_map <- proximity_map %>%
        addPolylines(data=nearest_school.join, lng=~X, lat=~Y)
      # Hospitals layer
      nearest_hospital <- nearest.hospital(id)
      nearest_hospital.join <-
        full_join(properties_table, nearest_hospital, on=c('X','Y'))
      proximity_map <- proximity_map %>%
        addMarkers(data=nearest_hospital, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['hospital'])
      proximity_map <- proximity_map %>%
        addPolylines(data=nearest_hospital.join, lng=~X, lat=~Y)
      # Shopping Malls layer
      nearest_shopping <- nearest.shopping(id)
      nearest_shopping.join <-
        full_join(properties_table, nearest_shopping, on=c('X','Y'))
      proximity_map <- proximity_map %>%
        addMarkers(data=nearest_shopping, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['shopping'])
      proximity_map <- proximity_map %>%
        addPolylines(data=nearest_shopping.join, lng=~X, lat=~Y)
      # Stations layer
      nearest_station <- nearest.station(id)
      nearest_station.join <-
        full_join(properties_table, nearest_station, on=c('X','Y'))
      proximity_map <- proximity_map %>%
        addMarkers(data=nearest_station, lng=~X, lat=~Y, popup=~Nearest, icon=~icons['station'])
      proximity_map <- proximity_map %>%
        addPolylines(data=nearest_station.join, lng=~X, lat=~Y)
      
      # Show selected property on map
      proximity_map <- proximity_map %>%
        addMarkers(data=properties_table, lng=~X, lat=~Y,
                   popup = paste('<center><b>',
                                 properties[id,]$Name,
                                 '<p style="color:navy">',
                                 str_to_title(properties[id,]$Street),
                                 '</p>',
                                 '</center>',
                                 sep=''),
                   icon=~icons['condo']) %>%
        setView(properties_table$X, properties_table$Y, zoom=15)
      
    } else {
      # Default view (if nothing is selected)
      proximity_map <- setView(proximity_map, 103.84, 1.35, zoom=11) %>%
        addMarkers(data=properties, lng=~X, lat=~Y, label=~Name)
    }
  })
  
  # [FOR OWNERS] Show map
  output$proximity.map_owner <- renderLeaflet(proximity.map_owner())
  
  # [FOR OWNERS] Detect if elements on map are clicked
  property.click_owner <- observeEvent(input$proximity.map_owner_marker_click, {
    lat <- input$proximity.map_owner_marker_click$lat
    lng <- input$proximity.map_owner_marker_click$lng
    property <- properties %>% filter(X==lng & Y==lat)
    
    # Adds clicked property into searchbox
    if (nrow(property) > 0) {
      updateTextInput(session, 'search_owner', value=property$Name)
    }
  })
  
  # [FOR OWNERS] Plot graph of past rental data
  rentals.graph <- reactive({
    if (length(input$search_owner) > 0) {
      past.rentals.graph(property.id(input$search_owner))
    } else {
      ggplot() +
        theme_economist()
    }
  })
  output$rentals <- renderPlot(rentals.graph())
  
  # [FOR OWNERS] Create hover tooltip for past rental graph
  output$rental_info <- renderUI({
    if (length(input$search_owner) > 0) {
      hover <- input$plot_hover
      rentals_table <- past.rentals(property.id(input$search_owner))
      point <- nearPoints(rentals_table, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      
      if (nrow(point) == 0) {
        return(NULL)
      }
      
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
      style <- paste0('position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85); ',
                      'left:', left_px, 'px; top:', top_px + 300, 'px;',
                      'padding: 10px')
  
      wellPanel(
        style = style,
        p(HTML(paste('<center> <b> $', point$Median,
                     '</b> / sq.ft. <br> in <i>', point$Quarter,'</i> </center>',
                     sep='')
               )
          )
      )
    }
  })
  
  output$help <- renderUI(HTML(help))
}