library(shiny)
library(shinydashboard)
library(leaflet)

#setwd("C:/Users/TKW/Documents/R/RentApp/data") # Change if necessary
properties <- read.csv("data/properties.csv", stringsAsFactors=FALSE)


header <- dashboardHeader()

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('For tenants', tabName='tenant', icon=icon('building')),
    menuItem('For owners', tabName='owner', icon=icon('building')),
    menuItem('About this app', tabName='help', icon=icon('question'))
  )
)

body <- dashboardBody(
  tabItems(
    # Tab 1: Tenants
    tabItem(tabName='tenant',
            fluidRow(
              column(width=8,
                     leafletOutput('proximity.map_tenant',
                                   height=700)
                     ),
              column(width=4,
                     box(width=NULL,
                         height=NULL,
                         selectizeInput('search_tenant', 'Search property:',
                                        choices=properties$Name,
                                        multiple=TRUE,
                                        options=list(placeholder='e.g. VARSITY PARK CONDOMINIUM',
                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                     maxItems=1)
                                        ),
                         dateRangeInput('period', 'I am renting a property from:'
                                        ),
                         checkboxGroupInput('layers', 'I am interested about nearby:',
                                            choices=c('Preschools',
                                                      'Schools',
                                                      'Hospitals',
                                                      'Shopping Malls',
                                                      'Stations',
                                                      'Construction')
                                            ),
                         htmlOutput('selected.data'),
                         htmlOutput('listings.available_tenant'),
                         tableOutput('listings_tenant'),
                         uiOutput('listings.url_tenant')
                         )
                     )
              )
            ),
    # Tab 2: Owners
    tabItem(tabName='owner',
            fluidRow(
              column(width=8,
                     leafletOutput('proximity.map_owner',
                                   height=400),
                     p(''),
                     plotOutput('rentals',
                                width=NULL, height=300,
                                hover = hoverOpts('plot_hover', delay=100)),
                     uiOutput('rental_info')
              ),
              column(width=4,
                     box(width=NULL, height=NULL,
                         selectizeInput('search_owner', 'Search property:',
                                        choices=properties$Name,
                                        multiple=TRUE,
                                        options=list(placeholder='e.g. VARSITY PARK CONDOMINIUM',
                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                     maxItems=1)
                         )
                     ),
                     box(width=NULL, height=NULL, #593
                         htmlOutput('property.info'),
                         htmlOutput('listings.available_owner'),
                         tableOutput('listings_owner'),
                         uiOutput('listings.url_owner')
                         )
                     )
              )
            ),
    # Tab 3: Help
    tabItem(tabName='help',
            uiOutput('help')
            )
    )
  )

dashboardPage(header, sidebar, body,
              skin='red')