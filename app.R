library(leaflet)
library(shiny)
library(raster)
library(rsconnect)
library(DT)
library(shinycssloaders)



#load data
load("TestPredlines.Rdata")
natBar<-read.csv("data/NatBarriers.csv")
kmVal<-read.csv("data/PDT_values_Range_allspp.csv")
culverts<-read.csv("data/Culverts_forApp.csv")
kmVal<-kmVal[,-c(2,3)]
colnames(kmVal)<-c("Predicted Range (km)","PDT","Species","Blocked Habitat (km)","Anthropogenic barriers (count)")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


#User interface -----
ui <- fluidPage(
  # App title ----
  titlePanel("Chehalis River Basin Salmonid Range of Occurence"),
  # Sidebar layout with input and output definitions ----  
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("varCO",
                  label = "Choose a coho salmon probablity decision threshold (PDT) to display",
                  choices = c("0.10","0.15","0.20","0.25","0.30","0.35","0.40","0.45", "0.50","0.55","0.60","0.65","0.70", "0.75","0.80","0.85","0.90"),
                  selected = "0.50"),
      h5("The predicted coho salmon range of occurrence is represented by the", span(strong("salmon lines"), style = "color:salmon")),
      selectInput("varSD", 
                  label = "Choose a steelhead trout probablity decision threshold (PDT) to display",
                  choices = c("0.10","0.15","0.20","0.25","0.30","0.35","0.40","0.45", "0.50","0.55","0.60","0.65","0.70", "0.75","0.80","0.85","0.90"),
                  selected = "0.50"),
      h5("The predicted steelhead trout range of occurrence is represented by the", span(strong("cyan lines"), style = "color:cyan")),
      selectInput("varCH", 
                  label = "Choose a chum salmon probablity decision threshold (PDT) to display",
                  choices = c("0.10","0.15","0.20","0.25","0.30","0.35","0.40","0.45", "0.50","0.55","0.60","0.65","0.70", "0.75","0.80","0.85","0.90"),
                  selected = "0.50"),
      h5("The predicted chum salmon range of occurrence is represented by the", span(strong("purple lines"), style = "color:purple")),
      actionButton("button", "Select")
    ),
    # Main panel for displaying outputs ----  
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Introduction",
                           h3("Welcome to our Shiny app"),
                           br(),
                           p("One of the risks faced by habitat restoration practitioners is whether habitats included in restoration planning will be used by the target species or, conversely, whether habitats excluded from restoration planning would have benefitted the target species. We developed this app to present a quantitative decision-making tool that can be used by practitioners that represents varying levels of risk tolerance. We used multiple probability decision thresholds (PDT) to predict the range of occurrence for three anadromous fishes (",em("Oncorhynchus"), "spp.) in a watershed in southwestern Washington, USA.",style = "text-align:justify"),  
                           br(),
                           p("The range of occurrence for anadromous fishes is defined by the upper limit of occurrence (ULO), where all habitat downstream of the ULO is used for some combination of rearing, spawning, or migration."),
                           br(),
                           p("The multiple probability decision thresholds (PDT) represent varying levels of risk tolerance. In the context of habitat restoration, practitioners are often confronted with the risk of potentially investing funds to connect habitats only to discover the newly connected areas fall outside the suitable range of occurrence of the target species and thus go unused. For this study, we consider the selection of low PDT values that include more false positive predictions to be high risk-tolerant and the selection of high PDT values that include more false negatives to be low risk-tolerant.",style = "text-align:justify"),
                           br(),
                           div(img(src="PDT_scenario.png",height=400,width=500),style = "text-align:center;"),
                           p(em("Figure 1. Species distribution models developed from binomial data (i.e., absent or present) require selection of a probability decision threshold (PDT) for a binary categorization of the output, above which species are predicted to be present. This graphic demonstrates a hypothetical response curve from a generalized linear model and how the PDT selected will determine what habitat is included in the range of occurrence based on a location's landscape characteristics.",style = "text-align:justify")),
                           br(),
                           p("While stakeholder groups engaged in habitat restoration generally share the goal of improving their local watershed, groups may diverge in preferred strategies. For example, one group may prioritize reconnecting the greatest quantity of habitat (i.e., high risk tolerance) and another group may want to prioritize the reconnection of habitat that has the greatest likelihood of being used by the target species (i.e., low risk tolerance). The ability to describe species distributions at multiple levels of risk-tolerance will allow diverse groups of decision makers to consider investments in restoration actions at select locations.",style = "text-align:justify"),
                           br(),
                           p(strong("To use this app please select a PDT from the drop down menu on the left side bar to visualize the predicted range of occurrence for each species of interest, then press the 'select' button to load the new layers. Please be patient! Also Included on the map are the anthropogenic barriers included in the analysis and natural barriers observed during on-the-ground field surveys. The user can click on a barrier feature or location along the stream network for more deatiled information associated with the location",style = "text-align:justify"))
                           ),
                  tabPanel("Methods",
                           br(),
                           p("Extensive field surveys covering 669 river km across two years documented the upper limit of occurrence (ULO) in 115 streams for coho salmon (",em("O. kisutch"),"), 97  streams for steelhead trout (",em("O. mykiss"),"), and 57 streams for chum salmon (",em("O. keta"),") as well as any local features, such as natural barriers, that influenced the ULOs."),
                           br(),
                           div(img(src="ULOmaps.png",height = 500, width = 600),style = "text-align:center;"),
                           p(em("Figure 1. Field documented upper limit of occurrence (ULO) locations (yellow dots) for coho salmon (a), steelhead trout (b) and chum salmon (c) in the Chehalis River basin, Washington, USA, collected between 2017-2019.")),
                           br(),
                           p("Generalized linear mixed models quantified the relationships between the ULOs and landscape attributes derived from remote sensing data. For each focal species, we fit the data with a generalized linear model with a binomial distribution and logit link. We used a multi-model selection process to identify the best suite of predictive variables for each species. This analysis provides a probabilistic prediction, P, of a stream reach being within the range of occurrence."),
                           p("For coho salmon:"),
                           div(img(src="coho_mod.png",height=100, width=800),style = "text-align:center;"),
                           p("where,",HTML(paste0(em("X"),tags$sub("1"))),"is", HTML(paste0("log",tags$sub("10"))),"(drainage area),",HTML(paste0(em("X"),tags$sub("2"))),"is mean annual precipitation,", HTML(paste0(em("X"),tags$sub("3"))),"is elevation,", em("Wet"), "is wetland presence", HTML(paste0(em("Geo"),tags$sub("1"))), "is glacial geology,",HTML(paste0(em("Geo"),tags$sub("2"))),"is sandstone geology,", HTML(paste0(em("Geo"),tags$sub("3"))),"is alluvium geology, and",HTML(paste0(em("a"),tags$sub("k"))),"is the normally distributed random intercept coefficient for sub-basin", em("k"),"."),
                           br(),
                           p("For steelhead trout:"),
                           div(img(src="steelhead_mod.png",height=100, width=800),style = "text-align:center;"),
                           p("where,",HTML(paste0(em("X"),tags$sub("1"))),"is", HTML(paste0("log",tags$sub("10"))),"(drainage area),",HTML(paste0(em("X"),tags$sub("2"))),"is slope,", em("Wet"), "is wetland presence", HTML(paste0(em("X"),tags$sub("3"))),"is elevation,",HTML(paste0(em("Geo"),tags$sub("1"))), "is glacial geology,",HTML(paste0(em("Geo"),tags$sub("2"))),"is sandstone geology,", HTML(paste0(em("Geo"),tags$sub("3"))),"is alluvium geology, and",HTML(paste0(em("a"),tags$sub("k"))),"is the normally distributed random intercept coefficient for sub-basin", em("k"),"."),
                           br(),
                           p("For chum salmon:"),
                           div(img(src="chum_mod.png",height=65, width=675),style = "text-align:center;"),
                           p("where,",HTML(paste0(em("X"),tags$sub("1"))),"is", HTML(paste0("log",tags$sub("10"))),"(drainage area),", HTML(paste0(em("X"),tags$sub("2"))),"is elevation,",HTML(paste0(em("Geo"),tags$sub("1"))), "is glacial geology,",HTML(paste0(em("Geo"),tags$sub("2"))),"is sandstone geology,", HTML(paste0(em("Geo"),tags$sub("3"))),"is alluvium geology."),
                           br(),
                           p("The range of occurrence for each species was defined by the cumulative set of ULOs predicted for the basin-wide set of stream segments. Total habitat quantity (km) for each species was calculated based on the summed lengths of stream reaches downstream of the ULOs for each stream profile. We calculated the total quantity of habitat within the range of occurrence using multiple PDTs, from 0.10 to 0.90 at 0.05 intervals."),
                           p("Locations of anthropogenic barriers to fish passage (e.g., culverts, dams, non-culvert road crossings; n=1,737) were obtained from the Washington Department of Fish and Wildlife's",
                             a("Fish Passage and Diversion Screening Inventory database",
                               href="https://fortress.wa.gov/dfw/public/PublicDownload/habitat/FishPassage/"),". Only structures listed in the database that impeded fish passage (i.e., less than 100% passability) were included in the analysis. The amount of upstream habitat blocked by anthropogenic barriers was summed based on the predicted range for each species using PDT values of 0.25, 0.50, and 0.75."),
                           p("Detailed description of the methods can be found in",
                             a("Walther 2021",
                               href="https://www.proquest.com/docview/2518158401?pq-origsite=gscholar&fromopenview=true"))
                           ),
                  tabPanel("Predicitve Map",
                           h4(em("Disclaimer: Please be patient. This map may take minutes to load depending on internet connection.")),
                           withSpinner(leafletOutput("map",height = 700),type=5),tableOutput("table")),
                  tabPanel("Recommended Use",
                           p("The model predictions from this study should be used to identify locations for further investigation when considering locations for restoration and not be used as the exclusive piece of information. When considering locations to restore connectivity, practitioners should consider upstream habitat quality, species life history, and productivity of nearby donor populations in addition to the quantity of habitat that would be made available. We recommend that decision-makers carefully assess how species' range of occurrence may change based on different risk-tolerance scenarios instead of using a single, commonly used probability decision threshold. Please reach out to",a("the authors",href="mailto:waltheej@gmail.com"), "if there is a specific location of interest to the user to request greater details than what is provided in this app."),
                           br(),
                           p("In addition to guiding restoration planning, we hope that this app can serve as a resource for logistical planning for field surveys across the basin."),
                           br(),
                           p("This app is meant to be a living document. As more on-the-ground surveys are conducted, we hope to update information on natural barriers encountered. Please forward any new barrier information and it will be incorporated into the app. We are happy to provide the field protocols for assessing natural barriers used in this study and occurrence survey protocols if there is interest in implementing the methods presented here elsewhere.")
                           ),
                  tabPanel("MetaData",
                           h3("Natural Barriers"),
                           p(em("Note: This is not a comprehensive dataset for all natural barriers across the basin, and only represents the documented barriers that were encountered during field surveys.")), 
                           p(strong("Types:"),"Types of barriers include falls, cascade, chute, bedrock slide, log jam, debris jam, beaver dam, mass wasting, or a hybrid of these different types (where more than one type is reported)."),
                           p(strong("Class:"),"Natural barriers encountered were classified as either permanent or transient barriers. Permanent barriers were geologically-formed physical obstructions present in the stream channel (e.g., cascade, falls) and transient barriers were natural obstructions likely to vary within and among years (e.g., beaver dam, log jam)."),
                           p(strong("Vertical height (m):"),"Measurement obtained in the fieldusing a laser range finder or stadio rod from the the surface of the plunge pool to the top of the barrier."),
                           p(strong("Horizontal distance (m):"),"Measurement obtained in the field using a laser range finder from the downstream end of the barrier to the upstream end of the barrier."),
                           p(strong("Plunge pool depth (m):"),"Measurement obtained in the field using a stadio rod"),
                           br(),
                           h3("Anthropogenic Barriers"),
                           p(strong("Type:"),"Type of barrier"),
                           p(strong("Site Id:"),"Unique identificaiton associated with an anthropogenic barrier. This ID can be used to look up further details of a specific barrier on WDFW's",a("Fish Passage online database", href="https://geodataservices.wdfw.wa.gov/hp/fishpassage/index.html")),
                           p(strong("Reason:"),"The criteria used to determine that the barrier was <100% passable. See WDFW's", a("fish passage manual",href="https://wdfw.wa.gov/sites/default/files/publications/02061/Fish%20Passage%20Inventory%2C%20Assessment%2C%20and%20Prioritization%20Manual.pdf"), "for details."),
                           br(),
                           h3("Predicted Range"),
                           p(strong("Stream Name:"),"Common stream name; if no common name, stream name has been left blank."),
                           p(strong("LLID:"),"Unique idenfication ID for each stream segment developed by WDFW GIS team by joining NHD segments along stream segment; short-hand for longititude, latitude identification."),
                           p(strong("ReachID:"),"Unique identification number that corresonds to the  NHD segment in the GIS NHD attribute layer used in this anaylsis."),
                           p(strong("Speices:"),"focal species"),
                           p(strong("Predicted Value:"),"The probabilistic prediction, P, of the stream reach being within the range of occurrence based on the predictive model."),
                           br(),
                           h3("Prediction Table"),
                           p(strong("Predicted Range (km):"),"The total amount of habitat within the predicted range of occurrence"),
                           p(strong("PDT:"),"Probability decision threshold used to determine the binary categorization of the model output, above which species are predicted to be present."),
                           p(strong("Species:"),"focal species"),
                           p(strong("Blocked Habitat (km):"),"The total amount of habitat within the predicted range of occurrence that is inaccessible due to anthropogenic barriers. This quantity was only predicted using a PDT of 0.25, 0.50, and 0.75."),
                           p(strong("Anthropogenic barriers (count):"),"The number of unique anthropogenic barriers within the predicted range of occurrence that is impeding access to upstream habitat."),
                           p(em("If there is interest in the amount of habitat blocked by a specific barrier or specific PDT not reported, please contact"), a(em("the authors."),href="mailto:waltheej@gmail.com"),em("We are happy to provide any addtional information requested."))
                           )
                  )
  )
))


# Server logic ----
server <- function(input, output) {
  #with action button
  tablePred<-eventReactive(input$button,{
    sel.dtSD <- switch(input$varSD, 
                       "0.10" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.1"),],
                       "0.15" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.15"),],
                       "0.20" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.2"),],
                       "0.25" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.25"),],
                       "0.30" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.3"),],
                       "0.35" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.35"),],
                       "0.40" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.4"),],
                       "0.45" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.45"),],
                       "0.50" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.5"),],
                       "0.55" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.55"),],
                       "0.60" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.6"),],
                       "0.65" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.65"),],
                       "0.70" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.7"),],
                       "0.75" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.75"),],
                       "0.80" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.8"),],
                       "0.85" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.85"),],
                       "0.90" = kmVal[which(kmVal$Species=="Steelhead"&kmVal$PDT=="0.9"),])
    sel.DTCo <- switch(input$varCO,
                       "0.10" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.1"),],
                       "0.15" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.15"),],
                       "0.20" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.2"),],
                       "0.25" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.25"),],
                       "0.30" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.3"),],
                       "0.35" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.35"),],
                       "0.40" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.4"),],
                       "0.45" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.45"),],
                       "0.50" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.5"),],
                       "0.55" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.55"),],
                       "0.60" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.6"),],
                       "0.65" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.65"),],
                       "0.70" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.7"),],
                       "0.75" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.75"),],
                       "0.80" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.8"),],
                       "0.85" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.85"),],
                       "0.90" = kmVal[which(kmVal$Species=="Coho"&kmVal$PDT=="0.9"),])
    sel.distCH <- switch(input$varCH, 
                         "0.10" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.1"),],
                         "0.15" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.15"),],
                         "0.20" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.2"),],
                         "0.25" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.25"),],
                         "0.30" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.3"),],
                         "0.35" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.35"),],
                         "0.40" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.4"),],
                         "0.45" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.45"),],
                         "0.50" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.5"),],
                         "0.55" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.55"),],
                         "0.60" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.6"),],
                         "0.65" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.65"),],
                         "0.70" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.7"),],
                         "0.75" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.75"),],
                         "0.80" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.8"),],
                         "0.85" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.85"),],
                         "0.90" = kmVal[which(kmVal$Species=="Chum"&kmVal$PDT=="0.9"),])
    rbind(sel.DTCo,sel.dtSD,sel.distCH)
  }, ignoreNULL = FALSE)
  output$table <- renderTable({tablePred()})
  sthdlayer<-eventReactive(input$button,{
    sel.dist <- switch(input$varSD, 
                       "0.10" = steelhead[[1]],
                       "0.15" = steelhead[[2]],
                       "0.20" = steelhead[[3]],
                       "0.25" = steelhead[[4]],
                       "0.30" = steelhead[[5]],
                       "0.35" = steelhead[[6]],
                       "0.40" = steelhead[[7]],
                       "0.45" = steelhead[[8]],
                       "0.50" = steelhead[[9]],
                       "0.55" = steelhead[[10]],
                       "0.60" = steelhead[[11]],
                       "0.65" = steelhead[[12]],
                       "0.70" = steelhead[[13]],
                       "0.75" = steelhead[[14]],
                       "0.80" = steelhead[[15]],
                       "0.85" = steelhead[[16]],
                       "0.90" = steelhead[[17]])
  }, ignoreNULL = FALSE)
  coholayer<-eventReactive(input$button,{
    sel.distCo <- switch(input$varCO,
                         "0.10" = coho[[1]],
                         "0.15" = coho[[2]],
                         "0.20" = coho[[3]],
                         "0.25" = coho[[4]],
                         "0.30" = coho[[5]],
                         "0.35" = coho[[6]],
                         "0.40" = coho[[7]],
                         "0.45" = coho[[8]],
                         "0.50" = coho[[9]],
                         "0.55" = coho[[10]],
                         "0.60" = coho[[11]],
                         "0.65" = coho[[12]],
                         "0.70" = coho[[13]],
                         "0.75" = coho[[14]],
                         "0.80" = coho[[15]],
                         "0.85" = coho[[16]],
                         "0.90" = coho[[17]])
  }, ignoreNULL = FALSE)
  chumlayer<-eventReactive(input$button,{
    sel.distCH <- switch(input$varCH, 
                         "0.10" = chum[[1]],
                         "0.15" = chum[[2]],
                         "0.20" = chum[[3]],
                         "0.25" = chum[[4]],
                         "0.30" = chum[[5]],
                         "0.35" = chum[[6]],
                         "0.40" = chum[[7]],
                         "0.45" = chum[[8]],
                         "0.50" = chum[[9]],
                         "0.55" = chum[[10]],
                         "0.60" = chum[[11]],
                         "0.65" = chum[[12]],
                         "0.70" = chum[[13]],
                         "0.75" = chum[[14]],
                         "0.80" = chum[[15]],
                         "0.85" = chum[[16]],
                         "0.90" = chum[[17]])
  }, ignoreNULL = FALSE)
  output$map = renderLeaflet({
    Sys.sleep(3) # s
    sel.distCo<-coholayer()
    sel.dist<-sthdlayer()
    sel.distCH<-chumlayer()
    leaflet() %>% addTiles() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -123.283957, lat = 46.902200, zoom = 9) %>%
      # Overlay groups
      addMarkers(data=natBar,~Longitude,~Latitude,group="Natural Barriers",
                 popup=paste("Type:",natBar$Type,"<br>",
                             "Class:",natBar$Class,"<br>",
                             "Vertical height (m):",natBar$VD,"<br>",
                             "Horizontal distance (m):",natBar$HD,"<br>",
                             "Plunge pool depth (m):",natBar$PPD,"<br>",
                             "Coordinates:",natBar$Latitude,", ",natBar$Longitude)) %>%
      addCircleMarkers(data=culverts,~Long, ~Lat,group="Anthropogenic Barriers",color="red",
                       stroke = FALSE, fillOpacity = 0.8,radius=4,
                       popup=paste("Type:",culverts$FeatureTyp,"<br>",
                                   "Site Id:",culverts$SiteId,"<br>",
                                   "Reason:",culverts$Reason,"<br>",
                                   "Coordinates:",culverts$Lat,", ",culverts$Long)) %>%
      addPolylines(data=sel.distCo,color = 'salmon', opacity = 1,group="Coho",
                   popup=paste("Stream Name:",sel.distCo$StreamName,"<br>",
                               "LLID:",sel.distCo$LLID,"<br>",
                               "ReachID:",sel.distCo$NHD_FID,"<br>",
                               "Species:",sel.distCo$Species,"<br>",
                               "Predicted value:",sel.distCo$pCo)) %>%
      addPolylines(data=sel.dist,color = 'cyan', opacity = 1,group="Steelhead",
                   popup=paste("Stream Name:",sel.dist$StreamName,"<br>",
                               "LLID:",sel.dist$LLID,"<br>",
                               "ReachID:",sel.dist$NHD_FID,"<br>",
                               "Species:",sel.dist$Species,"<br>",
                               "Predicted value:",sel.dist$pSD)) %>%
      addPolylines(data=sel.distCH,color = 'purple', opacity = 1,group="Chum",
                   popup=paste("Stream Name:",sel.distCH$StreamName,"<br>",
                               "LLID:",sel.distCH$LLID,"<br>",
                               "ReachID:",sel.distCH$NHD_FID,"<br>",
                               "Species:",sel.distCH$Species,"<br>",
                               "Predicted value:",sel.distCH$pCH)) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Natural Barriers","Anthropogenic Barriers","Coho","Steelhead","Chum"),
        options = layersControlOptions(collapsed = FALSE) 
      ) %>%
      hideGroup(c("Natural Barriers","Anthropogenic Barriers"))
  })
  }


#run app
shinyApp(ui, server)





