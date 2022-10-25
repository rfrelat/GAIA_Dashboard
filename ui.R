# Define UI for application
shinyUI(fluidPage(
  theme = bs_theme(bg = "rgb(249, 247, 245)", 
                   fg = "rgb(59, 37, 2)",
                   primary = "#5579B6",
                   font_scale = 0.7, bootswatch = "litera"),
  # run bs_theme_preview() to select customize
  
  # Application title
  titlePanel("Guiding Acid Soil Investments in sub-Saharan Africa - GAIA"),
  
  # Sidebar with the input from users
  sidebarLayout(
   sidebarPanel(
     conditionalPanel(
       'input.out === "Summary"',
       h5("Acknowledgment:"),
       HTML('The GAIA project is a partnership between the 
            International Maize and Wheat Improvement Center (CIMMYT), 
            Ethiopian Institute of Agricultural Research (EIAR), 
            Rwanda Agriculture and Animal Resources Development Board (RAB), 
            Tanzania Agricultural Research Institute (TARI), 
            among other partners, supported financially by the 
            Bill and Melinda Gates Foundation.'),
       h5("Contact:"),
       HTML("Further information please contact <a href='mailto:j.silva@cgiar.org'>João Vasco Silva</a>"),
       h2(""),
       img(src='BMGlogo.svg', align = "center", width="250"),
       h5(""),
       img(src='cimmytlogo.svg', align = "center", width="250")
       ),
     conditionalPanel(
       'input.out === "Soil acidity"',
       selectInput("countSSA", h5("Select a country"),
                   choices = listSSA,
                   selected = "Ethiopia"),
       h5("Data sources:"),
       HTML("- pH data was retrieved from <a href='https://www.isda-africa.com/isdasoil/'>iSDA-soil</a> <br>"),
       HTML("- Crop distribution and area were retrieved from <a href='https://www.mapspam.info/'>MapSPAM</a> <br>"),
       HTML("- Farming systems refer to the classification by <a href='https://www.fao.org/family-farming/detail/en/c/273641/'>Dixon et al. (2001)</a>"),
       HTML("<br> <br> Crop types aggregate data for the following crops:"),
       HTML("<br>- Cereals: maize, sorghum, wheat, barley, rice, and millet"),
       HTML("<br>- Legumes: bean, chickpea, lentil, cowpea, pigeonpea, soybean, groundnut"),
       HTML("<br>- RTBs: potato, sweet potatotes, cassava, banana, plantain"),
       HTML("<br>- Commodity: coffee, sugar cane, cotton, coconut, tea, tobacco"),
       h5(''),
       downloadButton("downloadPH", "Download data")
       ),
     conditionalPanel(
       'input.out === "Lime requirement" | input.out === "Return on investment"',
       selectInput("country", h5("Select the country"),
                  choices = listGAIA,
                  selected = "Ethiopia")
       ),
     conditionalPanel(
       'input.out === "Lime requirement"',
       selectInput("selLime", h5("Select a lime requirement method"),
                   choices = c("Cochrane", "Kamprath")),
       h5(""),
       HTML("Lime requirement refers to lime rate (in t CaCO3/ha) 
       predicted by each method given the soil properties in each pixel. 
       Further reading and explanation of the lime requirement methods 
       can be found in: <br>"),
       HTML("-Kamprath, E.J. (1970) <a href='https://doi.org/10.2136/sssaj1970.03615995003400020022x'>Exchangeable aluminum as a criterion for liming leached mineral soils.</a> Soil Science Society of America Journal. <br>"),
       HTML("-Cochrane, T.T., Salinas, J.G., Sánchez, P.A. (1980) An equation for liming acid mineral soils to compensate crop aluminum tolerance. Tropical Agriculture."),
       h5(''),
       downloadButton("downloadLime", "Download data")
       ),
     conditionalPanel(
       'input.out === "Return on investment"',
       uiOutput('province'),
       selectInput("selPrice", h5("Select a price for lime (USD/ton)"),
                   choices = c("50", "75", "100"),
                   selected = "100"),
       h5(''),
       HTML("Crop types aggregate data for the following crops:"),
       HTML("<br>- Cereals: maize, sorghum, wheat, barley, rice, and millet"),
       HTML("<br>- Legumes: bean, chickpea, lentil, cowpea, pigeonpea, soybean, groundnut"),
       HTML("<br>- RTBs: potato, sweet potatotes, cassava, banana, plantain"),
       HTML("<br>- Commodity: coffee, sugar cane, cotton, coconut, tea, tobacco"),
       h5(''),
       downloadButton("downloadROI", "Download data"),
       ),
    width=3),
    # Main panel with the graphs and the plots
    mainPanel(
      tabsetPanel(
        id = 'out',
        tabPanel("Summary",
                 fluidRow(
                   column(7,h1(""),
                          includeMarkdown("Info.md")),
                   column(5, 
                          h1(""),h1(""),h1(""), #create space above the images
                          img(src="p1.png", align = "center", width="300"),
                          h1(""),
                          img(src="p2.png", align = "center", width="300"),
                          HTML('<br>(C) J.V. Silva'))
                 )),
        tabPanel("Soil acidity",
                 fluidRow(
                   column(6,withSpinner(leafletOutput("mapPH"), type=4)),
                   column(6,withSpinner(parcatsOutput('alluPH'), type=4))),#parcatsOutput
                 fluidRow(withSpinner(DTOutput('tablePH'), type=4))
                 ),
        tabPanel("Lime requirement",
                 fluidRow(column(6, withSpinner(leafletOutput("mapLime"), type=4)),
                          column(6, withSpinner(plotlyOutput('donutLime'), type=4))),
                 fluidRow(withSpinner(DTOutput('tableLime'), type=4))
                 ),
        tabPanel("Return on investment", 
                 fluidRow(
                   column(6, withSpinner(leafletOutput("mapROI"), type=4)),
                   column(6, withSpinner(plotlyOutput('barROI'), type=4))),
                 fluidRow(withSpinner(DTOutput('tableROI'), type=4))
                )
      )
    )
  )
))


