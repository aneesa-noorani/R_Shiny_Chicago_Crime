#installing packages
if(!require('pacman'))install.packages('pacman')
pacman::p_load(readr, readxl, ggplot2, plotly, shiny, shinythemes, forcats, lubridate, 
               tidyr, dplyr, plyr, leaflet, htmltools, stringr)

crimes_2018 <- read_csv("Chicago_Crimes_2018.csv")

##---------------------------------------------------------------##

        #Tab 1: Data Preprocessing
#cutting data set down to just the columns we need
crimes_tab1 <- crimes_2018 %>% select(Date, `Primary Type`)

#checking to see if it has any missing data
sum(is.na.data.frame(crimes_tab1))
    #no missing data

    #parsing out month from Date column, & setting as integer
crimes_tab1$Date <- as.integer(substr(crimes_tab1$Date, start = 1, stop = 2))
    #renaming
names(crimes_tab1)[1] <- "Month"

#creating new dataframe that gives freq of crime, by month. this is what we will create the bar plot from
crimes_tab1_freq <- ddply(crimes_tab1, .(crimes_tab1$Month, crimes_tab1$`Primary Type`), nrow)

#renaming column names
names(crimes_tab1_freq) <- c("Month", "Primary Type", "Freq")

#converting month #s to proper names
crimes_tab1_freq$Month <- month.abb[crimes_tab1_freq$Month]

#list of unique crime types. also converting to normal case
unique_crime_types <- unique(str_to_sentence(unlist(crimes_tab1[,"Primary Type"])))

##---------------------------------------------------------------##
                #Tab 2: Data Preprocessing

    #cutting data set down to just the columns we need
crimes_tab2 <- crimes_2018 %>% select('ID', 'Location Description')
    #remove rows with missing data
crimes_tab2 <- na.omit(crimes_tab2)
    #converting 'Location Description' column into factors
crimes_tab2$`Location Description` <- as.factor(crimes_tab2$`Location Description`)

#length(levels(crimes_tab2$'Location Description'))

    #combining categories. starting out with 132 categories.
crimes_tab2$`Location Description` <- fct_collapse(crimes_tab2$`Location Description`,
'Airport/Aircraft' = c("AIRCRAFT", "AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA", 
    "AIRPORT BUILDING NON-TERMINAL - SECURE AREA", "AIRPORT EXTERIOR - NON-SECURE AREA", 
    "AIRPORT EXTERIOR - SECURE AREA", "AIRPORT PARKING LOT", "AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA",
    "AIRPORT TERMINAL LOWER LEVEL - SECURE AREA", "AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA", 
    "AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA", "AIRPORT TERMINAL UPPER LEVEL - SECURE AREA", 
    "AIRPORT TRANSPORTATION SYSTEM (ATS)", "AIRPORT VENDING ESTABLISHMENT", "AIRPORT/AIRCRAFT"),
'Animal Site' = c('ANIMAL HOSPITAL', 'HORSE STABLE', 'KENNEL'),
'Bank/Financial Site' = c('ATM (AUTOMATIC TELLER MACHINE)', 'BANK', 'SAVINGS AND LOAN', 
    'CREDIT UNION', 'CURRENCY EXCHANGE', 'COIN OPERATED MACHINE', 'PAWN SHOP'),
'Bar/Liquor Store' = c('BAR OR TAVERN', 'LIQUOR STORE', 'TAVERN/LIQUOR STORE'),
'Barber' = c('BARBER SHOP/BEAUTY SALON', 'BARBERSHOP'),
'CHA' = c('CHA APARTMENT', 'CHA GROUNDS', 'CHA HALLWAY/STAIRWELL/ELEVATOR', 'CHA PARKING LOT', 'CHA PARKING LOT/GROUNDS'),
'College' = c('COLLEGE/UNIVERSITY GROUNDS', 'COLLEGE/UNIVERSITY RESIDENCE HALL'),
'CTA' = c('CTA BUS', 'CTA BUS STOP', 'CTA GARAGE / OTHER PROPERTY', 'CTA PLATFORM', 'CTA PROPERTY', 'CTA STATION', 
          'CTA TRACKS - RIGHT OF WAY', 'CTA TRAIN'),
'Forest' = c('FOREST PRESERVE', 'WOODED AREA'),
'Garage' = c('GARAGE', 'GARAGE/AUTO REPAIR'),
'Gas Station' = c('GAS STATION', 'GAS STATION DRIVE/PROP.'),
'Govt Bldg' = c('GOVERNMENT BUILDING', 'GOVERNMENT BUILDING/PROPERTY', 'FEDERAL BUILDING'),
'Hospital/Health Site' = c('HOSPITAL', 'HOSPITAL BUILDING/GROUNDS', 'MEDICAL/DENTAL OFFICE', 'NURSING HOME/RETIREMENT HOME'),
'Lake' = c('BOAT/WATERCRAFT', 'RIVER BANK', 'LAKEFRONT/WATERFRONT/RIVERBANK'),
'Law Enforcement' = c('JAIL / LOCK-UP FACILITY', 'POLICE FACILITY/VEH PARKING LOT'),
'Hotel' = c('HOTEL', 'HOTEL/MOTEL'),
'Office' = c('COMMERCIAL / BUSINESS OFFICE', 'OFFICE'),
'Parking Lot' = c('PARKING LOT', 'PARKING LOT/GARAGE(NON.RESID.)'),
'Residence' = c('HOUSE', 'RESIDENCE', 'RESIDENCE PORCH/HALLWAY', 'RESIDENCE-GARAGE', 'RESIDENTIAL YARD (FRONT/BACK)', 
    'PORCH', 'YARD', 'DRIVEWAY', 'DRIVEWAY - RESIDENTIAL', 'POOL ROOM'),
'Retail' = c('APPLIANCE STORE', 'DEPARTMENT STORE', 'CLEANING STORE', 'DRUG STORE', 'RETAIL STORE', 'SMALL RETAIL STORE'),
'Road' = c('BRIDGE', 'HIGHWAY/EXPRESSWAY', 'STREET'),
'School' = c('SCHOOL, PRIVATE, BUILDING', 'SCHOOL, PRIVATE, GROUNDS', 'SCHOOL, PUBLIC, BUILDING', 'SCHOOL, PUBLIC, GROUNDS'),
'Vacant Lot' = c('VACANT LOT', 'VACANT LOT/LAND'),
'Vehicle/Auto Site' = c('AUTO', 'TAXICAB', 'VEHICLE - DELIVERY TRUCK', 
    'VEHICLE - OTHER RIDE SHARE SERVICE (E.G., UBER, LYFT)','VEHICLE NON-COMMERCIAL', 'VEHICLE-COMMERCIAL', 
    'VEHICLE-COMMERCIAL - ENTERTAINMENT/PARTY BUS', 'VEHICLE-COMMERCIAL - TROLLEY BUS', 'CAR WASH', 
     'AUTO / BOAT / RV DEALERSHIP'), 
'Youth Site' = c('DAY CARE CENTER', 'YMCA')
)

length(unique(crimes_tab2$'Location Description'))

#normal case
crimes_tab2["Location Description"] <- str_to_sentence(crimes_tab2$`Location Description`)

#list of unique crime locations
unique_crime_locations <- unique((unlist(crimes_tab2[,"Location Description"])))

#creating new dataframe that gives crime freq, by location. this is what we will create the bar plot from
crimes_tab2_freq <- count(crimes_tab2$`Location Description`)
names(crimes_tab2_freq) <- c("Location", "Freq")
#View(crimes_tab2_freq)

##---------------------------------------------------------------##
            #Tab 3: Data Preprocessing

    #cutting data set down to just the columns we need
crimes_tab3 <- crimes_2018 %>% select(Date, `Primary Type`)

    #checking to see if it has any missing data
sum(is.na.data.frame(crimes_tab3))
#no missing data

    #converting date column to actual date type
crimes_tab3$Date <- mdy_hms(crimes_tab3$Date)

    #parsing out hour from Date column, & setting as integer
crimes_tab3$Hour <- as.integer(substr(crimes_tab3$Date, start = 12, stop = 13))

    #creating new dataframe that gives freq of crime, by month
crimes_tab3_freq <- ddply(crimes_tab3, .(crimes_tab3$Hour, crimes_tab3$`Primary Type`), nrow)

    #renaming column names
names(crimes_tab3_freq) <- c("Hour", "Type", "Freq")

    #converting to factor
crimes_tab3_freq$Type <- as.factor(crimes_tab3_freq$Type)

##---------------------------------------------------------------##
            #Tab 4: Data Preprocessing

    #cutting data set down to just the columns we need
crimes_tab4 <- crimes_2018 %>% select(ID, Date, `Primary Type`, Description, `Location Description`, Latitude, Longitude)

    #omit only those columns with missing latitude or longitude, bc we won't be able to map those points
crimes_tab4 <- crimes_tab4 %>% drop_na(Latitude, Longitude)

    #renaming certain columns to make them more descriptive
names(crimes_tab4)[3:5] <- c("Crime Type", "Crime Description", "Location Description")

    #renaming certain columns to make them more descriptive
names(crimes_tab4)[3:5] <- c("Crime Type", "Crime Description", "Location Description")

##---------------------------------------------------------------##
# Define UI for application that draws a histogram
ui <- fluidPage(

navbarPage(title = "Shiny Assignment 2", theme = shinytheme("cerulean"),

#first tab
    tabPanel("Crime Frequency by Month & Crime Type",
                #first input - month
            selectInput(inputId = "crime_month",
            label = strong("Which month would you like to see crime data for?"),
            choices = crimes_tab1_freq$Month, selected = 1),
                #second input - crime type. allows multiple inputs!
            selectInput(inputId = "crime_type", multiple = TRUE,
            label = strong("Which crime type would you like to see data for?"),
            choices = unique_crime_types,
            selected = unique_crime_types),
        plotlyOutput(outputId = "barplot1", height = "800px", width = "1200px")
    ),  #end of Tab 1
    
#second tab
    tabPanel("Crime Frequency by Location",
         selectInput(inputId = "crime_location", multiple = TRUE,
                     label = strong("Which crime location would you like to see data for?"),
                     choices = unique_crime_locations,
                     selected = c("Apartment", "Residence", "Road", "Sidewalk", "Retail")),
         plotlyOutput("barplot2", height = "800px", width = "1200px")
    ), #end of 2nd tab

#third tab
    tabPanel("Heat Map: Crime Type vs. Hour",
             plotlyOutput(outputId = "heatmap", height = "800px", width = "1200px")
    ), #end of Tab 3
    
#fourth tab
tabPanel("Geographical Map", 
         dateRangeInput(
             inputId = "dateRange",
             label = "Select data range",
             start = "2018-01-01",
             end = "2018-12-31",
             format = "yyyy/mm/dd",
             separator = "-"
         ),
         leafletOutput(outputId = "geomap", height = "1200px", width = "1200px")
        ) #end of Tab 4
    ) #end of navbar page
) #end of ui / fluid page


##---------------------------------------------------------------##
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #TAB 1 OUTPUT
output$"barplot1" <- renderPlotly({
    
    #selecting crime types for the selected month from frequency data set
selected_crimes_tab1_freq <- filter(crimes_tab1_freq, Month == input$crime_month)
    #excluding month column at this point, so we only have the columns we need
selected_crimes_tab1_freq <- selected_crimes_tab1_freq[,2:3]
    #converting crime type list to normal case
selected_crimes_tab1_freq$`Primary Type` <- str_to_sentence(selected_crimes_tab1_freq$`Primary Type`)
    #filtering primary type. We're saying: in this data frame, only select the Primary Types that have been selected from input
crimefilter <- selected_crimes_tab1_freq[selected_crimes_tab1_freq$`Primary Type` %in% input$crime_type,]
    
    #defining sequence for y-axis ticks
yaxis_ticks <- seq(0,6600, 300)
        
    p <- ggplot(crimefilter,
                #plotting in descending order
                mapping = aes(x = reorder(crimefilter$`Primary Type`, crimefilter$Freq), y = crimefilter$Freq,
                              group = 1,
                              #text: defines what displays when you hover
                              text = paste0("Crime Type: ", crimefilter$`Primary Type`, "<br>", 
                                            "Count: ", crimefilter$Freq)
                    ) #end of aes
            ) + #end of ggplot
        coord_flip() + #flipping axes so graph is more readable
        geom_bar(stat = "identity", fill= "steelblue", width = 0.5) + theme_minimal() +
        labs(title = paste("Crime Frequency in ", input$crime_month, "\n by Crime Type", sep=""),
             caption = "Counts are aggregated across all years") +
        theme(plot.title = element_text(hjust = 0.5 ,color = "black", size = 14, face = "italic"),
              plot.caption = element_text(size = 11 ,face = "italic")) +
        xlab("Crime Type") + ylab("Crime Frequency") +
        scale_y_continuous(breaks = yaxis_ticks)
        
        #parse down ggplot from above down to only include 'text' as hover
        p %>% ggplotly(tooltip = "text") %>% plotly_build()
        
}) #end of Tab 1 output - barplot1

##---------------------------------------------------------------##

    #TAB 2 OUTPUT
output$"barplot2" <- renderPlotly({
    
    #selecting crime types for the selected month from frequency data set
    locationfilter <- filter(crimes_tab2_freq, Location %in% input$crime_location)
    
    #defining sequence for y-axis ticks
    yaxis_ticks <- seq(0,60000, 5000)
    
    p2 <- ggplot(data = locationfilter,
                 #plotting in descending order
                 mapping = aes(x = reorder(locationfilter$Location, locationfilter$Freq), y = locationfilter$Freq,
                               #group = 1,
                               #text: defines what displays when you hover
                               text = paste0("Crime Location: ", locationfilter$Location, "<br>", 
                                             "Count: ", locationfilter$Freq)
                 ) #end of aes
    ) +
        theme_gray() +
        coord_flip() + #flipping axes so graph is more readable
        geom_bar(stat = "identity", fill= "steelblue", width = 0.5) + theme_minimal() + 
        labs(title = paste("Crime Frequency by Crime Location", sep=""),
             caption = "Counts are aggregated across all years") +
        theme(plot.title = element_text(hjust = 0.5 ,color = "black", size = 14, face = "italic"),
              plot.caption = element_text(size = 11 ,face = "italic")) +
        xlab("Crime Location") + ylab("Crime Frequency") +
        scale_y_continuous(breaks = c(seq(0,60000, 5000)))
    
    
    #parse down ggplot from above down to only include 'text' as hover
    p2 %>% ggplotly(tooltip = "text") %>% plotly_build()
    
}) #end of Tab 2 output

##---------------------------------------------------------------##

    #TAB 3 OUTPUT
output$"heatmap" <- renderPlotly({
    
    p3 <- ggplot(crimes_tab3_freq, 
                 aes(x=crimes_tab3_freq$Hour, 
                     y=reorder(crimes_tab3_freq$Type, crimes_tab3_freq$Freq), 
                     fill=crimes_tab3_freq$Freq,
                     text = paste0("Crime Type: ", crimes_tab3_freq$Type, "<br>",
                                   "Hour: ", crimes_tab3_freq$Hour, "<br>",
                                   "Count: ", crimes_tab3_freq$Freq)
                 ) #end of aes
    ) + #end of ggplot
        geom_tile() +
        theme(plot.title = element_text(hjust = 0.5 ,color = "black", size = 14, face = "italic"),
              text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
        scale_fill_gradientn(colours = heat.colors(60, alpha = 0.5)) +
        xlab("Hour of the Day") + ylab("Crime Type") +
        labs(title = "Crime Frequency by Hour of the Day",
             fill = "Freq") +
        scale_x_continuous(breaks = c(seq(0,23,2)))
    
    p3 %>% ggplotly(tooltip = "text") %>% plotly_build()
    
}) #end of Tab 3 output - heatmap

##---------------------------------------------------------------##

    #TAB 4 OUTPUT
output$"geomap" <- renderLeaflet({
    
    #converting Date column to actual date format
crimes_tab4$Date <- mdy_hms(crimes_tab4$Date)

    #for computer memory purposes, I only plotted 1000 observations
crimes_tab4 <- sample_n(crimes_tab4, 1000)
    
    #creating new data set based on filtered dates
crimes_tab4_filtered = subset(crimes_tab4, 
    trunc(crimes_tab4$Date, unit="days") >= input$dateRange[1] & trunc(crimes_tab4$Date, unit="days") <= input$dateRange[2])
    
    labels1 <- paste('<p>', "<strong>Date/Time:", crimes_tab4_filtered$Date, '</p><p>',
                     "<strong>Type:", crimes_tab4_filtered$`Crime Type`, '</p><p>',
                     "<strong>Crime Description:", crimes_tab4_filtered$`Crime Description`, '</p><p>',
                     "<strong>Location Description:", crimes_tab4_filtered$`Location Description`, '</p><p>') #end of paste
    
    leaflet(data = crimes_tab4_filtered) %>% 
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
        addMarkers(lng = crimes_tab4_filtered$Longitude, 
                   lat = crimes_tab4_filtered$Latitude,
                   label = lapply(labels1, htmltools::HTML),
                   labelOptions = labelOptions(noHide = F)) %>% #end of addMarkers
        setView(lng = -87.7, lat = 41.8, zoom = 10)    

}) #end of Tab 4 output - geomap


} #end of server fxn

# Run the application 
shinyApp(ui = ui, server = server)
