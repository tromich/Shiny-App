
library(shiny)
library(tidyverse)
library(bslib)
library(raster)
library(rgdal)
library(ggplot2)

ui <- fluidPage(
  #theme setup
  theme = bs_theme(bootswatch="sandstone"),
  #create tabs
    navbarPage("Remote Sensing Data Analyzer",
               tabPanel("1. Description",
                        fluidPage(
                          tags$head(
                            tags$style(
                                HTML("
                                  .diva{
                                    background-color: #3E3F3A;
                                    outline: #3E3F3A solid 20px;
                                    color: #FFFFFF;
                                  }
                                  h1.a{
                                    color: #FFFFFF;
                                    text-decoration-line: underline;
                                  }
                                  h3.a{
                                    color: #FFFFFF;
                                  }
                                  p.a{
                                    color: #FFFFFF;
                                  }
                                     ")
                                )
                            
                          ),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          div(class="diva",
                          h1(class="a","LANDSAT Remote Sensing Data NDVI Analyzer",align="center"),
                          h3(class="a","Trevor Romich",align="center"),
                          p(class="a","This program is designed to: (1) extract LANDSAT 7 and 8 surface reflectance data files, converting them into a 
                            smaller filesize that it can easily work with, and (2) perform NDVI (normalized difference vegetation index) related analysis on compacted files that it
                            produces. Specifically it is capable of calculating average NDVI for each year in the provided data, as well as
                            the difference between summer and winter NDVI for each year with both summer and winter data. Several output figures
                            are provided to help the user interpret the results of the analysis.",align="center")
                          )
                        )
                 
               ),
               tabPanel("2. Instructions",
                        fluidPage(
                          
                          titlePanel("Instructions for use"),
                          hr(),
                          h3("Step 1. Download your data and put it in a single folder"),
                          fluidRow(p("Data MUST BE LANDSAT 7 or 8 data downloaded from EarthExplorer with unaltered filenames. 
                                     The data should be in .tar files, and should be the only contents of the folder.
                                     If you have pre-extracted data (a single .gri file), put this file in the folder instead and proceed
                                     to step 3.")),
                          hr(),
                          h3("Step 2. Extract the data"),
                          fluidRow(p("In the 3rd tab, enter the file path for the folder containing your data, using forward slashes (/).
                                     Additionally, enter the minimum and maximum latitude and longitude coordinates for your area of interest.
                                     (latitude and longitude should be entered for UTM projection, WGS84 datum)")),
                          fluidRow(p(         
                                     "Once you have selected the desired options, press 'extract' and wait.
                                     It may take several minutes or even longer to complete this process, depending on the size of files used.
                                     DO NOT change tabs or click 'extract' again until you see that a NEW .gri and .grd file have been created
                                     in the file path you provided.")),
                          hr(),
                          h3("Step 3. Analyze extracted data"),
                          fluidRow(p("In the 4th tab, enter the file path of the extracted FILE produced in step 2, using forward slashes (/).
                                      This file MUST be a .gri file that was produced by the extraction process in step 2.")),
                          fluidRow(p("Some sample files have been provided in the 'example files prepared' folder available on github.")),
                          fluidRow(p("Then, select the type of analysis you want to perform: annual mean NDVI or NDVI difference summer - winter.
                                     Once you have selected the desired options, press 'calculate' and wait. 
                                     It may take several minutes or even longer to complete this process, depending on the file size used.
                                     DO NOT change tabs or click 'calculate' again until your results have shown up below the button.")),
                          hr(),
                          h4("Data source information"),
                          fluidRow(p("The provided example data are extraction products produced by this program from LANDSAT-7 surface reflectance data.
                                    LANDSAT data is produced and made available by USGS/NASA, and can be accessed via ",a("EarthExplorer",href="https://earthexplorer.usgs.gov/"), ". 
                                    The remote sensing images used were collected on 05/30/2017 for the vicinity of Lake Tahoe in the United States,
                                      and downloaded from EarthExplorer on 09/17/2021."),
                                   p("For more information on how to access LANDSAT data products yourself,
                                     see the USGS webpage on ",a("LANDSAT data access",href="https://www.usgs.gov/landsat-missions/landsat-data-access"),"."))
                          
                        )
                        ),
               
               
               tabPanel("3. Manual Extraction",
                        fluidPage(
                          h3("ONLY USE THIS TAB IF YOU KNOW WHAT YOU ARE DOING!"),
                          fluidRow(column(4,textInput("filepath2",label=h3("File Directory Path"))),
                          column(4,textInput("extractedfilename",label=h3("Name of file to extract to, WITHOUT extension")))
                          ),
                          hr(),

                          #coordinates
                          h3("Enter the latitude and longitude range of interest below:"),
                          fluidRow(column(4,
                          numericInput("xmin",label=h5("Minimum longitude coordinate"),value = 731366),
                          
                          numericInput("xmax",label=h5("Maximum longitude coordinate"),value = 750085)
                          ),column(4,
                          numericInput("ymin",label=h5("Minimum latitude coordinate"),value = 4355675),
                          
                          numericInput("ymax",label=h5("Maximum latitude coordinate"),value = 4375513)
                          )),
                          hr(),
                          #button to execute extraction
                          actionButton("calculate2",label=h3("Extract")),
                          br(),
                          
                          fluidRow(column(8,textOutput("calculate2")))
                          
                        )
                        ),
               tabPanel("4. Analyze Extracted data",
                        fluidPage(
                          fluidRow(
                                  
                            column(4,textInput("filepath",label=h3("Path to target file"))),
                            column(4,selectInput("calccommand", label = h3("Choose Analysis Type"), choices = list("Annual Average NDVI" = 1, "NDVI Difference Summer-Winter" = 2), selected = 1)),
                            #button to execute calculations
                            column(3,offset=1,actionButton("calculate",label=h3("Calculate")))
                            ),
                          helpText("Remember: .gri files only!"),
                          helpText("The path to the target file must be the FULL path WITH the file extension, ex. C:/documents/file.gri"),
                          helpText("Annual Average NDVI: For all years in the dataset, gets the average of all NDVI values in the year for each point."),
                          helpText("NDVI Difference Summer - Winter: For all years in the dataset, gets the average of all Jun/Jul/Aug values at each location and subtracts from them the average of all Jan/Feb/Mar values at the same location."),
                          hr(),
                          
                          #actual outputs
                          h3("View your results here once calculation is finished:"),
                          fluidRow(column(8,textOutput("successfailure"))),
                          hr(),
                          tableOutput("tableoutput"),
                          tableOutput('erroryears'),
                          hr(),
                          fluidRow(column(4,numericInput("yearselect",label="Select the year to view",value=2000)),
                                   column(4,actionButton("updateplot",label = h5("Update Map")))
                          ),
                          h4(textOutput("rasteroutputtitle"),align="center"),
                          plotOutput("rasteroutput"),
                          
                          hr(),
                          h4(textOutput("graphoutputdesc")),
                          plotOutput("graphoutput"),

                          br(),
                          br()
                          
                        )
               ),
             
               
               )
)
  


server <- function(input, output) {
  
  ######START Import and analyze prepared data######
  
  
  output$filepath = renderPrint({input$filepath})
  
  ###TEST
  #test = "12345"
  #output$test = renderText({test})
  
  ####START Functionality for choosing which map to view
  mapstuff = reactiveValues()
  #create dummy values to start us off
  mapstuff$rasters = stack(setValues(raster(nrows=10,ncols =10),0),setValues(raster(nrows=10,ncols =10),2))
  mapstuff$rasteryears = c(2000,2001)
  mapstuff$raster2display = 1
  mapstuff$analysistype = "average annual NDVI"
  #nlayers (rasterstack) lets you see the # of layers
  
  
  #let the user pick
  observeEvent(input$updateplot,{
    #when the user presses the button to update the plot
    
    #get the selected year
    selected_year = as.numeric(isolate(input$yearselect))
    
    
    years_to_choose = isolate(mapstuff$rasteryears)
    
    #attempt to find this year in mapstuff$rasteryears
    selected_year_index = 0
    for (i in 1:length(years_to_choose)){
      if (years_to_choose[i] == selected_year){
        selected_year_index = i
      }
    }
    #if we got an index, make it the raster2display
    
    if (selected_year_index != 0){
      mapstuff$raster2display = selected_year_index
      
      #actually display the plot
      output$rasteroutput = renderPlot({
        plot(mapstuff$rasters[[mapstuff$raster2display]],
             xlab="Longitude (UTM)",ylab="Latitude (UTM)"
        )
      })
      output$rasteroutputtitle = renderText({
        str_c("Plot of ",mapstuff$analysistype," (indicated by color) for the year ",mapstuff$rasteryears[mapstuff$raster2display])
      })
    }
    #otherwise we will need to display an error message
    if (selected_year_index == 0){
      output$rasteroutputtitle = renderText({
        "Please select a valid year!"
      })
      #actually display the plot
      output$rasteroutput = renderPlot({
        plot(0,0)
      })
    }
    
    
  })
  
  
  #####END Functionality for choosing which map to view
  
  
  observeEvent(input$calculate, {
  
    
  
    
    #####START calculate the requested values
    #the program will be fed an option choice for the type of calculation, either:
    #[1]Average Annual NDVI - average annual NDVI over the whole year
    
    #[2]Seasonal NDVI change Summer - Winter - average annual NDVI for JUN/JUL/AUG - average annual NDVI for JAN/FEB/MAR, for each year
    
    cc = renderText({input$calccommand})
    calccommand = as.numeric(cc())
    valid_year_index = 1 #set this because it's used in error checking later, and has to be > 0 for the program to assume no error
    
    
    
    ###Import the data
    #get the file path as a text string from the input
    fp = renderText({input$filepath})
    
    #test to see if we get an error from loading filename
    #this will return a value of our choosing if there is an error, but will successfully execute the command if there is not
    NDVIs2 = tryCatch(stack(fp()),warning = function(w){return("e")},error=function(e){return("e")})
    yearlist = data.frame(matrix(nrow=0,ncol=3))
    
    
    #get the data if it's a valid file name
    #NDVIs2 = stack(fp()) #text in 'calculate the requested values' is copypasted from a work file that uses "NDVIs2" so changing it to that here is faster
    
    #only do the rest if we got a valid filename
    
    #attempting to perform a logical comparison with a raster stack will itself cause an error
    #doing it with a single raster does not seem to though
    #thus there are two possible cases in which we want to execute the code:
      #1. the logical comparison returns an error - which can be tested using tryCatch a 2nd time
      #2. the logical comparison is TRUE, for checking if it does NOT equal "e"
      
      
    
    if (tryCatch((NDVIs2 != "e"),warning=function(w){return("TRUE")},error=function(e){return("TRUE")})){
      #In either case, get the size of the raster stack
      layercount = dim(NDVIs2)[3]
      #generate a rasterstack for the results to be put in
      pNDVI = stack()
      
      #get the number of unique years in the dataset
      layernames = as.data.frame(labels(NDVIs2))
      layernames = rownames_to_column(layernames,var="index")#create an index column
      layernames = mutate(layernames,year=substr(layernames$`labels(NDVIs2)`,2,5)) #create a year column
      layernames = mutate(layernames,month=substr(layernames$`labels(NDVIs2)`,6,7)) #create a month column
      layernames = mutate(layernames,month=substr(layernames$`labels(NDVIs2)`,6,7)) #create a month column
      yearlist = count(layernames,year) #this now is a dataframe with the years of the sample in increasing order in the 'year' column
      yearlist$error = FALSE #create an error message column for each year, defaulting to no error
            
            #if commanded to perform the average annual operation
            if (calccommand==1){
              
              tNDVI = data.frame(matrix(nrow=0,ncol=3))
              colnames(tNDVI)=c("Year","Mean NDVI","SD")  
              for (i in 1:dim(yearlist)[1]){
                ##for each year
                #get the current year we're working with
                year_cur = yearlist$year[i]
                
                ###raster average for the entire year
                #generate a list of just the layernames which have this year
                layers_cur = filter(layernames,year==year_cur)
                
                temp_raster = raster()
                for (i2 in 1:length(layers_cur$index)){ #get all the layers we want
                  temp_raster = addLayer(temp_raster,NDVIs2[[as.numeric(layers_cur$index[i2])]])
                }
                
                
                #average them - removing NAs
                #put this in pNDVI
                pNDVI = addLayer(pNDVI,mean(temp_raster,na.rm=TRUE))
                
                ###average across space
                #get all data points for the year
                all_data_pts_yearly = as.vector(temp_raster)[!is.na(as.vector(temp_raster))]
                #get mean for the year
                tNDVI[i,] = c(as.numeric(year_cur),mean(all_data_pts_yearly),sd(all_data_pts_yearly))
                
              }##end for loop
              
              #give pNDVI the appropriate layer names
              names(pNDVI) =  yearlist$year 
              
              plotyaxislabel = c("Mean NDVI - Annual")
              
              ##generate summary statistics
              #mean NDVI over all years
              overall_mean=mean(as.vector(NDVIs2),na.rm=TRUE)
              overall_sd=sd(as.vector(NDVIs2),na.rm=TRUE)
              #start date and end date
              #start date is the first value in layernames
              startdate = substr(layernames$`labels(NDVIs2)`[1],2,9)
              enddate = substr(layernames$`labels(NDVIs2)`[(length(layernames$`labels(NDVIs2)`))],2,9)
              summarytbl = data.frame(matrix(nrow=1,ncol=4))
              colnames(summarytbl)=c("Start Year","End Year","Mean across all years and site","Standard deviation")
              summarytbl[1,1]=substr(startdate,1,4)
              summarytbl[1,2]=substr(enddate,1,4)
              summarytbl[1,3]=overall_mean
              summarytbl[1,4]=overall_sd
              
              ###TEST
              #test = startdate
              #output$test = renderText({test})
            }
            
            
            #if commanded to perform the seasonal difference operation
            if (calccommand == 2){
              #create the error years table
              #get the number of unique years in the dataset
              valid_years = data.frame(matrix(nrow=0,ncol=1))
              valid_year_index = 0 #this is the method where we care about this, there may be some years that do not have valid data
              ##for each year
              for (i in 1:dim(yearlist)[1]){
                #get the current year we're working with
                year_cur = yearlist$year[i]
                
                #value for pNDVI is (summer average) - (winter average) for the year
                
                layers_cur_winter = data.frame(matrix(nrow=0,ncol=4))
                layers_cur_summer = data.frame(matrix(nrow=0,ncol=4))
                #get the list of layers with this year and month = 1,2, or 3
                layers_cur_winter = filter(layernames,year==year_cur & (month=="01"|month=="02"|month=="03"))
                #same, but for summer months (6,7,8)
                layers_cur_summer = filter(layernames,year==year_cur & (month=="06"|month=="07"|month=="08"))
                #check the size of these two layers, if either is zero then we cannot calculate for this year and we return an error
                if (dim(layers_cur_winter)[1]==0|dim(layers_cur_summer)[1]==0){
                  yearlist$error[i]=TRUE
                }
                
                #unless we added an error, continue:
                if (yearlist$error[i]==FALSE){
                  #count this as a valid year
                  valid_year_index = valid_year_index+1
                  valid_years[valid_year_index,] = year_cur
                  
                  
                  #for the current year, get the average for summer and the average for winter and then subtract
                  #winter
                  temp_raster_winter = raster()
                  temp2_raster_winter=raster()
                  for (i2 in 1:length(layers_cur_winter$index)){ #get all the layers we want
                    temp_raster_winter = addLayer(temp_raster_winter,NDVIs2[[as.numeric(layers_cur_winter$index[i2])]])
                  }
                  temp2_raster_winter = mean(temp_raster_winter,na.rm=TRUE)
                  #summer
                  temp_raster_summer = raster()
                  temp2_raster_summer = raster()
                  for (i2 in 1:length(layers_cur_summer$index)){ #get all the layers we want
                    temp_raster_summer = addLayer(temp_raster_summer,NDVIs2[[as.numeric(layers_cur_summer$index[i2])]])
                  }
                  temp2_raster_summer = mean(temp_raster_summer,na.rm=TRUE)
                  #subtract
                  pNDVI = addLayer(pNDVI,(temp2_raster_summer-temp2_raster_winter))
                  
                  #give it the name of the current year
                  names(pNDVI[[valid_year_index]])=year_cur
                  
                  
                }
                
                
                #put this in tNDVI, format should be column 1 = year, column 2 = average NDVI
                
                
                
              }##end for loop
              
              
              ###only do this stuff if there are values in pNDVI
              #if we want the average seasonal difference in NDVI over the entire area,
              #it doesn't make sense to dissociate points from their location before we take the difference
              #so this time we should just take the average of each layer in pNDVI
              
              if(valid_year_index>0){
                tNDVI = data.frame(matrix(nrow=valid_year_index,ncol=3))
                colnames(tNDVI)=c("Year","Mean NDVI","SD")
                tNDVI$Year=valid_years[1:valid_year_index,]
                tNDVI$`Mean NDVI`[1:valid_year_index]=cellStats(pNDVI,stat='mean',na.rm=TRUE)
                tNDVI$SD[1:valid_year_index]=cellStats(pNDVI,stat='sd',na.rm=TRUE)
                plotyaxislabel = c("Mean NDVI Difference Summer - Winter")
                
                
                ##generate summary statistics
                #mean NDVI change over all years THAT HAD SUMMER AND WINTER DATA
                #pNDVI contains all NDVI change calcs for each point, at each year where there was sufficient data
                overall_mean=mean(as.vector(pNDVI),na.rm=TRUE)
                overall_sd=sd(as.vector(pNDVI),na.rm=TRUE)
                #start date and end date
                #get them from valid_years
                startdate = valid_years[1,]
                enddate = valid_years[dim(valid_years)[1],]
                summarytbl = data.frame(matrix(nrow=1,ncol=4))
                colnames(summarytbl)=c("Start Year","End Year","Mean across all years and site","Standard deviation")
                summarytbl[1,1]=substr(startdate,1,4)
                summarytbl[1,2]=substr(enddate,1,4)
                summarytbl[1,3]=overall_mean
                summarytbl[1,4]=overall_sd
                
                
                ###TEST
                #test = startdate
                #output$test = renderText({test})
              }
              
            }
    }
    
    
    #####END calculate the requested values
    
    #####START Display the three outputs
    
    ###check for errors:
    
    #is there no data at all?
    
    #if so, return empty raster for rasteroutput, empty plot for graphoutput, and empty table for tableoutput + erroryears
    if((dim(yearlist)[1]==0) | (valid_year_index == 0)){
      output$successfailure = renderText({c("Error - insufficient data to produce results for even one year!")})  
      names(yearlist) = c("Year","# satellite images","Excluded due to error?")
      output$erroryears = renderTable({yearlist}) #display the empty table
      
      
      #give the mapstuff the right display values
      mapstuff$rasters = setValues(raster(nrows=10,ncols =10),0) #just display a blank raster
      
      
      ###timeseries of results
      output$graphoutput = renderPlot({
        ggplot()+theme_minimal() #just display a blank plot
      })
      summarytbl = data.frame(matrix(nrow=1,ncol=4))
      colnames(summarytbl)=c("Start Year","End Year","Mean across all years and site","Standard deviation")
      output$tableoutput = renderTable({summarytbl})
    }
    
    
    if((dim(yearlist)[1]!=0) & (valid_year_index !=0)){
    #if not, continue as usual
    output$successfailure = renderText({c("Analysis Successful! Scroll down to view the rest of your results.")})  
      
    ##were there any years excluded?
    #display the yearlist table, which includes this information
    names(yearlist) = c("Year","# satellite images","Excluded due to error?") #rename the columns to be what we want
    output$erroryears = renderTable({yearlist})
    
    
    ###raster map viewer
    mapstuff$rasters=pNDVI #give it all of them
    #also give it the list of years
    if(calccommand==1){
      mapstuff$rasteryears = as.vector(yearlist$Year)
      atyp="annual average NDVI"
      mapstuff$analysistype = atyp
    }
    if(calccommand==2){
      mapstuff$rasteryears = as.vector(tNDVI$Year)
      atyp="summer - winter NDVI difference"
      mapstuff$analysistype = atyp
    }
    
    
    ###timeseries of results
    #this should be a 'simple' ggplot of the calculated value vs year, from tNDVI
    #it might be nice to include error bars of +/- 1 SD, since I have that data
    output$graphoutput = renderPlot({
      ggplot(data=tNDVI,aes(x=Year,y=`Mean NDVI`))+geom_point(size = 8,shape="x")+theme_minimal()+
        theme(axis.title=element_text(size=15),axis.text = element_text(size=12))+
        labs(y=plotyaxislabel)
    })
    output$graphoutputdesc = renderText({
      str_c("Plot of ",atyp," for each year with provided data")
    })
    
    output$tableoutput = renderTable({summarytbl})
    #####END Display the three outputs
    }
    
  })
  
  
  ######END Import and analyze prepared data######
  
  ######START inputs from manual settings page######
  output$filepath2 = renderPrint({input$filepath2})
  
  observeEvent(input$calculate2, {
    #get the filepath 
    target_directory = renderText(input$filepath2)
    target_directory = target_directory()
    
    #get the output filename
    fn = renderText(input$extractedfilename)
    fn = fn()
    #use a default filename if the user didn't provide one
    if (length(fn)==0){
      fn = c("outputfile")
    }
    
    xmn=renderText({input$xmin})
    
    xmx=renderText({input$xmax})
    
    ymn=renderText({input$ymin})
    
    ymx=renderText({input$ymax})
    
      
    
    #for testing purposes we manually define the 'subset area' - geographic coordinates that bound the zone we actually care about within our data
    #currently this is APPROXIMATELY in the location I would use for my research project
    subset_area = extent(c(as.numeric(xmn()),as.numeric(xmx()),as.numeric(ymn()),as.numeric(ymx())))
    
    
    ##check for validity of target directory using tryCatch
      #we should have:
        #setwd works properly [it is an actual directory]
        etest1=tryCatch(setwd(target_directory),warning=function(w){return("e")},error=function(e){return("e")}) #this will return the target directory to etest1 if it works, otherwise "e"
    #only continue if etest1 != e
        
                
    if (etest1 != "e"){
    
    #change directory to [target directory]
    setwd(target_directory)
    
    #get the file name list
    #only get files for the correct geographic area (and ignore folders)
    filenames = list.files(pattern=glob2rx("*_??????_*.tar$")) #there are a couple of dupes, don't worry about them - should be pruned by the person inputting
    
    #only continue from here if the file list has length > 0
    if (length(filenames) > 0){
    
    #create the rasterstack to hold the NDVI for each timestamp
    NDVIs2 = stack()
    dates = data.frame(matrix(nrow=1,ncol=1))
    ###loop for all files in the namelist
    for (i in 1:length(filenames)){
      #in the target directory:
      #get the [i] name in the file names list
      targetfile = filenames[i]
      
      #untar the file to a temporary folder for working with the contents and move to that folder
      untar(targetfile,exdir="temp")
      setwd("temp")
      
      #how we handle it is different depending on if target file is landsat 7 or landsat 8:
      if (substr(targetfile,3,4)=="07"){ #if it's landsat 7 data
        #for landsat 7, NDVI is band4-band3/(band4+band3) so we want band3 and band4 files
        
        #example filename for extracted files: LE07_L2SP_043033_20170428_20200831_02_T1_SR_B1.tif
        #the "B1 at the end is telling us it's band 1
        #this should occur in characters 45 and 46 of the filename
        
        #load the file with "B3" as characters 45 and 46
        band3 = raster(list.files(pattern=glob2rx("*B3.TIF$"),full.names=TRUE))
        
        #get the date info from the name - the first date is when it was actually measured
        measdate = substr(list.files(pattern=glob2rx("*B3.TIF$"),full.names=TRUE),20,27)
        dates[i]=measdate
        
        #load the file with "B4" as characters 45 and 46
        band4 = raster(list.files(pattern=glob2rx("*B4.TIF$"),full.names=TRUE))
        
        #calculate the NDVI
        NDVI = (band4-band3)/(band4+band3)
        
        #get only the [subset area] that we care about
        NDVI = crop(NDVI,subset_area)
        
      }
      if (substr(targetfile,3,4)=="08") { #if it's landsat 8 data
        #for landsat 8, NDVI is band5-band4/(band5+band4) so we want band5 and band4 files
        
        
        #example filename for extracted files: LC08_L2SP_042033_20130322_20200913_02_T1_SR_B4.tif
        #the "B4 at the end is telling us it's band 4
        #this should occur in characters 45 and 46 of the filename
        
        #load the file with "B4" as characters 45 and 46
        band4 = raster(list.files(pattern=glob2rx("*B4.TIF$"),full.names=TRUE))
        
        
        #get the date info from the name - the first date is when it was actually measured
        measdate = substr(list.files(pattern=glob2rx("*B4.TIF$"),full.names=TRUE),20,27)
        dates[i]=measdate
        #load the file with "B5" as characters 45 and 46
        band5 = raster(list.files(pattern=glob2rx("*B5.TIF$"),full.names=TRUE))
        
        #calculate the NDVI
        NDVI = (band5-band4)/(band5+band4)
        
        #get only the [subset area] that we care about
        NDVI = crop(NDVI,subset_area)
        
        
      }
      
      #append the obtained NDVI value to the stack
      
      NDVIs2 = addLayer(NDVIs2,NDVI)
      
      #destroy all files in temp folder completely - this is somewhat scary to use since it irreversibly removes the target files, but the way it's written it should be only removing the files in the currently set working directory
      file.remove(list.files())
      
      #return to target directory
      setwd(target_directory)
    }
    #name the layers appropriately:
    names(NDVIs2) = dates
    #export the results so that they can be loaded more quickly
    writeRaster(NDVIs2,fn,format='raster',bylayer=FALSE) #this works, more or less - there is error between the result when imported and the pre-export object, but it's on the order of 10^-8, so should be acceptable (and probably unavoidable)
    ###
    #Inform the user of the extraction success
    output$calculate2 = renderText({c("Extraction Successful!")})
    
    }
    
      #if it didn't work we tell the user that it didn't
      if(length(filenames)==0){
        output$calculate2 = renderText({c("Could not extract - no valid files in destination!")})
      }
    }
      #if it didn't work we tell the user that it didn't
      if(etest1=="e"){
        output$calculate2 = renderText({c("Could not extract - not a valid folder path!")})
      } 
    })
  ######END inputs from manual extraction######
  
}

# Run the application 
shinyApp(ui = ui, server = server)
