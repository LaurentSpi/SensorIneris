##=================================
## SensorsIneris data visualisation
##=================================
##
## Before using this script, you must name your working dataframe as Global.df and you must fill the SensorIneris_plot_metadata.csv.
##
##
##===============================================

## List of packages to install
Packages <- c("openair")

for(i in Packages) {
  
  ## Installing packages
  if(i %in% rownames(installed.packages()) == FALSE) {
    print(sprintf("Installing %s", 1),quote = FALSE)
    install.packages(i)
  } else {
    print(sprintf("Package %s already installed",i),quote = FALSE)
    
  }
  #print(i,quote=FALSE)
  do.call("library", as.list(i))
  #library(i, character.only = TRUE)
  print(sprintf("Package %s loaded",i),quote = FALSE)
  
}

## Source SensorToolBox
source(choose.files(caption = "Select SensorIneris_Toolbox file"))

#################################
## Load Files and initial data ##
#################################
WD <- getwd()
if(!dir.exists(file.path(WD, "Data treatment"))){dir.create(file.path(WD, "Data treatment"))}
Eval_WD <- file.path(WD, "Data treatment")

##################################
## Timeseries and correlation plot
##################################
## Load the SensorIneris_plot_metadata.csv file which contains all the needed info for the timeplot
Metadata.file <- choose.files(caption = "Select SensorIneris_plot_metadata.csv files")
print(paste0("Importing Metadata from ", Metadata.file))
Metadata <- read.csv(Metadata.file, sep = ";", colClasses = "character", na.strings = "", header = TRUE)

## Define size of the output graphs
WidthTimeplot <- 20
HeightTimeplot <- 18
WidthEtalonnage <- 20
HeightEtalonnage <- 22

## Prepare output dataframe
Cal_Model.df <- data.frame(Folder = character(), Sensor = character(), Reference = character(), Intercept = double(), Slope = double(), RSquare = double()
                                 , DevStd = double(), MAPE = double(), inf.MAPE = double(), stringsAsFactors = FALSE)

## Timeseries and correlation plot
for (i in 1:nrow(Metadata)) {
  
  Sensors <- unlist(strsplit(Metadata[i,"Sensors"], ",", fixed = TRUE))
  if(is.na(Metadata[i,"Y_axe_names"])){
    Code <- Sensors
  } else {
    Code <- unlist(strsplit(Metadata[i,"Y_axe_names"], ",", fixed = TRUE))
  }
  Pollutant <- c(Metadata[i,"Reference"])
  if(length(Sensors) == 1){
    Name.code <- c(Pollutant, Code)
    Pollutant <- c(Pollutant, Sensors)
  } else {
    for(j in 1:length(Sensors)){
      if(j == 1){
        Name.code <- c(Pollutant, Code[j])
      } else {
        Name.code <- c(Name.code, Code[j])
      }
      Pollutant <- c(Pollutant, Sensors[j])
    }
  }
  
  if(is.na(Metadata[i,"TimePlot_Name"])){
    TimePlotName <- paste0(Metadata[i,"Folder_Name"],"_",Metadata[i,"Reference"])
  } else {
    TimePlotName <- Metadata[i,"TimePlot_Name"]
  }
  
  
  print(paste0("Data treatment for:"))
  print(paste0(Sensors))
  print(paste0("compared to ", Pollutant[1]))
  
  if(length(Sensors) == 1){Couleur <- c("red","black")}
  if(length(Sensors) == 2){Couleur <- c("red","black","blue")}
  if(length(Sensors) == 3){Couleur <- c("red","black","blue","purple")}
  if(length(Sensors) == 4){Couleur <- c("red","black","blue","purple","green4")}
  if(length(Sensors) == 5){Couleur <- c("red","black","blue","purple","green4","gold")}
  if(length(Sensors) == 6){Couleur <- c("red","black","blue","purple","green4","gold","pink4")}
  
  ## Set the date format based on the period time length >= 7 days  
  if(difftime(as.POSIXct(Metadata[i,"End_date"],format = "%d/%m/%Y %H:%M"), as.POSIXct(Metadata[i,"Start_date"],format = "%d/%m/%Y %H:%M")) >= 7){
    timeplot.date.format <- "%d%b"
  } else {
    timeplot.date.format <- "%d%b%Hh"
  }
  
  ## subset the desired time period from the global dataframe
  temp.df <- subset(Global.df, Global.df$date >= as.POSIXct(Metadata[i,"Start_date"],format = "%d/%m/%Y %H:%M")
                          & Global.df$date <= as.POSIXct(Metadata[i,"End_date"],format = "%d/%m/%Y %H:%M"))
  
  ## replace NaN with NA in the subset database
  temp.df[is.nan.data.frame(temp.df)] <- NA
  
  ## set the output folder
  if(!dir.exists(file.path(Eval_WD,Metadata[i,"Folder_Name"]))){dir.create(file.path(Eval_WD,Metadata[i,"Folder_Name"]))}
  Print_WD <- file.path(Eval_WD,Metadata[i,"Folder_Name"])
  
  ## timeplots
  print(paste0("Plotting time plot for ", Metadata[i,"Reference"]," against ",paste0(Sensors, collapse = ", ")))
  timePlot(mydata = temp.df
           , pollutant = Pollutant
           , plot.type = "l"
           , lwd = 1.5
           , group = FALSE
           , main = ""
           , ylab = ""
           , name.pol = Name.code
           , auto.text = FALSE
           , date.format = timeplot.date.format
           , cols = Couleur
           , key = TRUE
           , key.columns = 2
           , key.position = "top"
           , y.relation = "same")
  
  dev.copy(png, filename = file.path(Print_WD, paste0("Timeplot_", TimePlotName,".png"))
           , units = "cm", res = 1024, width = WidthTimeplot, height = HeightTimeplot)
  dev.off()
  print(paste0("Time plot saved in ", Metadata[i,"Folder_Name"], " folder"))
  
  for (s in 1:length(Sensors)) {
    ## Check if there is data to plot
    if(!any(!is.na(temp.df[, Sensors[s]]))){
      print(paste0(Sensors[s], " has no data to plot"))
      next
    }
    
    
    ## Correlation based on 10 minutes data
    print(paste0("Plotting Correlation graph for ", Sensors[s]))
    Limit.XY <- Etalonnage(x = temp.df[, Pollutant[1]]
                                 , s_x = NULL
                                 , y = temp.df[, Sensors[s]]
                                 , s_y = NULL
                                 , AxisLabelX = Metadata[i,"X_axe_name"]
                                 , AxisLabelY = Code[s]
                                 , Title = ""
                                 , Marker = 19
                                 , Couleur = "blue"
                                 , ligne = "p" 
                                 , XY_same = FALSE
                                 , lim = NULL
                                 , steps = c(10, 10) 
                                 , digitround = NULL
                                 , marges = NULL
                                 , PlotAxis = "s"
                                 , OrdonneeOrigine = c(0,0))
    
    ## Add the X=Y line to ease the comparison between graphs
    lines(x= c(min(Limit.XY),max(Limit.XY)), y=c(min(Limit.XY),max(Limit.XY)), type = "l", col = "green4")
    mtext(paste0("Ligne Y=X "),line=-36.3,adj=1,padj=0,col= "green4",cex=1.2)
    
    Cal_Model <- Cal_Line(x = temp.df[, Pollutant[1]]
                          , s_x = NULL
                          , y = temp.df[, Sensors[s]]
                          , s_y = NULL
                          , Mod_type = "Linear"
                          , Matrice = NULL
                          , line_position = -1.3
                          , Couleur = "red"
                          , Sensor_name = NULL
                          , f_coef1 = "%.2f"
                          , f_coef2 = "%.2f"
                          , f_R2 = "%.3f"
                          , lim = Limit.XY
                          , marges = NULL
                          , Covariates = NULL
                          , Equation = "Simple")
    
    dev.copy(png, filename = file.path(Print_WD, paste0(paste0(Sensors[s], " VS ", Pollutant[1]), ".png"))
             , units = "cm", res = 1024, width = WidthEtalonnage, height = HeightEtalonnage)
    dev.off()
    
    Cal_Model_temp.df <- data.frame(Folder = character(), Sensor = character(), Reference = character(), Intercept = double(), Slope = double(), RSquare = double()
                                    , DevStd = double(), MAPE = double(), stringsAsFactors = FALSE)
    Cal_Model_temp.df[1, "Folder"] <- Metadata[i,"Folder_Name"]
    Cal_Model_temp.df[1, "Sensor"] <- Sensors[s]
    Cal_Model_temp.df[1, "Reference"] <- Pollutant[1]
    Cal_Model_temp.df[1, "Intercept"] <- coef(Cal_Model)[1]
    Cal_Model_temp.df[1, "Slope"] <- coef(Cal_Model)[2]
    Cal_Model_temp.df[1, "RSquare"] <- summary(Cal_Model)$r.squared
    Cal_Model_temp.df[1, "DevStd"] <- sd(resid(Cal_Model))
    Cal_Model_temp.df[1, "MAPE"] <- mape(actual=Cal_Model$x[,2],pred=Cal_Model$fitted.values)[1]
    Cal_Model_temp.df[1, "inf.MAPE"] <- mape(actual=Cal_Model$x[,2],pred=Cal_Model$fitted.values)[2]
    
    Cal_Model.df <- rbind(Cal_Model.df, Cal_Model_temp.df)
    
    remove(Limit.XY, Cal_Model, Cal_Model_temp.df)
  }
  remove(temp.df)
}

## Save the model dataframe
if(nrow(Cal_Model.df) != 0){
  write.csv(Cal_Model.df, file.path(Eval_WD, paste0("Cal_Model.csv")))
  print("Cal_Model.df dataframe saved as .csv")
}
