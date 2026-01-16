library(xlsx)
library(reshape2)
library(ggplot2)
library(ggh4x)
library(wbstats)
library(jsonlite)
library(httr)
library(doParallel)
library(ggrepel)
library(ggforce)
library(scico)

#### INITIALIZATION ####
# Empty the workspace and memory
rm(list = ls(all.names = TRUE))
gc()

# Set some parameters
NoCores <- detectCores() - 1 # Set the number of cores to use for parallel processing
CarbonBudget = data.frame(TempTarget = c(1.5,2),
                          BudgetGtCO2 = c(500, 900)) # Set the carbon budget for 1.5C (50%) and 2C (83%) - AR6 WG1 - and adjust for 2020-2021 emissions and net-flux based on Global Carbon Project
CountryAssumptions <- subset(read.xlsx("CountryAssumptions.xlsx", sheetIndex = 1, colIndex = 1:3)) # Read assumptions for countries

LabelAccountingFramework <- c("Territorial\naccounting", "Consumption-based\naccounting")
names(LabelAccountingFramework) <- c(0,1)
LabelEmissions <- c("Consumption-based emissions", "Territorial emissions")
names(LabelEmissions) <- c("Consumption Emissions", "Territorial Emissions")
LabelHistoricResponsibility <- c("No historic responsibility", paste0("Historic responsibility from ",as.character(seq(1990,2021,5))))
names(LabelHistoricResponsibility) <- c(0,seq(1990,2021,5))

#### PREPARATION OF DATA ######
# Read data from Global Carbon Project and WorldBank
DataWorldBank <- wb_data(c(GDP = "NY.GDP.MKTP.CD", GDPpc = "NY.GDP.PCAP.CD"), country = "all", start_date = 1960, end_date = 2021) # Read GDP and GDP per capita data from WorldBank
# Rename variables and countries
colnames(DataWorldBank)[which(names(DataWorldBank) == "date")] <- "Year" 
colnames(DataWorldBank)[which(names(DataWorldBank) == "country")] <- "Country"
DataWorldBank$Country[DataWorldBank$iso3c == "EGY"] <- "Egypt"
DataWorldBank$Country[DataWorldBank$iso3c == "GMB"] <- "Gambia"
DataWorldBank$Country[DataWorldBank$iso3c == "IRN"] <- "Iran"
DataWorldBank$Country[DataWorldBank$iso3c == "RUS"] <- "Russia"
DataWorldBank$Country[DataWorldBank$iso3c == "KOR"] <- "South Korea"
DataWorldBank$Country[DataWorldBank$iso3c == "TUR"] <- "Türkiye"

# Add iso classification for countries from 'DataWorldBank' to 'CountryAssumptions'
CountryAssumptions <- merge(CountryAssumptions, subset(DataWorldBank, Country %in% CountryAssumptions$Country & Year == 2021, select = c(Country,iso3c)), by = "Country", all.x = TRUE)
CountryAssumptions <- subset(CountryAssumptions, !Country %in% c("Gambia", "Norway"))
# Download and Read data from Global Carbon Project
GCBDataFile <- tempfile(fileext = ".xlsx")
download.file("https://globalcarbonbudgetdata.org/downloads/jGJH0-data/National_Fossil_Carbon_Emissions_2024v1.0.xlsx", destfile = GCBDataFile, mode = "wb")
DataGlobalCarbonBudget <- melt(merge(melt(data = read.xlsx(GCBDataFile, sheetName = "Territorial Emissions", startRow = 12), id = "NA.", value.name = "Territorial Emissions", variable.name = "Country"),
                                     melt(data = read.xlsx(GCBDataFile, sheetName = "Consumption Emissions", startRow = 9), id = "NA.", value.name = "Consumption Emissions", variable.name = "Country"), by = c("Country", "NA."), all.x = TRUE), id = c("NA.", "Country"), value.name = "EmissionsMtCO2", variable.name = "Accounting")
GCBDataFileGlobal <- tempfile(fileext = ".xlsx")
download.file("https://globalcarbonbudgetdata.org/downloads/jGJH0-data/Global_Carbon_Budget_2024_v1.0.xlsx", destfile = GCBDataFileGlobal, mode = "wb")
DataGlobalCarbonBudgetGlobal <- read.xlsx(GCBDataFileGlobal, sheetName = "Global Carbon Budget", startRow = 22)

# Rename variables and countries
colnames(DataGlobalCarbonBudget)[which(names(DataGlobalCarbonBudget) == "NA.")] <- "Year" # Rename column for year from 'NA.' to 'Year'
DataGlobalCarbonBudget$EmissionsMtCO2 <- DataGlobalCarbonBudget$EmissionsMtCO2 * 44/12 # Convert emission data from MtC to MtCO2
DataGlobalCarbonBudget$Country <- as.character(DataGlobalCarbonBudget$Country) # Convert 'Country' to character
DataGlobalCarbonBudget$Country <- gsub(".", " ", DataGlobalCarbonBudget$Country, fixed = TRUE) # Replace '.' with ' ' in 'Country'
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "USA"] <- "United States" # Replace 'USA' with 'United States' in 'Country'
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "EU27"] <- "European Union" # Replace 'EU27' with 'European Union' in 'Country
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "Slovakia"] <- "Slovak Republic" # Replace 'Slovakia' with 'Slovak Republic' in 'Country
DataGlobalCarbonBudget <- merge(DataGlobalCarbonBudget, subset(CountryAssumptions, select = c(Country, iso3c)), by = "Country") # Add iso classification for countries to 'DataGlobalCarbonBudget'
DataGlobalCarbonBudgetALL <- DataGlobalCarbonBudget

DataGlobalCarbonBudget <- rbind(subset(DataGlobalCarbonBudget, Country != "World"), data.frame(Year = DataGlobalCarbonBudgetGlobal$Year, 
                                                                   Country = "World", 
                                                                   Accounting = "World", 
                                                                   EmissionsMtCO2 = DataGlobalCarbonBudgetGlobal$fossil.emissions.excluding.carbonation*1000*44/12 + DataGlobalCarbonBudgetGlobal$land.use.change.emissions*1000*44/12,
                                                                   iso3c = "WLD")) 
CarbonBudget$BudgetGtCO2 <- CarbonBudget$BudgetGtCO2 - sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World" & DataGlobalCarbonBudget$Year %in% c(2020,2021)])/1000 # Adjust the carbon budget for the world based on the Global Carbon Project data
# Define function to call the UN API
callAPI <- function(relative_path, topics_list=FALSE){
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, relative_path)
  response <- fromJSON(target)
  # Checks if response was a flat file or a list (indicating pagination)
  # If response is a list, we may need to loop through the pages to get all of the data
  if (class(response)=="list"){
    # Create a dataframe from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)){
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)}
  # Otherwise, we will simply load the data directly from the API into a dataframe
  else{
    if (topics_list==TRUE){
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    }
    else{
      df <- fromJSON(target)        
      return(df)
    }
  }
}
#UNLocations <- callAPI("/locations/")

# Download and read data from the UN Population Division
EUStatesISO2 <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE") # Define the ISO2 codes for the EU states
# Download data from the UN Population Division (not used due to long response times)
#DataUNPopulation <- 
#  foreach(i = UNLocations$id[UNLocations$iso3 %in% CountryAssumptions$iso3c | UNLocations$iso2 %in% EUStatesISO2], .combine = "rbind") %do% {
#    subset(callAPI(paste0("/data/indicators/49/locations/",i,"/start/1960/end/2070")), sexId == 3 & variantId == 4, select = c(location, iso3, iso2, timeLabel, value))
#  }
#https://population.un.org/dataportal/data/indicators/49/locations/32,36,40,56,76,100,124,152,156,170,188,191,196,203,208,818,233,231,246,250,270,276,348,356,360,364,372,380,392,398,404,428,440,442,470,484,504,528,554,566,578,604,608,616,620,410,642,643,682,702,703,705,710,724,752,756,764,792,784,826,840,704,900,300/start/1960/end/2070/table/pivotbylocation?df=1f046dc3-8d29-4199-b85a-b024caa951e3

# Read data from the UN Population Division file instead of downloading (due to long response times in the UN Population API)
DataUNPopulation <- subset(read.csv("unpopulation_dataportal.csv"), SexId == 3 & VariantId == 4, select = c(Location, Iso3, Iso2, Time, Value)) # Read Population data from the UN Population Division

DataUNPopulation <- rbind(DataUNPopulation, cbind(data.frame(Location = rep("European Union", length(1960:2100)),
                                                             Iso3 = rep("EUU", length(1960:2100)),
                                                             Iso2 = rep("EU", length(1960:2100)),
                                                             aggregate(Value ~ Time, subset(DataUNPopulation, Iso2 %in% EUStatesISO2),sum)))) # Aggregate the population data for the EU states
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Location")] <- "Country" # Rename 'Location' to 'Country'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Time")] <- "Year" # Rename 'Time' to 'Year'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Value")] <- "Population" # Rename 'Value' to 'Population'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso2")] <- "iso2c" # Rename 'Iso2' to 'iso2c'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso3")] <- "iso3c" # Rename 'Iso3' to 'iso3c'
DataUNPopulation$Year <- as.numeric(DataUNPopulation$Year) # Convert 'Year' to numeric
DataUNPopulation <- merge(subset(DataUNPopulation, select = -Country), subset(CountryAssumptions, select = c(Country, iso3c)), by = "iso3c") # Add iso classification country names to 'DataUNPopulation' based on 'CountryAssumptions'

# Read GDP and Population data from the SSP scenarios, and estimate annual pathways
DataSSPFutureGDP_data <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926)) # Read GDP and Population data from the SSP scenarios
DataSSPFutureGDP <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5)), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5))) # Read metadata only from the SSP scenarios

# Extract data for every 10 years from 2010 to 2100 from the SSP scenarios and estimate annual values inbetween by means of linear interpolation.
for (i in 2010:2100) { 
  if (i %in% seq(2010,2100,10)) {
    DataSSPFutureGDP <- cbind(DataSSPFutureGDP, data = DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",i,sep = ""))])
    colnames(DataSSPFutureGDP)[which(names(DataSSPFutureGDP) == paste("X",i,sep = ""))] <- i
    DataYear = i
  } else {
    DataSSPFutureGDP <- cbind(DataSSPFutureGDP, data = (DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",DataYear+10,sep = ""))]-DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",DataYear,sep = ""))])/10 + DataSSPFutureGDP[which(names(DataSSPFutureGDP) == i-1)])
    colnames(DataSSPFutureGDP)[length(names(DataSSPFutureGDP))] <- i
  }
}

DataSSPFutureGDP <- melt(DataSSPFutureGDP, measure.vars = as.character(2010:2100), value.name = "Value", variable.name = "Year") # Reshape the data
DataSSPFutureGDP$Year <- as.character(DataSSPFutureGDP$Year) # Convert 'Year' to character
DataSSPFutureGDP$Year <- as.numeric(DataSSPFutureGDP$Year) # Convert 'Year' to numeric
DataSSPFutureGDP$Region[DataSSPFutureGDP$Region == "World"] <- "WLD" # Replace 'World' with 'WLD' in 'Region'
DataSSPFutureGDP <- rbind(DataSSPFutureGDP, cbind(Model = "Aggregate", Region = "EUU",
                                                  aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% DataWorldBank$iso3c[DataWorldBank$iso2c %in% EUStatesISO2]), sum))) # Aggregate the data for the EU states

# Preparation for analysis
CountryAssumptions$iso3c[CountryAssumptions$Country == "Rest of world"] <- "ROW" # Replace 'Rest of world' with 'ROW' in 'iso3c'
DataUNPopulation <- rbind(subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c),
                          cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", aggregate(Population ~ Year, subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE] & Country != "World"), sum))) # Aggregate the population data for the analyzed countries and temporarily store as 'Rest of World'
DataGlobalCarbonBudget <- rbind(subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country & Year >= 1990 & Year <= 2021),
                                cbind(Country = "Rest of world", iso3c = "ROW", aggregate(EmissionsMtCO2 ~ Year+Accounting, subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE] & Year >= 1990 & Year <= 2021 & Country != "World"), sum))) # Aggregate the emissions data for the analyzed countries and temporarily store as 'Rest of World'
DataWorldBank <- rbind(subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c),
                       cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", merge(aggregate(GDP ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE] & Country != "World"), sum), aggregate(GDPpc ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE] & Country != "World"), sum), by = "Year"))) # Aggregate the current and past GDP for the analyzed countries and temporarily store as 'Rest of World'
DataSSPFutureGDP <- rbind(subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c),
                          cbind(Model = "Aggregate", Region = "ROW", aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE] & Region != "WLD"), sum))) # Aggregate future GDP and population for the analyzed countries in the SSP scenarios and temporarily store as 'Rest of World'

DataUNPopulation <- DataUNPopulation[order(DataUNPopulation$Country, DataUNPopulation$Year),] # Order the population data
DataGlobalCarbonBudget <-DataGlobalCarbonBudget[order(DataGlobalCarbonBudget$Country, DataGlobalCarbonBudget$Year),] # Order the Carbon Budget data
DataWorldBank <- DataWorldBank[order(DataWorldBank$Country, DataWorldBank$Year),] # Order the World Bank data
DataSSPFutureGDP <- DataSSPFutureGDP[order(DataSSPFutureGDP$Region, DataSSPFutureGDP$Year),] # Order the future GDP data

DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"] <- DataUNPopulation$Population[DataUNPopulation$Country == "World"] - DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"] # Calculate the population for the rest of the world
DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world" & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world" & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"] # Calculate the emissions for the rest of the world
DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world" & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world" & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"] # Calculate the emissions for the rest of the world
DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "World"] - DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] # Calculate the GDP for the rest of the world
DataWorldBank$GDPpc[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] / DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world" & DataUNPopulation$Year <= 2021] # Calculate the GDP per capita for the rest of the world
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "GDP|PPP"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"] # Calculate the GDP for the rest of the world
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "Population"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"] # Calculate the population for the rest of the world

# Define Annual Capability function based on Supplementary Equation 4 #
AnnualCapability <- function(Country, TempTarget) {
  AnnualBudget <- c()
  for (Year in 2022:2070) {
    AnnualBudget = c(AnnualBudget,
                     GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] * 
                       if (Country == "World") {1} else {
                         (DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
                            DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"]) /
                           sum(DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE & CountryAssumptions$Country != "World"] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
                                 DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region %in% CountryAssumptions$iso3c[CountryAssumptions$EUMemberState == FALSE & CountryAssumptions$Country != "World"] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"])
                       })
  }
  TotalBudget = sum(AnnualBudget)
  return(TotalBudget)
}

# Define Annual Per Capita Convergence function based on Supplementary Equation 3 #
AnnualPerCapitaConvergence <- function(Country, TempTarget, AccountingFramework, ConvYear) {
  AnnualBudget <- c()
  for (Year in 2022:2070) {
    AnnualBudget = c(AnnualBudget, 
                     GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] *
                       (min((Year-2021)/(ConvYear-2021),1) * 
                          DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year == Year]/
                          DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year == Year] +
                          max(1-(Year-2021)/(ConvYear-2021),0) * 
                          (DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Accounting == "Territorial Emissions" & DataGlobalCarbonBudget$Year == 2021]*
                             (1-AccountingFramework) + DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Accounting == "Consumption Emissions" & DataGlobalCarbonBudget$Year == 2021]*AccountingFramework) /
                          DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World" & DataGlobalCarbonBudget$Year == 2021]))
  }
  TotalBudget = sum(AnnualBudget)
  return(TotalBudget)
}

# Calculate the global emission curves for 1.5 and 2 degrees Celsius
GlobalEmissionCurves <- 
  foreach(TempTarget = c(1.5, 2), .combine = "rbind") %:%
  foreach(Year = 2022:2070, .combine = "rbind") %do% {
    GlobalNetZero <- 2022 + 2 * CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget]*1000 / DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World" & DataGlobalCarbonBudget$Year == 2021]
    LinearFunc <- function(x) pmax(0, DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World" & DataGlobalCarbonBudget$Year == 2021] * (1 - (x - 2022) / (GlobalNetZero - 2022)))
    data.frame(TempTarget = TempTarget, Year = Year, EmissionsMtCO2 = if (min(Year + 1, GlobalNetZero) <= Year) {0} else {(LinearFunc(Year) + LinearFunc(min(Year + 1, GlobalNetZero)))/2*(min(Year + 1, GlobalNetZero) - Year)})
  }

#### CALCULATION OF CARBON BUDGETS ####
Cluster <- makeCluster(NoCores) # Create a cluster for parallel processing
registerDoParallel(Cluster) # Register the number of cores for parallel processing
NationalCarbonBudgets <- 
  foreach(a = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"), .combine = "rbind") %:%
  foreach(t = CarbonBudget$TempTarget, .combine = "rbind") %:%
  foreach(f = seq(0,1,0.05), .combine = "rbind") %:%
  foreach(y = c(0,seq(1990,2021,5)), .combine = "rbind") %:%
  foreach(c = CountryAssumptions$Country[CountryAssumptions$Country != "World"], .combine = "rbind") %dopar% {
    if (a == "Equal Cumulative per Capita" & y != 0) {
      NationalCarbonBudget = (CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == t] * 1e3 + sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Accounting == "World" & DataGlobalCarbonBudget$Year %in% y:2021])) * 
        sum(DataUNPopulation$Population[DataUNPopulation$Country == c & DataUNPopulation$Year %in% y:2100]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year %in% y:2100]) -
        (sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Year %in% y:2021 & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"])*(1-f) +
           sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Year %in% y:2021 & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"])*f)
    } else if (a == "Contraction and Convergence" & y == 0) {
      NationalCarbonBudget = AnnualPerCapitaConvergence(c, t, f, 2050)
    } else if (a == "Capability" & y == 0) {
      NationalCarbonBudget = AnnualCapability(c, t)
    } else if (a == "Annual Equal per Capita" & y == 0) {
      NationalCarbonBudget = sum(GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == t & GlobalEmissionCurves$Year %in% 2022:2070] * 
                                   (DataUNPopulation$Population[DataUNPopulation$Country == c & DataUNPopulation$Year %in% 2022:2070] / 
                                      DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year %in% 2022:2070]))
    } else if (a == "Grandfathering" & y == 0) {
      NationalCarbonBudget = CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == t] * 1e3 * (DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Year == 2021 & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"]*(1-f) + 
                                                                                               DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Year == 2021 & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"]*f) / DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2021 & DataGlobalCarbonBudget$Accounting == "World"]
    }
    if ((a == "Equal Cumulative per Capita" & y != 0) | (a != "Equal Cumulative per Capita" & y == 0)) {
      data.frame(Country = c, EconDevelopment = CountryAssumptions$Development[CountryAssumptions$Country == c], AllocationPrinciple = a, TempTarget = t, AccountingFramework = f, HistoricResponsibility = y,
                 NationalCarbonBudget = NationalCarbonBudget,
                 ImplicitNetZero = ifelse(NationalCarbonBudget > 0, NationalCarbonBudget/(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Accounting == "Territorial Emissions" & DataGlobalCarbonBudget$Year == 2021]*(1-f) + 
                                                                                            DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == c & DataGlobalCarbonBudget$Accounting == "Consumption Emissions" & DataGlobalCarbonBudget$Year == 2021]*f)*2 + 2022, NA)
      )}
    
  }
stopCluster(Cluster)
rm(Cluster)

NationalCarbonBudgets <- cbind(NationalCarbonBudgets, CompleteResultsAccounting = NA, AnyNetZeroAbove2100 = FALSE)
for (a in c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability")) {
  for (y in c(0,seq(1990,2021,5))) {
    for (t in CarbonBudget$TempTarget) {
      for (c in CountryAssumptions$Country[CountryAssumptions$Country != "World"]) {
        if (sum(NationalCarbonBudgets$NationalCarbonBudget[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] > 0) == 21 & 
            ((a == "Equal Cumulative per Capita" & y != 0) | (a != "Equal Cumulative per Capita" & y == 0))) {
          NationalCarbonBudgets$CompleteResultsAccounting[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] <- TRUE
          if (sum(NationalCarbonBudgets$ImplicitNetZero[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] > 2100) == 21) {
            NationalCarbonBudgets$AnyNetZeroAbove2100[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] <- TRUE
          }
        } else if (sum(NationalCarbonBudgets$NationalCarbonBudget[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] > 0) == 0 & 
                   ((a == "Equal Cumulative per Capita" & y != 0) | (a != "Equal Cumulative per Capita" & y == 0))) {
          NationalCarbonBudgets$CompleteResultsAccounting[NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t & NationalCarbonBudgets$Country == c] <- FALSE
        }
      }
    }
  }
}

NationalCarbonBudgets <- cbind(NationalCarbonBudgets, SameImplicitNetZero = NA)
for (f in seq(0,1,0.1)) {
  NationalCarbonBudgets$SameImplicitNetZero[NationalCarbonBudgets$AccountingFramework == f] <- ceiling(NationalCarbonBudgets$ImplicitNetZero[NationalCarbonBudgets$AccountingFramework == 0]) == ceiling(NationalCarbonBudgets$ImplicitNetZero[NationalCarbonBudgets$AccountingFramework == 1])
}

NationalCarbonBudgets$AllocationPrinciple <- factor(NationalCarbonBudgets$AllocationPrinciple, levels = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"))
NationalCarbonBudgets$EconDevelopment <- factor(NationalCarbonBudgets$EconDevelopment, levels = c("High","Upper-middle","Lower-middle","Low", "Rest of world", "World"))
NationalCarbonBudgets$Country <- factor(NationalCarbonBudgets$Country, levels = c("World", "Rest of world", DataWorldBank$Country[DataWorldBank$Year == 2021 & !DataWorldBank$Country %in% c("World", "Rest of world")][order(DataWorldBank$GDPpc[DataWorldBank$Year == 2021 & !DataWorldBank$Country %in% c("World", "Rest of world")])]))

CountriesWithNegativeBudgets <- 
  foreach(a = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"), .combine = "rbind") %:%
  foreach(t = CarbonBudget$TempTarget, .combine = "rbind") %:%
  foreach(y = c(0,seq(1990,2021,5)), .combine = "rbind") %do% {
    if ((a == "Equal Cumulative per Capita" & y != 0) | (a != "Equal Cumulative per Capita" & y == 0)) {
      data.frame(AllocationPrinciple = a, HistoricResponsibility = y, TempTarget = t,
                 ListofCountries = paste(unique(NationalCarbonBudgets$Country[NationalCarbonBudgets$NationalCarbonBudget < 0 & NationalCarbonBudgets$AccountingFramework %in% c(0,1) & NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t]), collapse = "\n"))
    }}
CountriesWithNegativeBudgets$AllocationPrinciple <- factor(CountriesWithNegativeBudgets$AllocationPrinciple, levels = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"))

CountriesWithLateNetZero <- 
  foreach(a = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"), .combine = "rbind") %:%
  foreach(t = CarbonBudget$TempTarget, .combine = "rbind") %:%
  foreach(y = c(0,seq(1990,2021,5)), .combine = "rbind") %do% {
    if ((a == "Equal Cumulative per Capita" & y != 0) | (a != "Equal Cumulative per Capita" & y == 0)) {
      data.frame(AllocationPrinciple = a, HistoricResponsibility = y, TempTarget = t,
                 ListofCountries = paste(unique(NationalCarbonBudgets$Country[!is.na(NationalCarbonBudgets$ImplicitNetZero) & NationalCarbonBudgets$ImplicitNetZero > 2070 & NationalCarbonBudgets$AccountingFramework %in% c(0,1) & NationalCarbonBudgets$AllocationPrinciple == a & NationalCarbonBudgets$HistoricResponsibility == y & NationalCarbonBudgets$TempTarget == t]), collapse = "\n"))
    }}
CountriesWithLateNetZero$AllocationPrinciple <- factor(CountriesWithLateNetZero$AllocationPrinciple, levels = c("Equal Cumulative per Capita", "Annual Equal per Capita", "Grandfathering", "Contraction and Convergence", "Capability"))

#### PLOTTING PPT ####
ResultsCarbonBudgetSweden <- ggplot() +
  geom_col(data = cbind(Graph = "Historic Responsiblity", subset(NationalCarbonBudgets, TempTarget == 1.5 & Country == "Sweden" & !HistoricResponsibility %in% c(0,1995,2005,2015, 2020) & AccountingFramework %in% c(1,0))),
           mapping = aes(x = as.character(HistoricResponsibility), y = NationalCarbonBudget, fill = as.character(AccountingFramework)), position = position_dodge2()) +
  geom_col(data = cbind(Graph = "", subset(NationalCarbonBudgets, TempTarget == 1.5 & Country == "Sweden" & HistoricResponsibility == 0 & AccountingFramework %in% c(1,0))),
           mapping = aes(x = AllocationPrinciple, y = NationalCarbonBudget, fill = as.character(AccountingFramework)), position = position_dodge2()) +
  facet_grid(cols = vars(Graph), scales = "free", space = "free", switch = "x") +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  scale_y_continuous(breaks = seq(-600, 800, 100)) +
  scale_fill_scico_d(labels = LabelAccountingFramework, palette = "roma") +
  labs(y = expression(paste("National Carbon Budget (Mt",CO[2],")")), fill = "") +
  theme_bw() + theme(legend.position = "right",
                     strip.placement = "outside",
                     strip.background.x = element_blank(),
                     axis.title.x = element_blank())

png(filename = "ResultsCarbonBudgetSweden.png", width = 8.63, height = 3.13, units = "in", res = 300)
print(ResultsCarbonBudgetSweden)
dev.off()

png(filename = "ResultsCarbonBudgetSwedenTransparent.png", width = 8.63, height = 3.13, units = "in", res = 300)
print(ResultsCarbonBudgetSweden + annotate("rect", ymin = 380, ymax = 415, xmin = -Inf, xmax = Inf, color = "black", alpha = 0.5) + scale_fill_manual(labels = LabelAccountingFramework, values = c(scico(2, palette = "roma")[1], "transparent"))
      )
dev.off()

ResultsSweden <- ggplot(data = subset(NationalCarbonBudgets, Country == "Sweden" & TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010))) +
  geom_point(mapping = aes(x = ImplicitNetZero, y = as.character(HistoricResponsibility), color = AccountingFramework, fill = AccountingFramework), size = 5, shape = 25) +
  #geom_boxplot(data = subset(NationalCarbonBudgets, Country == "Sweden" & TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
  #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
  geom_text(size = 2, color = "dimgray", mapping = aes(x = 2045, y = as.character(HistoricResponsibility),
                                                       label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2070", "")), hjust = .5) +
  geom_text(size = 2, color = "dimgray", mapping = aes(x = 2045, y = as.character(HistoricResponsibility),
                                                       label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
  facet_wrap(~AllocationPrinciple, scales = "free", labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility), nrow = 2) +
  scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
  scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
  scale_y_discrete(labels = NULL) +
  coord_cartesian(xlim = c(2020,2070)) +
  guides(color = guide_colorbar(barwidth = 20)) +
  labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom",
                                  strip.background = element_rect(fill = "black", color = "transparent"),
                                  strip.text = element_text(color = "white"),
                                  axis.ticks.length.y = unit(0, "cm"))

png(filename = "ResultsNetZeroSweden.png", width = 6, height = 4, units = "in", res = 300)
print(ResultsSweden)
dev.off()

for (c in c("Sweden", "United States", "China", "European Union", "South Africa", "Kazakhstan")) {
  EmissionTrendsSE <- ggplot(data = merge(subset(DataGlobalCarbonBudget, Country == c | Country == "World" & Accounting == "World"), subset(DataUNPopulation, Country %in% c("World", c)), by = c("Year", "Country"))) +
    geom_line(mapping = aes(x = Year, y = EmissionsMtCO2/Population*1e6, linetype = Accounting, color = Accounting)) +
    scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +
    scale_color_manual(values = c(scico(2, palette = "roma"),"black")) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 6) +
    guides(linetype = guide_legend(nrow = 2), color = guide_legend(nrow = 2)) +
    labs(x = "Year", y = expression(paste("Emissions per capita (t",CO[2], ")")), linetype = "", color = "") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom")
  
  png(filename = paste0("EmissionTrends", CountryAssumptions$iso3c[CountryAssumptions$Country == c], ".png"), width = 3.5, height = 4, units = "in", res = 300)
  print(EmissionTrendsSE)
  dev.off()
}

for (t in c(1.5,2)) {
  ResultsAllCountries <- ggplot(data = subset(NationalCarbonBudgets, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering" & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE])) +
    geom_point(mapping = aes(x = ImplicitNetZero, y = Country, color = AccountingFramework, fill = AccountingFramework), shape = 25) +
    #geom_boxplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
    #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
    geom_text(size = 2, color = "palegreen4", mapping = aes(x = 2045, y = Country,
                                                            label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2070", "")), hjust = .5) +
    geom_text(size = 2, color = "maroon3", mapping = aes(x = 2045, y = Country,
                                                         label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
    facet_grid(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    coord_cartesian(xlim = c(2020,2070)) +
    guides(color = guide_colorbar(barwidth = 20)) +
    labs(x = "Net-zero year", color = "", y = NULL, fill = "") +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background = element_rect(fill = "black", color = "transparent"),
                                      strip.text = element_text(color = "white"),
                                      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  png(filename = paste0("ResultsAllCountries",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(ResultsAllCountries)
  dev.off()
}

for (t in c(1.5,2)) {
  ResultsEUMemberStates <- ggplot(data = subset(NationalCarbonBudgets, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering" & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == TRUE])) +
    geom_point(mapping = aes(x = ImplicitNetZero, y = Country, color = AccountingFramework, fill = AccountingFramework), shape = 25) +
    #geom_boxplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
    #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
    geom_text(size = 2, color = "palegreen4", mapping = aes(x = 2045, y = Country,
                                                            label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2070", "")), hjust = .5) +
    geom_text(size = 2, color = "maroon3", mapping = aes(x = 2045, y = Country,
                                                         label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
    facet_grid(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    coord_cartesian(xlim = c(2020,2070)) +
    guides(color = guide_colorbar(barwidth = 20)) +
    labs(x = "Net-zero year", color = "", y = NULL, fill = "") +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background = element_rect(fill = "black", color = "transparent"),
                                      strip.text = element_text(color = "white"),
                                      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  png(filename = paste0("ResultsEUMemberStates",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(ResultsEUMemberStates)
  dev.off()
}

for (t in c(1.5,2)) {
  PlotDataCompareTargets <- cbind(subset(NationalCarbonBudgets, Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE & CountryAssumptions$iso3c != "ROW"] & TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AccountingFramework == 1 & AllocationPrinciple != "Grandfathering"),
                                  TerritorialNetZero = subset(NationalCarbonBudgets, Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE & CountryAssumptions$iso3c != "ROW"] & TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AccountingFramework == 0 & AllocationPrinciple != "Grandfathering")$ImplicitNetZero)
  ResultsCompareTargets <- ggplot() +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    geom_point(data = PlotDataCompareTargets, mapping = aes(x = TerritorialNetZero, y = ImplicitNetZero, color = EconDevelopment)) +
    geom_text_repel(size = 2, data = subset(PlotDataCompareTargets, abs(TerritorialNetZero - ImplicitNetZero) > 5 & TerritorialNetZero < 2070 & ImplicitNetZero < 2070), 
                    mapping = aes(x = TerritorialNetZero, y = ImplicitNetZero, label = Country), 
                    nudge_x = 0.5, nudge_y = 0.5, min.segment.length = 0, max.overlaps = 30) +
    #geom_text(data = subset(CountriesWithNegativeBudgets, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering"), 
    #          mapping = aes(label = ListofCountries, x = 2060, y = 2040), hjust = 1, vjust = 1, size = 2) + 
    #geom_text(data = subset(CountriesWithLateNetZero, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering"), 
    #          mapping = aes(label = ListofCountries, x = 2020, y = 2070), hjust = 1, vjust = 1, size = 2) + 
    facet_wrap(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_scico_d(palette = "roma") +
    coord_cartesian(xlim = c(2020,2070), ylim = c(2020,2070)) +
    labs(x = "Territorial net-zero year", y = "Consumption-based net-zero year", color = "Economic development") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom",
                                    strip.background = element_rect(fill = "black", color = "transparent"),
                                    strip.text = element_text(color = "white"))
  
  png(filename = paste0("ResultsCompareTargets",t,".png"), width = 6, height = 4, units = "in", res = 300)
  print(ResultsCompareTargets)
  dev.off()
}

ResultsAllCountriesOnlyEqualPerCapita <- ggplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & AllocationPrinciple == "Annual Equal per Capita")) +
  geom_point(mapping = aes(x = ImplicitNetZero, y = Country, color = AccountingFramework, fill = AccountingFramework), shape = 25) +
  #geom_boxplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
  #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
  geom_text(size = 2, color = "palegreen4", mapping = aes(x = 2045, y = Country,
                                                          label = ifelse(Country %in% c("Kenya", "Nigeria", "Ethiopia") & AccountingFramework == 0.5, "Net-zero after 2100", "")), hjust = .5) +
  geom_text(size = 2, color = "maroon3", mapping = aes(x = 2045, y = Country,
                                                       label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
  facet_grid(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
  scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer\nresponsibility", "Symmetrical", "Producer\nresponsibility")) +
  scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer\nresponsibility", "Symmetrical", "Producer\nresponsibility")) +
  scale_x_continuous(breaks = seq(2020, 2100, 10)) +
  coord_cartesian(xlim = c(2020,2100)) +
  guides(color = guide_colorbar(barwidth = 12)) +
  labs(x = "Net-zero year", color = "", y = NULL, fill = "") +
  theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                    strip.background = element_rect(fill = "black", color = "transparent"),
                                    strip.text = element_text(color = "white"),
                                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

png(filename = "ResultsAllCountriesOnlyEqualPerCapita.png", width = 4, height = 5.11, units = "in", res = 300)
print(ResultsAllCountriesOnlyEqualPerCapita)
dev.off()

ResultsAnnualEmissionsSweden <- ggplot() +
  geom_line(data = subset(DataGlobalCarbonBudget, Country == "Sweden"),
            mapping = aes(x = Year, y = EmissionsMtCO2, linetype = "Historical emissions")) +
  geom_segment(data = merge(subset(DataGlobalCarbonBudget, Country == "Sweden" & Year == 2021 & Accounting == "Consumption Emissions"), 
                            subset(NationalCarbonBudgets, Country == "Sweden" & TempTarget == 1.5 & AccountingFramework == 1 & HistoricResponsibility %in% c(0,1990,2000,2010)), by = "Country"),
            mapping = aes(x = 2021, y = EmissionsMtCO2, xend = ImplicitNetZero, yend = 0, color = as.character(HistoricResponsibility), linetype = AllocationPrinciple)) +
  geom_segment(data = merge(subset(DataGlobalCarbonBudget, Country == "Sweden" & Year == 2021 & Accounting == "Territorial Emissions"), 
                            subset(NationalCarbonBudgets, Country == "Sweden" & TempTarget == 1.5 & AccountingFramework == 0 & HistoricResponsibility %in% c(0,1990,2000,2010)), by = "Country"),
               mapping = aes(x = 2021, y = EmissionsMtCO2, xend = ImplicitNetZero, yend = 0, color = as.character(HistoricResponsibility), linetype = AllocationPrinciple)) +
  annotate("point", x = 2045, y = 0, color = "darkgreen") +
  facet_grid(~Accounting, labeller = labeller(Accounting = LabelEmissions)) +
  scale_color_manual(values = c("black", scico(3, palette = "roma")), labels = c("Equal Cumulative per Capita", "1990", "2000", "2010")) +
  scale_y_continuous(limits = c(0,80)) +
  coord_cartesian(xlim = c(2015,2050)) +
  labs(y = expression(paste("Annual emissions of carbon dioxide (Mt",CO[2],")")), color = "", linetype = "") +
  theme_bw() + theme(legend.position = "right",
                     legend.direction = "vertical",
                     strip.background = element_rect(fill = "black", color = "transparent"),
                     strip.text = element_text(color = "white"),
                     axis.title.x = element_blank())
  
png(filename = "ResultsAnnualEmissionsSweden.png", width = 8.63, height = 3.13, units = "in", res = 300)
print(ResultsAnnualEmissionsSweden)
dev.off()


#### PLOTTING PAPER ####
DataSampleCountries <- cbind(subset(NationalCarbonBudgets, Country %in% c("Sweden", "European Union", "United States", "China", "South Africa") & TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering"), HistoricRespOrOther = "Historic Responsibility", CombHistOther = "")
DataSampleCountries$HistoricRespOrOther[DataSampleCountries$HistoricResponsibility == 0] <- "No Historic Responsibility"
DataSampleCountries$CombHistOther[DataSampleCountries$HistoricResponsibility == 0] <- as.character(DataSampleCountries$AllocationPrinciple[DataSampleCountries$HistoricResponsibility == 0])
DataSampleCountries$CombHistOther[DataSampleCountries$AllocationPrinciple == "Annual Equal per Capita"] <- "Annual Equality"
DataSampleCountries$CombHistOther[DataSampleCountries$HistoricResponsibility != 0] <- paste0("Historic Responsibility from ",as.character(DataSampleCountries$HistoricResponsibility[DataSampleCountries$HistoricResponsibility != 0]))

ResultsSampleCountries <- ggplot(data = DataSampleCountries) +
  geom_point(size = 2, shape = 25, mapping = aes(x = ImplicitNetZero, y = CombHistOther, color = AccountingFramework, fill = AccountingFramework)) +
  geom_text(size = 2.5, color = "dimgray", mapping = aes(x = 2040, y = CombHistOther,
                                                       label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2070", "")), hjust = .5) +
  geom_text(size = 2.5, color = "dimgray", mapping = aes(x = 2040, y = CombHistOther,
                                                       label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
  geom_text(size = 2.5, color = "black", data = data.frame(Country = c("China", "European Union", "South Africa", "Sweden", "United States"),
                                                           Label = c("a)", "b)", "c)", "d)", "e)")),
            mapping = aes(x = 2022, y = .75, label = Label)) +
  facet_grid(~as.character(Country), scales = "free") +
  scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consuming country's responsibility", "Symmetrical", "Producing country's responsibility")) +
  scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consuming country's responsibility", "Symmetrical", "Producing country's responsibility")) +
  scale_y_discrete(labels = label_wrap_gen(width = 24)) +
  coord_cartesian(xlim = c(2020,2060)) +
  guides(color = guide_colorbar(barwidth = 20)) +
  labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom",
                                  strip.background = element_rect(fill = "black", color = "transparent"),
                                  strip.text = element_text(color = "white"),
                                  axis.ticks.length.y = unit(0, "cm"),
                                  legend.key.height = unit(0.3, "cm"),
                                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, color = "black"),
                                  axis.text.y = element_text(color = "black"))

png(filename = "ResultsSampleCountries.png", width = 88*2, height = 85, units = "mm", res = 500)
print(ResultsSampleCountries)
dev.off()

TrendsSampleCountries <- ggplot() +
  geom_line(data = merge(subset(DataGlobalCarbonBudget, Country %in% c("Sweden", "European Union", "United States", "China", "South Africa")), subset(DataUNPopulation, Country %in% c("Sweden", "European Union", "United States", "China", "South Africa")), by = c("Year", "Country")),
            mapping = aes(x = Year, y = EmissionsMtCO2/Population*1e6, linetype = Accounting, color = Accounting)) +
  geom_line(data = merge(subset(DataGlobalCarbonBudget, Country == "World" & Accounting == "World", select = c("Year", "EmissionsMtCO2", "Accounting")), subset(DataUNPopulation, Country == "World", select = c("Year", "Population")), by = c("Year")),
            mapping = aes(x = Year, y = EmissionsMtCO2/Population*1e6, linetype = Accounting, color = Accounting)) +
  geom_text(size = 2.5, color = "black", data = data.frame(Country = c("China", "European Union", "South Africa", "Sweden", "United States"),
                                                           Label = c("a)", "b)", "c)", "d)", "e)")),
            mapping = aes(x = 1990, y = 0, label = Label),
            vjust = -0.5, hjust = -0.3) +
  facet_grid2(cols = vars(Country), scales = "free", independent = TRUE) +
  scale_linetype_manual(values = c("dashed", "dotdash", "solid"), labels = c("Territorial Emissions", "Consumption-based Emissions", "World average")) +
  scale_color_manual(values = c(scico(2, palette = "roma"),"black"), labels = c("Territorial Emissions", "Consumption-based Emissions", "World average")) +
  scale_y_continuous(limits = c(0, NA), n.breaks = 6) +
  guides(linetype = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
  labs(x = "Year", y = expression(paste("Emissions per capita (t",CO[2], ")")), linetype = "", color = "") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom",
                                  strip.background = element_rect(fill = "black", color = "transparent"),
                                  strip.text = element_text(color = "white"),
                                  axis.text = element_text(color = "black"))

png(filename = "TrendsSampleCountries.png", width = 88*2, height = 60, units = "mm", res = 500)
print(TrendsSampleCountries)
dev.off()

DataForSupplementary <- data.frame()
for (t in c(1.5,2)) {
  for (a in c("All", "EU")) {
  PlotDataCompareTargets <- cbind(subset(NationalCarbonBudgets, Country %in% CountryAssumptions$Country[CountryAssumptions$iso3c != "ROW" & CountryAssumptions$EUMemberState == ifelse(a == "All", FALSE, TRUE)] & TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AccountingFramework == 1 & AllocationPrinciple != "Grandfathering"),
                                  TerritorialNetZero = subset(NationalCarbonBudgets, Country %in% CountryAssumptions$Country[CountryAssumptions$iso3c != "ROW" & CountryAssumptions$EUMemberState == ifelse(a == "All", FALSE, TRUE)] & TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AccountingFramework == 0 & AllocationPrinciple != "Grandfathering")$ImplicitNetZero,
                                  TerritorialBudget = subset(NationalCarbonBudgets, Country %in% CountryAssumptions$Country[CountryAssumptions$iso3c != "ROW" & CountryAssumptions$EUMemberState == ifelse(a == "All", FALSE, TRUE)] & TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AccountingFramework == 0 & AllocationPrinciple != "Grandfathering")$NationalCarbonBudget,
                                  CombHistOther = NA)
  PlotDataCompareTargets$CombHistOther[PlotDataCompareTargets$HistoricResponsibility == 0] <- as.character(PlotDataCompareTargets$AllocationPrinciple[PlotDataCompareTargets$HistoricResponsibility == 0])
  PlotDataCompareTargets$CombHistOther[PlotDataCompareTargets$AllocationPrinciple == "Annual Equal per Capita"] <- "Annual Equality"
  PlotDataCompareTargets$CombHistOther[PlotDataCompareTargets$HistoricResponsibility != 0] <- paste0("Historic Responsibility from ",as.character(PlotDataCompareTargets$HistoricResponsibility[PlotDataCompareTargets$HistoricResponsibility != 0]))
  DataForSupplementary <- rbind(DataForSupplementary, subset(PlotDataCompareTargets, select = c("TempTarget", "AllocationPrinciple", "HistoricResponsibility", "EconDevelopment", "Country", "ImplicitNetZero", "NationalCarbonBudget", "TerritorialNetZero", "TerritorialBudget")))
  ResultsCompareTargets <- ggplot() +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    annotate("rect", xmin = 2088, xmax = 2102, ymin = 2025, ymax = 2050, fill = "lightgray") +
    annotate("rect", xmin = 2018, xmax = 2032, ymin = 2080, ymax = 2100, fill = "lightgray") +
    geom_point(data = PlotDataCompareTargets, mapping = aes(y = ifelse(NationalCarbonBudget<0 & EconDevelopment == "High", 2090, ifelse(NationalCarbonBudget<0 & EconDevelopment == "Upper-middle", 2085, ifelse((ImplicitNetZero>2100|TerritorialNetZero>2100) & EconDevelopment == "Low", 2040, ifelse((ImplicitNetZero>2100|TerritorialNetZero>2100) & EconDevelopment == "Lower-middle", 2035, ifelse((ImplicitNetZero>2100|TerritorialNetZero>2100) & EconDevelopment == "Upper-middle", 2030, ifelse((ImplicitNetZero>2100|TerritorialNetZero>2100) & EconDevelopment == "High", 2025, TerritorialNetZero)))))), 
                                                            x = ifelse(NationalCarbonBudget<0, 2025, ifelse(ImplicitNetZero>2100|TerritorialNetZero>2100, 2095, ImplicitNetZero)), 
                                                            color = EconDevelopment)) +
    geom_text(data = as.data.frame(table(subset(PlotDataCompareTargets, ImplicitNetZero>2100 | TerritorialNetZero>2100, select = c(EconDevelopment, CombHistOther)))),
              mapping = aes(label = ifelse(Freq != 0, Freq, ""), x = 2097, y = ifelse(EconDevelopment == "Low", 2040, ifelse(EconDevelopment == "Lower-middle", 2035, ifelse(EconDevelopment == "Upper-middle", 2030, 2025)))), hjust = 0, vjust = 0.5, size = 2) +
    annotate("text", x = 2095, y = 2045, label = ">2100", size = 2) +
    geom_text(data = as.data.frame(table(subset(PlotDataCompareTargets, NationalCarbonBudget<0, select = c(EconDevelopment, CombHistOther)))),
              mapping = aes(label = ifelse(Freq != 0, Freq, ""), x = 2027, y = ifelse(EconDevelopment == "Low", 2075, ifelse(EconDevelopment == "Lower-middle", 2080, ifelse(EconDevelopment == "Upper-middle", 2085, 2090)))), hjust = 0, vjust = 0.5, size = 2) +
    annotate("text", x = 2025, y = 2095, label = expression(paste("<0 ",CO[2])), size = 2) +
    geom_text_repel(size = 2, data = subset(PlotDataCompareTargets, abs(TerritorialNetZero - ImplicitNetZero) > 4 & TerritorialNetZero < 2100 & ImplicitNetZero < 2100), 
                    mapping = aes(y = TerritorialNetZero, x = ImplicitNetZero, label = Country), 
                    nudge_x = 0.5, nudge_y = 0.5, min.segment.length = 0, max.overlaps = 30, xlim = c(2020,2100), ylim = c(2020,2100)) +
    geom_text(size = 2.5, color = "black", data = data.frame(CombHistOther = c("Annual Equality", "Capability", "Contraction and Convergence", "Historic Responsibility from 1990", "Historic Responsibility from 2000", "Historic Responsibility from 2010"),
                                                             Label = c("a)", "b)", "c)", "d)", "e)", "f)")),
              mapping = aes(x = 2018, y = 2022, label = Label)) +
    facet_wrap(~CombHistOther, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_manual(values = scico(4, palette = "roma")[1:ifelse(a=="All", 4,2)]) +
    coord_cartesian(xlim = c(2018,2100), ylim = c(2018,2100)) +
    labs(y = "Producing countries bear responsibility", x = "Consuming countries bear responsibility", color = "Economic development") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom",
                                    legend.margin = margin(0,0,0,0),
                                    strip.background = element_rect(fill = "black", color = "transparent"),
                                    strip.text = element_text(color = "white"))
  
  png(filename = paste0("ResultsCompareTargets",t,a,".png"), width = 6, height = 4, units = "in", res = 300)
  print(ResultsCompareTargets)
  dev.off()
  }
}
colnames(DataForSupplementary)[which(names(DataForSupplementary) == "ImplicitNetZero")] <- "ConsumptionBasedNetZero"
colnames(DataForSupplementary)[which(names(DataForSupplementary) == "NationalCarbonBudget")] <- "ConsumptionBasedCarbonBudget"
DataForSupplementary$Country <- as.character(DataForSupplementary$Country)
DataForSupplementary <- DataForSupplementary[order(DataForSupplementary$TempTarget, DataForSupplementary$AllocationPrinciple, DataForSupplementary$HistoricResponsibility, DataForSupplementary$EconDevelopment, DataForSupplementary$Country, decreasing = FALSE), ]
DataForSupplementary$HistoricResponsibility[DataForSupplementary$HistoricResponsibility == 0] <- NA
DataForSupplementary <- DataForSupplementary[, c(1,2,3,4,5,6,8,7,9)]
write.xlsx(DataForSupplementary, "DataForSupplementary.xlsx", row.names = FALSE, showNA = FALSE)

for (t in c(1.5,2)) {
  PlotCarbonBudgets <- ggplot(data = rbind(cbind(LargeBudgets = TRUE, subset(NationalCarbonBudgets, TempTarget == t & AccountingFramework %in% c(0,1) & Country != "Rest of world" & HistoricResponsibility %in% c(0,1990,2000,2010) & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE] & Country %in% c("United States", "China", "India", "European Union"))),
                                           cbind(LargeBudgets = FALSE, subset(NationalCarbonBudgets, TempTarget == t & AccountingFramework %in% c(0,1) & Country != "Rest of world" & HistoricResponsibility %in% c(0,1990,2000,2010) & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE] & !Country %in% c("United States", "China", "India", "European Union"))))) +
    #geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = ifelse(LargeBudgets == FALSE, as.numeric(factor(subset(Country, LargeBudgets == LargeBudgets)))-.5, 0), ymax = ifelse(LargeBudgets == FALSE, as.numeric(factor(subset(Country, LargeBudgets == LargeBudgets)))+.5, 0), fill = EconDevelopment), alpha = 0.2) +
    geom_col(mapping = aes(y = Country, x = NationalCarbonBudget/1e3, fill = as.character(AccountingFramework)), position = position_dodge2()) +
    facet_grid2(LargeBudgets~AllocationPrinciple+HistoricResponsibility, scales = "free", space = "free_y", axes = "x", independent = "x",
                labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_fill_scico_d(labels = LabelAccountingFramework, palette = "roma") +
    labs(x = expression(paste("National Carbon Budget (Gt",CO[2],")")), fill = "", y = NULL) +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background.x = element_rect(fill = "black", color = "transparent"),
                                      strip.text.x = element_text(color = "white"),
                                      strip.background.y = element_blank(),
                                      strip.text.y = element_blank())
  
  png(filename = paste0("ResultsCarbonBudgets",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(PlotCarbonBudgets)
  dev.off()
  
  PlotCarbonBudgetsEU <- ggplot(data = rbind(cbind(LargeBudgets = TRUE, subset(NationalCarbonBudgets, TempTarget == t & AccountingFramework %in% c(0,1) & Country != "Rest of world" & HistoricResponsibility %in% c(0,1990,2000,2010) & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == TRUE] & Country %in% c("Germany", "Italy", "Spain", "France", "Poland", "Romania"))),
                                             cbind(LargeBudgets = FALSE, subset(NationalCarbonBudgets, TempTarget == t & AccountingFramework %in% c(0,1) & Country != "Rest of world" & HistoricResponsibility %in% c(0,1990,2000,2010) & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == TRUE] & !Country %in% c("Germany", "Italy", "Spain", "France", "Poland", "Romania"))))) +
    #geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = ifelse(LargeBudgets == FALSE, as.numeric(factor(subset(Country, LargeBudgets == LargeBudgets)))-.5, 0), ymax = ifelse(LargeBudgets == FALSE, as.numeric(factor(subset(Country, LargeBudgets == LargeBudgets)))+.5, 0), fill = EconDevelopment), alpha = 0.2) +
    geom_col(mapping = aes(y = Country, x = NationalCarbonBudget/1e3, fill = as.character(AccountingFramework)), position = position_dodge2()) +
    facet_grid2(LargeBudgets~AllocationPrinciple+HistoricResponsibility, scales = "free", space = "free_y", axes = "x", independent = "x",
                labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_fill_scico_d(labels = LabelAccountingFramework, palette = "roma") +
    labs(x = expression(paste("National Carbon Budget (Gt",CO[2],")")), fill = "", y = NULL) +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background.x = element_rect(fill = "black", color = "transparent"),
                                      strip.text.x = element_text(color = "white"),
                                      strip.background.y = element_blank(),
                                      strip.text.y = element_blank())
  
  png(filename = paste0("ResultsCarbonBudgetsEU",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(PlotCarbonBudgetsEU)
  dev.off()
  
  ResultsAllCountries <- ggplot(data = subset(NationalCarbonBudgets, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering" & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == FALSE])) +
    geom_point(mapping = aes(x = ImplicitNetZero, y = Country, color = AccountingFramework, fill = AccountingFramework), shape = 25) +
    #geom_boxplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
    #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
    geom_text(size = 2, color = "palegreen4", mapping = aes(x = 2060, y = Country,
                                                            label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2100", "")), hjust = .5) +
    geom_text(size = 2, color = "maroon3", mapping = aes(x = 2060, y = Country,
                                                         label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
    facet_grid(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    coord_cartesian(xlim = c(2020,2100)) +
    guides(color = guide_colorbar(barwidth = 20)) +
    labs(x = "Net-zero year", color = "", y = NULL, fill = "") +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background = element_rect(fill = "black", color = "transparent"),
                                      strip.text = element_text(color = "white"),
                                      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  png(filename = paste0("ResultsAllCountries",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(ResultsAllCountries)
  dev.off()

  ResultsEUMemberStates <- ggplot(data = subset(NationalCarbonBudgets, TempTarget == t & HistoricResponsibility %in% c(0,1990,2000,2010) & AllocationPrinciple != "Grandfathering" & Country %in% CountryAssumptions$Country[CountryAssumptions$EUMemberState == TRUE])) +
    geom_point(mapping = aes(x = ImplicitNetZero, y = Country, color = AccountingFramework, fill = AccountingFramework), shape = 25) +
    #geom_boxplot(data = subset(NationalCarbonBudgets, TempTarget == 1.5 & HistoricResponsibility %in% c(0,1990,2000,2010) & CompleteResultsAccounting == TRUE),
    #             mapping = aes(x = ImplicitNetZero, y = Country), alpha = 0) +
    geom_text(size = 2, color = "palegreen4", mapping = aes(x = 2060, y = Country,
                                                            label = ifelse(AnyNetZeroAbove2100 == TRUE & AccountingFramework == 0.5, "Net-zero after 2100", "")), hjust = .5) +
    geom_text(size = 2, color = "maroon3", mapping = aes(x = 2060, y = Country,
                                                         label = ifelse(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5, "Negative carbon budget", "")), hjust = .5) +
    facet_grid(~AllocationPrinciple+HistoricResponsibility, labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)) +
    scale_color_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    scale_fill_scico(palette = "roma", breaks = c(1,0.5,0), labels = c("Consumer responsibility", "Symmetrical", "Producer responsibility")) +
    coord_cartesian(xlim = c(2020,2100)) +
    guides(color = guide_colorbar(barwidth = 20)) +
    labs(x = "Net-zero year", color = "", y = NULL, fill = "") +
    theme_bw(base_size = 7.4) + theme(legend.position = "bottom",
                                      strip.background = element_rect(fill = "black", color = "transparent"),
                                      strip.text = element_text(color = "white"),
                                      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  png(filename = paste0("ResultsEUMemberStates",t,".png"), width = 8.38, height = 5.11, units = "in", res = 300)
  print(ResultsEUMemberStates)
  dev.off()
}

#### Left-over code ####
write.xlsx(NationalCarbonBudgets, "NationalCarbonBudgets.xlsx")

