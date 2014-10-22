## Loading and preprocessing the data

fURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipFileName <- "./data.zip"
summaryFileName <- "./summarySCC_PM25.rds"
codeFileName <- "./Source_Classification_Code.rds"

## If csv file not in working directory, download from repository and extract to working directory
if (!file.exists(summaryFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName, mode='wb')
        unzip(zipFileName)
} else {message("Data file found in working directory")}

## read data into table from txt file and convert date from character to POSIXLT
if (!exists("NEI")){
        ## Calculate a rough estimate of how much memory the dataset will require
        ## in memory before reading into R. Make sure your computer has enough memory
        fileSizeMb <- file.info(summaryFileName)$size*1.0e-6
        memLimit <- memory.limit()
        
        ## Exit with error message if computer does not have enough memory
        if (fileSizeMb > memLimit){
                stop("Insufficient memory to read table - aborting script.")
        } else {
                message("Reading table from summary data file")
                NEI <- readRDS(summaryFileName) 
        }} else { message("Using summary data table from memory")}

if (!exists("SCC")){
        ## Calculate a rough estimate of how much memory the dataset will require
        ## in memory before reading into R. Make sure your computer has enough memory
        fileSizeMb <- file.info(codeFileName)$size*1.0e-6
        memLimit <- memory.limit()
        
        ## Exit with error message if computer does not have enough memory
        if (fileSizeMb > memLimit){
                stop("Insufficient memory to read table - aborting script.")
        } else {
                message("Reading table from code data file")
                SCC <- readRDS(codeFileName) 
        }} else { message("Using code data table from memory")}

## Subsetting Data to Include only Baltimore Values
baltdata <- NEI[NEI$fips == "24510",]

## Aggregating emmisions data by year
sumdata <- aggregate(baltdata$Emissions, list(baltdata$year), FUN = "sum")
colnames(sumdata) <- c("Year", "Emissions")

## Creating sub-directory for "figures" if it doesn't already eixst
subDir <- "figures"
if (!file.exists(subDir)){
        dir.create(subDir)
} 

## Loading the graphics package
png(file="./figures/plot2.png", width=480, height=480)

## Plotting the chart
plot(sumdata$Year, sumdata$Emissions, type = "l", 
     xlab = "Year", ylab = expression('Total PM'[2.5]*" Emissions"), 
     main = expression('Total PM'[2.5]*" Emissions in Baltimore, MD from 1999 to 2008"))

## Turning off the graphics package
dev.off()