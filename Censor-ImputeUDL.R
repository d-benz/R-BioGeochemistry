#ImputeUDLS:
#"Parametric replacement of values above upper detection limit for compositional data using classical and robust methods based on ilr-transformations with special choice of balances." robCompositions package

#SETUP
#Create data file with top row for column headers with names that are alphanumeric and only use the _ symbols with no spaces
#Assumption: exploratory data analysis has already been run to determine which elements are suitable for further statistical analysis
#Tip: over-limit values should represent less than 20% of the total values for a single element
#Usage: replace upper detection limits (UDLs) with Inf (e.g. Find/Replace in Excel)
#Save file as .csv and make sure all .csv values are converted to numbers showing sufficient decimal places.


install.packages("robCompositions")
#installs package in default library location; only need to do this the first time using the package or if you want to update

library (robCompositions) #load R package into current session

setwd("D:/Insert_File_Location") #set location of files and where they will be saved
UDL <- read.csv("Insert_File_Name.csv", header=TRUE) #bring csv file into R with first row set as the column names
UDLx <-UDL[c(36:70)] #create matrix to only show element columns (e.g., column number starts at 1 therefore column 36 to 70 will be extracted)

names(UDLx) #check all elements columns needed are included
summary(UDLx) #check min/max values to ensure all UDL were changed to Inf

res.lm <- imputeUDLs(UDLx, dl=c(0,0,0,0,0,0,0,0,0,10000.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100.00,0,0,0,0,0,0), method="lm", variation=TRUE)
#run UDL imputation where c(...) are the upper detection limit values with appropriate decimal places and 0 represents no UDL value
#Tip: setup UDL values in a row in Excel then copy/paste between the () then highlight spaces between the UDLs and type in ,
#Common Errors: missed column, wrong units, missing decimal places
#Ensure the UDL is in the same units as the values in the column

res.lm #check imputation

y=res.lm$x #extract imputed matrix
write.csv(unclass(y), file = "Insert_File_Name_impUDLilr.csv") #save imputed matrix

#only the element columns are saved and will have to be re-merged with the original file, the row order will remain the same
#QA/QC tip - keep all iterations of the data to check that the columns were merged correctly

rm(list=ls()) #clear workspace
