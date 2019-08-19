#Imputation of missing values in compositional data
#"This function offers several k-nearest neighbor methods for the imputation of missing values in compositional data." robCompositions package
#The Aitchison metric should be chosen when dealing with compositional data, the Euclidean metric otherwise.

#SETUP
#Create data file with top row for column headers with names that are alphanumeric and only use the _ symbols with no spaces
#Assumption: exploratory data analysis has already been run to determine which elements are suitable for further statistical analysis
#Tip: missing values should represent less than 20% of the total values for a single element
#Usage: replace missing values with NA (e.g. Find/Replace in Excel)
#Save file as .csv and make sure all .csv values are converted to numbers showing sufficient decimal places.

install.packages("robCompositions")
#installs package in default library location; only need to do this the first time using the package or if you want to update

library (robCompositions) #load R package into current session

setwd("D:/Insert_File_Location") #set location of files and where they will be saved
MISS <- read.csv("Insert_File_Name.csv", header=TRUE) #bring csv file into R with first row set as the column names
MISSx <-MISS[c(12:23)] #create matrix to only show element columns (e.g., column number starts at 1 therefore column 12 to 23 will be extracted)

names(MISSx) #check all elements columns needed are included
summary(MISSx) #check min/max values to ensure all missing values were changed to NA

MISSimp <- impKNNa(MISSx, method = "knn", k = 3, metric = "Aitchison",
                   agg = "median", primitive = TRUE, normknn = TRUE, das = FALSE,
                   adj = "median")
#run k-nearest neighbour imputation using the Aitchinson metric

MISSimp$xImp #check imputation

y=MISSimp$xImp #extract imputed matrix
write.csv(unclass(y), file = "Insert_File_Name_imp-miss.csv") #save imputed matrix

#only the element columns are saved and will have to be re-merged with the original file, the row order will remain the same
#QA/QC tip - keep all iterations of the data to check that the columns were merged correctly

rm(list=ls()) #clear workspace