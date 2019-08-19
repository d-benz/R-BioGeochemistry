#Principal Component Analysis Workflow including:
#Imputation of below detection limit (BDL) values, calculation of centred log-ratio (clr) values and RQ-Principal Component Analysis
#ImputeBDLS: "Parametric replacement of values above upper detection limit for compositional data using classical and robust methods based on ilr-transformations with special choice of balances." robCompositions package
#clr: "Undertakes a centred log-ratio transformation to remove the effects of closure in a data matrix." rgr package
#RQ-PCA: "The function carries out a Principal Components Analysis (PCA) and estimates the Mahalanobis distances for a compositional dataset and places them in an object to be saved and post-processedfor display and further manipulation." rgr package

#SETUP
#Create data file with top row for column headers with names that are alphanumeric and only use the _ symbols if needed with no spaces
#Assumption: exploratory data analysis has already been run to determine which elements are suitable for further statistical analysis
#Tip: below-limit values should represent less than 40% of all values and less than 40% within a single column or row (imputation method uses all rows & columns to calculate an appropriate BDL value)
#Usage: replace below detection limits (BDLs) with 0 (a.k.a., zero; e.g., Find/Replace in Excel) and avoid use on <50 samples
#Convert all values to the same units (e.g., ppm) and remove the unit descriptor from the column header (because PCA results have no units and it will make the biplot neater/accurate)
#Save file as .csv and make sure all .csv values are converted to numbers showing sufficient decimal places.

install.packages("robCompositions")
#installs package in default library location; only need to do this the first time using the package or if you want to update
install.packages("rgr")
#installs package in default library location; only need to do this the first time using the package or if you want to update

library (rgr) #load rgr package into current session
library (robCompositions) #load robCompositions package into current session

setwd("D:/Insert_File_Location") #set location of files and where they will be saved
cen <- read.csv("Insert_File_Name.csv", header=TRUE) #bring csv file into R with first row set as the column names

#Impute BDL values
#**robCompositions impRZilr** robCompositions package (all <DL to zero, type in lower DL for each column)
# 0 (zero) for <DL and max value plus half the UDL for over-limits if they represent less than 5% of the data-if not using imputed over-limit values

x <-cen[c(5:10)] #create matrix to only show element columns (e.g., column number starts at 1 therefore column 5 to 10 will be extracted)

names(x) #check all elements columns needed are included
summary(x) #check min/max values to ensure all BDL were changed to 0

xia <- impRZilr(x, dl=c(10,1000,10,0.2,1000,10,0.5,10000,10,1,2,20,2,10,0.5,1,
                        1,4,0.4,10,3,10,0.4,1000,100,10,1000,2,5,10,3000,5,0.1,2,0.2,20,5,10,2,50,1,
                        200,0.5,0.1,0.5,20,4,100,20), 
                eps=0.01, method="lm")#Proof2018Veg
#run BDL imputation where c(...) are the lower detection limit values with appropriate decimal places and units
#Tip: setup BDL values in a row in Excel then copy/paste between the () then highlight spaces between the BDLs and type in ,
#Common Errors: missed/extra column, wrong units, missing decimal places
#Ensure the BDL is in the same units as the values in the column

xia$x #check imputation

y=xia$x #extract imputed matrix
write.csv(unclass(y), file = "Insert_File_Name_impBDLilr.csv") #save imputed matrix


#**To clr**
y -> x

matrix <- as.matrix(x) #convert x to a matrix
summary(matrix) #check conversion

z <- clr(matrix) #to clr using rgr package
summary(z) #check conversion

write.csv(z, file = "Insert_File_Name_clr.csv") #save clr results
#only the element columns are saved and will have to be re-merged with the original file, the row order will remain the same
#QA/QC tip - keep all iterations of the data to check that the columns were merged correctly

#**Classical PCA on raw values** rgr package (don't use clr transformation, gx.mva.closed will open the values prior to PCA)
save.pca <- gx.mva.closed(as.matrix(x, proc = "mcd", 
                                    wts = NULL))#clr PCA

#Display Mahalanobis distances (right click graph to copy/paste into Paint or other program as a metafile)
gx.md.plot(save.pca, main = "", ifadd = c(0.98, 0.95, 0.9), cexf = 0.6, cex = 0.8)
gx.md.print(save.pca, pcut = 0.05)

#Display PCA Results (right click graph to copy/paste into Paint or other program as a metafile)
gx.rqpca.screeplot(save.pca, main = " ") #choose first few PCs before elbow for rotation or interpretation with the exception of dataset with <100 samples
gx.rqpca.loadplot(save.pca, main = " ", cex.main=0.8)

#Biplot (right click graph to copy/paste into Paint or other program as a .metafile)
gx.rqpca.plot(save.pca, main = " ", v1 = 1, v2 = 2, rplot=TRUE, qplot = TRUE, 
              rowids = TRUE, cex.lab = 0.9, cex.main = 0.9, rcex = 1, qcex = 0.8, rcol = 2, 
              qcol = 1) #displays biplot of PC1 versus PC2 (change V2 =  3 to display PC1 versus PC3 ... etc. for PCs of interest)
#check for adequate separation of elements
#use this plot, inconjuction with loadings, for interpretation

#Write PCA results to csv files
names(save.pca) #check saved PCA files

rload=save.pca$rload #extract loadings
write.csv(unclass(rload), file = "Insert_File_Name_loadings.csv") #save loadings
#for small datasets (<100 samples) only use loadings with 0.6 or greater values for interpretation (see Benz 2017 thesis for more details)

scores= save.pca$rqscore #extract scores
write.csv(unclass(scores), file = "Insert_File_Name_scores.csv") #save scores
#only the element columns are saved and will have to be re-merged with the original file, the row order will remain the same
#QA/QC tip - keep all iterations of the data to check that the columns were merged correctly
#use these values, merged with locations, for displaying spatial trends

eval= save.pca$eigenvalues #extract eigenvalues
write.csv(unclass(eval), file = "Insert_File_Name_eigenvalues.csv") #save eigenvalues
#for small datasets (<100 samples) loading value is a better indicator than an eigenvalue >= 1, only use loadings with 0.6 or greater values for interpretation (see Benz 2017 thesis for more details)

rm(list=ls()) #clear workspace