###############################################
####  Prepare Excel file for import to R   ####
###############################################

# 1.  Rectify submission project information with VolWQdb submission, project, characterisitc, methods, units, speciation,
#     Field or Lab analysis, Analyytical organization Limit of quantitiation and low level QC limit.  Each of these must be 
#     valid values from their respective VolWQdb fields (except LOQ and LLQC limit).  

# 2.  Assign LASAR ID values to all stations in the Workbook

# 3.  Sample metadata must include the fields below and their names should match exactly (see 5 for conditional metadata fields):
       # 'site' the site description field, 
       # 'LASAR' the LASAR ID value 
       # 'SiteIDcontext' should be DEQ as a value for these, 
       # 'DateTime' in the format (yyyy-mm-dd HH:MM) and make sure it is not the formula of date + time (use copy as values in Excel),
       # 'DupBatchKey' used when date is not the only way duplicates are batched together, could be sampler abbreviation, equipment group, 
        # route number, etc.  For samples batched per day blank is OK.
       # 'DEQ_Comment' location where DEQ can insert comments that come up during Excel prep, if DEQ adds comments in the result comment 
        # fields they should be prefixed with 'DEQ- ',
       # 'Org_Comment' comments that were included in the submission by the submitting organization, and
       # 'samplers' blank is ok but many submissions will include this field.

# 4.  Each result field is designated by the prefix of the characteristic id (ca) and one or more suffixes.
      # CHARACTERISTIC ABBREVIATIONS (prefix): makes sure they match below shortened versions in the file CharNames.RData
      # Valid Values Charactersitic Abbreviations
       load("//deqlab1/WQM/Volunteer Monitoring/datamanagement/R/VolGrabWQ/CharNames.RData") 
      # Hope to not need path like //deqlab1/WQM/Volunteer Monitoring/datamanagement/R/VolGrabWQ/
       View(CharNames)  
       # The QC criteria for each characteristic also needs to be included in the QCcrit.csv file.

       QCcrit <- read.csv("//deqlab1/WQM/Volunteer Monitoring/datamanagement/R/VolGrabWQ/QCcrit.csv",header=TRUE, stringsAsFactors = FALSE)  # criteria for grading
       # duplicate data make sure it is in your folder, hope to not need //deqlab1/WQM/Volunteer Monitoring/datamanagement/R/VolGrabWQ/
       View(QCcrit)
       
      # SUFFIXES for each of the result fields.  Note each characteristic must have a field with the characteristic id prefix
       # and each of the suffixes below, Note include the '_' so temperature result would be 't_r':
       # "_r" the result value (numeric only)
       # "_qual" qualififer field primarily for < or >
       # "d_r" the duplicate value result (numeric only)
       # "d_qual" the duplicate value result qualifier ( < or >)
       # "_PREC" the organization assigend precision data quality level
       # "_ACC" the organization assigend accuracy data quality level
       # "_DQL" the overall data quality level assigned in the Excel file (usually by the organization) 
       # "_m" method valid value from VolWQdb.t_Method.ShortName
       # "_cmnt" comment field

# 5.  In some sampling regimes the additonal sample metadata fields may be required.  Each of these is defaulted to NA unless noted otherwise.
       # 'endDateTime' in the format (yyyy-mm-dd HH:MM) for samples that have both a start and end time
       # 'media' this needs to be a valid Storet sample media, the script defaults to 'Water'
       # 'smplColMthd' required field if not a field parameter.  'Direct Fill' or '2ndContainer' are options, see VolWQdb.t_Method.ShortName  
          # filter by the MethodType = Sample Collection.
       # 'smplColEquip' populated if the equipment has a specific ID number or name.
       # 'smplColEquipName' required when sample collection method is present see VolWQdb.t_Type.Type with TypeFilter = CollectionEquipmentName
       # 'smplColEquipComnt' Any additional comment about the sample collection equipment for teh activity
       # 'smplDpth' a text field for the depth of the sample collection, can be number or descriptor like 'bottom'
       # 'smplDpthUnit' required ('ft', or 'm') if the smplDpth field is numeric
       

# 6.  Remove any text characters from result fields and add a qualifier field for ">", "<", etc.  The value used is roughly 1/1000th of the 
#       reported level, so a <1 changes to 0.999 in the result field with a < in the qual field.  Save the result fields as numbers so 
#       qualified values don't get coerced to NA.
# 7.  Any "0" results are likely invalid and will kill the script if it is a parameter that has log plots or relative percent difference QC
#       calculations. Generally these should be converted to < the LOQ.  if there will be plotting on log axis it kills the operation. 


#####################################################
#010  Import Excel Data Using XLConnect Package ####
#####################################################



# Install Packages
library(XLConnect)  #For user manual use vignette("XLConnect") and help(XLConnect)
library(plyr)
library(dplyr)
library(reshape2)
library(psych)



#####################################################
# 020 Enter data needed to import files from Excel ##
#####################################################


#####################################################
#####################################################
#
#   ###   #   #  ###  # #  ###   ###
#    #    ##  #  # #  # #   #    #
#    #    # # #  ###  # #   #    ###
#    #    #  ##  #    # #   #      #
#   ###   #   #  #    ###   #    ###
# 
######################################################
######################################################

# Data submission details

subid <- "0232" # NEEDS to be hand enetered here or added to the Excel file...shouldn't need to be a field in excel file uploaded to R
actorg <- "MalSWCD" # Sampling orgnanization abreviation from volunteer database organization table

#  INPUT  Remove the "#" from the line in front of the duplicate batch type the data represents
 dbatch <- "Day"  # Duplicates batches are once a day without additional groupings

# dbatch <- "Day+Crew"  # Duplicates are done once a day by a sampling crew- multiple crews on one day
# dbatch <- "Sampler" # Duplicates done by a sampler at regular frequency, but not daily

 #A.Britson addition****
 #code to change QCcrit values, modify as necessary
 #need to change AbsDiff QC crit for sl
 
  QCcrit$A[QCcrit$charid=='sl' & QCcrit$QCcalc=="AbsDiff"]<-.01
  QCcrit$B[QCcrit$charid=='sl' & QCcrit$QCcalc=="AbsDiff"]<-.02
 
 #View(QCcrit)
 
 #(A.Britson)need to insert code to change QCcrit when units are different....perhaps need to add units to QCcrit file?
 #A.Britson out****
 
########################
# Excel Workbook details
dir <- "//deqlab1/Vol_Data/Malheur/2016_18"  #INPUT the directory you want to retrieve and write files to, change the text in the quotes
file <- "ForR_Malheur WC 2016-2018 .xlsx" # INPUT within the quotes the complete path of file





# Data worksheet details
sheet1 <- "data"  # INPUT for the name of the worksheet
#sr1 <- 1 # INPUT the row number for the start of the date, usually the header row

#nr1 <- 118  # INPUT the number of rows in your csv file

# Project information worksheet details
sheet2 <- "Project_Info" 
sr2 <- 6

nr2 <- 15



#####################################################
###############################################
#  Done with data input


######################################################
######################################################
#
###           EXCEL FILE PROCESSING
#
######################################################

##  Load the data to create data frame "gd"
#setwd(dir) #- directory is set in each load or save call
gdwb <- loadWorkbook(paste0(dir,'/',file))  #  loads workbook identified in "file" above

prj <- readWorksheet(gdwb, sheet = sheet2, startRow= sr2, endRow= nr2) 
gd <- readWorksheet(gdwb, sheet = sheet1) # Generates the dataframe with result data

str (gd)



#rename all the relevant header info 
names(gd)<-gsub("TEMP_","t_",names(gd))
names(gd)<-gsub("pH_", "ph_",names(gd))
names(gd)<-gsub("DO_" ,"do_",names(gd))
names(gd)<-gsub("SPCOND_", "sc_",names(gd))
names(gd)<-gsub("TURB_","tb_",names(gd))
names(gd)<-gsub("BACTERIA_","ec_",names(gd)) 
names(gd)<-gsub("_RESULT","_r",names(gd))
names(gd)<-gsub("_DUP","d_r",names(gd))
names(gd)<-gsub("_COMMENTS", "_cmnt",names(gd))
names(gd)<-gsub("_METHOD","_m",names(gd))
names(gd)<-gsub("_d","d_",names(gd))
names(gd)


############################
#  Add Items Column to df  #
############################
gd$item<- seq_along(gd$DateTime)

############################
# Generate Activity roots  #
############################

for (i in seq_along(gd$DateTime)){
  gd$actroot[i] <- paste0(strftime(gd$DateTime[i], format = "%Y%m%d%H%M"),"-",gd$LASAR[i])
}

###############################################
###  Confirm Names of uploaded files are correct
#####################

sufx <- c("_r", "_qual", "d_r", "d_qual", "_PREC", "_ACC", "_DQL", "_m", "_cmnt") # suffixes to add to each Character ID
charid <- unique(prj[,"CharID"]) # pull out vector of character id's
charlst <- lapply(charid, paste0, sufx) # create list of character id specific element names in gd
charlst


gdmeta <- c("LASAR", "SiteIDcontext","DateTime","DupBatchKey", "item", "actroot")
charsall <- lapply(charlst, append, gdmeta, after = 0) # creates a list of vectors each with all the headers to pull for each parameter
names(charsall) <- charid # name each list element with the characteristic
charsall

#### check that all column names in charsall match column names in gd...any FALSE's need to be fixed
###
##
#
unlist(charsall) %in% names(gd)
if (any(unlist(charsall) %in% names(gd) == FALSE)) {
  print(paste0('Input file miissing the following:  ', unlist(charsall)[which(unlist(charsall) %in% names(gd) == FALSE)]))
} 
#
##
###
#### If/when you have failures run names(gd) and count out against unlist(charsall)



#########################
#   *** REVIEW str    ***
#########################

#??? Code improvement...automatically search all fields ending in "_r" to make sure they are numeric or just trim and sub all.
# confirm that the data file has numeric values for all *_r and *d_r columns and no values were coerced to NA
# If you find result fields as character class may result from "" cells.  Try "count(gd$field) to see if have blanks

#gd$td_r <- sub("^$",NA, gd$td_r) # Replaces "" cells with NA.  Put the correct
#count(gd$td_r)
#gd$td_r <- as.numeric(gd$td_r)

###########################################################################
##  Check that all the result fields are numeric
##
str(gd[,grep("*_r", names(gd))])

for (i in (grep("*_r", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.numeric(gd[,i])
}

for (i in (grep("*_qual", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

for (i in (grep("*_PREC", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

for (i in (grep("*_ACC", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

for (i in (grep("*_DQL", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

for (i in (grep("*_m", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

for (i in (grep("*_cmnt", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

# Comment to character
for (i in (grep("*_Comment", names(gd)))){ ## Make all the result fields numeric
  gd[,i]<- as.character(gd[,i])
}

str(gd)
############################################################################

# count on needing to fix these with the Excel import.
#??? not sure if I need to change SubID and ActRoot to factors
#gd$TIME <- as.factor(gd$TIME)
#gd$site <- as.factor(gd$site)
gd$LASAR <- as.factor(gd$LASAR)
#gd$stn <- as.factor(gd$stn)
gd$site <- as.character(gd$site)
gd$Org_Comment[which(gd$Org_Comment=="")] <- NA
gd$Org_Comment <- as.character(gd$Org_Comment)
gd$DupBatchKey[which(gd$DupBatchKey=="")] <- NA

gd$samplers[which(gd$samplers=="")] <- NA
gd$samplers <- as.character(gd$samplers)

str(gd)




##########################################################################
##########################################################################
####                      DUPLICATE DATA PROCEDURES                   ####
##########################################################################
##########################################################################

##############################################################
# Generate table with duplicates in all fields (except item) #
#     Remove these duplicate data from gd dataframe          #
##############################################################

redundantdata<-gd[duplicated(gd[,c(names(gd[1:(length(names(gd))-2)]))]),] # pull out duplicated rows based on all columns except item
write.csv(redundantdata, file = paste0(dir,'/',subid,"RedundantData.csv")) # create csv of deleted rows of data
gd<-gd[!duplicated(gd[,c(names(gd[(1:length(names(gd))-1)]))]),] # clean duplicated rows from dataset

#############################################################################
# Generate table for duplicates in data results and DateTime/Station combos #
#############################################################################

redundantresults<-gd[which(duplicated(gd[,c(grep("_r",names(gd)),grep("actroot",names(gd)))])),] # vector with duplicates for actroot and data fields.
write.csv(redundantresults, file = paste0(dir,'/',subid,"RedundantResults.csv")) # create csv of data matching results at same station Datetime combo



########################################################
# Generate vector of duplicate DateTime/Station combos #
# Add "b" to redundant activity root fields            #
# then adds "c" to additional redundancy...see note    #
############### ########################################


redundantActRt <- gd[which(duplicated(gd$actroot)),]

# Add a "b" to the end of the actroots in gd dataframe

#for (i in which(duplicated(gd$actroot))){
#  gd$actroot[i]<- sub(gd$actroot[i],paste0(gd$actroot[i],"b"),gd$actroot[i])
#}
gd$actroot[which(duplicated(gd$actroot))] <- paste0(gd$actroot[which(duplicated(gd$actroot))],"b") # first duplicates
substr(gd$actroot[which(duplicated(gd$actroot))],
       nchar(gd$actroot[which(duplicated(gd$actroot))]),
       nchar(gd$actroot[which(duplicated(gd$actroot))]))<-"c" # second duplicates


# Add "b" to the end of the actroots in redundantActRt dataframe/csv file
for (i in seq_along(redundantActRt$actroot)){
  redundantActRt$actroot[i]<- sub(redundantActRt$actroot[i],paste0(redundantActRt$actroot[i],"b"),redundantActRt$actroot[i])
}
substr(redundantActRt$actroot[which(duplicated(redundantActRt$actroot))],
       nchar(redundantActRt$actroot[which(duplicated(redundantActRt$actroot))]),
       nchar(redundantActRt$actroot[which(duplicated(redundantActRt$actroot))]))<-"c" 

write.csv(redundantActRt, file = paste0(dir,'/',subid,"RedundantActRt.csv")) # create csv of duplicated datetime/station combos

# # # # This needs to be more robust using max Freq of table(gd$actroot) to run substr on duplicated adding letters[1:maxduplicated]



############################################################################
####                                                                    ####
####        Reshape messy flat file to tidy Columnar file               ####
####                                                                    ####
############################################################################
# Get messy gd data frame element names to extract to tidy columnar data

gdl<-gd[0,charsall[[1]]] # create empty data frame for long format of data
charid <- as.vector(character())
gdl<- cbind(gdl,charid)
gdl$charid<-as.character(gdl$charid)
names(gdl)<- c(gdmeta,"r","rqual","dr","drqual","prec", "acc","dql","mthd","cmnt","charid") # rename columns to generic names for all parameters
str(gdl)


# Stack results and duplicate results in long format by each character ID
for (x in 1:length(charsall)) {
  # subset gd based on columns listed in charsall
  tmp <- gd[,charsall[[x]]]
  names(tmp) <- c(gdmeta,"r","rqual","dr","drqual","prec", "acc","dql","mthd","cmnt")
  tmp$charid <- names(charsall[x])
  if (nrow(tmp) > 0) {
    ifelse(x == 1, gdl <- tmp, gdl <- rbind(gdl, tmp))
  }
  rm(tmp)
}

# remove blank results
gdl <- gdl[!is.na(gdl$r),]
str(gdl)

#### Populate method columns to default methods from project dataframe
for (i in seq_along(gdl$item)) { 
  if(is.na(gdl$mthd[i])) gdl$mthd[i] <- prj$MethodShortName[which(prj$CharID == gdl$charid[i])] }
# NOTE- Risk this method only works if there is only one method for each characteristic.


# Create activity type for samples and field primaries
for(i in seq_along(gdl$r)){
  gdl$ActType[i] <- if(is.na(gdl$dr[i])){ 
    "S"} else {
      "FP"}}

##################################################
### Create QC Data Constants, Criteria, etc. #####
##################################################

## Duplicate QC calculation (gdl$QCcalc) methods

charAD <- c("t", "ph", "w", "do", "dos")  # character ID's using absolute difference as QC calculation method

charLD <- c("ec", "ent", "fc") # character ID's using log difference as QC calculation method
#QCcrit <- read.csv("QCcrit.csv",header=TRUE)  # criteria for grading duplicate data make sure it is in your folder
names(QCcrit) <- c("charid","QCcalc", "A","B","Source") # 4/22/19 cut out "Unit",
# all other parameters use relative percent difference for duplicate QC calculations
#unless the low level criteria (prj$LowLevQClimit) are met

## Duplicate Difference Criteria


# Create QC type for all field primary records
for (i in seq_along(gdl$r)) {
  gdl$QCcalc[i] <- if (gdl$ActType[i] %in% "S"){# if it is not a duplicate put "NA" in QCcalc
    "NA"
  } else if (gdl$charid[i] %in% charLD) { # if the characteristic is on the list of Log Diff characteristics 
    "LogDiff"
  } else if (gdl$charid[i] %in% charAD) { # if the characteristic is on the list of Absolute difference charactistics
    "AbsDiff"
  } else if (mean(c(gdl$r[i],gdl$dr[i])) > (prj$LowLevQClimit[which(gdl$charid[i]==prj$CharID)])) { # if it is on neither list
    #then it uses RPD if it is not a low level AbsDiff result depending on how it compares to the LowLevelQClimit in the project info
    #for conductivity or other parameters that do not have a low limit "0" must be in the prj$LowLevQClimit for the char, NA will break the code.
    "RPD"
  } else "AbsDiff"
}

# For all duplicates add the gdl$prec_val with the appropriate LogDiff, AbsDiff, or RPD value.
for (i in seq_along(gdl$r)) {
  gdl$prec_val[i] <- if (gdl$QCcalc[i] %in% "NA"){
    "NA"
  } else if (gdl$QCcalc[i] %in% "LogDiff") {
    log10(gdl$r[i])-log10(gdl$dr[i]) # calculate log difference
  } else if (gdl$QCcalc[i] %in% "AbsDiff") {
    gdl$r[i]-gdl$dr[i] # calculate absolute difference
  } else if (gdl$QCcalc[i] %in% "RPD") {
    (gdl$r[i]-gdl$dr[i])/mean(c(gdl$r[i],gdl$dr[i])) # calculate relative percent difference
  } else "99999"
}
# convert precision values to numeric values
gdl$prec_val <- as.numeric(gdl$prec_val) # warning about coercion seems to be OK...can do a count before and after to see # of NA's


# Change QC criteria when the abslolute difference criteria is less than the limit of quantitation
for (i in which(QCcrit$charid %in% prj$CharID & QCcrit$QCcalc %in% 'AbsDiff')) {
  if (!is.na(prj$LOQ[which(prj$CharID %in% QCcrit$charid[i])]) & (QCcrit$A[i] < prj$LOQ[which(prj$CharID %in% QCcrit$charid[i])])){
    QCcrit$A[i] <- as.numeric(prj$LOQ[which(prj$CharID %in% QCcrit$charid[i])])
    QCcrit$B[i] <- 2 * prj$LOQ[which(prj$CharID %in% QCcrit$charid[i])]
    QCcrit$Source[i] <- 'Precision criteria based on submissions LOQ, A <= LOQ, B <= 2LOQ'
  }
}




# Determine DQL based on comparison of precision value to QC criteria
for (i in seq_along(gdl$r)) {
  gdl$DEQ_prec[i] <- if(gdl$QCcalc[i] == "NA"){
    "NA"
  } else ifelse(abs(gdl$prec_val[i]) <= (QCcrit$A[which(QCcrit$charid == gdl$charid[i] & QCcrit$QCcalc == gdl$QCcalc[i])]), "A",
                (ifelse(abs(gdl$prec_val[i]) <= (QCcrit$B[which(QCcrit$charid == gdl$charid[i] & QCcrit$QCcalc == gdl$QCcalc[i])]), "B","C")))
}


############   Tidy the dataset so samples, primaries, and duplicates are all in the same result column
# Stack up the FD's under the rest of the data. and remove the dr column

# create dataframe with primary result fields removed
fd <- gdl[,c("actroot","LASAR","SiteIDcontext","DateTime","DupBatchKey","item", "dr","drqual","prec","acc","dql","mthd","cmnt","charid","ActType","QCcalc","prec_val","DEQ_prec")]
fd <- fd[!is.na(fd$dr),]
# label results as field duplicates
fd$ActType <- "FD"
# rename columns to generic names
names(fd)<- c("actroot","LASAR","SiteIDcontext","DateTime","DupBatchKey","item", "r","rqual","prec","acc","dql","mthd","cmnt","charid","ActType","QCcalc","prec_val","DEQ_prec")
# create dataframe without duplicates
s_fp <- gdl[,c("actroot","LASAR","SiteIDcontext","DateTime","DupBatchKey","item", "r","rqual","prec","acc","dql","mthd","cmnt","charid","ActType","QCcalc","prec_val","DEQ_prec")]
# bind the duplicate data rows to the bottom of the sample and field primary rows
gdtidy <- rbind(s_fp,fd)

save(charlst, file = paste0(dir,'/',subid,'-charlst.RData'))

save(QCcrit, file = paste0(dir,'/',subid,'-QCcrit.RData'))

save(charAD, file = paste0(dir,'/',subid,'-CharAD.RData'))
save(charLD, file = paste0(dir,'/',subid,'-CharLD.RData'))
save(gdtidy, file = paste0(dir,'/',subid,'-gdtidy.RData'))
save(prj, file = paste0(dir,'/',subid,'-Project.RData'))
save(gdl, file = paste0(dir,'/',subid,'-gdl.RData'))
save(gd, file = paste0(dir,'/',subid,'-gd.RData')) # file needed for activity and result meta data



########################################################################################################
########################################################################################################
####                                                                                                ####
####                 gdtidy is now ready for running stats and charts                               ####
####                                                                                                ####
########################################################################################################
########################################################################################################