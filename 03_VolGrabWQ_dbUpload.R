# Prepare and import tidy volunteer data set into the Volunteer Water Quality Database.


# Install Packages
library(plyr)
library(dplyr)
library(reshape2)
library(psych)
library(RODBC)

# Data submission details
subid <- "0023" # NEEDS to be hand enetered here or added to the Excel file...shouldn't need to be a field in excel file uploaded to R
actorg <- "HRWG" # Sampling orgnanization abreviation from volunteer database organization table

#  INPUT  Remove the "#" from the line in front of the duplicate batch type the data represents
dbatch <- "Day"  # Duplicates batches are once a day without additional groupings
# dbatch <- "Day+Crew"  # Duplicates are done once a day by a sampling crew- multiple crews on one day
# dbatch <- "Sampler" # Duplicates done by a sampler at regular frequency, but not daily

########################
# Tidy water quality dataset details
dir <- "//deqlead02/Vol_Data/Hood River/2015"  #INPUT the directory you want to retrieve and write files to, change the text in the quotes



#################################################################################
#################################################################################
##                                                                             ##
##                                                                             ##
##    Activity Codes and Sample Batching for VolDB Tables                      ##  
##                                                                             ##
##                                                                             ##
#################################################################################
#################################################################################

###########################################################


# Load the data
load(paste0(dir,'/',subid,'-gdtidy.Rdata'))
# Load the project information
load(paste0(dir, subid,'-Project.RData'))




##### Generate Activity ID

act<-gdtidy[,c("LASAR","SiteIDcontext","DateTime","DupBatchKey","cmnt", "charid","ActType", "item", "actroot")] 
# get the columns I need...actualy don't need the actID column
str(act)

##################################################################################
#S,FP, and FD need to be translated into FM or LSR, FQMR or LQD, and QFQMR or QLQD


# need to convert ActType to lab/field specific activity type.
lab<- unique(prj$CharID[which(prj$FieldOrLab == "Lab")]) # List of lab parameters
field <- unique(prj$CharID[which(prj$FieldOrLab == "Field")]) # List of field parameters


# Generate Activity Type consistent with STORET codes....This code WILL NOT work correctly if parameter is both field and lab
for (i in seq_along(act$charid)){
  act$AT[i] <- if(act$ActType[i] == "S" && act$charid[i] %in% field){
    "FM"
  } else if(act$ActType[i] == "S" && act$charid[i] %in% lab){
    "LSR"
  } else if(act$ActType[i] == "FP" && act$charid[i] %in% field){
    "FQMR"
  } else if(act$ActType[i] == "FP" && act$charid[i] %in% lab){
    "LQD"
  } else if(act$ActType[i] == "FD" && act$charid[i] %in% field){
    "QFQMR"
  } else if(act$ActType[i] == "FD" && act$charid[i] %in% lab){
    "QLQD"
  } else "CheckActivityType"
}


##########################
# Generate Activity ID's #
##########################
for (i in seq_along(act$LASAR)){
  act$id[i] <- paste0(subid,"-",act$actroot[i],"-",act$AT[i])
}

########################
# Generate Result ID's #
########################

for (i in seq_along(act$LASAR)){
  act$rsltid[i] <- paste0(act$id[i],"-",act$charid[i])
}


######################################################
# Generate t.Result for upload to Volunteer Database #
######################################################

#merge act and prj2 with subset of gdtidy to get all the fields for t.Result
#####  Add required fields to gdtidy
prj2<-prj[,c("CharID","MethodShortName","UNITS", "MethodSpeciation","ANALYTICAL.ORGANIZATION","LOQ")]
names(prj2)<- c("charid", "mthd", "unit","MethodSpeciation","AnalyticalOrganization","LOQ")
t.rslt<- merge(gdtidy,prj2, all.x = TRUE) # use all.x TRUE to handle "D" data w/o methods.
act2<-act[,c("charid","actroot","LASAR","DateTime","id","rsltid","ActType")]
t.rslt2 <- merge(t.rslt, act2)

###################
#############
########
####
##  Test Data LOSS
length(t.rslt$r) == length(t.rslt2$r)
##  If False error in assigning activity ID's and Result ID's to gdtidy
####
#########
#############
####################

# Pull out the matching fields for the result table in database
t.result <- t.rslt2[,c("rsltid","id", "charid", "r","rqual","unit","mthd","MethodSpeciation","AnalyticalOrganization",
                       "LOQ", "acc", "prec","dql","prec_val","DEQ_prec", "cmnt")]
#rename columns to match the database.
names(t.result) <- c("ResultID", "ActivityID", "CharID", "Result", "RsltQual", "Unit","Method","MethodSpeciation",
                     "AnalyticalLaboratory","LOQ","QCorigACC","QCorigPREC","QCorigDQL","PrecisionValue","DEQ_PREC",
                     "Org_RsltComment")
t.result$RsltStatus <- "Preliminary"
str(t.result)
write.csv(t.result, file = paste0(dir,'/',subid, '-t.result.csv'))




##########################################################
#  Generate table of activity ID's vs. character counts  #
##########################################################
AidCid <- dcast(act, id~charid, length)# (A)ctivity (id)'s vs (C)haracter(id)'s count.

###################
#############
########
####
##  Test 
lapply(AidCid[,-1],max) # Display number of results/activity ID ---
##  All values should be <=1 or errors will ensue, similar to which(duplicated(act$rsltid))
####
#########
#############
####################



#######################################################################
# Summarise act by id and take first of what should be duplicate keys #
#######################################################################

by_id<- group_by(act, id)
actinfo <- summarise(by_id,
                     count = n(),
                     LASAR = first(LASAR),
                     DateTime = first(DateTime),
                     DupBatchKey = first(DupBatchKey),
                     acttype = first(AT),
                     actroot = first(actroot),
                     item = first(item))

actinfo$subid <- subid # add submission ID number for activity table

str(actinfo)

#########################################################################################
# Use the activity root to move the organization and DEQ comments into activity info df #
# Use ifelse statements to pull in other fields which may be poplulated in Excel file   #
#########################################################################################
# 
load(paste0(dir, '/', subid,'-gd.RData'))

for (i in seq_along(actinfo$id)){ # the ifelse statements are unnecessary now and should be removed to be consistent with gdtidy names.
  actinfo$site.desc[i]<- gd$site[which(gd$actroot == actinfo$actroot[i])]
  actinfo$siteIDcontext[i] <- ifelse("siteIDcontext" %in% names(gd), gd$siteIDcontext[which(gd$actroot == actinfo$actroot[i])], "DEQ")
  actinfo$OrgComnt[i] <- gd$Org_Comment[which(gd$actroot == actinfo$actroot[i])]
  actinfo$DEQcomnt[i] <- ifelse('DEQ_Comment' %in% names(gd), gd$DEQ_Comment[which(gd$actroot == actinfo$actroot[i])], NA)# in template
  actinfo$endDateTime[i] <- ifelse("endDateTime" %in% names(gd), gd$endDateTime[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$media[i] <- ifelse("media" %in% names(gd), gd$media[which(gd$actroot == actinfo$actroot[i])], "Water")
  actinfo$smplColMthd[i] <- ifelse("smplColMthd" %in% names(gd), gd$smplColMthd[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$smplColEquip[i] <- ifelse("smplColEquip" %in% names(gd), gd$smplColEqup[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$smplColEquipName[i] <- ifelse("smplColEquipName" %in% names(gd), gd$smplColEquipName[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$smplColEquipCommt[i] <- ifelse("smplColEquipComnt" %in% names(gd), gd$smplColEquipComnt[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$smplDpth[i] <- ifelse("smplDpth" %in% names(gd), gd$smplDpth[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$smplDpthUnit[i] <- ifelse("smplDpthUnit" %in% names(gd), gd$smplDpthUnit[which(gd$actroot == actinfo$actroot[i])], NA)
  actinfo$samplers[i] <- ifelse("samplers" %in% names(gd), gd$samplers[which(gd$actroot == actinfo$actroot[i])], NA) # in template
}

actinfo$endDateTime<- as.POSIXct(actinfo$endDateTime, origin = "1970-01-01")

actinfo$actorg <- actorg

t.Activity <- actinfo[,c("id","acttype", "subid", "LASAR","siteIDcontext", "site.desc", "DateTime", "endDateTime", "media", "actorg", "smplColMthd", 
                         "smplColEquip", "smplColEquipName", "smplColEquipCommt", "smplDpth", "smplDpthUnit", "OrgComnt","DEQcomnt", "samplers")]

names(t.Activity) <- c("ActivityID","ActivityType", "SubID", "SiteID", "SiteID_Context", "SiteDescription", "StartDateTime", "EndDateTime",
                       "Media", "ActivityOrg", "SmplColMthd", "SmplColEquip", "SmplEquipID", "SmplColEquipComment", "SmplDepth", "SmplDepthUnit", "Org_Comment",
                       "DEQ_Comment","Samplers")

write.csv(t.Activity, file = paste0(dir,'/',subid, '-t.Activity.csv'))



#####################################################################################################
#####################################################################################################
#
####                    OPEN RODBC CONNECTION TO VOLUNTEER DATABASE                              ####
#
#####################################################################################################
#####################################################################################################
# 
# 

ch <- odbcConnect("VolWQdb", case="nochange")
odbcGetInfo(ch)
sqlTypeInfo(ch)

############################################
##                                        ##
#         Activity Table                   #
##                                        ##
############################################

sqlSave(ch, t.Activity, tablename = "t_Activity", append = TRUE, rownames = FALSE, colnames = FALSE, safer = TRUE,
        varTypes = c("DATETIME", "VARCHAR"))



############################################
##                                        ##
#           Result Table                   #
##                                        ##
############################################

# save results as temp table

sqlSave(ch, t.result, tablename = "TempResult", append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE)

#Append results to t_Results

qry <- "INSERT INTO t_Result ( ResultID, ActivityID, CharID, Result, RsltQual, Unit, Method, MethodSpeciation, AnalyticalLaboratory, LOQ, LOQ_Unit, QCorigACC, QCorigPREC, QCorigDQL, PrecisionValue, DEQ_PREC, RsltStatus, Org_RsltComment )
SELECT [TempResult].ResultID, [TempResult].ActivityID, [TempResult].CharID, [TempResult].Result, [TempResult].RsltQual, [TempResult].Unit, [TempResult].Method, [TempResult].MethodSpeciation, [TempResult].AnalyticalLaboratory, [TempResult].LOQ, [TempResult].Unit, [TempResult].QCorigACC, [TempResult].QCorigPREC, [TempResult].QCorigDQL, [TempResult].PrecisionValue, [TempResult].DEQ_PREC, [TempResult].RsltStatus, [TempResult].Org_RsltComment
FROM TempResult;"

sqlQuery(ch, qry, max = 0, buffsize = length(t.result$Result))

# Delete temp table with results
sqlDrop(ch, "TempResult")


############################################################################################
### Generate Activity Groups and Junction Table Linking Activities to Activity Groups    ###
############################################################################################


if (dbatch == "Day") {
  ##########################################################################
  ## Activity Groups for daily batch
  # Activities grouped together based on daily duplicates, only one field set a day
  
  bhdate<- unique(substr(actinfo$id,0,13))
  t.ActGrp <- as.data.frame.character(bhdate)
  names(t.ActGrp) <- "ActGrpID"
  t.ActGrp$ActGrpType <- "QC Sample"
  t.ActGrp$ActGrpComment <- substr(t.ActGrp$ActGrpID,6,13)
  
  
  for (i in seq_along(bhdate)) {
    # subset actinfo based on date characters in id 
    tmp <- actinfo[which(substr(actinfo$id,0,13)== bhdate[i]),"id"]
    tmp$bhcmnt <- substr(bhdate[i],6,13)
    # create group act id
    tmp$bhdate <- bhdate[i]
    if (nrow(tmp) > 0) {
      ifelse(i == 1, bh.col <- tmp, bh.col <- rbind(bh.col, tmp))
    }
    rm(tmp)
  }
  names(bh.col) <- c("ActID","DupBatchKey","ActGrp")
  t.ActGrp2Act <- bh.col[,c("ActGrp", "ActID", "DupBatchKey")]
  rm(bh.col)
  
} else if (dbatch == "Day+Crew") {
  ##########################################################################
  ## Activity Groups for Daily samples with Multiple Crews
  # Activities grouped together based on crew specific daily duplicates
  
  # Activity Group code is subid-date-DupBatchKey
  t.ActGrp<- actinfo[,"DupBatchKey"]
  
  t.ActGrp$bhdate<- substr(actinfo$id,0,13)
  t.ActGrp$ActGrpID<- paste0(t.ActGrp$bhdate,"-",t.ActGrp$DupBatchKey)
  t.ActGrp <- ddply(t.ActGrp,~ActGrpID,summarise,dat=first(bhdate),dbk=first(DupBatchKey))
  t.ActGrp$ActGrpType <- "QC Sample"
  t.ActGrp <- t.ActGrp[,c("ActGrpID","ActGrpType","dbk")]
  names(t.ActGrp) <- c("ActGrpID", "ActGrpType", "ActGrpComment")
  
  for(i in 1:nrow(t.ActGrp)){
    #print(t.ActGrp$dbk[i])
    # subset actinfo "id" field by rows which match subid-YYYYMMDD and DupBatchKey fields.
    tmp <- actinfo[which((substr(actinfo$id,0,13)==t.ActGrp$dat[i]) & (actinfo$DupBatchKey == t.ActGrp$dbk[i])),c("id","DupBatchKey")]
    tmp$ActGrpID <- t.ActGrp$ActGrpID[i]
    if (nrow(tmp) > 0) {
      ifelse(i == 1, bh.col <- tmp, bh.col <- rbind(bh.col, tmp))
    }
    rm(tmp)
  }
  
  names(bh.col) <- c("ActID","DupBatchKey","ActGrpID")
  t.ActGrp2Act <- bh.col[,c("ActGrpID", "ActID", "DupBatchKey")]
  rm(bh.col)
  
} else if (dbatch == "Sampler") {
  ############################################################################
  ### Activity Grouping for Sampler Batch
  # Activities batched by sampler with duplicates collected over time
  
  bhsmplr<- unique(sort(unlist(strsplit(actinfo$DupBatchKey,","))))
  t.ActGrp <- as.data.frame.character(bhsmplr)
  names(t.ActGrp) <- "ActGrpComment"
  t.ActGrp$ActGrpType <- "QC Sample"
  t.ActGrp$ActGrpID <- paste0(subid, "-", t.ActGrp$ActGrpComment)
  t.ActGrp <- t.ActGrp[,c(3,2,1)]
  
  
  for (i in seq_along(bhsmplr)) {
    # subset actinfo based on partial match of batch info and needed columns
    tmp <- actinfo[grep(bhsmplr[i],actinfo$DupBatchKey),c("id","DupBatchKey")]
    # create group act id
    tmp$bhname <- paste0(subid,"-",bhsmplr[i])
    if (nrow(tmp) > 0) {
      ifelse(i == 1, bh.col <- tmp, bh.col <- rbind(bh.col, tmp))
    }
    rm(tmp)
  }
  names(bh.col) <- c("ActID","DupBatchKey","ActGrp")
  t.ActGrp2Act <- bh.col[,c("ActGrp", "ActID", "DupBatchKey")]
  rm(bh.col)
} else {
  Error.ActivityGroups <- "Error with Activity Grouping, check that 'dbatch' is assigned as Day, Day+Crew or Sampler at start of script"
}
write.csv(t.ActGrp, file = paste0(dir,'/',subid, '-t.ActGrp.csv'))
names(t.ActGrp2Act)<- c("ActGrpID", "ActivityID", "AG2AComment")
write.csv(t.ActGrp2Act, file = paste0(dir,'/',subid, '-t.ActGrp2Act.csv'))


############################################
##                                        ##
#       Activity Group Table               #
##                                        ##
############################################

sqlSave(ch, t.ActGrp, tablename = "TempActGrp", append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE)

qryAG <- "INSERT INTO t_ActGrp ( ActGrpID, ActGrpType, ActGrpComment )
SELECT TempActGrp.ActGrpID, TempActGrp.ActGrpType, TempActGrp.ActGrpComment
FROM TempActGrp;"

sqlQuery(ch, qryAG, max = 0, buffsize = length(t.ActGrp$ActGrpID))#2Act$ActGrpID))

sqlDrop(ch, "TempActGrp")



#################################################
##                                            ##
#  Activity to Activity Group Junction Table   #
##                                            ##
################################################

sqlSave(ch, t.ActGrp2Act, tablename = "TempAG2A", append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE)

qryAG2A <- "INSERT INTO tjct_ActGrp2Act ( ActGrpID, ActivityID, AG2AComment )
SELECT TempAG2A.ActGrpID, TempAG2A.ActivityID, TempAG2A.AG2AComment
FROM TempAG2A;"

sqlQuery(ch, qryAG2A, max = 0, buffsize = length(t.ActGrp2Act$ActGrpID))

sqlDrop(ch, "TempAG2A")
