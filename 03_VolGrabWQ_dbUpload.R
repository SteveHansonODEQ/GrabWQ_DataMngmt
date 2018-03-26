# Prepare and import tidy volunteer data set into the Volunteer Water Quality Database.


# Install Packages
library(plyr)
library(dplyr)
library(reshape2)
library(psych)
library(RODBC)
library(lubridate)

# Data submission details
subid <- "0036" # NEEDS to be hand enetered here or added to the Excel file...shouldn't need to be a field in excel file uploaded to R
actorg <- "MSWCD" # Sampling orgnanization abreviation from volunteer database organization table

#  INPUT  Remove the "#" from the line in front of the duplicate batch type the data represents
 dbatch <- "Day"  # Duplicates batches are once a day without additional groupings
# dbatch <- "Day+Crew"  # Duplicates are done once a day by a sampling crew- multiple crews on one day
# dbatch <- "Sampler" # Duplicates done by a sampler at regular frequency, but not daily

########################
# Tidy water quality dataset details
dir <- "//deqlab1/Vol_Data/Pudding/2009_2010/deq10submit"  #INPUT the directory you want to retrieve and write files to, change the text in the quotes



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
load(paste0(dir, '/',subid,'-Project.RData'))
# Load summary data
DatSum <- read.csv(paste0(dir,'/', subid,'_DataSummary.csv'),header=TRUE)



##### Generate Activity ID

act<-gdtidy[,c("LASAR","SiteIDcontext","DateTime", "DupBatchKey","cmnt", "charid","ActType", "item", "actroot")] 
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
  actinfo$startTimeZone [i] <- ifelse("startTimeZone" %in% names(gd), gd$startTimeZone[which(gd$actroot == actinfo$startTimeZone[i])], NA) # empty start time zone
  actinfo$endTimeZone [i] <- ifelse("endTimeZone" %in% names(gd), gd$endTimeZone[which(gd$actroot == actinfo$endTimeZone[i])], NA)
}


###########################
# Generate Timezone field #
###########################

   #  ##  ##
  ## #   #
 ###  #   #  UMES STATION IS IN 'America/Los_Angeles' TIME ZONE !!
#  #   #   #
#  # ##  ##


actinfo$startTimeZone <- as.character(ifelse(dst(actinfo$DateTime) == T,"PDT", "PST"))

actinfo$endDateTime<- as.POSIXct(actinfo$endDateTime, origin = "1970-01-01")
actinfo$endTimeZone <- as.character(ifelse(dst(actinfo$endDateTime) == T,"PDT", "PST"))


actinfo$actorg <- actorg

t.Activity <- actinfo[,c("id","acttype", "subid", "LASAR","siteIDcontext", "site.desc", "DateTime", "startTimeZone", "endDateTime", "endTimeZone", 
                         "media", "actorg", "smplColMthd", "smplColEquip", "smplColEquipName", "smplColEquipCommt", "smplDpth", "smplDpthUnit", 
                         "OrgComnt","DEQcomnt", "samplers")]

names(t.Activity) <- c("ActivityID","ActivityType", "SubID", "SiteID", "SiteID_Context", "SiteDescription", "StartDateTime", "StartDateTimeZone",
                       "EndDateTime", "EndDateTimeZone", "Media", "ActivityOrg", "SmplColMthd", "SmplColEquip", "SmplEquipID", "SmplColEquipComment", 
                       "SmplDepth", "SmplDepthUnit", "Org_Comment", "DEQ_Comment","Samplers")

write.csv(t.Activity, file = paste0(dir,'/',subid, '-t.Activity.csv'))




     ##################################################
     ######                                      ######
     ###                                            ###
     #           Apply Anomaly Test Criteria          #
     ##                                            ###
     ######                                      ######
     ##################################################

AnomCrit <- read.csv("AnomCrit.csv") # criteria for code looking for outliers


##############################################################
##  Create dataframes specific to different Anomaly Tests  

# Real range values for both low and high range
AnomCRR<- AnomCrit[which(!is.na(AnomCrit$RealRngL) & !is.na(AnomCrit$RealRngU)),c("charid", "RealRngL", "RealRngU")]

# Real range values with no lower limit, upper limit only
AnomCRRU<- AnomCrit[which(is.na(AnomCrit$RealRngL) & !is.na(AnomCrit$RealRngU)),c("charid", "RealRngL", "RealRngU")]

# Ambient ranges criteria
AnomCAmb<- AnomCrit[which(!is.na(AnomCrit$amb01L) & !is.na(AnomCrit$amb05L) & !is.na(AnomCrit$amb95U) & !is.na(AnomCrit$amb99U)),
                    c("charid","amb01L","amb05L","amb95U","amb99U")]

# WQ standard
AnomCwqs<- AnomCrit[which(!is.na(AnomCrit$stdL) | !is.na(AnomCrit$stdU) | !is.na(AnomCrit$stdLlp) | !is.na(AnomCrit$stdUlp)),
                    c("charid","stdL","stdU","stdLlp","stdUlp")]

# Limit of Quantitation criteria
AnomLOQ<- prj[which(!is.na(prj$LOQ)),c("CharID","LOQ")]

#####################################################
#Create empty dataframe to past anomaly into
t.Anomaly <- data.frame(rsltid = as.character(),anom =as.character(), stringsAsFactors = FALSE)


#####################################################
# Run nested for loop to cycle through each characteristic and each anomaly test
for (i in unique(prj$CharID)) {  # i is a character id abbeviation
  tmpr<- t.rslt2[which(t.rslt2$charid == i),c("rsltid","LASAR","r","rqual")]; # create temp df of just i charid
  for (j in seq_along(tmpr$r)){ # j is a row number
    rid <- tmpr$rsltid[j] # result id value for specific result
    r <- tmpr$r[j] # specific result
    l <- as.character(tmpr$LASAR[j]) # Station number for specific result
    
    ##########################################
    # Real Range Test Anomaly Tests
    if (i %in% AnomCRRU$charid){
      if(r > AnomCRRU$RealRngU[which(AnomCRRU$charid == i)]){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "OverUpRange"))
      }
    } else if (i %in% AnomCRR$charid){
      if(r < AnomCRR$RealRngL[which(AnomCRR$charid == i)] | r > AnomCRR$RealRngU[which(AnomCRR$charid == i)] ){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "OutOfRange"))
      }
    }
    
    ##########################################
    # Limit of Quanitation (LOQ) Anomaly Test
    if (i %in% AnomLOQ$CharID){
      LOQ <- AnomLOQ$LOQ[which(AnomLOQ$CharID == i)];
      if(r < LOQ-(LOQ*0.1)){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "BelowLOQ"))
      };
      rm(LOQ)
    }
    
    ######################################
    # Submission specific Anomaly tests
    CharSum <- DatSum[DatSum$Station == "All",]
    tmpc <- CharSum[CharSum$Characteristic == i,] # CharSum with only the character id of i
    # test for submission outliers
    if (r < tmpc$X5th..){ # if r is less than 5% for submission
      t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Lowest5%SubmValues"))
    } else if (r < tmpc$X10th..) { # if r is less than 10% for submission
      t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Lowest10%SubmValues"))
    } else if (r > tmpc$X95th..){ # if r is higher than 95th percentile of submission
      t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Highest95%SubmValues"))
    } else if (r > tmpc$X90th..){ # if r is higher than 90th percentile of submission
      t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Highest90%SubmValues"))
    }
    rm(tmpc)
    
    # test for station specific submission outliers
    tmpc <- DatSum[DatSum$Station == l & DatSum$Characteristic == i,]
    if (tmpc$Count >= 10) {
      if (r < tmpc$X5th..) {
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Lowest5%SubmStnValues"))
      } else if (r > tmpc$X95th..){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Highest95%SubmStnValues"))
      }
    }
    rm(tmpc)
    
    ###############################################
    # Test against ambient parameter ranges anomalies
    if (i %in% AnomCAmb$charid){ #only run these tests if character id included in ambient list
      tmpc <- AnomCAmb[AnomCAmb$charid == i,]
      if (r < tmpc$amb01L){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Lowest1%AmbientValues"))
      } else if (r < tmpc$amb05L) {
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Lowest5%AmbientValues"))
      } else if (r > tmpc$amb99U) {
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Highest99%AmbientValues"))
      } else if (r > tmpc$amb95U){
        t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "Highest95%AmbientValues"))
      }
      rm(tmpc)
    }
    
    ###############################################
    # Test against water quality standards anomalies
    # Standards 1 low crit, 2 low crits, 1 range, 2 ranges, 1 high crit, 2 high crit
    if (i %in% AnomCwqs$charid) {
      tmpc <- AnomCwqs[AnomCwqs$charid == i,]; 
      if (!is.na(tmpc$stdL)) {
        if (r < tmpc$stdL) {
          t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "ViolatesWQStandard"))
        } else (if (!is.na(tmpc$stdLlp)) {
          if (r < tmpc$stdLlp) {
            t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "ViolatesLessProtectiveWQStandard"))
          }
        })
      }
      rm(tmpc)
    }
    
    if (i %in% AnomCwqs$charid) {
      tmpc <- AnomCwqs[AnomCwqs$charid == i,]; 
      if (!is.na(tmpc$stdU)) {
        if (r > tmpc$stdU) {
          t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "ViolatesWQStandard"))
        } else (if (!is.na(tmpc$stdUlp)) {
          if (r > tmpc$stdUlp) {
            t.Anomaly<- rbind(t.Anomaly, data.frame(rsltid = rid, anom = "ViolatesLessProtectiveWQStandard"))
          }
        })
      }
      rm(tmpc)
    }
  }
  rm(tmpr)
  rm(i)
  rm(j)
  rm(rid)
  rm(l)
}

names(t.Anomaly) <- c("ResultID", "AnomalyType")

write.csv(t.Anomaly, file = paste0(dir,'/',subid, '-t.anomaly.csv'))



#####################################################################################################
#####################################################################################################
#
####                    OPEN RODBC CONNECTION TO VOLUNTEER DATABASE                              ####
#
#####################################################################################################
#####################################################################################################
# 
# 


ch <- odbcConnectAccess("//deqlab1/wqm/Volunteer Monitoring/datamanagement/VolWQdb.mdb", case="nochange")
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



############################################
##                                        ##
#           Anomaly Table                  #
##                                        ##
############################################

sqlSave(ch, t.Anomaly, tablename = 'TempAnom', append = FALSE, rownames = FALSE, colnames = FALSE, safer = TRUE, verbose = FALSE)

qryAnom <- 'INSERT INTO t_Anomaly ( ResultID, AnomalyType )
SELECT TempAnom.ResultID, TempAnom.AnomalyType
FROM TempAnom;
'
sqlQuery(ch, qryAnom, max = 0, buffsize = length(t.Anomaly$ResultID))

sqlDrop(ch, 'TempAnom')



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
  t.ActGrp <- t.ActGrp[,c("ActGrpID", "dat", "ActGrpType","dbk")]
  names(t.ActGrp) <- c("ActGrpID", "dat", "ActGrpType", "ActGrpComment")
  
  for(i in 1:nrow(t.ActGrp)){
    #print(t.ActGrp$dbk[i])
    # subset actinfo "id" field by rows which match subid-YYYYMMDD and DupBatchKey fields.
    tmp <- actinfo[which((substr(actinfo$id,0,13)==t.ActGrp$dat[i]) & (actinfo$DupBatchKey == t.ActGrp$ActGrpComment[i])),c("id","DupBatchKey")]
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


########
####
##
#  Be sure to close the connection after finishing
odbcClose(ch)
#
##
####
########


##    ####  #   #  ###
# #   #  #  ##  #  #
#  #  #  #  # # #  ##
#  #  #  #  #  ##  #
##    ####  #   #  ####
