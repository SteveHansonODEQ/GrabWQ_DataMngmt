# Input a tidy dataset from 01_VolGrabWQ_XLinTidyOut.R to get summary stats and plots of data and duplicates


#  Capture the default par settings
par.default <- par()
# Install Packages
library(plyr)
library(dplyr)
library(reshape2)
library(psych)



########################################################
# 020 Enter data needed to import tidy dataset from R ##
########################################################


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
subid <- "0096" # NEEDS to be hand enetered here or added to the Excel file...shouldn't need to be a field in excel file uploaded to R
actorg <- "TLBP" # Sampling orgnanization abreviation from volunteer database organization table


########################
# Tidy water quality dataset details
dir <- "//deqlab1/Vol_Data/TenmileLake/2012_15wrkCpy/Trib"  #INPUT the directory you want to retrieve and write files to, change the text in the quotes
#dat <- paste0(dir,'/',subid,'-gdtidy.Rdata')

############################################################################
############################################################################
############################################################################
####                                                                    ####
####                    Create Data Summaries                           ####
####                                                                    ####
############################################################################
############################################################################
############################################################################

load(paste0(dir,'/',subid,'-gdtidy.Rdata'))
load(paste0(dir,'/',subid,'-Project.Rdata'))
load(paste0(dir,'/',subid,'-charlst.Rdata'))
load(paste0(dir,'/',subid,'-gdl.Rdata'))
QCcrit <- read.csv("QCcrit.csv",header=TRUE)  # criteria for grading duplicate data make sure it is in your folder
names(QCcrit) <- c("charid","QCcalc", "A","B","Source")



# Summarize characteristics, combining all stations.
CharSum<-ddply(gdtidy[which(gdtidy$ActType == "S" | gdtidy$ActType == "FP"),],"charid",summarise,
               N = length(r),
               Mean = mean(r),
               GeoMean = geometric.mean(r),
               Min = min(r),
               Fifth = quantile(r,probs=.05),
               Tenth = quantile(r,probs=.1),
               Twentieth = quantile(r,probs=.2),
               TwentyFifth = quantile(r,probs=.25),
               Median = quantile(r,probs=.5),
               SeventyFifth =quantile(r,probs=.75),
               Eightieth = quantile(r,probs=.8),
               Ninetieth = quantile(r,probs=.9),
               NinetyFifth = quantile(r,probs=.95),
               Max = max(r))

# Get station name of "All" for the summary above
LASAR<-rep("All",times=length(charlst))
CharSum<-cbind(LASAR,CharSum)


# Generate summary for characteristics for each station
SampleSummary<-ddply(gdtidy[which(gdtidy$ActType == "S" | gdtidy$ActType == "FP"),],c("LASAR","charid"),summarise,
                     N = length(r),
                     Mean = mean(r),
                     GeoMean = geometric.mean(r),
                     Min = min(r),
                     Fifth = quantile(r,probs=.05),
                     Tenth = quantile(r,probs=.1),
                     Twentieth = quantile(r,probs=.2),
                     TwentyFifth = quantile(r,probs=.25),
                     Median = quantile(r,probs=.5),
                     SeventyFifth =quantile(r,probs=.75),
                     Eightieth = quantile(r,probs=.8),
                     Ninetieth = quantile(r,probs=.9),
                     NinetyFifth = quantile(r,probs=.95),
                     Max = max(r))
DataSummary<-rbind(CharSum,SampleSummary)
names(DataSummary) = c("Station", "Characteristic","Count", "Mean","GeoMean", "Min", "5th %", "10th %", "20th %","25th %", "Median", "75th %","80th %", "90th %", "95th %", "Max")
write.csv(DataSummary, file = paste0(dir,'/',subid,'_DataSummary.csv'))


############################################################################
####                                                                    ####
####                          Create QC Summaries                       ####
####                                                                    ####
############################################################################
####    Create QC Summaries by charid

# Create summary statistics for each station using same statistics (minus a few percentiles) as for result data
qcsum<-ddply(gdtidy[which(gdtidy$ActType == "FD"),],c("charid","QCcalc"),summarise,
             N = length(prec_val),
             Mean = mean(prec_val),
             Min = min(prec_val),
             Fifth = quantile(prec_val,probs=.05),
             Tenth = quantile(prec_val,probs=.1),
             TwentyFifth = quantile(prec_val,probs=.25),
             Median = quantile(prec_val,probs=.5),
             SeventyFifth =quantile(prec_val,probs=.75),
             Ninetieth = quantile(prec_val,probs=.9),
             NinetyFifth = quantile(prec_val,probs=.95),
             Max = max(prec_val))

# Need to count the number of each type of duplicate that is A divided by total number of duplicates

DQLcnts<- ddply(gdtidy[which(gdtidy$ActType == "FD"),],c("charid", "QCcalc"), summarise,
                cntA = sum(DEQ_prec=="A"),
                cntB = sum(DEQ_prec=="B"),
                CE = length(DEQ_prec)-((sum(DEQ_prec=="A"))+(sum(DEQ_prec=="B"))))

#Count the number of duplicates of each DQL each character id
for (i in seq_along(qcsum$charid)){
  qcsum$cntA[i] <- DQLcnts$cntA[i]
  qcsum$cntB[i] <- DQLcnts$cntB[i]
  qcsum$cntCE[i] <- DQLcnts$CE[i]
}

# Calculate the percent of duplicates meetinge each DQL
qcsum$perA <- qcsum$cntA/qcsum$N
qcsum$perB <- qcsum$cntB/qcsum$N
qcsum$perCE <- qcsum$cntCE/qcsum$N

#rename columns
names(qcsum) = c("charid","QCcalc", "Count", "Mean","Min", "5th %", "10th %", "25th %", "Median", "75th %","90th %", "95th %", "Max",
                 "CountA", "CountB","CountCorE", "PercentA", "PercentB", "PercentCorE")

#Generate CSV table to circulate
write.csv(qcsum, file = paste0(dir,'/',subid,'_QCSummary.csv'))


######################################################################
######################################################################

##     #####   #      #####  #####    #####
##     #   #   #      #   #    #      #
##     ####    #      #   #    #      #####
##     #       #      #   #    #          #
##     #       ####   #####    #      #####

######################################################################
######################################################################


load("CharNames.RData") # Check to make sure this data file is in your directory and includes all your paramters
load(paste0(dir,'/',subid,'-CharAD.RData'))
load(paste0(dir,'/',subid,'-CharLD.RData'))
# save(CharNames,file = "CharNames.RData") # if you need to add CharNames[r,c]<- "value"
######################################################################
####
####    Box plots of station results for each charid

#####
###
##
# Add parameters to CharAD so it doesn't plot on log scale.
charAD<- append(c("sl","mv","csa","dos","bod5"),charAD)
#
##
###
####

for (i in unique(gdtidy$charid)){
  jpeg(paste0(dir,'/',subid, i,"BxPlt.jpg"), width = 800, height = 480)#I'd really like to size to vary based how many LASAR
  par(las =2, cex.axis =0.75) # cex.axis = 0.75 to decrease size of axis labels when lots of stations
  boxplot(gdtidy$r[which(gdtidy$charid == i)] ~ gdtidy$LASAR[which(gdtidy$charid == i)],
          log = ifelse(i %in% charAD,"","y"), 
          xlab = "Sites",
          ylab = paste(CharNames$CharLong[which(CharNames$charid==i)], "(", prj$UNITS[which.max(prj$CharID==i)],")", sep= " "),
          # use which.max b/c can have multiple methods/analytical orgs for the same charid.
          main= paste(CharNames$CharLong[which(CharNames$charid==i)],"Results", sep= " ")
  )
  dev.off()
}


######################################################################
####
####    Time Series Charts of Duplicate Differences


for (i in seq_along(qcsum$charid)){
  md <- max(abs(qcsum$Min[i]),qcsum$Max[i]) # get maximum of duplicate differences
  A <- QCcrit$A[which(QCcrit$charid == qcsum$charid[i] & QCcrit$QCcalc == qcsum$QCcalc[i])] # A criteria
  B <- QCcrit$B[which(QCcrit$charid == qcsum$charid[i] & QCcrit$QCcalc == qcsum$QCcalc[i])] # B criteria
  yl <- max(md,B) #  Y axix limit larger of maximum difference and B level QC criteria
  
  jpeg(paste0(dir,'/',subid, qcsum$charid[i],"_",qcsum$QCcalc[i],"_","vTime.jpg"), width = 800, height = 480)
  par(las=0)
  plot(gdtidy$DateTime[which(gdtidy$charid == qcsum$charid[i] & gdtidy$QCcalc == qcsum$QCcalc[i])],
       gdtidy$prec_val[which(gdtidy$charid == qcsum$charid[i] & gdtidy$QCcalc == qcsum$QCcalc[i])], 
       ylim = c(-yl, yl), xaxt = "n", xlab = "DateTime", 
       ylab = paste0(qcsum$charid[i]," diff by ",qcsum$QCcalc[i]), pch = 19, cex = 1.5, col= rgb(0,0,0, 0.3))
  axis.POSIXct(side = 1, gdtidy$DateTime[which(gdtidy$charid == qcsum$charid[i] & gdtidy$QCcalc == qcsum$QCcalc[i])],
               at = seq(min(gdtidy$DateTime[which(gdtidy$charid == qcsum$charid[i] & gdtidy$QCcalc == qcsum$QCcalc[i])]),
                        max(gdtidy$DateTime[which(gdtidy$charid == qcsum$charid[i] & gdtidy$QCcalc == qcsum$QCcalc[i])]),
                        "3 months"), format = "%b-%y")
  title(main = paste0(CharNames$CharLong[which(CharNames$charid == qcsum$charid[i])]," Duplicate Differences by ", qcsum$QCcalc[i]))
  abline(0,0)  #line along 0 axis
  abline(A,0, lty = 2, col = "grey") # dashed grey line along + A criteria
  abline(-A,0, lty = 2, col = "grey") # dashed grey line along - A criteria
  abline(B,0, lty = 3, col = "grey") # dashed grey line along + B criteria
  abline(-B,0, lty = 3, col = "grey") # dashed grey line along - B criteria
  dev.off()
  rm(md)
  rm(A)
  rm(B)
  rm(yl)
}

#############################################################################
#####
####     Primary vs Duplicate Plots

for (i in unique(qcsum$charid)){  # i is each charid-- "do", "ph", etc.
  mx <- max(gdtidy$r[which(gdtidy$charid == i)]) + 0.1*(median(gdtidy$r[which(gdtidy$charid == i)]))# set axes limits as max observed value plus 10% of median observed value for the parameter  not just !gdtidy$ActType == 'S'
  mn <- min(gdtidy$r[which(gdtidy$charid == i)])/2 # min value to set for axes as half of the minimum observed value'
  # Create vectors with xy coordinates for criteria lines
  A <- QCcrit$A[which(QCcrit$charid == i)] # A criteria
  names(A) <- QCcrit$QCcalc[which(QCcrit$charid == i)] # named for QC calcs
  B <- QCcrit$B[which(QCcrit$charid == i)] # B criteria
  names(B) <- QCcrit$QCcalc[which(QCcrit$charid == i)] # named for QC calcs
  LLL <- min(prj$LowLevQClimit[which(prj$CharID == i)])# Low level limit for RPD criteria
  QCc <- qcsum$QCcalc[which(qcsum$charid == i)] # vector with qc calc method...AD, LD, RPD or AD & RPD
  ## Generate QC line XY datasets
  if (identical(QCc,"AbsDiff")){ # actually don't need the subsets by "AbsDiff" in script below
    Aux <- mn:(mx-A["AbsDiff"]) # A line, upper, x range
    Auy <- Aux+A["AbsDiff"] # A line, upper, y range
    Bux <- mn:(mx-B["AbsDiff"])
    Buy <- Bux+B["AbsDiff"]
    Alx <- (mn+A["AbsDiff"]):mx
    Aly <- Alx-A["AbsDiff"]
    Blx <- (mn+B["AbsDiff"]):mx
    Bly <- Blx-B["AbsDiff"] # X and Y for upper and lower abs diff--(8 vectors A&B criteria, upper and lower line x&Y
  } else if (identical(QCc,"LogDiff")){ 
    Aux <- mn:10^(log10(mx)-A) # A line, upper, x range
    Auy <- 10^(log10(Aux)+A) # A line, upper, y range
    Bux <- mn:10^(log10(mx)-B)
    Buy <- 10^(log10(Bux)+B)
    Alx <- 10^(log10(mn)+A):mx
    Aly <- 10^(log10(Alx)-A)
    Blx <- 10^(log10(mn)+B):mx
    Bly <- 10^(log10(Blx)-B)# X and Y for upper and lower log diff
  } else if (identical(QCc,"RPD")){ 
    Aux <- mn:(mx-(mx*A)) # A line, upper, x range
    Auy <- Aux+(Aux*A) # A line, upper, y range
    Bux <- mn:(mx-(mx*B))
    Buy <- Bux+(Bux*B)
    Alx <- (mn+(mn*A)):mx
    Aly <- Alx-(Alx*A)
    Blx <- (mn+(mn*B)):mx
    Bly <- Blx-(Blx*B)# X and Y for upper and lower RPD
  } else { # in script below need to figure out which of A and B criteria to use, so subset required
    Aux <- mn:(mx-A["AbsDiff"]) # A line, upper, x range
    Auy <- Aux+A["AbsDiff"] # A line, upper, y range
    Bux <- mn:(mx-B["AbsDiff"])
    Buy <- Bux+B["AbsDiff"]
    Alx <- (mn+A["AbsDiff"]):mx
    Aly <- Alx-A["AbsDiff"]
    Blx <- (mn+B["AbsDiff"]):mx
    Bly <- Blx-B["AbsDiff"] # X and Y for upper and lower abs diff--(8 vectors A&B criteria, upper and lower line x&Y 
    hiAux <- (LLL-(A["RPD"]/2)):(mx-(mx*A["RPD"])) # A line, upper, x range for high range values
    hiAuy <- hiAux+(hiAux*A["RPD"]) # A line, upper, y range
    hiBux <- (LLL-(B["RPD"]/2)):(mx-(mx*B["RPD"]))
    hiBuy <- hiBux+(hiBux*B["RPD"])
    hiAlx <- (LLL-(A["RPD"]/2)):mx
    hiAly <- hiAlx-(hiAlx*A["RPD"])
    hiBlx <- (LLL-(B["RPD"]/2)):mx
    hiBly <- hiBlx-(hiBlx*B["RPD"])# X and Y for LLL upper and lower using AbsDiff & X and Y for High upper and lower using RPD---16 vectors
  }
  
  ## Plot the data FP on X axis and FD on y axis  #############
  jpeg(paste0(dir,'/',subid,i,"FPvFDplt.jpeg"), width = 630, height = 630)
  par(xaxs = "r", yaxs ="r", pty = "s") # need to figure out better number format and distribution
  plot(gdl$r[which(!gdl$ActType == "S" & gdl$charid == i)], 
       gdl$dr[which(!gdl$ActType == "S" & gdl$charid == i)], 
       xlab = paste0("Field Primary (", unique(prj$UNITS[which(prj$CharID == i)]),")"), 
       ylab = paste0("Field Duplicate (", unique(prj$UNITS[which(prj$CharID == i)]),")"), 
       xlim = c(mn,mx), ylim = c(mn,mx), type = "n", log = ifelse(i %in% charAD,"","xy"), asp = 1) 
  points(gdl$r[which(!gdl$ActType == "S" & gdl$charid == i)], 
         gdl$dr[which(!gdl$ActType == "S" & gdl$charid == i)], 
         pch = 19, cex = 1.5, col = rgb(0,0,0, 0.3))
  title(main = paste0(CharNames$CharLong[which(CharNames$charid == i)], " Duplicate vs Primary"))
  abline(0,1)  #1:1 line
  
  ## Plot the QC lines ###########################################
  
  if (length(QCc)==1){
    # Plot the 4 lines if only one QCcalc for charid
    points(Aux, Auy, type = "l", lty = 2, col = "grey") # High result A criteria RPD > 0
    points(Alx, Aly, type = "l", lty = 2, col = "grey")
    points(Bux, Buy, type = "l", lty = 3, lwd = 0.5, col = "grey") # High result B criteria RPD > 0
    points(Blx, Bly, type = "l", lty = 3, lwd = 0.5, col = "grey")
  } else {
    # Plot the 8 lines if 2 QC calcs for charid, upper and lower lines on either side of the low level QC limit
    points(Aux, Auy, type = "l", lty = 2, col = "grey") # High result A criteria RPD > 0
    points(Alx, Aly, type = "l", lty = 2, col = "grey")
    points(hiAux, hiAuy, type = "l", lty = 2, col = "grey") # High result A criteria RPD > 0
    points(hiAlx, hiAly, type = "l", lty = 2, col = "grey")
    points(Bux, Buy, type = "l", lty = 3, lwd = 0.5, col = "grey") # High result B criteria RPD > 0
    points(Blx, Bly, type = "l", lty = 3, lwd = 0.5, col = "grey")
    points(hiBux, hiBuy, type = "l", lty = 3, lwd = 0.5, col = "grey") # High result B criteria RPD > 0
    points(hiBlx, hiBly, type = "l", lty = 3, lwd = 0.5, col = "grey")
  }
  
  dev.off() # turn off chart
  
}


######################################
#      END PLOTTING SCRIPT           #
######################################
