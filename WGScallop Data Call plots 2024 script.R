
##### ICES WGScallop data call plots

# Adam Delargy 
# 09/25/2024

library(splus2R)	# for is.inf() 


#####################

####### PLOTTING FUNCTIONS

legend.fun<-function(...){				# this function is used for a global legend on multipanel plots 
  opar<-par(fig=c(0,1,0,1),oma=rep(0,4),
            mar=rep(0,4), new=TRUE)
  on.exit(par(opar))
  plot(0,0,type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

ices.plot <- function(areas, dat6, colname, ylabel){			# this function allows for plotting of a time series of data 
	NS <- dat6[which(dat6$icesarea %in% areas),]			# subset to a ices areas of interest
	NS <- droplevels(NS)						# drop latent levels (good practice)
	NS$icesarea <- factor(NS$icesarea, levels=sort(areas))
	t1 <- tapply(NS[[paste(colname)]],list(NS$Year,NS$icesarea),sum,na.rm=TRUE)	# perform calculation to get total of y variable by year and ICES area. 
	plot(t1[,1]/1e6 ~ rownames(t1), ylim=c(0,max(t1/1e6,na.rm=TRUE)), type='b', pch=16, ylab=paste(ylabel), xlab='Year', las=1)	# make combined line and dot plot for first ices area 
	for(i in 2:dim(t1)[2]){								# loop through remaining  ices areas
		lines(t1[,i]/1e6 ~ rownames(t1), col=i, type='b', pch=16)		# add a line and dots for each ices area
	}
	legend.fun("top", colnames(t1),col=1:dim(t1)[2],fill=1:dim(t1)[2],bty="n",cex=1.1,ncol=ceiling(dim(t1)[2]/2))		# insert global legend 
}

ices.lpue <- function(areas, dat6, colname1, colname2, ylabel){		# this function allows for a plot of cpue by dividing landings by effort
	NS <- dat6[which(dat6$icesarea %in% areas),]			
	NS <- droplevels(NS)
	NS$icesarea <- factor(NS$icesarea, levels=sort(areas))						
	t1 <- tapply(NS[[paste(colname1)]],list(NS$Year,NS$icesarea),sum, na.rm=TRUE)		# calculation for landings
	t2 <- tapply(NS[[paste(colname2)]],list(NS$Year,NS$icesarea),sum,na.rm=TRUE)		# calculation for effort
	t2 <- ifelse(t2 == 0, NA, t2) 								# change any 0's to NA
	lpue <- t1/t2										# divide landings by effort
	plot(lpue[,1] ~ rownames(lpue), ylim=c(0,max(lpue,na.rm=TRUE)), type='b', pch=16, ylab=paste(ylabel), xlab='Year', las=1)
	for(i in 2:dim(t1)[2]){
		lines(lpue[,i] ~ rownames(lpue), col=i, type='b', pch=16)
	}
	legend.fun("top", colnames(lpue),col=1:dim(lpue)[2],fill=1:dim(lpue)[2],bty="n",cex=1.1,ncol=ceiling(dim(lpue)[2]/2))
}



ices.plot.coun <- function(areas, dat6, colname, ylabel){		# this function makes a stacked barplot of yvariable by year, and bars split by country. 
	nc=ceiling(sqrt(length(areas)))					# estimate number of columns to split screen in to, so that plot sizes are (as close to) equal in x an y dimension
	nr=ceiling(length(areas)/nc)					# estimate number of rows [ditto]
	par(mfrow=c(nr,nc), mar=c(4,2,2,2), oma=c(1,3,1,1))		# split screen
	for(i in 1:length(areas)){					# loop through ices areas
		NS <- dat6[which(dat6$icesarea==areas[i]),]		# subset
		NS <- droplevels(NS)
		t1 <- tapply(NS[[paste(colname)]],list(NS$Year,NS$Country),sum, na.rm=TRUE)		# calculation by year and country
		dts <- t(as.matrix(t1))									# transpose results for plotting
      	dts[which(is.na(dts))] <- 0								# chnage NAs to 0
		coldex <- match(rownames(dts),levels(dat$Country))					# ensure the colour use for each country is consistent if this function is used multiple times
		set.cols <- cols[coldex]								# extract the correct colours for plotting 
		
		dbar <- barplot(dts/1e6, las=1, ylab="", xlab="", col=set.cols, main=paste(areas[i]), ylim=c(0, max((colSums(dts)/1e6)*1.1)))		# make stacked barplot
		#lines((colSums(dts)/1e6)~dbar, type='b', pch=16, col=2)
	}
	legend.fun("bottom", legend=substr(paste(levels(dat$Country)),1,4),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(levels(dat$Country))/2))	
	mtext(paste(ylabel), side=2, line=1.5, outer=TRUE)	# add global y-axis label
}


##############################################################
##########################################################


#####################

# set working directory and load data 

setwd("")

dat <- read.table("data all to 2023.txt",header=TRUE)

# formatting
dat$Statistical.rectangle <- as.factor(dat$Statistical.rectangle)
dat$Species.code <- as.factor(dat$Species.code)
dat$icesarea <- as.factor(dat$icesarea)
dat$Country <- as.factor(dat$Country)


### remove Denmark from data at this stage because they only reported SCX
dat <- dat[which(!(dat$Country=="Denmark")),]
dat <- droplevels(dat)

### load FAO areas
fao <- read.csv("Rectangles.csv",header=TRUE)


#### basic checks 
table(dat$Country, dat$Year)
table(dat$Country,dat$Metier)
table(dat$Country,dat$Species.code)

unique(dat$icesarea)

####

##### 
#

# additionally classify entries by ICES area (not just sub area)

dat$area2 <- factor(NA, levels=c("I","II","III","IV","V","VI","VII","VIII"))

dat$area2 <- ifelse(dat$icesarea %in% c("I"), "I", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("IIa","IIb"), "II", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("IIIa"), "III", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("IVa","IVb","IVc"), "IV", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("Vb1","Vb2"), "V", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("VIa","VIb"), "VI", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("VIIa","VIIb","VIIc","VIId","VIIe","VIIf","VIIg","VIIh","VIIj","VIIk"), "VII", dat$area2)
dat$area2 <- ifelse(dat$icesarea %in% c("VIIIa","VIIIb","VIIIc","VIIId"), "VIII", dat$area2)

################## MAKE PLOTS 

# PECTEN MAXIMUS 
# LANDINGS

pecdat <- dat[which(dat$Species.code=="SCE"),]		# subset to king scallops
pecdat <- droplevels(pecdat)


### table of country by year total pecten landings by FAO area

colnames(fao)[1] <- "Statistical.rectangle"
# remove Statistical Rectangles labelled UNK from fao
fao <- fao[which(fao$Statistical.rectangle!="UNK"),]
fao <- droplevels(fao)

# use match function to assign each record to an FAO area based on statistical rectangle
m1 <- match(pecdat$Statistical.rectangle,fao$Statistical.rectangle)
pecdat$FAO.Fishing.Area.Code <- c()
for(i in 1:nrow(pecdat)){
	pecdat$FAO.Fishing.Area.Code <- fao$FAO.Fishing.Area.Code[m1[i]]
}


# remove UNK from Pecten maximus data
pecdat_t <- pecdat[which(pecdat$Statistical.rectangle!="UNK"),]
pecdat_t <- droplevels(pecdat_t)

# for this specific request we want to present UK as a single country
levels(pecdat_t$Country)[which(levels(pecdat_t$Country) %in% c("England and Wales","Northern Ireland","Scotland"))] <- "UK"

# sum landings by year, country, and FAO area
obj6 <- tapply(pecdat_t$Landings, list(pecdat_t$Year,pecdat_t$Country,pecdat_t$FAO.Fishing.Area.Code), sum, na.rm=TRUE)


setwd("")
for(i in 1:length(unique(pecdat_t$FAO.Fishing.Area.Code))){	# make a table of landings by year and country for each FAO area
	tmp1  <- as.data.frame(obj6[,,i])
	# convert NAs to 0
	tmp1[is.na(tmp1)] <- 0
	# add total column 

	tmp1$Total <- rowSums(tmp1)	

	write.csv(tmp1,paste0(sort(unique(pecdat_t$FAO.Fishing.Area.Code))[i],".csv"))
}


##### OK onwards wih the figures

# 1. North Sea(Area 4) 

NSarea <- c("IVa","IVb","IVc")
ices.plot(areas = NSarea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 2. Area 7 (excluding channel)

Warea <- c("VIIc","VIIb","VIIa","VIIk","VIIj","VIIg","VIIf")
ices.plot(areas = Warea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 3. Area 7 (channel) 

Carea <- c("VIIh","VIIe","VIId")
ices.plot(areas = Carea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 4. Area 8 

Farea <- c("VIIIa","VIIIb","VIIIc","VIIId")
ices.plot(areas = Farea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")


# 5. Areas 1, 2, 3, 5 and 6 

Rarea <- c("IIIa","VIa","Vb2","VIb","IIa","Vb1","IIb","I")
ices.plot(areas = Rarea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")


#####################################


# AEQUIPECTEN OPERCULARIS  
# LANDINGS

aqedat <- dat[which(dat$Species.code=="QSC"),]		# subset to queen scallops
aqedat <- droplevels(aqedat)

# 1. North Sea

ices.plot(areas = NSarea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 2. Area VII (excluding channel)

ices.plot(areas = Warea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 3. Area VII (channel)
ices.plot(areas = Carea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 4. Area VIII, VI and II

Larea <- c(Farea,"VIa","IIb")

ices.plot(areas = Larea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

#######

# PECTEN MAXIMUS 
# EFFORT

# for France Pecten maximus effort only use dredges
# this is because France data contains effort from too many metiers that would not target scallops
# note this is only for effort, all France Pecten maximus landings were included in the plots earlier

frgears <- unique(dat$Metier[which(substr(dat$Metier,1,3)=="DRB")])			# extract all the metiers that being with DRB

pecdatfra <- pecdat[which(pecdat$Country=="France" & pecdat$Metier %in% frgears),]	# subset French data to only metiers that begin with DRB
pecdatfra <- droplevels(pecdatfra)

# cut and paste

pecnfra <- pecdat[which(pecdat$Country!="France"),]			# cut out the French data
pecnfra <- droplevels(pecnfra)

peceff <- rbind(pecnfra,pecdatfra)					# add back in the French data that only include DRB metiers

# make levels order same as pecdat 
peceff$icesarea <- factor(peceff$icesarea, levels=levels(pecdat$icesarea))

peceff$Effort <- as.numeric(peceff$Effort)

# in addition, remove Norway from effort as wrong units provided
# landings plots earlier do contain Norway data
peceff <- peceff[which(!(peceff$Country=="Norway")),] 
peceff <- droplevels(peceff)


# 1. North Sea(Area 4) 

ices.plot(areas = NSarea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")

# 2. Area 7 (excluding channel)

ices.plot(areas = Warea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")


# 3. Area 7 (channel) 

ices.plot(areas = Carea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")

# 4. Area 8 

ices.plot(areas = Farea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")


# 5. Areas 2, 3, 5 and 6 

ices.plot(areas = Rarea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")


#####################################

# AEQUIPECTEN OPERCULARIS  
# EFFORT

# 

aqedat$Effort <- as.numeric(aqedat$Effort)
# remove Norway for the same reason as above 

aqeeff <- aqedat[which(!(aqedat$Country=="Norway")),]
aqeeff <- droplevels(aqeeff)

# 1. North Sea

ices.plot(areas = NSarea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 2. Area VII (excluding channel)

ices.plot(areas = Warea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 3. Area VII (channel)
ices.plot(areas = Carea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 4. Area VIII, VI and II

ices.plot(areas = Larea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

################################################

######### PECTEN MAXIMUS 
## LPUE 

# units are kg per kW day 

# to keep data internally consistent, these LPUE plots only include France landings and effort from dredges
# Norway continues to be excluded due to effort units
# data from all other countries are presented as provided

# 1. North Sea(Area 4) 

ices.lpue(areas = NSarea, dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")

# 2. Area 7 (excluding channel)

ices.lpue(areas = Warea, dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")


# 3. Area 7 (channel) 

ices.lpue(areas = Carea, dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")


# 4. Area 8 

ices.lpue(areas = Farea, dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")


# 5. Areas 2, 3, 5 and 6 

ices.lpue(areas = Rarea, dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")

######### AEQUIPECTEN OPERCULARIS 
## LPUE 

# units are kg per kW day 

# Norway data excluded as wrong effort units

# 1. North Sea

ices.lpue(areas = NSarea, dat6 = aqeeff, colname1="Landings", colname2="Effort", ylabel="LPUE")

# 2. Area VII (excluding channel)

ices.lpue(areas = Warea, dat6 = aqeeff, colname1="Landings", colname2="Effort", ylabel="LPUE")

# 3. Area VII (channel)
ices.lpue(areas = Carea, dat6 = aqeeff, colname1="Landings", colname2="Effort", ylabel="LPUE")

# 4. Area VIII, VI and II

ices.lpue(areas = Larea, dat6 = aqeeff, colname1="Landings", colname2="Effort", ylabel="LPUE")


###############################################

# Irish Sea pecten plots (specific request)

par(mfrow=c(2,2), mar=c(2,4,2,2))	 # split screen
ices.plot(areas="VIIa", dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")
ices.plot(areas = "VIIa", dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")
ices.lpue(areas = "VIIa", dat6 = peceff, colname1="Landings",colname2="Effort", ylabel="LPUE")

# ignore errors - plot works 

################################################

# make plots of data by countries as stacked bar chart 

# specify colours to distinguish nations for upcoming stacked bar plots
cols <- c("#330000","#FF0000","#00FF00","#0000FF","#00FFFF","#FF00FF","#FFFF00","#A0A0A0","#FF9933","#663300","#FFFFFF")

################################################


##########

# PECTEN landings by country

# 1. North Sea(Area 4) 

ices.plot.coun(areas = NSarea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 2. Area 7 (excluding channel)

Wsarea <- c("VIIc","VIIb","VIIk","VIIj","VIIg","VIIf")
ices.plot.coun(areas = Wsarea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

ices.plot.coun(areas = "VIIa", dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 3. Area 7 (channel) 
ices.plot.coun(areas = Carea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 4. Area 8 
ices.plot.coun(areas = Farea, dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")


# 5. Areas 2, 3, 5 and 6 

ices.plot.coun(areas = "VIa", dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

#ices.plot.coun(areas = "IIa", dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

#########
# QUEEN SCALLOP BY COUNTRY 

# 1. North Sea

ices.plot.coun(areas = NSarea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 2. Area VII (excluding channel)

ices.plot.coun(areas ="VIIa", dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 3. Area VII (channel)
ices.plot.coun(areas = Carea, dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

# 4. Area VIII, VI and II

ices.plot.coun(areas = "VIa", dat6 = aqedat, colname="Landings", ylabel="Landings (thousand tonnes)")

#########################



##########

# PECTEN effort by country

# 1. North Sea(Area 4) 

ices.plot.coun(areas = NSarea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")

# 2. Area 7 (excluding channel)

ices.plot.coun(areas = Wsarea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")

ices.plot.coun(areas = "VIIa", dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")


# 3. Area 7 (channel) 
ices.plot.coun(areas = Carea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")

# 4. Area 8 
ices.plot.coun(areas = Farea, dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")


# 5. Areas 2, 3, 5 and 6 

ices.plot.coun(areas = "VIa", dat6 = peceff, colname="Effort", ylabel="Effort (million kW days)")



#########
# AQUI effort BY COUNTRY 

# 1. North Sea

ices.plot.coun(areas = NSarea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 2. Area VII (excluding channel)

ices.plot.coun(areas ="VIIa", dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 3. Area VII (channel)
ices.plot.coun(areas = Carea, dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")

# 4. Area VIII, VI and II

ices.plot.coun(areas = "VIa", dat6 = aqeeff, colname="Effort", ylabel="Effort (million kW days)")


#########################

#######################################################

############################################################

#######################################################################

############################################################

#######################################################

#########################

## Specific plots and tables that have been requested for reports over the years 

setwd("")

### 
# Total landings for king scallops 
# by year and ICES area 


pecland <- round(tapply(pecdat$Landings, list(pecdat$Year,pecdat$area2), sum, na.rm=TRUE)/1e3,1)		# calculation of landings by year and ICES high level area
# above in tonnes

# NAs to zero 

pecland <- ifelse(is.na(pecland),0,pecland)						# change NAs to 0

# remove columns (areas) with no landings reaching 0.1 tonnes in any year		
pecland <- pecland[,-(which(colSums(pecland)==0))]

# make a total column 

tmp <- colnames(pecland)

pecland2 <- cbind(pecland,rowSums(pecland))
colnames(pecland2) <- c(tmp,"Total")

# write.csv(pecland2, "King scallop landings by area.csv")


# Plot data from above table


tiff("Landings by area.tiff", width=20, height=15, units="cm", res=600)				# tiff to save image in working directory

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(pecland))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
dbar <- barplot(dts/1e3, las=1, ylab="", xlab="", col=set.cols, ylim=c(0, max((colSums(dts)/1e3)*1.1)))
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings (thousand tonnes)", side=2, line=1.5, outer=TRUE)

dev.off()		# completes image save 


# same but as line plot


tiff("Landings by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(pecland))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings (thousand tonnes)", side=2, line=1.5, outer=TRUE)

dev.off()

####### now the same plots but for effort

effpec <- round(tapply(peceff$Effort, list(peceff$Year,peceff$area2), sum, na.rm=TRUE)/1e3,1)
# above in thousand kw days 

# NAs to zero 

effpec <- ifelse(is.na(effpec),0,effpec)


tiff("Effort by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(effpec))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Effort (million kW days)", side=2, line=1.5, outer=TRUE)

dev.off()


##### now same but as lpue 

# probably want to remove AREA II from this because it's largely driven by Norway
# who did not report effort in the correct units

# also need to remove area III effort that has no landings 
# and area I landings that have no effort

# remove II and III from effort 
effpec <- effpec[,-which(colnames(effpec) %in% c("II","III"))]

# remove I and II from landings 
pecland <- pecland[,-which(colnames(pecland) %in% c("I","II"))]

peclpue <- pecland/effpec		# calculate lpue

# Infs to zero 
peclpue <- ifelse(is.nan(peclpue),0,peclpue)

tiff("LPUE by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(peclpue))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings per unit effort", side=2, line=1.5, outer=TRUE)

dev.off()


####################################

### 
# Total landings for queen scallops 
# by year and ICES area 

aqeland <- round(tapply(aqedat$Landings, list(aqedat$Year,aqedat$area2), sum, na.rm=TRUE)/1e3,1)

# NAs to zero 

aqeland <- ifelse(is.na(aqeland),0,aqeland)

# remove columns (areas) with no landings reaching 0.1 tonnes in any year

aqeland <- aqeland[,-(which(colSums(aqeland)==0))]

# get a total column 

tmp <- colnames(aqeland)

aqeland2 <- cbind(aqeland,rowSums(aqeland))
colnames(aqeland2) <- c(tmp,"Total")

#write.csv(aqeland2, "Queenie landings by area.csv")

# Figure of above table


tiff("Queenie landings by area.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(aqeland))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
dbar <- barplot(dts/1e3, las=1, ylab="", xlab="", col=set.cols, ylim=c(0, max((colSums(dts)/1e3)*1.1)))
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings (thousand tonnes)", side=2, line=1.5, outer=TRUE)

dev.off()


# same but as line plot


tiff("Queenie landings by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(aqeland))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings (thousand tonnes)", side=2, line=1.5, outer=TRUE)

dev.off()

####### now effort for queen scallops

effaqe <- round(tapply(aqeeff$Effort, list(aqeeff$Year,aqeeff$area2), sum, na.rm=TRUE)/1e3,1)
# above in thousand kw days 

# NAs to zero 

effaqe <- ifelse(is.na(effaqe),0,effaqe)

tiff("Queenie effort by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(effaqe))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Effort (million kW days)", side=2, line=1.5, outer=TRUE)

dev.off()


##### now lpue 

# remove II from effort, as no landings for this area

effaqe <- effaqe[,-which(colnames(effaqe)=="II")]

aqelpue <- aqeland/effaqe

# Infs to zero 
aqelpue <- ifelse(is.inf(aqelpue),0,aqelpue)

tiff("Queenie LPUE by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(aqelpue))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$area2)))
set.cols <- cols[coldex]
plot(dts[1,]~as.numeric(colnames(dts)), las=1, xlab="",ylab="", ylim=c(0,max(dts)),col=set.cols[1], type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=paste(sort(unique(dat$area2))),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$area2)))/2))
mtext("Landings per unit effort", side=2, line=1.5, outer=TRUE)

dev.off()

####################################
####################################

### 
# Tables of total landings and effort reported by country for VIIa (Irish Sea)
# king scallops 

pecviia <- pecdat[which(pecdat$icesarea=="VIIa"),]
pecviia <- droplevels(pecviia)

pecviia_eff <- peceff[which(peceff$icesarea=="VIIa"),]
pecviia_eff <- droplevels(pecviia_eff)

peclandviia <- round(tapply(pecviia$Landings, list(pecviia$Year, pecviia$Country), sum, na.rm=TRUE)/1e3,1)
# this one is in tonnes

# NAs to zero 

peclandviia <- ifelse(is.na(peclandviia),0,peclandviia)

#write.csv(peclandviia, "Irish Sea country.csv")
#write.table(pecviia, "Irish Sea pecten landing.txt", row.names=FALSE)
#write.table(pecviia_eff, "Irish Sea pecten effort.txt", row.names=FALSE)



# plot of the above is here from earlier:


tiff("Irish Sea king landings.tiff", width=20, height=15, units="cm", res=600)

ices.plot.coun(areas = "VIIa", dat6 = pecdat, colname="Landings", ylabel="Landings (thousand tonnes)")

dev.off()

# same but as line plot

tiff("Irish Sea king landings by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(peclandviia))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$Country)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=substr(paste(sort(unique(dat$Country))),1,4),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$Country)))/2))
mtext("Landings (thousand tonnes)", side=2, line=1.5, outer=TRUE)

dev.off()

####### now effort


peceffviia <- peceff[which(peceff$icesarea=="VIIa"),]
peceffviia <- droplevels(peceffviia)

effpecviia <- round(tapply(peceffviia$Effort, list(peceffviia$Year,peceffviia$Country), sum, na.rm=TRUE)/1e3,1)
# above in thousand kw days 

# NAs to zero 

effpecviia  <- ifelse(is.na(effpecviia),0,effpecviia)


tiff("Irish Sea king effort by area line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(effpecviia))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$Country)))
set.cols <- cols[coldex]
plot(dts[1,]/1e3~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)/1e3), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]/1e3~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=substr(paste(sort(unique(dat$Country))),1,4),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$Country)))/2))
mtext("Effort (million kW days)", side=2, line=1.5, outer=TRUE)

dev.off()


##### now lpue 

# remove France from landings nd effort
# France landings are very small from this area, yet sizeable effort reported skews the plot 

peclandviia <- peclandviia[,-which(colnames(peclandviia)=="France")]
effpecviia <- effpecviia[,-which(colnames(effpecviia)=="France")]

peclpueviia <- peclandviia/effpecviia

# NANs to zero 
peclpueviia <- ifelse(is.nan(peclpueviia),0,peclpueviia)

tiff("Irish sea king LPUE by country line.tiff", width=20, height=15, units="cm", res=600)

par(mar=c(4,2,2,2), oma=c(1,3,1,1))
dts <- t(as.matrix(peclpueviia))
dts[which(is.na(dts))] <- 0
coldex <- match(rownames(dts),sort(unique(dat$Country)))
set.cols <- cols[coldex]
plot(dts[1,]~as.numeric(colnames(dts)), las=1, xlab="",ylab="", col=set.cols[1], ylim=c(0,max(dts)), type="b", pch=16)
for(i in 2:dim(dts)[1]){
	lines(dts[i,]~as.numeric(colnames(dts)), type='b', col=set.cols[i], pch=16)
}
legend.fun("bottom", legend=substr(paste(sort(unique(dat$Country))),1,4),col=cols,fill=cols,bty="n",cex=1.1,ncol=ceiling(length(sort(unique(dat$Country)))/2))
mtext("Landings per unit effort", side=2, line=1.5, outer=TRUE)

dev.off()


####################################


####################################

# Table of Irish Sea landings by year and stat square 

peclandviia2 <- round(tapply(pecviia$Landings, list(pecviia$Year, pecviia$Statistical.rectangle), sum, na.rm=TRUE)/1e3,1)
# this one is in tonnes

# NAs to zero 

peclandviia2 <- ifelse(is.na(peclandviia2),0,peclandviia2)

#write.table(peclandviia2, "Irish Sea stat square.txt")

####################################



################################### METIER PLOTS 


###### PECTEN MAXIMUS LANDINGS BY METIER 

# all
pecmets <- tapply(pecdat$Landings, pecdat$Metier, sum)				# calculation of landings by metier 
barplot(pecmets/1e6, las=3, xlab="", ylab="Landings (thousand tonnes)")		

# minus DRB_MOL
rems <- pecmets[which(rownames(pecmets)!="DRB_MOL")]				# remove DRB_MOL to see scale of other metiers
barplot(rems/1e6, las=3, xlab="", ylab="Landings (thousand tonnes)")


# sort by quantity of landings 
rems <- rev(sort(rems))
rem_l <- rems # for later 								

# plot the top eight metiers (minus DRB_MOL) by year 
t8 <- tapply(pecdat$Landings, list(pecdat$Metier,pecdat$Year), sum) 		# calculation of landings by metier and year
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Landings (thousand tonnes)", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)

### now look by country 

# how many metiers did countries report scallop landings from? 


dum1 <- table(pecdat$Metier, pecdat$Country, pecdat$Year)			# pivot table of metier, country and year (obtains counts of number of records)

dum1 <- as.data.frame(dum1)							# convert from table to data frame
dum1 <- dum1[which(dum1$Freq>0),]						# subset to situations where metier is reported
dum2 <- table(dum1$Var1,dum1$Var2)						# perform second pivot table to get each metier by number of times country reported it
dum2 <- ceiling(dum2/5)*5							# convert year in to 5 year bin 

dum3 <- apply(dum2,2,table)							# apply pivot table to get the number of metiers in each year group by country
dum4 <- as.data.frame(unlist(dum3))						# formatting
dum5 <- (strsplit(rownames(dum4), "[.]"))					# formatting
dum4<- cbind(dum4,as.data.frame(matrix(unlist(dum5), ncol=2, byrow=TRUE)))	# formatting
colnames(dum4) <- c("Count","Country","Yearclass")			# formatting

# cut 0 entries 
dum4 <- dum4[which(dum4$Yearclass!=0),]					# delete entries where no metiers were recorded for that country
dum4$prop <- as.vector(unlist(tapply(dum4$Count,dum4$Country,FUN=function(x){x/sum(x)})))	# get the proportion of the number of metiers reported by each country, that were reported in x number of years


## get totals 

# 
metcon <- tapply(pecdat$Landings, list(pecdat$Metier, pecdat$Country), sum)		# get the total landings by metier for each country
metcon2 <- metcon/metcon				# convert these to 1s and 0s (i.e. metier present or absent)							
# change NaNs to 0			
metcon2[which(is.na(metcon2))] <- 0
totals <- colSums(metcon2)				# get the total number of metiers by country

# multiply proportions by total to get the number of times each metier 
# was reported by each Country (sorted in to groups of five)

dum4$Country <- factor(dum4$Country, levels=sort(unique(dum4$Country))) 	# force order of countries in data frame. required for data alignment
store <- c()		# create blank for storing loop outputs

for(i in 1:length(levels(dum4$Country))){		# loop through each country
	set <- dum4[which(dum4$Country==levels(dum4$Country)[i]),]	# subset to country
	set <- droplevels(set)
	tmp1 <- round(set$prop * totals[i])		# get the proportion of the total number of metiers reported that are reported in x number of years
	store <- c(store,tmp1)				# save results
}
dum4$final <- store
dum4$Yearclass <- as.numeric(dum4$Yearclass)

dum4$Yearclass <- factor(dum4$Yearclass, levels=sort(unique(dum4$Yearclass)))		# force order of years for plot
dum7 <- tapply(dum4$final,list(dum4$Country,dum4$Yearclass),mean)				# formatting step, doesn't change calculations
dum7[which(is.na(dum7))] <- 0

dum7 <- t(as.matrix(dum7))

par(mar=c(4,5,4,1))
barplot(dum7, la=1, col=cols, ylab="Number of metiers reported", xlab="Country")
legend.fun("top", legend=c("1-5","6-10","11-15","16-20","21"), pch=15, col=cols, bty='n', ncol=5, title="Number of years metier reported")

# this plot shows the number of metiers reported on the y axis, the country on the x-axis, and the bars are broken up by the number of years
# each country has reported certain metiers for. This plot helps to understand which countries report the same metiers every year, and
# which countries have metiers that appear rarely in the time series.


##### next make plot of the landings by country 
# plot the top eight metiers (minus DRB_MOL) by year 

t8 <- tapply(pecdat$Landings, list(pecdat$Metier,pecdat$Country), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Country",ylab="Landings (thousand tonnes)", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)


################################

################################ PECTEN MAXIMUS EFFORT 

# all
pecmets <- tapply(peceff$Effort, peceff$Metier, sum, na.rm=TRUE)
par(mar=c(6,4,1,1))
barplot(pecmets/1e6, las=3, xlab="", ylab="Effort (million kW-days)")

# minus DRB_MOL
rems <- pecmets[which(rownames(pecmets)!="DRB_MOL")]
barplot(rems/1e6, las=3, xlab="", ylab="Effort (million kW-days)")


# plot the top eight metiers (minus DRB_MOL) by year 

t8 <- tapply(peceff$Effort, list(peceff$Metier,peceff$Year), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rem_l)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0

coldex1 <- match(rownames(dts2),sort(names(rem_l)[1:8]))	
set.cols <- cols[coldex1]

par(mar=c(4,4,1,4)) 
barplot(dts2/1e6, las=1, xlab="Year",ylab="Effort (million kW-days)", col=set.cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=set.cols, bty='n', cex=0.75)


### effort by country 

rems <- rev(sort(rems))

t8 <- tapply(peceff$Effort, list(peceff$Metier,peceff$Country), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Effort (million kW-days", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)

############################

#######  	QUEEN SCALLOP METIER PLOTS 


############################


# all

aqedat <- droplevels(aqedat)
aqemets <- tapply(aqedat$Landings, aqedat$Metier, sum)
par(mar=c(6,4,1,1))
barplot(aqemets/1e6, las=3, xlab="", ylab="Landings (thousand tonnes)")


# sort by quantity of landings 

rems <- aqemets
rems <- rev(sort(rems))
rem_l <- rems

# plot the top eight metiers (minus DRB_MOL) by year 

t8 <- tapply(aqedat$Landings, list(aqedat$Metier,aqedat$Year), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,5)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Landings (thousand tonnes)", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)

### now look by country 

# how many metiers did countries report queen scallop landings from? 


dum1 <- table(aqedat$Metier, aqedat$Country, aqedat$Year)

dum1 <- as.data.frame(dum1)
dum1 <- dum1[which(dum1$Freq>0),]

dum2 <- table(dum1$Var1,dum1$Var2)
dum2 <- ceiling(dum2/5)*5

dum3 <- apply(dum2,2,table)
dum4 <- as.data.frame(unlist(dum3))
dum5 <- (strsplit(rownames(dum4), "[.]"))
dum4<- cbind(dum4,as.data.frame(matrix(unlist(dum5), ncol=2, byrow=TRUE)))
colnames(dum4) <- c("Count","Country","Yearclass")

# cut 0 entries 
dum4 <- dum4[which(dum4$Yearclass!=0),]
dum4$prop <- as.vector(unlist(tapply(dum4$Count,dum4$Country,FUN=function(x){x/sum(x)})))


## get totals 

# 
metcon <- tapply(aqedat$Landings, list(aqedat$Metier, aqedat$Country), sum)
metcon2 <- metcon/metcon
# change NaNs to 0
metcon2[which(is.na(metcon2))] <- 0
totals <- colSums(metcon2)

# multiply proportions by total to get the number of times each metier 
# was reported by each Country (sorted in to groups of five)

dum4$Country <- factor(dum4$Country, levels=sort(unique(dum4$Country))) 
store <- c()

for(i in 1:length(levels(dum4$Country))){
	set <- dum4[which(dum4$Country==levels(dum4$Country)[i]),]
	set <- droplevels(set)
	tmp1 <- round(set$prop * totals[i])
	store <- c(store,tmp1)	
}
dum4$final <- store
dum4$Yearclass <- as.numeric(dum4$Yearclass)

dum4$Yearclass <- factor(dum4$Yearclass, levels=sort(unique(dum4$Yearclass)))
dum7 <- tapply(dum4$final,list(dum4$Country,dum4$Yearclass),mean)
dum7[which(is.na(dum7))] <- 0

dum7 <- t(as.matrix(dum7))

par(mar=c(4,5,4,1))
barplot(dum7, las=1, col=cols, ylab="Number of metiers reported", xlab="Country")
legend.fun("top", legend=c("1-5","6-10","11-15","16-20","21"), pch=15, col=cols, bty='n', ncol=5, title="Number of years metier reported")


##### next make plot of the landings by country 
# plot the top eight metiers (minus DRB_MOL) by year 

t8 <- tapply(aqedat$Landings, list(aqedat$Metier,aqedat$Country), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Landings (thousand tonnes)", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)


###################################

################################ QUEENIE EFFORT 

# all
pecmets <- tapply(aqeeff$Effort, aqeeff$Metier, sum, na.rm=TRUE)
par(mar=c(6,4,1,1))
barplot(pecmets/1e6, las=3, xlab="", ylab="Effort (million kW-days)")


# sort by quantity of effort
rems <- pecmets
rems <- rev(sort(rems))

# plot the top eight metiers (minus DRB_MOL) by year 

t8 <- tapply(aqeeff$Effort, list(aqeeff$Metier,aqeeff$Year), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rem_l)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

coldex1 <- match(rownames(dts2),sort(names(rem_l)[1:8]))	
set.cols <- cols[coldex1]

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Effort (million kW-days)", col=set.cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=set.cols, bty='n', cex=0.75)


### effort by country 

t8 <- tapply(aqedat$Effort, list(aqedat$Metier,aqedat$Country), sum) 
dts2 <- t8[which(rownames(t8) %in% names(rems)[1:8]),] 

# plot these 
par(mar=c(4,4,1,4)) 

dts2 <- as.matrix(dts2)
dts2[which(is.na(dts2))] <- 0
barplot(dts2/1e6, las=1, xlab="Year",ylab="Effort (million kW-days", col=cols)
legend.fun("right", legend=rownames(dts2), pch=15, col=cols, bty='n', cex=0.75)

###################################################
###################################################

# specific metier plots for report


## KING SCALLOP LANDINGS 

# create other category, which is the metiers after the top seven

top8 <- names(rev(sort(tapply(pecdat$Landings,pecdat$Metier, sum, na.rm=TRUE))))[1:8]

pecdat$Metier2 <- ifelse(pecdat$Metier %in% top8, as.character(pecdat$Metier), "Other")

### landings by metier by country

t8 <- tapply(pecdat$Landings, list(pecdat$Metier2,pecdat$Country), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

colnames(t8) <- substr(colnames(t8), 1,4)

tiff("King scallop landings metier by country.tiff", width=20, height=15, units="cm", res=600)

barplot(t8/1e6, las=1, xlab="Country",ylab="Landings (thousand tonnes)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

### landings by metier by ices area

t8 <- tapply(pecdat$Landings, list(pecdat$Metier2,pecdat$area2), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

tiff("King scallop landings metier by area.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="ICES area",ylab="Landings (thousand tonnes)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.3)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()


### KING SCALLOP EFFORT 

# create other category, which is the metiers after the top seven

peceff$Country <- factor(peceff$Country, levels=sort(levels(peceff$Country)))

top8 <- names(rev(sort(tapply(peceff$Effort,peceff$Metier, sum, na.rm=TRUE))))[1:8]

peceff$Metier2 <- ifelse(peceff$Metier %in% top8, as.character(peceff$Metier), "Other")

### effort by metier by country

t8 <- tapply(peceff$Effort, list(peceff$Metier2,peceff$Country), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

colnames(t8) <- substr(colnames(t8), 1,4)

tiff("King scallop effort metier by country.tiff", width=20, height=15, units="cm", res=600)

barplot(t8/1e6, las=1, xlab="Country",ylab="Effort (million kw-Days)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

### effort by metier by ices area

t8 <- tapply(peceff$Effort, list(peceff$Metier2,peceff$area2), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

tiff("King scallop effort metier by area.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="ICES area",ylab="Effort (million kW-days)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

## QUEEN SCALLOP LANDINGS 

# create other category, which is the metiers after the top seven

top8 <- names(rev(sort(tapply(aqedat$Landings,aqedat$Metier, sum, na.rm=TRUE))))[1:8]

aqedat$Metier2 <- ifelse(aqedat$Metier %in% top8, as.character(aqedat$Metier), "Other")

### landings by metier by country

t8 <- tapply(aqedat$Landings, list(aqedat$Metier2,aqedat$Country), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

colnames(t8) <- substr(colnames(t8), 1,4)

tiff("Queen scallop landings metier by country.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="Country",ylab="Landings (thousand tonnes)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("top", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

### landings by metier by ices area

t8 <- tapply(aqedat$Landings, list(aqedat$Metier2,aqedat$area2), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

tiff("Queen scallop landings metier by area.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="ICES area",ylab="Landings (thousand tonnes)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.3)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

### QUEEN SCALLOP EFFORT 

# create other category, which is the metiers after the top seven

top8 <- names(rev(sort(tapply(aqeeff$Effort,aqeeff$Metier, sum, na.rm=TRUE))))[1:8]

aqeeff$Metier2 <- ifelse(aqeeff$Metier %in% top8, as.character(aqeeff$Metier), "Other")

### effort by metier by country

t8 <- tapply(aqeeff$Effort, list(aqeeff$Metier2,aqeeff$Country), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

colnames(t8) <- substr(colnames(t8), 1,4)

tiff("Queen scallop effort metier by country.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="Country",ylab="Effort (million kw-Days)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()

### effort by metier by ices area

t8 <- tapply(aqeeff$Effort, list(aqeeff$Metier2,aqeeff$area2), sum, na.rm=TRUE) 

# plot these 
par(mar=c(4,4,1,4)) 

t8 <- as.matrix(t8)
t8[which(is.na(t8))] <- 0

tiff("Queen scallop effort metier by area.tiff", width=20, height=15, units="cm", res=500)

barplot(t8/1e6, las=1, xlab="ICES area",ylab="Effort (million kW-days)", col=cols,ylim=c(0, max((colSums(t8)/1e6)*1.1)))
legend.fun("topright", legend=rownames(t8), pch=15, col=cols, bty='n', cex=1)

dev.off()


###################################################
###################################################

#### END 

