#### Comparison of RDBES and WGScallop data call landings

# Adam Delargy 

# 09/27/2024

# load data 


setwd("")

wgs <- read.table("data all to 2023.txt",header=TRUE)

rdbes <- read.csv("Landing aggregated RDBES 2021 2023.csv",header=TRUE)

#################################################################

# subset WGScallop to same remit of data 

wgs <- wgs[which(wgs$Year %in% 2021:2023),]
wgs <- droplevels(wgs)

# RDBES has more nations 

# want to delete Spain and Norway, as these don't exist for these years in WGScallop

rdbes <- rdbes[which(!(rdbes$CLvesselFlagCountry %in% c("SPAIN","NORWAY"))),]
rdbes <- droplevels(rdbes)

# delete Denmark and Slovenia from WGScallop, as they don't exist in RDBES

wgs <- wgs[which(!(wgs$Country %in% c("Denmark","Slovenia"))),]
wgs <- droplevels(wgs)

# Next we want to combine Guernsey and Jersey to Channel Islands in the RDBES
# Same with England and Wales, these need combined to match the WGScallop countries

rdbes$CLvesselFlagCountry <- ifelse(rdbes$CLvesselFlagCountry=="England" | rdbes$CLvesselFlagCountry=="Wales","England and Wales",rdbes$CLvesselFlagCountry)
rdbes$CLvesselFlagCountry <- ifelse(rdbes$CLvesselFlagCountry=="GUERNSEY" | rdbes$CLvesselFlagCountry=="JERSEY","Channel Islands",rdbes$CLvesselFlagCountry)

# now need to fix the metiers in rdbes -> cut these down to Level 5

rdbes$CLmetier6 <- substr(rdbes$CLmetier6,1,7)

## need to classify with rdbes data into ices areas based on statistical rectangle 

m1 <- match(rdbes$CLstatisticalRectangle,wgs$Statistical.rectangle)

rdbes$icesarea <- c(NA,nrow(rdbes))
for(i in 1:nrow(rdbes)){
	rdbes$icesarea[i] <- wgs$icesarea[m1[i]]
}

## make sure both datasets cover the same areas

areas <- sort(intersect(wgs$icesarea,rdbes$icesarea))

wgs <- wgs[which(wgs$icesarea %in% areas),]
wgs <- droplevels(wgs)

rbdes <- rdbes[which(rdbes$icesarea %in% areas),]
rbdes <- droplevels(rdbes)

# OK, data now covers the same countries and ICES sub areas
# although later there is  disrepancy for just the king scallop areas
# Data do not necessarily cover the same metiers - see later for that



#################################################################

#### king scallop landings 

wgs_k <- wgs[which(wgs$Species.code=="SCE"),]
wgs_k <- droplevels(wgs_k)

rdbes_k <- rdbes[which(rdbes$CLspeciesFaoCode=="SCE"),]
rdbes_k <- droplevels(rdbes_k)


## overall 

sum(wgs_k$Landings)
sum(rdbes_k$SumCLofficialWeight_kg)

# very close
# 222 thousand tonnes in WGScallop
# 214 thousand tonnes in RDBES


## by Country 

t56 <- tapply(wgs_k$Landings,wgs_k$Country,sum,na.rm=TRUE)
t57 <- tapply(rdbes_k$SumCLofficialWeight_kg,rdbes_k$CLvesselFlagCountry,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)

## by Year

t56 <- tapply(wgs_k$Landings,wgs_k$Year,sum,na.rm=TRUE)
t57 <- tapply(rdbes_k$SumCLofficialWeight_kg,rdbes_k$CLyear,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)

## by Metier 

# get a list of the metiers that are in both data sets
cmets <- sort(intersect(wgs_k$Metier,rdbes_k$CLmetier6))

wgs_k <- wgs_k[which(wgs_k$Metier %in% cmets),]
wgs_k <- droplevels(wgs_k)

rdbes_k <- rdbes_k[which(rdbes_k$CLmetier6 %in% cmets),]
rdbes_k <- droplevels(rdbes_k)

t56 <- tapply(wgs_k$Landings,wgs_k$Metier,sum,na.rm=TRUE)
t57 <- tapply(rdbes_k$SumCLofficialWeight_kg,rdbes_k$CLmetier6,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=3, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)

## by ICES area 

# one ICES area has no king scallop landings in RDBES -> so we want to strip this out of both
# get a list of the ices areas that are in both data sets
careas <- sort(intersect(wgs_k$icesarea,rdbes_k$icesarea))

wgs_k <- wgs_k[which(wgs_k$icesarea %in% careas),]
wgs_k <- droplevels(wgs_k)

rdbes_k <- rdbes_k[which(rdbes_k$icesarea %in% careas),]
rdbes_k <- droplevels(rdbes_k)


t56 <- tapply(wgs_k$Landings,wgs_k$icesarea,sum,na.rm=TRUE)
t57 <- tapply(rdbes_k$SumCLofficialWeight_kg,rdbes_k$icesarea,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)

#################################################################

#### queenscallop landings 

wgs_q <- wgs[which(wgs$Species.code=="QSC"),]
wgs_q <- droplevels(wgs_q)

rdbes_q <- rdbes[which(rdbes$CLspeciesFaoCode=="QSC"),]
rdbes_q <- droplevels(rdbes_q)

## overall 

sum(wgs_q$Landings)
sum(rdbes_q$SumCLofficialWeight_kg)

# very close
# 22 thousand tonnes in WGScallop
# 23 thousand tonnes in RDBES


## by Country 

t56 <- tapply(wgs_q$Landings,wgs_q$Country,sum,na.rm=TRUE)
t57 <- tapply(rdbes_q$SumCLofficialWeight_kg,rdbes_q$CLvesselFlagCountry,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topleft",legend=rownames(t58),fill=1:2)

## by Year

t56 <- tapply(wgs_q$Landings,wgs_q$Year,sum,na.rm=TRUE)
t57 <- tapply(rdbes_q$SumCLofficialWeight_kg,rdbes_q$CLyear,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topleft",legend=rownames(t58),fill=1:2)

## by Metier 

# get a list of the metiers that are in both data sets
cmets <- sort(intersect(wgs_q$Metier,rdbes_q$CLmetier6))

wgs_q <- wgs_q[which(wgs_q$Metier %in% cmets),]
wgs_q <- droplevels(wgs_q)

rdbes_q <- rdbes_q[which(rdbes_q$CLmetier6 %in% cmets),]
rdbes_q <- droplevels(rdbes_q)

t56 <- tapply(wgs_q$Landings,wgs_q$Metier,sum,na.rm=TRUE)
t57 <- tapply(rdbes_q$SumCLofficialWeight_kg,rdbes_q$CLmetier6,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=3, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)

## by ICES area 

# get a list of the ices areas that are in both data sets
careas <- sort(intersect(wgs_q$icesarea,rdbes_q$icesarea))

wgs_q <- wgs_q[which(wgs_q$icesarea %in% careas),]
wgs_q <- droplevels(wgs_q)

rdbes_q <- rdbes_q[which(rdbes_q$icesarea %in% careas),]
rdbes_q <- droplevels(rdbes_q)


t56 <- tapply(wgs_q$Landings,wgs_q$icesarea,sum,na.rm=TRUE)
t57 <- tapply(rdbes_q$SumCLofficialWeight_kg,rdbes_q$icesarea,sum,na.rm=TRUE)

t58 <- rbind(t56,t57)
rownames(t58) <- c("WGScallop","RDBES")

barplot(height=t58/1e6,beside=TRUE,las=1, ylab="Landings (thousand tonnes)", col=1:2)
legend("topright",legend=rownames(t58),fill=1:2)



#################################################################