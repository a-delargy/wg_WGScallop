### ICES WGScallop data call 2024

# script to clean up common issues in data

# Adam Delargy 
# 09/24/2024 

# load previously cleaned data from other years up to 2022
# and load 2023 data
# also load reference list of what statistical rectangles are
# in certain ICES areas for spatial classification

setwd("")

dat23 <- read.csv("data 2023 combined.csv", header=TRUE)

datold <- read.table("data all to 2022.txt", header=TRUE)

areas <- read.csv("ICES Rectangles & Areas.csv",header=TRUE)

#### process 2023 data  

table(dat23$Country)
# will need to align country names with those from the old data
# GBE is England and Wales
# GBC is Channel Islands
# GBI is the Isle of Man

# look for unusual things
table(dat23$Year)
table(dat23$Month,dat23$Country)
unique(dat23$ICES.Statistical.rectangle)
# got blanks, ' 

table(dat23$Metier.level.5,dat23$Country)

table(dat23$Species.code,dat23$Country)
# Denmark have used SCX again, and no other species code

# quick initial plots of data
barplot(tapply(dat23$Landings,dat23$Country,sum,na.rm=TRUE))

barplot(tapply(dat23$Effort,dat23$Country,sum,na.rm=TRUE))

# lpue
barplot(tapply(dat23$Landings,dat23$Country,sum,na.rm=TRUE)/tapply(dat23$Effort,dat23$Country,sum,na.rm=TRUE))

####


###### first work with 2023 data to apply the same cleans that have
# already been made to previous data 

# problems exist with ICES rectangle names:
#, ', '42E7

# fix these:
# replace "'" with NA
dat23$ICES.Statistical.rectangle <- ifelse(dat23$ICES.Statistical.rectangle=="'",NA,dat23$ICES.Statistical.rectangle) 
# remove "'" at the beginning of rectangle name
dat23$ICES.Statistical.rectangle <- ifelse(substr(dat23$ICES.Statistical.rectangle,1,1)=="'",substr(dat23$ICES.Statistical.rectangle,2,10),dat23$ICES.Statistical.rectangle) 

# some statistical rectangles in E&W appears in numerical format - NOT RELEVANT THIS YEAR, BUT PRESERVING CODE
#dat21$ICES.Statistical.rectangle <- ifelse(substr(dat21$ICES.Statistical.rectangle,5,5)=="E",paste0(substr(dat21$ICES.Statistical.rectangle,1,1),substr(dat21$ICES.Statistical.rectangle,3,3),"E",as.numeric(substr(dat21$ICES.Statistical.rectangle,7,8))-1),dat21$ICES.Statistical.rectangle)

### change country names 

dat23$Country <- as.factor(dat23$Country)
levels(dat23$Country) <- c("Belgium","Denmark","France","Scotland","Channel Islands","England and Wales","Isle of Man","Northern Ireland","Ireland","Netherlands","Slovenia")

### formatting
dat23$Country <- as.character(dat23$Country)
dat23$ICES.Statistical.rectangle <- as.character(dat23$ICES.Statistical.rectangle)


### classify 2023 in to ICES sub areas 

# below routine uses match function to find where statistical rectangle is stored in 'areas' df
# then copies over the appropriate ICES sub area
m1 <- match(dat23$ICES.Statistical.rectangle,areas$Rectangle)
dat23$icesarea <- factor(NA, levels=unique(areas$ICES.Area))
for(i in 1:nrow(dat23)){
	dat23$icesarea[i] <- areas$ICES.Area[m1[i]]
}

dat23$icesarea <- as.character(dat23$icesarea)
# done

# string combined 2000 to 2022 and the 2023 data into one final data frame
datf <- data.frame(Country=c(datold$Country,dat23$Country),Year=c(datold$Year,dat23$Year),
			Month=c(datold$Month,dat23$Month),ICES.area=c(rep(NA,nrow(datold)),dat23$ICES.area),
			Statistical.rectangle=c(datold$Statistical.rectangle,dat23$ICES.Statistical.rectangle),
			Metier=c(datold$Metier,dat23$Metier.level.5),Species.code=c(datold$Species.code,dat23$Species.code),
			Landings=c(datold$Landings,dat23$Landings),Effort=c(datold$Effort,dat23$Effort),
			icesarea=c(datold$icesarea,dat23$icesarea))

### WRITE

write.table(datf,"data all to 2023.txt",row.names=FALSE)

###


