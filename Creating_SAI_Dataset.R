#### Installing and opening packages ####

install.packages("rgdal")
install.packages("maptools")
install.packages("ggmap")
install.packages
install.packages("psych")
install.packages("GISTools")
install.packages("tripack")
install.packages("spam")
install.packages("spdep")
install.packages("stargazer")
library(rgdal)
library(maptools)
library(sp)
library(foreign)
library(ggmap)
library(psych)
library(GISTools)
library(tripack)
library(spdep)
library(stargazer)

#### Reading in files ####
SA_1 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_1")
SA_2 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_2")
SA_3 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_3")
SA_4 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_4")
SA_5 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_5")
SA_6 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_6")
SA_7 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_7_new")
SA_8 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_8")
SA_9 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_9")
SA_10 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_10_new")
SA_11 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_11")
SA_12 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_12t")
SA_13 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_13")
SA_14 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_14")
SA_15 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_15")
SA_16 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_16")
SA_17 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_17")
SA_18 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_18")
SA_19 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA19")
SA_20 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_20")
SA_21 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_21")
SA_22 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_22")
SA_23 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_23")
SA_24 <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_24")

#### Creating Unique IDs ####

SA_1 <- spChFIDs(SA_1, paste("SA_1", row.names(SA_1), sep="."))
SA_2 <- spChFIDs(SA_2, paste("SA_2", row.names(SA_2), sep="."))
SA_3 <- spChFIDs(SA_3, paste("SA_3", row.names(SA_3), sep="."))
SA_4 <- spChFIDs(SA_4, paste("SA_4", row.names(SA_4), sep="."))
SA_5 <- spChFIDs(SA_5, paste("SA_5", row.names(SA_5), sep="."))
SA_6 <- spChFIDs(SA_6, paste("SA_6", row.names(SA_6), sep="."))
SA_7 <- spChFIDs(SA_7, paste("SA_7", row.names(SA_7), sep="."))
SA_8 <- spChFIDs(SA_8, paste("SA_8", row.names(SA_8), sep="."))
SA_9 <- spChFIDs(SA_9, paste("SA_9", row.names(SA_9), sep="."))
SA_10 <- spChFIDs(SA_10, paste("SA_10", row.names(SA_10), sep="."))
SA_11 <- spChFIDs(SA_11, paste("SA_11", row.names(SA_11), sep="."))
SA_12 <- spChFIDs(SA_12, paste("SA_12", row.names(SA_12), sep="."))
SA_13 <- spChFIDs(SA_13, paste("SA_13", row.names(SA_13), sep="."))
SA_14 <- spChFIDs(SA_14, paste("SA_14", row.names(SA_14), sep="."))
SA_15 <- spChFIDs(SA_15, paste("SA_15", row.names(SA_15), sep="."))
SA_16 <- spChFIDs(SA_16, paste("SA_16", row.names(SA_16), sep="."))
SA_17 <- spChFIDs(SA_17, paste("SA_17", row.names(SA_17), sep="."))
SA_18 <- spChFIDs(SA_18, paste("SA_18", row.names(SA_18), sep="."))
SA_19 <- spChFIDs(SA_19, paste("SA_19", row.names(SA_19), sep="."))
SA_20 <- spChFIDs(SA_20, paste("SA_20", row.names(SA_20), sep="."))
SA_21 <- spChFIDs(SA_21, paste("SA_21", row.names(SA_21), sep="."))
SA_22 <- spChFIDs(SA_22, paste("SA_22", row.names(SA_22), sep="."))
SA_23 <- spChFIDs(SA_23, paste("SA_23", row.names(SA_23), sep="."))
SA_24 <- spChFIDs(SA_24, paste("SA_24", row.names(SA_24), sep="."))

#### Combing and saving full shapefile ####

SA_all <- rbind(SA_1, SA_2, SA_3, SA_4, SA_5, SA_6, SA_7, SA_8, SA_9, SA_10, SA_11, SA_12, 
                SA_13, SA_14, SA_15, SA_16, SA_17, SA_18, SA_19, SA_20, SA_21, SA_22, SA_23, SA_24)


SA_all <- SA_all[-c(6:11)]
SA_all <- SA_all[-c(7:91)]
SA_all <- SA_all[c(1:8)]

SA_all$Sum_TotPop <-as.numeric(SA_all$Sum_TotPop)

## Exporting to Open in GIS ##

writeOGR(obj = SAIDemo, dsn = "N:/ProviderData/SA_All", layer = "SA_all", driver = "ESRI Shapefile")

#### Reading in Final DBF from GIS ####

SAI <- read.dbf(file = "N:/ProviderData/SA_Files/SAI_GIS.dbf")
  SAI <-SAI[c(1:10, 19)] 

colnames(SAI)[11] <- "SAI"

write.dbf(SAI,"N:/ProviderData/SAI_Final.dbf")

SAIDemo <- readOGR(dsn= "N:/ProviderData/SA_Files", layer = "SA_Demo")
SAI_Demo <- as.data.frame(SAIDemo)

White <- read.csv(file = "N:/ProviderData/White.csv")
  colnames(White)[1] <- "AFFGEOID"

Rural <- read.csv(file = "N:/ProviderData/Rural.csv")
  colnames(Rural)[1] <- "AFFGEOID"

SAI_Demo <- merge(SAI_Demo, White, by = "AFFGEOID")
SAI_Demo <- merge(SAI_Demo, Rural, by = "AFFGEOID")

#### Cleaning Data, renaming Variables, adding additional variables, and saving as DTA ####

SAI_Demo <- SAI_Demo[c(1:20,22:26,28:33,45)]

colnames(SAI_Demo)[14] <- "Hispanic"
  SAI_Demo$Hispanic <- as.integer(SAI_Demo$Hispanic)
colnames(SAI_Demo)[15] <- "African_Amer"
  SAI_Demo$African_Amer <- as.integer(SAI_Demo$African_Amer)
colnames(SAI_Demo)[16] <- "Amer_Ind"
  SAI_Demo$Amer_Ind <- as.integer(SAI_Demo$Amer_Ind)
colnames(SAI_Demo)[17] <- "Asian"
  SAI_Demo$Asian <- as.integer(SAI_Demo$Asian)
colnames(SAI_Demo)[18] <- "Hawaiian"
  SAI_Demo$Hawaiian <- as.integer(SAI_Demo$Hawaiian)
colnames(SAI_Demo)[19] <- "Other_Race"
  SAI_Demo$Other_Race <- as.integer(SAI_Demo$Other_Race)
colnames(SAI_Demo)[20] <- "Two_Races"
  SAI_Demo$Two_Races <- as.integer(SAI_Demo$Two_Races)
colnames(SAI_Demo)[21] <- "25_OverPop"


SAI_New <- SAI_Demo[-c(26,27,28,29,30,31,33)]

#### ADDING ADDITIONAL DEMOGRAPHIC VARIABLES AND REMOVING INCORRECT VARIABLES ####

insurance <- read.csv(file = "N:/ProviderData/Insurance.csv")
  colnames(insurance)[1] <- "AFFGEOID"

SAI_New <- merge(SAI_New, insurance, by = "AFFGEOID")

SAI_New <- SAI_New[,c(1:12,26,13:25,27:31)]

colnames(SAI_New)[28] <- "Urban"

SAI_New2 <- SAI_New[-c(14,15,16,17,18,19,20,21,22,23,24,25,26,29,30,31)]

insurance <- read.csv(file = "N:/ProviderData/insurance_new_new.csv")
  SAI_New2 <- merge(SAI_New2, insurance, by = "AFFGEOID")

ethnicity <- read.csv(file = "N:/ProviderData/ethnicity_new.csv")
  SAI_New2 <- merge(SAI_New2, ethnicity, by = "AFFGEOID")

education <- read.csv(file = "N:/ProviderData/Education.csv")
  SAI_New2 <- merge(SAI_New2, education, by = "AFFGEOID")

medianincome <- read.csv(file = "N:/ProviderData/median_income.csv")
  SAI_New2 <- merge(SAI_New2, medianincome, by = "AFFGEOID")

#### CLEANING DATA ####
SAI_New2 <- SAI_New2[-c(2:12)]

SAI_New2 <- SAI_New2[c(1,2,9,23,4,5,6,7,8,3,10:22)]

#### ADDING HEALTH VARIABLES ####

beds <- read.dbf(file = "N:/ProviderData/Beds.dbf")
  beds <- beds[c(5,18)]
  SAI_New2 <- merge(SAI_New2, beds, by = "AFFGEOID")
  colnames(SAI_New2)[34] <- "Beds"

#### CREATING PROPORTION VARIABLES ####
  
SAI_New2$Share_White <- SAI_New2$White / SAI_New2$TotPop
SAI_New2$Share_Hispanic <- SAI_New2$Hispanic / SAI_New2$TotPop
SAI_New2$Share_Black <- SAI_New2$Black / SAI_New2$TotPop
SAI_New2$Share_NativeAmer <- SAI_New2$NativeAmer / SAI_New2$TotPop
SAI_New2$Share_Asian <- SAI_New2$Asian / SAI_New2$TotPop
SAI_New2$Share_PacIslander <- SAI_New2$PacIslander / SAI_New2$TotPop
SAI_New2$Share_Other <- SAI_New2$Other / SAI_New2$TotPop
SAI_New2$Share_TwoRaces <- SAI_New2$TwoRaces / SAI_New2$TotPop
SAI_New2$Share_No_HS <- SAI_New2$No_HS / SAI_New2$Pop_25
SAI_New2$Share_HS <- SAI_New2$HS / SAI_New2$Pop_25
SAI_New2$Share_AA <- SAI_New2$AA / SAI_New2$Pop_25
SAI_New2$Share_Bachelors <- SAI_New2$Bachelors / SAI_New2$Pop_25
SAI_New2$Share_Graduate <- SAI_New2$Graduate / SAI_New2$Pop_25
SAI_New2$Share_Private <- SAI_New2$Private_alone / SAI_New2$Uninsitutionalized
SAI_New2$Share_Public <- SAI_New2$Public_alone / SAI_New2$Uninsitutionalized
SAI_New2$Share_Unsur <- SAI_New2$Uninsured / SAI_New2$Uninsitutionalized

SAI_New2 <- SAI_New2[c(1:7,37,8,38,9,39,24,10,11,25,12,26,13,27,14,28,15,29,16,30,17,18,31,19,32,20,33,21,34,22,35,23,36)]

SAI_New2$Total_Other <- SAI_New2$PacIslander + SAI_New2$NativeAmer + SAI_New2$Other + SAI_New2$TwoRaces
SAI_New2$Total_Other_Share <- SAI_New2$Total_Other / SAI_New2$TotPop

SAI_New2$MI <- SAI_New2$Median_Income/1000
SAI_New2$Pop <- SAI_New2$TotPop/100
#### WRITING CSV AND DTA FILES ####

write.csv(SAI_New2,"N:/ProviderData/SAI_Final_New3.csv" )

write.dta(SAI_New2, "N:/ProviderData/SAI_Final_New3.dta")



#### Creating new shapefile ####

Tracts <- readOGR(dsn= "N:/ProviderData/Census Tracts", layer = "cb_2015_53_tract_500k")

Tracts_SAI <- merge(Tracts, SAI_New2, by = "AFFGEOID")

#### Removing Rows with N/A's



Tracts3 <- na.omit(SAI_New2)
Tracts3 <- merge(Tracts, Tracts3, by = "AFFGEOID", all.x = FALSE)


#### Summary Statistics ####
Tracts_3 <- na.omit(SAI_New2)

stats <- stargazer(Tracts_3, type = "text", title = "Descriptive Statistics", digits=1) # results copied into a txt file and opened in excel for further formatting

