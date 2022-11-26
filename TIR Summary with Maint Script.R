setwd("C:/Users/Hamptons/Desktop/Coursera/Work")
library(plyr)
library(dplyr)
library(tidyverse)

T <- read.csv("20210721 - FMTV A2 PVT TIR Summary Prompt.csv", skip = 1)
### If MILES is a factor, get rid of the commas before converting to a numeric:
T$MILES <- gsub(",", "", T$MILES)
T$MILES <- as.numeric(as.character(T$MILES))



T_sut <- T[T$ITEM. == "2-2L4R",]
names(T_sut)
T_sut <- T_sut[c(2, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16)]

M <- read.csv("20210721 - FMTV A2 PVT TIR Summary with Maint Totals Prompt.csv", skip = 1)            
M_sut <- M[M$ITEM. == "2-2L4R", ]
M_sut <- subset(M_sut, select = c(2, 18, 19, 20, 21, 22))

M_sut_Crew <- subset(M_sut, Level.Used == "CREW")

MSC <- M_sut_Crew$TIR.
length(MSC)
MSC_unique <- unique(M_sut_Crew$TIR.)
length(MSC_unique)
MSC[duplicated(MSC)] ## No duplicates

names(M_sut_Crew) <- gsub("Active", "Crew_Active", names(M_sut_Crew))
names(M_sut_Crew) <- gsub("Mr", "Crew_Level", names(M_sut_Crew))
names(M_sut_Crew) <- gsub("Maint", "Crew_Level", names(M_sut_Crew))

M_sut_Maint <- subset(M_sut, Level.Used == "MAINTAINER")

MSM <- M_sut_Maint$TIR.
length(MSM)
MSM_unique <- unique(M_sut_Maint$TIR.)
length(MSM_unique)
MSM[duplicated(MSM)] ## Returns K2-BU40078 and K2-BU40079

###### IF DUPLICATES FOUND ####
BU40078a <- c("K2-BU40078", 
              "NON-CHARGEABLE", "NO TEST", "MAINTAINER", 2.1, 2.1)
BU40079a <- c("K2-BU40079", 
              "NON-CHARGEABLE", "NO TEST", "MAINTAINER", 1.2334, 1.2334)
BU40850a<- c("K2-BU40850",
             "CHARGEABLE", "UNSCHEDULED", "MAINTAINER", 0.8167, 0.8167)

M_sut_Maint_a <- M_sut_Maint[!M_sut_Maint$TIR. == "K2-BU40078",]
M_sut_Maint_b <- M_sut_Maint_a[!M_sut_Maint_a$TIR. == "K2-BU40079",]
M_sut_Maint_c <- M_sut_Maint_b[!M_sut_Maint_b$TIR. == "K2-BU40850",]
M_sut_Maint <- rbind(M_sut_Maint_c, BU40078a, BU40079a, BU40850a)
###########


names(M_sut_Maint) <- gsub("Active", "Org_Active", names(M_sut_Maint))
names(M_sut_Maint) <- gsub("Maint", "Org_Level", names(M_sut_Maint))
names(M_sut_Maint) <- gsub("Mr", "Org_Level", names(M_sut_Maint))

M_sut_BD <- subset(M_sut, Level.Used == "BELOW DEPOT")

MSBD <- M_sut_BD$TIR.
length(MSBD)
MSBD_unique <- unique(M_sut_BD$TIR.)
length(MSBD_unique)
MSBD[duplicated(MSBD)] ## Returns K2-BU40078 and K2-BU40079

names(M_sut_BD) <- gsub("Active", "BD_Active", names(M_sut_BD))
names(M_sut_BD) <- gsub("Maint", "BD_Level", names(M_sut_BD))
names(M_sut_BD) <- gsub("Mr", "BD_Level", names(M_sut_BD))

M_sut_merged_1 <- merge(as.data.frame(M_sut_Crew), as.data.frame(M_sut_Maint), by = "TIR.", all = TRUE)
M_sut_merged_2 <- merge(as.data.frame(M_sut_merged_1), as.data.frame(M_sut_BD), by = "TIR.", all = TRUE)
M_sut_merged <- M_sut_merged_2[c(1, 2, 3, 5, 6, 7, 8, 10, 11, 12, 14, 15, 16)]

SUT <- merge(as.data.frame(T_sut), as.data.frame(M_sut_merged), by = "TIR.", all = TRUE)
write.csv(SUT, "2-2L4R.csv", row.names = TRUE)

#########################

L1R <- read.csv("2-2L1R.csv")
L2R <- read.csv("2-2L2R.csv")
L4R <- read.csv("2-2L4R.csv")

RAM_miles <- max(L1R$MILES) + max(L2R$MILES) + max(L4R$MILES)