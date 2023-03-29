require(dplyr)
require(writexl)
#Compounds to be used
########################################################################
compounds <- c("9,10-OH-9,10-HPHN", "1,2-OH-1,2-HPHN","COOHs",
               "2-OHPHN","3-OHPHN", "9-OHPHN", 
               "1-OHPHN","4-OHPHN-d9" ,"4-OHPHN")

#Data cleaning 
########################################################################
data <- data.frame(read.csv('QuantDoc.csv', header = F)) #load the data
#%>% c(data[1,] == "BPA-d16 (ISTD) Results" )

col.names <- unlist(gsub('Method', '', data[1,])) #Cleaning up row names 
col.names <- unlist(gsub('Results', '', col.names)) #to allow for easier
col.names <- unlist(gsub(' ', '', col.names)) #data treatment later on


type.index <- match('Type',c(unlist(filter(data[2,])))) #locate the column for type of sample
name.index <- match('Name',c(unlist(filter(data[2,])))) 


for (i in 1:(length(col.names) -1 )){
  #iterates through the row names to give them all a name
  if (col.names[i+1] == ""){col.names[i+1] <- col.names[i]} 
  
}
data[1,] <- col.names
sample.name <- data[1, type.index]

##############################################
#Finds relevant rows
#cal.data <- data %>% filter(V5 %in% c("Type",sample.name,"Cal"))


colnames(data) <- NULL

rownames(data) <- NULL
################################################################################

#Using the cleaned data, the dataframe can be made more compact
###############################################################################
df.dat <- data.frame()



for (compound in compounds){
  dat.temp <- data[,unlist(data[1,]) == compound]
  dat.temp$Compound <- compound
  dat.temp[2,][length(dat.temp[2,])] <- "Compound"
  colnames(dat.temp) <- dat.temp[2,]
  dat.temp <- dat.temp[-c(1,2),]
  df.dat <- rbind(df.dat, dat.temp)
}

df.dat$Type <- data[,type.index][3:length(data[,type.index])]
df.dat$Name <- data[,name.index][3:length(data[,name.index])]


write_xlsx(df.dat,"ExcelDoc.xlsx")

