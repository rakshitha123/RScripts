require(XML)
library(hash)
library(forecast)
library(ggplot2)
library(reshape2)
library(zoo)

dir <- 'C:/Projects/NYTimes/NYTimes/nyt_corpus/data/2007/Extracted/01'
input_file <- read.csv(file = paste('C:/Projects/NYTimes/outputs/online_sections_2007_01.csv', sep=''))
sections <- input_file$Section

cols <- c()
classes <- c()

for(i in 1:length(sections)){
  cols[i] <- as.character(sections[i])
  classes[i] <- "integer"
}

cols[length(cols)+1] <- "date"
classes[length(classes)+1] <- "character"

section_propotions <- read.table(text = "", colClasses = classes, col.names = cols)



all_files_days<-list.files(dir)



for(day in 1:length(all_files_days)){
  all_files<-list.files(paste0(dir,"/",all_files_days[day]))
  print(day)
  section_propotions[day,"date"] <- day
  
  for(col in 1:length(cols)-1){
    section_propotions[day,cols[col]] <- 0
  }
  
  for (i in 1:length(all_files)){
    xml_files <- htmlParse(paste0(dir,"/",all_files_days[day],"/",all_files[i]), useInternalNodes=T)
    section <- (xml_files['//meta[@name="online_sections"]/@content'])[[1]][["content"]]
    values <- strsplit(as.character(section),"; ")  
    
    for(i in values){
      for(j in 1:length(i)){
          section_propotions[day,i[j]] <- section_propotions[day,i[j]] + 1/length(all_files)
      }
    }
  }
}


section_propotions <- section_propotions[,colSums(is.na(section_propotions))<nrow(section_propotions)]
print(section_propotions)

write.csv(section_propotions, paste('C:/Projects/NYTimes/Outputs/section_propotions.csv', sep=''), row.names = FALSE)






