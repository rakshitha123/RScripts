#Can use to get section counts for all days separately to draw a seasonal plot

require(XML)
library(hash)
library(ggplot2)
library(fpp2)
library(forecast)

dir <- 'C:/Projects/NYTimes/NYTimes/nyt_corpus/data'


all_files_years<-list.files(dir)
xml_files <- list()
year_range <- c(1987:2007)

sections_names <- c()

# createTable <- function(){
#   df<-data.frame(1987,0, 0, 0, 0, 0, 0, 0)
#   names(df)<-c("Year", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
#   
#   for(i in 1988:2007){
#     de<-data.frame(i,0, 0, 0, 0, 0, 0, 0)
#     names(de)<-c("Year", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
#     df <- rbind(df, de)
#   }
#   
#   df
# }


createMatrix <- function(){
  inputs <- matrix(, ncol=7,nrow=21)
  
  inputs_columns <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  inputs_rows <- c(1987:2007)

  colnames(inputs) <- inputs_columns
  rownames(inputs) <- inputs_rows
  inputs <- as.table(inputs)
  
  for(x in 1:nrow(inputs)){
    for(y in 1:ncol(inputs)){
      inputs[x,y] <- 0
    }
  }
  
  inputs
}


convertIntoTs <- function(obs){
  result <- c()
  
  for(x in 1:nrow(obs)){
    for(y in 1:ncol(obs)){
      result <- c(result, obs[x,y])
    }
  }
  
  
  result <- ts(data = result, start = 1987, frequency = 7)
  result
}


plotGraph <- function(data){
  ggseasonplot(convertIntoTs(data), labels = inputs_columns ,main=paste0("Article counts for ",deparse(substitute(data))," from 1987-2007"), ylab="Article Counts")
}



for(year in 1:length(all_files_years)){
  
  currentYear <- all_files_years[year]
  all_files_months<-list.files(paste0(dir,"/",currentYear,"/Extracted"))
  
  for(month in 1:length(all_files_months)){
    currentMonth <- all_files_months[month]
    all_files_days<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth))
    print(paste0(dir,"/",currentYear,"/",currentMonth))
    
    for(day in 1:length(all_files_days)){
      all_files<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth, "/", all_files_days[day]))
      print(length(all_files))
      for (f in 1:length(all_files)){
        xml_files <- htmlParse(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[f]), useInternalNodes=T)
        section <- (xml_files['//meta[@name="online_sections"]/@content'])[[1]][["content"]]
        dayName <- (xml_files['//meta[@name="publication_day_of_week"]/@content'])[[1]][["content"]]
        year <- (xml_files['//meta[@name="publication_year"]/@content'])[[1]][["content"]]
        
        
        values <- strsplit(as.character(section),"; ")  
        
        for(i in values){
          
          for(j in 1:length(i)){
            if((!(is.null(dayName))) && (!(is.null(year))) && (as.character(dayName)!="") && (as.character(year)!="") && (strtoi(year) %in% year_range)){
              if(!(exists(gsub("\\s", "", as.character(i[j]))))){
                sections_names <- c(sections_names, gsub("\\s", "", as.character(i[j])))
                assign((gsub("\\s", "", as.character(i[j]))), createMatrix())
              }
              current <- get(gsub("\\s", "", as.character(i[j])))
              #print(paste0(all_files[f], " ", year, " ", dayName))
              current[as.character(year), as.character(dayName)] <- current[as.character(year), as.character(dayName)] +1
              assign((gsub("\\s", "", as.character(i[j]))), current)
            }
          }
        }
      }
    }
  }
  
}


print(sections_names)

for(i in sections_names){
  write.csv(get(gsub("\\s", "", i)), paste('C:/Projects/NYTimes/Outputs/seasonal_plots/all/',i,'.csv', sep=''), row.names = FALSE)
}














