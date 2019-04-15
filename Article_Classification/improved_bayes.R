library(hash)


token_count <-2000
document_count <- 25000



top_tokens <- read.csv('C:/Projects/Trevor/Article Classification/data/outputs/top_token_doc_counts.csv')
all_token_counts <- readRDS(file=file.path('C:/Projects/Trevor/Article Classification/data/outputs/all_token_counts.rds'))
topic.file <- 'C:/Projects/Trevor/Article Classification/data/rcv1-v2.topics.qrels'



#Creating table with doc ids and their categories

topics <- read.delim(topic.file, header=FALSE, sep=' ',
                       colClasses=c('character', 'integer', 'integer'),
                       col.names = c('category', 'itemid', 'value'))
  
topics <- as.data.frame(topics)
  
topics <- topics[grep('.CAT', topics$category),]    #Take rows only have category ending with CAT
topics$category <- substr(topics$category,0,1)   # Take the first letter of category
  
topics <- aggregate(category ~., topics, toString)   # Aggregate with comma separated categories. Each row has doc id and comma separated category names
  
topics$category =  gsub(", ","",topics$category)   # Remove commas in categories

topics <- head(topics,document_count)    #Get the required set of documents 

category_Counts <- aggregate(topics$value, list(topics$category), sum) #Getting total docs for each category - add value column grouped by category





#Create category doc id table

category_doc_list <- hash()  #hashmap with category and doc id 
  
for(topic in 1:nrow(topics)){
  category_doc_list[[topics[topic,"category"]]] = c(category_doc_list[[topics[topic,"category"]]], as.character(topics[topic,"itemid"]))
}

tokens <- top_tokens[1:token_count,]
  



create_freq_table <- function(){
  cols <- c("Token")
  classes <- c("character")
  
  for (cat in 1:nrow(category_Counts)){
    cols <- c(cols, as.character(category_Counts[cat,"Group.1"]))
    classes <- c(classes, "integer")
  }
  
  freq_tbl <- read.table(text = "", colClasses = classes, col.names = cols)
  
  #for(token in 1:token_count){
  #  freq_tbl[token,1]=as.character(tokens[token,"token"])
  #  for(cat in 1:nrow(category_Counts)){
  #    doc_ids <- category_doc_list[[as.character(category_Counts[cat,"Group.1"])]]
  #    count <- 0
  #    print(length(doc_ids))
  #    for(id in 1:length(doc_ids)){
  #      if(as.character(tokens[token, 'token']) %in% names(all_token_counts[[as.character(doc_ids[id])]])){
  #        count = count + 1
  #      }
  #    }
  #    freq_tbl[token, cat+1]=count
  #  }
  #}
  
  for(topic in 1:nrow(topics)){
    if((topic %% 100)==0){
      print(paste0("doc ", topic))
    }
    
    cat <- as.character(topics[topic, "category"])
    
    doc_tokens <- names(all_token_counts[[as.character(topics[topic, "itemid"])]])
    
    for(token in 1:token_count){
      freq_tbl[token,1]=as.character(tokens[token,"token"])
      if(is.na(freq_tbl[token, cat])){
        freq_tbl[token, cat] = 0
      }
      if(as.character(tokens[token,"token"]) %in% doc_tokens){
        freq_tbl[token, cat]=freq_tbl[token, cat] +1
      }
    }
  }
  freq_tbl
}


freq_tbl <- create_freq_table()
freq_tbl
write.csv(freq_tbl, paste('C:/Projects/Trevor/Article Classification/my_codes/Outputs/freq_table_2000_25000.csv', sep=''), row.names = FALSE)



calculateProbability <- function(itemid, category, category_index){
  doc_tokens <- names(all_token_counts[[as.character(itemid)]])
  category_frequency <- category_Counts[category_index, "x"]
  
  estimated_prob <- category_frequency #f(y)
  
  for(token in 1:nrow(tokens)){
    if(as.character(tokens[token, 'token']) %in% doc_tokens){
      #for yes values
      estimated_prob <- estimated_prob * (freq_tbl[token,as.character(category)]/category_frequency) #f(xj^y)/f(y)
    }
    else{
      #for no values
      estimated_prob <- estimated_prob * ((category_frequency - freq_tbl[token,as.character(category)])/category_frequency)
    }
  }
  
  estimated_prob
}



getAccuracy <- function(data){
  count <- 0
  for(i in 1:nrow(data)){
    if(as.character(data[i,"Actual"])==as.character(data[i,"Predicted"])){
      count <- count+1
    }
  }
  print(paste0(count," out of ",nrow(data)," correct"))
}



create_prob_table <- function(){
  prob_cols <- c("itemid")
  prob_classes <- c("character")
  
  for (cat in 1:nrow(category_Counts)){
    prob_cols <- c(prob_cols, as.character(category_Counts[cat,"Group.1"]))
    prob_classes <- c(prob_classes, "integer")
  }
  
  prob_cols <- c(prob_cols, "Actual","Predicted")
  prob_classes <- c(prob_classes, "character", "character")
  
  prob_tbl <- read.table(text = "", colClasses = prob_classes, col.names = prob_cols)
  
  for (topic in 1:nrow(topics)){
    prob_tbl[topic,1]=as.character(topics[topic,"itemid"])
    for (cat in 1:nrow(category_Counts)){
      prob_tbl[topic, cat+1] = calculateProbability(as.character(topics[topic,"itemid"]), category_Counts[cat,"Group.1"], cat)
    }
  }
  
  prob_tbl$Actual <- topics$category
  
  temp <- prob_tbl
  temp$itemid <- NULL
  temp$Predicted <- colnames(temp)[apply(temp,1,which.max)]
  
  prob_tbl$Predicted <- temp$Predicted  
  
  prob_tbl
}

prob_tbl <- create_prob_table()
getAccuracy(prob_tbl)




