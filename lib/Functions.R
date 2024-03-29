#speeches in which the word "women" appear
#tdm.tidy.women <- tdm.tidy[tdm.tidy$term %in% c("women","woman","girls","girl"),]
#tdm.tidy.women
#consider all instances of "women", "woman", "girls", and "girl" to be equal/same
#split/apply/combine to summarize the number of times "women" (and like) are mentioned in each speech
#library(plyr)
#instances.women <- ddply(tdm.tidy.women[,2:3],.(document),summarize,total_count=sum(count))
#(instances.women <- instances.women[order(instances.women[,2],decreasing=T),])

get.tdm.tidy.term <- function(tdm.tidy,term.vector){
  return(tdm.tidy[tdm.tidy$term %in% term.vector , ]) #tdm.tidy.term output
}

get.instances <- function(tdm.tidy.term){
  instances.term <- ddply(tdm.tidy.term[,2:3],.(document),
                           summarize,total_count=sum(count))
  return(instances.term[order(instances.term[,2],decreasing=T),]) #instances.term output
}



#only look at the speeches that mention "women" (and the like) 
#should be 16 speeches in the new corpus
#speeches.women <- unique(tdm.tidy.women$document)
#pattern <- paste(speeches.women,collapse ="|")
#ff.women <- Corpus(DirSource(folder.path,pattern=pattern))

get.speeches <- function(tdm.tidy.term){
  return(unique(as.data.frame(tdm.tidy.term)[,2])) #speeches.term output
}

get.ff.term <- function(folder.path,speeches.term){
  pattern <- paste(speeches.term,collapse ="|")
  return(Corpus(DirSource(folder.path,pattern=pattern))) #ff.term output
}


#This function is meant to be called by the function assoc.words(), defined below
#This function takes in a corpus object, a vector of terms, and a numeric, 
  #preprocesses the data, and outputs the x number of significant terms/words 
  #before and after the incidence of a word in the vector of terms, term.vector
association <- function(my.corpus,term.vector,x=10){
  #preprocessing data
  my.corpus<-tm_map(my.corpus, stripWhitespace)
  my.corpus<-tm_map(my.corpus, content_transformer(tolower))
  my.corpus<-tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus<-tm_map(my.corpus, removeWords, character(0))
  my.corpus<-tm_map(my.corpus, removePunctuation)
  my.corpus<-tm_map(my.corpus, PlainTextDocument)
  speech <- my.corpus[[1]]$content
  actual <- unlist(strsplit(speech,"^[[:space:]]+|[[:space:]]+"))
  indices <- which(actual %in% term.vector)
  assoc.words<-NULL
  #we want the x words before & after each occurrence of a word in term.vector
  for (i in indices){
    assoc.words <- c(assoc.words,actual[(i-x):(i+x)])
  }
  return(assoc.words)
}



#This function takes three inputs: (1) folder.path, a character string of the path 
  #a specific folder, (2) speeches.term, a character vector of file names 
  #that correspond to a speech that contains a specified word or its equal,
  #and (3) x, a numeric that specifies the number of significant words before 
  #and after each incidence of a specified word or its equal to extract 
#This function outputs a named list of the significant words associated with a
  #specified word or its equal 
#This function calls on the function association() 
assoc.words <- function(folder.path,speeches.term,x=10){
  my.list <- list() 
  for (i in 1:length(speeches.term)){
    my.corpus <- Corpus(DirSource(folder.path,pattern=speeches.term[i]))
    my.list <- c(my.list, list(association(my.corpus,term.vector,x)))
  }
  names(my.list) <- speeches.term
  return(my.list) #assoc.term output
}


#get dates of each speech in speeches.term
#"inaugWoodrowWilson-1.txt" 
#library(stringr)
#This function takes in a character vector of file names of relevant speeches that
  #contain the specified term or its equivalents
#This function returns a dataframe of two columns: (1) Speech (filename) and (2) Date
  #with the dataframe ordered in ascending order from top to bottom by year 
get.speech.date <- function(speeches.term){
  expr <- "[A-Z][a-z]+[A-Z|a-z]+-[1-4]"
  speech.dates.term <- rep(NA,length(speeches.term))
  for (i in 1:length(speeches.term)){
    v <- str_split(str_extract(speeches.term[i],expr),"-")
    #v <- str_split(str_extract("inaugDonaldJTrump-1.txt",expr),"-")
    name. <- v[[1]][1] ; p.term <- as.numeric(v[[1]][2]) + 1
    ii <- str_locate_all(name.,"[A-Z]")[[1]]
  
    if (nrow(ii) == 3){
      name. <- paste(substr(name.,start=1,stop=as.numeric(ii[2,1]-1)), " ",
                  substr(name.,start=as.numeric(ii[2,1]),stop=as.numeric(ii[3,1]-1)),
                  ". ", substr(name.,start=as.numeric(ii[3,1]),stop=nchar(name.)),
                  sep="")
    } else {
      name. <- paste(substr(name.,start=1,stop=as.numeric(ii[2,1]-1)),
                     substr(name.,start=as.numeric(ii[2,1]),stop=nchar(name.)))
    }
    #presumably dates is a global variable  
    speech.dates.term[i] <- as.character(dates[dates$PRESIDENT == name. , p.term])
  }
  dated.speeches.term <- cbind(speeches.term,speech.dates.term)
  colnames(dated.speeches.term) <- c("Speech (file name)","Date")
  
  ordered.speeches.term <- sapply(speech.dates.term,str_sub,start=-4) #this is a list
  #coerce to vector
  ordered.speeches.term <- as.vector(ordered.speeches.term) 
  i <- order(as.numeric(ordered.speeches.term),decreasing = F)
  dated.speeches.term <- dated.speeches.term[i,]
  return(dated.speeches.term)
}


#This function gets the full text from each speech that contains the 
  #specified word or its equivalents 
#This function returns a dataframe with 3 columns: Speech, Date, Full Speech Text
    #called dated.speeches.term.f 
    #each row in dated.speeches.term.f corresponds to one of the inaugural speeches 
    #that contain the specified word or its equivalents
full.text <- function(ff.term,dated.speeches.term){
  speeches.term.list <- list() 
  length(speeches.term.list) <- nrow(dated.speeches.term)
  names(speeches.term.list) <- dated.speeches.term[,1]
  for (i in dated.speeches.term[,1]){
    speeches.term.list[[i]] <- ff.term[[i]]$content
  }
  for (i in 1:length(speeches.term.list)){
    if (names(speeches.term.list)[i] =="inaugDonaldJTrump-1.txt") {
      speeches.term.list[i] <- paste(unlist(speeches.term.list[i]),collapse="")
    }
  }
  dated.speeches.term.f <- cbind(dated.speeches.term,speeches.term.list)
  colnames(dated.speeches.term.f)[3] <- "Full Speech Text"
  return(dated.speeches.term.f)
}


get.sentences <- function(dated.speeches.term.f){
  sentence.list=list()
  length(sentence.list) <- nrow(dated.speeches.women.f)
  
  for(i in 1:nrow(dated.speeches.women.f)){
    sentences=sent_detect(dated.speeches.term.f[,3][i],
                          endmarks = c("?", ".", "!", "|",";"))
    if(length(sentences)>0){
      emotions=get_nrc_sentiment(sentences)
      word.count=word_count(sentences)
      # colnames(emotions)=paste0("emo.", colnames(emotions))
      # in case the word counts are zeros?
      emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
      sentence.list[[i]] <- cbind(sentences=as.character(sentences), 
                                  word.count,
                                  emotions,
                                  sent.id=1:length(sentences)
      )
    }
  }
  
  names(sentence.list) <- dated.speeches.term.f[,1]  
  return(sentence.list)
}



##For one element in sentence.list:
#get the sentences that contain "women"
#vec.sent <- sentence.list[[1]][,1]
#logical vector indicating whether sentence contains "women"
#sapply(vec.sent,grepl,pattern="women") 
#vec.sent[sapply(vec.sent,grepl,pattern="women")]

##For all elements in sentence.list:
get.sentences.term <- function(sentence.list,term.vector){
  #First, create an empty list
  sentence.list.term=list() 
  #Second, set the length of the newly created empty list
  #this improves efficiency and allows us to reference list elements by indices
  length(sentence.list.term) <- length(sentence.list)  
  
  #Iterate over each element (dataframe) in sentence.list: 
  for (i in 1:length(sentence.list)){
    #For each dataframe: 
    #Use a placeholder variable for the data from the first column, i.e. the vector of 
    #sentences that make up the corresponding speech
    vec.sent <- sentence.list[[i]][,1]
  
  #Identify the relevant row(s) of data from the dataframe then combine them into a
    #new dataframe
  #Then, set the new dataframe as the corresponding element in our new list
    for (j in 1:length(term.vector)){
      sentence.list.term[[i]] <-
      rbind(sentence.list.term[[i]], 
        sentence.list[[i]][sapply(vec.sent,grepl,pattern=term.vector[j]),])
    }
  }
  #Let's name our new list with the same names as our old list, since each element 
    #in our new list and its counterpart in the old list correspond to the same speech
  #i.e., each element (dataframe) is named after the file-name of the respective       
    #inaugural speech
  names(sentence.list.term) <- names(sentence.list)
  return(sentence.list.term)
}

