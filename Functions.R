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
  return(unique(tdm.tidy.term$document)) #speeches.term output
}

get.ff.term <- function(folder.path,speeches.term){
  pattern <- paste(speeches.term,collapse ="|")
  return(Corpus(DirSource(folder.path,pattern=pattern)) #ff.term output
}


#This function is meant to be called by the function assoc.words(), defined below
#This function takes in a corpus object, a vector of terms, and a numeric, 
  #preprocesses the data, and outputs the x number of significant terms/words 
  #before and after the incidence of a word in the vector of terms, term.vector
association <- function(my.corpus,term.vector,x){
  #preprocessing data
  my.corpus<-tm_map(my.corpus, stripWhitespace)
  my.corpus<-tm_map(my.corpus, content_transformer(tolower))
  my.corpus<-tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus<-tm_map(my.corpus, removeWords, character(0))
  my.corpus<-tm_map(my.corpus, removePunctuation)
  my.corpus<-tm_map(my.corpus, PlainTextDocument)
  speech <- my.corpus[[1]]$content
  actual <- unlist(strsplit(speech,"^[[:space:]]+|[[:space:]]+"))
  indices <- which(actual %in% c("women","woman","girls","girl"))
  assoc.words<-NULL
  #we want the 10 words before & after each occurrence of "women" (and the like)
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
assoc.words <- function(folder.path,speeches.term,x){
  my.list <- list() 
  for (i in 1:length(speeches.term)){
    my.corpus <- Corpus(DirSource(folder.path,pattern=speeches.term[i]))
    my.list <- c(my.list, list(association(my.corpus,x)))
  }
  names(my.list) <- speeches.term
  return(my.list)
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
    name. <- v[[1]][1] ; p.term <- as.numeric(v[[1]][2])
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
  colnames(dated.speeches.women) <- c("Speech (file name)","Date")
  
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
  for (i in 1:length(speeches.term.list)){
    speeches.term.list[[i]] <- ff.term[[i]]$content
  }
  names(speeches.term.list) <- dated.speeches.term[,1]
  dated.speeches.term.f <- cbind(dated.speeches.term,speeches.term.list)
  colnames(dated.speeches.term.f)[3] <- "Full Speech Text"
  return(dated.speeches.term.f)
}


