suppressPackageStartupMessages(library("ggplot2"))  # For viz
suppressPackageStartupMessages(library(pdftools))   # for reading PDF files
suppressPackageStartupMessages(library(lattice))    # for some graphics
suppressPackageStartupMessages(library(quanteda))   # for text_readability 
suppressPackageStartupMessages(library(syuzhet))    # For Sentiment 
suppressPackageStartupMessages(library(tictoc))     # For Timing
suppressPackageStartupMessages(library(udpipe))     # For nlp
suppressPackageStartupMessages(library(data.table)) # For Data Manipulation
suppressPackageStartupMessages(library(readr))      # For writing data
suppressPackageStartupMessages(library(stringi))      # For writing data
suppressPackageStartupMessages(library(stringr))      # For writing data
setwd("~/R/Practical_Text/src")
pathname <- "../PDF_Input/"
filelist.ls <- list.files(path=pathname,pattern="*.pdf")
filepath.ls <-paste0(pathname,filelist.ls)

udmodel_english <- udpipe_load_model(file = '../udpipe/english-ud-2.0-170801.udpipe')

normalit<-function(m){
  (m - min(m,na.rm=TRUE))/(max(m,na.rm=TRUE)-min(m,na.rm=TRUE))
}

tr_total.ls <- list()
sentiment_total.ls <- list()
noun.ls <- list()
adj.ls <- list()
verb.ls <- list()
rake.ls <- list()
counter <- 1
noun_counter <- 1
adj_counter <- 1
verb_counter <- 1
rake_counter <- 1
for(file in filelist.ls) {
  pathname <- "../PDF_Input/"
  file_to_process <- paste0(pathname,file)
  print(file_to_process)
  filename_no_extension <- substr(file,1,nchar(file)-4)
  text <- pdf_text(file_to_process)
  section_size <- floor(length(text)/5)
  final_bit <- length(text)-(section_size*5)
  doc_id_text <-  c(rep(paste0(filename_no_extension," Section 01"),section_size)
                   ,rep(paste0(filename_no_extension," Section 02"),section_size) 
                   ,rep(paste0(filename_no_extension," Section 03"),section_size)
                   ,rep(paste0(filename_no_extension," Section 04"),section_size)
                   ,rep(paste0(filename_no_extension," Section 05"),section_size+final_bit)
                   )
  
  raw.df <- data.frame(doc_id=doc_id_text,raw_text=text,stringsAsFactors = FALSE)
  
  tr.df <- textstat_readability(corpus(raw.df,docid_field="doc_id",text_field="raw_text"))
  
  spl <- strsplit(as.character(tr.df$document), "\\.")
  tr.df$section <- sapply(lapply(spl,head,-1),paste,collapse="\\.")
  tr.df$subsection <- sapply(lapply(spl,tail,-1),paste,collapse="\\.")
  tr_total.ls[[counter]] <- tr.df
  
  Sentiment <- get_nrc_sentiment(raw.df$raw_text)
  
  sentiment.df <- data.frame(section_id=doc_id_text,Sentiment,stringsAsFactors = FALSE)
  sentiment.df$document <- substr(sentiment.df$section_id,1,nchar(sentiment.df$section_id)-11)
  sentiment.df$section <- substr(sentiment.df$section_id,nchar(sentiment.df$section_id)-10,nchar(sentiment.df$section_id))
  sentiment_total.ls[[counter]] <- sentiment.df
  
  ann.raw <- udpipe_annotate(udmodel_english,text,doc_id=doc_id_text)
  
  ann.df <- data.frame(ann.raw)
  noun <- subset(ann.df, upos %in% c("NOUN"))
  for(ntmp in unique(noun$doc_id)) { 
    tmp.df <- noun[noun$doc_id==ntmp,]
    nfreq <- txt_freq(tmp.df$token)
    noun.ls[[noun_counter]] <- data.frame(doc_id=ntmp,nfreq,stringsAsFactors = FALSE)
    noun_counter <- noun_counter + 1
    }
 
   adj <- subset(ann.df, upos %in% c("ADJ")) 
  for(atmp in unique(adj$doc_id)) { 
    tmp.df <- adj[adj$doc_id==atmp,]
    afreq <- txt_freq(tmp.df$token)
    adj.ls[[adj_counter]] <- data.frame(doc_id=atmp,afreq,stringsAsFactors = FALSE)
    adj_counter <- adj_counter + 1
  }
   verbs <- subset(ann.df, upos %in% c("VERB"))   
   for(vtmp in unique(verbs$doc_id)) { 
     tmp.df <- verbs[verbs$doc_id==vtmp,]
     vfreq <- txt_freq(tmp.df$token)
     verb.ls[[adj_counter]] <- data.frame(doc_id=vtmp,vfreq,stringsAsFactors = FALSE)
     verb_counter <- verb_counter + 1
   }
#   
#   rake <- keywords_rake(ann.df , term = "lemma", group = "doc_id", 
#                         relevant = full_ann.df$upos %in% c("NOUN", "ADJ"))
#   rake$key <- factor(rake$keyword, levels = rev(rake$keyword))
# # 
   for(rtmp in unique(ann.df$doc_id)) { 
     tmp.df <- ann.df[ann.df$doc_id==rtmp,]
     tmp.df$phrase_tag <- as_phrasemachine(tmp.df$upos, type = "upos")
     phrases <- keywords_phrases(x = tmp.df$phrase_tag, term = tmp.df$token, 
                               pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                               is_regex = TRUE, detailed = FALSE)
     phrases <- subset(phrases, ngram > 1 & freq > 3)
     phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
     rake.ls[[rake_counter]] <- data.frame(doc_id=rtmp,phrases,stringsAsFactors = FALSE)
     rake_counter <- rake_counter+1
   }
  
  #noun.ls[[counter]] <- noun
  counter <-counter + 1
  
  
}
tr_final.df        <- rbindlist(tr_total.ls)
sentiment_final.df <- rbindlist(sentiment_total.ls)
noun.df            <- rbindlist(noun.ls)
adj.df             <- rbindlist(adj.ls)
verb.df            <- rbindlist(verb.ls)
rake.df            <- rbindlist(rake.ls)

noun.df <- noun.df[grep("[:print:]",noun.df$key),]
adj.df  <-  adj.df[grep("[:print:]",adj.df$key),]
verb.df <- verb.df[grep("[:print:]",verb.df$key),]
rake.df <- rake.df[grep("[:print:]",rake.df$key),]
# write_csv(noun.df,'../Tableau_input/noun.csv')

write_csv(tr_final.df,'../Tableau_input/Readability.csv')
write_csv(sentiment_final.df,'../Tableau_input/Sentiment.csv')
write_csv(noun.df,'../Tableau_input/noun.csv')
write_csv(adj.df,'../Tableau_input/adj.csv')
write_csv(verb.df,'../Tableau_input/verb.csv')
write_csv(rake.df,'../Tableau_input/rake.csv')

index_string <- function(x) {paste0(sort(unlist(strsplit(as.character(x), " ",fixed=TRUE)[[1]])),collapse="") }
noun_graph.df <- 
  merge(data.frame(source=noun.df$doc_id,term=noun.df$key,stringsAsFactors = FALSE)
        ,data.frame(term=noun.df$key,target=noun.df$doc_id,stringsAsFactors = FALSE))

noun_graph.df <- noun_graph.df[noun_graph.df$source!=noun_graph.df$target,]

#noun_graph.df$index <- paste(noun_graph.df$source,noun_graph.df$target,sep=" ")

#noun_graph.df$index <- apply(noun_graph.df[4],2,index_string)

# index_string <- function(x) {paste0(sort(unlist(strsplit(as.character(x), " ",fixed=TRUE)[[1]])),collapse="") }
# noun_graph.df$index <- sapply(noun_graph.df$index, function (x) paste(sort(unlist(strsplit(as.character(x), " ",fixed=TRUE)[[1]])),sep=" "))
# 
# # for(i in 1:nrow(noun_graph.df)) {
#   noun_graph.df[i,]$index <- paste(sort(unlist(strsplit(noun_graph.df[i,]$index," "))),collapse="")
# }

#unique_noun_graph.df <- ddply(noun_graph.df,~index+term,summarize,source=min(source),target=min(target))

write_csv(noun_graph.df[,c(2,3,1)],'../Graph_Input/noun_graph.csv')


adj_graph.df <- 
  merge(data.frame(source=adj.df$doc_id,term=adj.df$key,stringsAsFactors = FALSE)
        ,data.frame(term=adj.df$key,target=adj.df$doc_id,stringsAsFactors = FALSE))

adj_graph.df <- adj_graph.df[adj_graph.df$source!=adj_graph.df$target,]

#adj_graph.df$index <- paste(adj_graph.df$source,adj_graph.df$target,sep=" ")

#adj_graph.df$index <- apply(adj_graph.df[4],2,index_string)

# for(i in 1:nrow(adj_graph.df)) {
#   adj_graph.df[i,]$index <- paste(sort(unlist(strsplit(adj_graph.df[i,]$index," "))),collapse="")
# }

#unique_adj_graph.df <- ddply(adj_graph.df,~index+term,summarize,source=min(source),target=min(target))

write_csv(adj_graph.df[,c(2,3,1)],'../Graph_Input/adj_graph.csv')


verb_graph.df <- 
  merge(data.frame(source=verb.df$doc_id,term=verb.df$key,stringsAsFactors = FALSE)
        ,data.frame(term=verb.df$key,target=verb.df$doc_id,stringsAsFactors = FALSE))

verb_graph.df <- verb_graph.df[verb_graph.df$source!=verb_graph.df$target,]

# verb_graph.df$index <- paste(verb_graph.df$source,verb_graph.df$target,sep=" ")
# for(i in 1:nrow(verb_graph.df)) {
#   verb_graph.df[i,]$index <- paste(sort(unlist(strsplit(verb_graph.df[i,]$index," "))),collapse="")
# }
# 
# unique_verb_graph.df <- ddply(verb_graph.df,~index+term,summarize,source=min(source),target=min(target))

write_csv(verb_graph.df[,c(2,3,1)],'../Graph_Input/verb_graph.csv')


rake_graph.df <- 
  merge(data.frame(source=rake.df$doc_id,term=rake.df$key,stringsAsFactors = FALSE)
        ,data.frame(term=rake.df$key,target=rake.df$doc_id,stringsAsFactors = FALSE))

rake_graph.df <- rake_graph.df[rake_graph.df$source!=rake_graph.df$target,]

# rake_graph.df$index <- paste(rake_graph.df$source,rake_graph.df$target,sep=" ")
# for(i in 1:nrow(rake_graph.df)) {
#   rake_graph.df[i,]$index <- paste(sort(unlist(strsplit(rake_graph.df[i,]$index," "))),collapse="")
# }
# 
# unique_rake_graph.df <- ddply(rake_graph.df,~index+term,summarize,source=min(source),target=min(target))

write_csv(rake_graph.df[,c(2,3,1)],'../Graph_Input/rake_graph.csv')

