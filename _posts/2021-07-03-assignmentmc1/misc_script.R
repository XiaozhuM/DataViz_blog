#change document1 and document2 to integer
sim_df$document1 <- as.integer(as.character(sim_df$document1))
sim_df$document2 <- as.integer(as.character(sim_df$document2))

#table for merging source
df_source <- df_corpus %>%
  select(Doc_id,Source,date) 

#swap doc1 and doc2 id to make doc1 date always smaller than doc2 date
#add rowid as pair_id, add percentile as ranking based on cosine
sim_tmp <- sim_df %>%
  rowid_to_column(var="pair_id") %>%
  mutate(rank=ntile(cosine,200)) %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  filter(Source1 != Source2) %>%
  filter(date1 != date2) %>%
  transform(document1=ifelse(date1<=date2, document1, document2),
            document2=ifelse(date1<=date2, document2, document1)) %>%
  transform(Source1=ifelse(date1<=date2, Source1, Source2),
            Source2=ifelse(date1<=date2, Source2, Source1))

#get the documents with highest similarity and earlier date
df_primary <- sim_tmp %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  group_by(Source1) %>%
  count(document1) %>%
  mutate(role="primary") 

#get the later
df_secondary <- sim_tmp %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  group_by(Source2) %>%
  count(document2) %>%
  mutate(role="secondary") 
 
df_join <- df_corpus %>%
  left_join(df_primary, by=c("Doc_id"="document1"), suffix=c("_pri","_sec")) %>%
  left_join(df_secondary, by=c("Doc_id"="document2"), suffix=c("_pri","_sec")) %>%
  select(-Source1,-Source2) %>%
  unite("role", n_pri, role_pri, n_sec, role_sec, sep="_",na.rm=TRUE)

#dependancies between sources
sim_nodes <- df_source %>%
  distinct(Source) %>%
  rename(label=Source) %>%
  rowid_to_column("id")

sim_edges <- sim_tmp %>%
  group_by(Source1, Source2) %>%
  summarise(weight=n()) %>%
  left_join(sim_nodes, by=c("Source1"="label")) %>%
  rename(from=id) %>%
  left_join(sim_nodes, by=c("Source2"="label")) %>%
  rename(to=id) %>%
  ungroup() %>%
  select(from, to, weight)
  
#build network with network package
sim_network <- network(sim_edges, vertex.attr=sim_nodes, matrix.type="edgelist",
                       ignore.eval=FALSE)
plot(sim_network, vertex.cex=3)


#
sim_edges <- mutate(sim_edges, width = weight)
sim_nodes <- mutate(sim_nodes, title = label)

  

#get the max cosine for each document
sim_tmp_max <- sim_tmp %>%
  group_by(document1) %>%
  summarise(max=max(cosine))

#select only the most similar doc to each document
# keep only the top 5% similar pairs and rank again 
sim_tmp <- sim_tmp %>%
  right_join(sim_tmp_max, by=c("document1"="document1", "cosine"="max")) %>%
  filter(rank==20) %>%
  mutate(rank=ntile(cosine, 20))

#check if there are multiple similar items to 1 text, inspect closer
sim_tmp %>%
  group_by(document1) %>%
  summarise(n=n(), mean(rank)) %>%
  filter(n>1)

sim_tmp %>%
  filter(document1==731)

#only 557 and 652 in document 1 have 2 most similar items, by checking, they have
#cosine similarity of 0.311 between 557 and  710, 829; cosine 0.722 between 652 
#with 782 and 731. the rank is 1 and 12. one in each is removed because the similarity
#is captured by 710+829 and 782+731
sim_tmp <- sim_tmp %>%
  filter(pair_id != 114064,pair_id !=145034)

#tmp table for only doc1
tmp1 <- sim_tmp %>%
  select(pair_id, document1, cosine, rank) %>%
  rename(document=document1)

#merge tmp table with selected doc2
sim_tmp %>%
  select(pair_id, document2, cosine, rank) %>%
  rename(document=document2) %>%
  bind_rows(tmp1) %>%
  group_by(document) %>%
  summarise(n=n(),mean(rank)) %>%
  arrange(desc(n))


sim_tmp2 <- sim_tmp %>%
  left_join(df_corpus, by= c("document1" = "Doc_id"), suffix=c("_1","_2")) %>%
  left_join(df_corpus, by= c("document2" = "Doc_id"), suffix=c("_1","_2")) 

colnames(sim_tmp2)[colnames(sim_tmp2) == 'Text_1'] <- 'Text'

pair_pri <- sim_tmp %>%
  select(cosine,pair_id,document1,document2) %>%
  group_by(document1) %>%
  arrange(document1,desc(cosine)) %>%
  summarize(similar_later=paste(document2, collapse="_"))

pair_sec <- sim_tmp %>%
  select(cosine,pair_id,document1,document2) %>%
  group_by(document2) %>%
  arrange(document2,desc(cosine)) %>%
  summarize(similair_earlier=paste(document1, collapse="_"))

df_join <- df_join %>%
  left_join(pair_pri, by=c("Doc_id"="document1")) %>%
  left_join(pair_sec, by=c("Doc_id"="document2"))
  




sim_dashboard <- prepare_data(
  dataset = sim_tmp2,
  date_based_corpus = FALSE,
  columns_doc_info=c("pair_id","document1","Title_1","Author_1", "Source_1","date_1", "Location_1",
                     "simple.matching","document2", "Title_2","Author_2","Source_2","date_2",
                     "Location_2", "Text_2"),
  grouping_variable = "Source_1"
)



#----------------------------bias--------------
source <- c("Homeland Illumination")

POK_corpus <- df_corpus %>%
  filter(str_detect(Text, 'Elian Karel')) %>%
  group_by(Source) %>%
  filter(Source==source)

stopwords <- c(stopwords("english"),"pok","kronos","protectors", "elodis",
               "years", "many","homeland","illumination","abila","several",
               "today","people", "elian", "karel")

POK_dfm <- dfm(corpus(POK_corpus, docid_field="Doc_id", text_field="Text"), 
                  remove=stopwords, remove_punct=TRUE)
POK_dfm <- dfm_trim(POK_dfm, min_termfreq = 4, max_docfreq = 10)

#POK_corpus <- kwic(news_corpus, "POK")

POK_lda_HI <- stm(POK_dfm, K = 5, verbose = FALSE)
plot(POK_lda_HI)    


#use udpipe
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
POK_pos <- udpipe_annotate(ud_model, x = POK_corpus$Text, doc_id = POK_corpus$Doc_id)

POK_pos <- as.data.frame(POK_pos)
tag <- c("JJ","VB")

POK_words <- POK_pos %>%
  filter(str_detect(xpos, tag)) %>%
  select(lemma) %>%
  group_by(lemma) %>%
  summarise(n=n()) %>%
  arrange(desc(n))



#-------------email header network------------
email_matrix <- GAStech_edges %>%
  select(source, target) %>%
  group_by(source, target) %>%
  summarise(n=n()) 
  
email_matrix <- graph.data.frame(email_matrix, directed=TRUE)
email_matrix <- get.adjacency(email_matrix, attr="n", sparse=TRUE)


chorddiag(as.matrix(email_matrix), 
          groupnamePadding=20, groupPadding=3,
          showTicks=FALSE)

# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(email_matrix, transparency = 0.5)


#---backup----
---
  title: "Assignment_MC1"
description: |
  A short description of the post.
author:
  - name: Xiaozhu Mao
url: {}
date: 07-03-2021
output:
  distill::distill_article:
  self_contained: false
toc: true
toc_float: true
draft: True
---
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, eval=TRUE)
```  

#### Loading in packages 

The packages will be used in the assignment. 

```{r tidy=TRUE}
packages = c('stringr', 'tidyr','plyr','tidyverse', 'tm', 'lubridate', 
             'corporaexplorer', 'quanteda','quanteda.textstats',"igraph",
             "visNetwork", "tidygraph", "ggraph",
             "networkD3", "stm")
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
```  

#### Loading in data  
1. get the list of articles in all paper sources  

```{r warning=FALSE}
list_of_paper <- list.files(path = "MC1/News Articles", recursive = TRUE,
                            pattern = ".", 
                            full.names = TRUE)
```    

2. iteratively load in each article and save into a list of dataframes 

```{r warning=FALSE, tidy=TRUE}
df_list <- list()
num <- 1
for (i in list_of_paper){
  temp <- lapply(i, readLines)
  
  temp <- lapply(1:length(temp),
                 function(j) data.frame(
                   news_no=str_extract(i, "(?<=\\/)\\d+"),
                   rawdata=temp[[j]],
                   stringsAsFactors = FALSE))
  
  df_temp <- do.call(rbind, temp)
  df_temp[,c("type","entry")] <-
    str_trim(str_split_fixed(df_temp$rawdata,":",2))
  
  df_temp <- df_temp[,c("news_no","type","entry")]
  df_temp <- pivot_wider(df_temp, names_from = type, values_from = entry)
  
  df_list[[num]] <- df_temp
  num <- num+1
}
```     

3. bind all dataframes in the list  

```{r}
df <- do.call(rbind.fill,df_list)
```

#### Data cleaning and pre-processing  

1. check data types  

```{r echo=TRUE}
glimpse(df)
```  

convert "published" to datetime, it is currently a character and format all to unify the format  

```{r warning=FALSE}
formats <- c("%d %B %Y","%d%B %Y","%B %d, %Y","%Y/%m%d")
df <-df %>%
  mutate(date = parse_date_time(PUBLISHED, formats))
``` 

convert Doc_id to int

```{r}
df$news_no <- as.integer(df$news_no)
```

convert date to date format

```{r}
df$date <- as.Date(as.POSIXct(df$date,tz="GMT"))
```

change column name and adjust column order in order to load to corpus  

```{r}
colnames(df) <- c("Doc_id","Source","Title","Published","Location","Text","Author","Note","date")
df_corpus <- df[,c("Doc_id","Text","Source","Title","Author","date","Location","Note")]
```  

use quanteda to load corpus and clean the corpus by removing stopwords and punctuations. Calculate cosine similarity between each pair of document and convert to a dataframe. 

```{r warning=FALSE}
news_corpus <- corpus(df_corpus, docid_field="Doc_id", text_field="Text")
clean_corpus <- dfm(news_corpus, remove=stopwords("english"), remove_punct=TRUE)
sim <- textstat_simil(clean_corpus, margin="document", method="cosine")
sim_df <- as.data.frame(sim)
```

change document1 and document2 to integer

```{r}
sim_df$document1 <- as.integer(as.character(sim_df$document1))
sim_df$document2 <- as.integer(as.character(sim_df$document2))
```

create a table of source for merging later

```{r}
df_source <- df_corpus %>%
  select(Doc_id,Source,date) 
```

swap doc1 and doc2 id to make doc1 date always earlier than doc2 date. add rowid as pair_id, split to 200 bins as ranking based on cosine, highest cosine value indicates highest rank, and highest similarity

```{r}
sim_tmp <- sim_df %>%
  rowid_to_column(var="pair_id") %>%
  mutate(rank=ntile(cosine,200)) %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  filter(Source1 != Source2) %>%
  filter(date1 != date2) %>%
  transform(document1=ifelse(date1<=date2, document1, document2),
            document2=ifelse(date1<=date2, document2, document1)) %>%
  transform(Source1=ifelse(date1<=date2, Source1, Source2),
            Source2=ifelse(date1<=date2, Source2, Source1))
```

since doc 1 is published earlier than doc2, here we assume doc1 is primary news and the matching doc2 is the secondary news. Count the number of primary and secondary news in each news source and label them as primary or secondary.

```{r echo=TRUE}
df_primary <- sim_tmp %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  group_by(Source1) %>%
  count(document1) %>%
  mutate(role="primary") 

df_secondary <- sim_tmp %>%
  filter(rank==200) %>%
  left_join(df_source, by=c("document1"="Doc_id"), suffix=c("1","2")) %>%
  left_join(df_source, by=c("document2"="Doc_id"), suffix=c("1","2")) %>%
  group_by(Source2) %>%
  count(document2) %>%
  mutate(role="secondary") 

df_join <- df_corpus %>%
  left_join(df_primary, by=c("Doc_id"="document1"), suffix=c("_pri","_sec")) %>%
  left_join(df_secondary, by=c("Doc_id"="document2"), suffix=c("_pri","_sec")) %>%
  select(-Source1,-Source2) %>%
  unite("role", n_pri, role_pri, n_sec, role_sec, sep="_",na.rm=TRUE)

glimpse(df_join)
```

get all the documents that are similar, similar_later are the documents with later published date, similar_earlier are the documents with earlier published date. This information will be used to enable easy search and compare of document.

```{r echo=TRUE, message=FALSE, warning=FALSE}
pair_pri <- sim_tmp %>%
  select(cosine,pair_id,document1,document2) %>%
  group_by(document1) %>%
  arrange(document1,desc(cosine)) %>%
  summarize(similar_later=paste(document2, collapse="_"))

pair_sec <- sim_tmp %>%
  select(cosine,pair_id,document1,document2) %>%
  group_by(document2) %>%
  arrange(document2,desc(cosine)) %>%
  summarize(similar_earlier=paste(document1, collapse="_"))

df_join <- df_join %>%
  left_join(pair_pri, by=c("Doc_id"="document1")) %>%
  left_join(pair_sec, by=c("Doc_id"="document2"))

glimpse(df_join)
```

use corporaexplore to visualiza all primary and secondary sources.

```{r eval=FALSE, warning=FALSE, message=FALSE, include=TRUE}
dashboard <- prepare_data(
  dataset = df_join,
  date_based_corpus = FALSE,
  columns_doc_info=c("Doc_id","Title","Author", "date", "Location",
                     "role","similar_later", "similar_earlier"),
  grouping_variable = "Source")

saveRDS(dashboard, "./dashboard/corpus_dashboard.rds", compress = FALSE)
```

```{r echo=FALSE, eval=FALSE, include=TRUE}
#deploy app to shiny.io
library(rsconnect)
rsconnect::deployApp('/Users/xiaozhumao/XiaozhuM/DataViz_blog/_posts/2021-07-03-assignmentmc1/dashboard')
```

click [here](https://xiaozhumao.shinyapps.io/dashboard/) to view the shiny app

<iframe src=" https://xiaozhumao.shinyapps.io/dashboard/"
style="border: 1px solid black; width: 100%; height: 500px;">
  </iframe>
  
  Next, create nodes list and edges list for visualizing dependancies network between sources

```{r warning=FALSE}
sim_nodes <- df_source %>%
  distinct(Source) %>%
  rename(label=Source) %>%
  rowid_to_column("id")

sim_edges <- sim_tmp %>%
  group_by(Source1, Source2) %>%
  summarise(weight=n()) %>%
  left_join(sim_nodes, by=c("Source1"="label")) %>%
  rename(from=id) %>%
  left_join(sim_nodes, by=c("Source2"="label")) %>%
  rename(to=id) %>%
  ungroup() %>%
  select(from, to, weight)
```

visualize dependencies between sources in a network graph

```{r echo=TRUE, message=FALSE, warning=FALSE}
sim_table <- sim_tmp %>%
  group_by(Source1, Source2) %>%
  summarise(similar_doc_count=n()) %>%
  left_join(sim_nodes, by=c("Source1"="label")) %>%
  left_join(sim_nodes, by=c("Source2"="label")) %>%
  select(Source1, Source2, similar_doc_count) %>%
  filter(similar_doc_count>=10) %>%
  arrange(desc(similar_doc_count))

view(sim_table)

```

```{r echo=TRUE, warning=FALSE}
visNetwork(sim_nodes, sim_edges) %>%
  visEdges(arrows = 'middle') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout="layout_in_circle")

```


#### stopped here
install chorddiag and load library

```{r}
devtools::install_github("mattflor/chorddiag")
library(chorddiag)
```

read in email headers node and edge

```{r message=FALSE}
GAStech_nodes <- read_csv("MC1/data/GAStech_email_node.csv")
GAStech_edges <- read_csv("MC1/data/GAStech_email_edge.csv")
```

because all emails were sent in 2014 Jan, additional column is created for day of month. This will be used to visualize the number of emails in each day by each sender, considering a email will be sent to multiple receiver.

```{r}
GAStech_edges_aggregated <- GAStech_edges %>%
  filter(source!=target) %>%
  transform(SentDate=dmy(SentDate)) %>%
  mutate(day=day(SentDate)) %>%
  separate(Subject, c("type","Subject"), ":") %>%
  transform(Subject=ifelse(type=="RE", Subject, type),
            type=ifelse(type=="RE", "reply", "direct"))
```

Manipulate table to detect any employee who sends email to all employees, i.e. send to 54 recipients. Group the records by source, day, type, subject and main subject. The table below indicates that only 2 people who send to the whole company, Mat Bramar and Ruscella Mies Haber. They both work in administration department. Mat is assistant to CEO. From the subjects, we can conlude that he regularly sends announcement, reminders, and matters that relate to the whole company, such as "All staff announcement" and "Changes to travel policy".

Ruscella is assistant to engineering group manager. She sends only 2 types of emails to the whole company, "Daily morning announcements" and "Good morning, GasTech!" Both are daily news updates for the whole company to read. 

```{r message=FALSE}
email_eachday <- GAStech_edges_aggregated %>%
  group_by(source, day, type, Subject, MainSubject) %>%
  summarise(n=n()) %>%
  filter(n==53) %>%
  select(source, day, Subject) %>%
  arrange(source,Subject)
```

preparation for constructing graph. 2 chunks below are not used now. 

```{r}
GAStech_edges_1 <- GAStech_edges_aggregated %>%
  group_by(source, target, day) %>%
  summarise(weight=n()) %>%
  left_join(GAStech_nodes, by=c("source"="label"), suffix=c(".from", ".to")) %>%
  left_join(GAStech_nodes, by=c("target"="label"),suffix=c(".from", ".to")) %>%
  rename(from=id.from, to=id.to) %>%
  ungroup() %>%
  select(from, to, day,weight) 
```

```{r}
GAStech_graph <- tbl_graph(nodes=GAStech_nodes, edges=GAStech_edges_1)
```

```{r echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
email_matrix <- GAStech_edges_aggregated %>%
  select(source, target) %>%
  group_by(source, target) %>%
  summarise(n=n()) 

email_matrix <- graph.data.frame(email_matrix, directed=TRUE)
email_matrix <- get.adjacency(email_matrix, attr="n", sparse=TRUE)

chorddiag(as.matrix(email_matrix), 
          width=500, height=500,
          groupnamePadding=20, groupPadding=3, groupnameFontsize=10, 
          showTicks=FALSE)

```

There is a large number of emails sent from and received by Mat Bramar and Ruscella Mie Haber. This is because they are responsible for administrative task, such as sending announcements and reminder. 

Next, we will remove the reminder and announcement emails that were sent to all employees for better interpretation of the emails.This includes both directed email and reply emails to all. 

```{r}
GAStech_edges2 <- email_eachday %>%
  mutate(delete="yes") %>%
  right_join(GAStech_edges_aggregated, by=c("source"="source",
                                            "Subject"="Subject",
                                            "type"="type",
                                            "day"="day")) %>%
  filter(is.na(delete)) 
```
```{r}
email_matrix2 <- GAStech_edges2 %>%
  filter(type=="direct") %>%
  select(source, target) %>%
  group_by(source, target) %>%
  summarise(n=n()) 

email_matrix2 <- as.matrix(get.adjacency(graph.data.frame(
  email_matrix2, directed=TRUE),
  attr="n", sparse=TRUE))

chorddiag(email_matrix2, 
          width=500, height=500,
          groupnamePadding=20, groupPadding=3, groupnameFontsize=10, 
          showTicks=FALSE)
```

------









