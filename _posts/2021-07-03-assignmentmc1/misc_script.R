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




