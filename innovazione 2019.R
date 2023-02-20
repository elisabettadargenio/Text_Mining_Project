dati19<-read.csv('F:/Social Media/2019_ita.csv', header = T)

head(dati19)


#in dati19b togliamo tutte le parole precedute da # o @
dati19b<-dati19
dati19b$tweet<- gsub('#\\S+', '', dati19b$tweet)
dati19b$tweet<- gsub('@\\S+', '', dati19b$tweet)


#inseriamo la variabile id nel dataset dati19b
#togliamo da dati19b i caratteri particolari e normalizziamo il testo tramite normalizzaTesti
dati19b<-dati19b%>% mutate(id=1:n())
dati19b$tweet = gsub("[^\x01-\x7FÀ-ú]", "",dati19b$tweet)
dati19b$tweet<- normalizzaTesti(dati19b$tweet)
#creiamo un tibble con variabili solo il testo dei tweet di dati19b e la variabile id
st= tibble(text=dati19b$tweet)%>% mutate(id=1:n())  
head(st)

#contiamo la frequenza per ogni termine
st%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%arrange(desc(tot))
#rimuoviamo "wwwurlwww" perchè è il modo che ci dà normalizzaTesti per indicare i link di siti internet
#rimuoviamo anche "non" e "piã" perchè non sono rilevanti per la nostra analisi
st$text=gsub("wwwurlwww", "", st$text)
st$text=gsub("non", "", st$text)
st$text=gsub("piã", "", st$text)
#rimuoviamo anche pic twitter e com poichè 'pic.twitter.com' è un formato per poter pubblicare immagini
st$text=gsub("pic", "", st$text)
st$text=gsub("twitter", "", st$text)
st$text=gsub("com", "", st$text)

#controlliamo se serve rimuovere altre parole
st%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%arrange(desc(tot))
#rimuoviamo "lâ" e "âï" perchè inutili per le analisi
st$text=gsub("lâ", "", st$text)
st$text=gsub("âï", "", st$text)

#rimuoviamo anche la parola innovazione
st$text=gsub("innovazione", "", st$text)
#controlliamo di nuovo se serve rimuovere altre parole
st%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%arrange(desc(tot))%>%
  filter(word!='del')%>%
  filter(word!='nuovo')%>%
  print(n=20)

#bigrammi-> troppo bassa la frequenza e non danno informazioni interessanti, decidiamo di non inserirli
st%>% unnest_ngrams(big,text,n=2)%>%  
  filter(!is.na(big))%>%           
  count(big) %>% arrange(desc(n))


#utilizziamo il pacchetto worldcloud per avere una rappresentazione grafica delle parole più utilizzate
library(wordcloud)
#mettiamo in un oggetto il conteggio di parole
df_words<- st%>% unnest_tokens(word, text)%>% count(word)%>% arrange(desc(n))
ccol = RColorBrewer::brewer.pal(8,"Set3")
wordcloud(words = df_words$word,freq = df_words$n,min.freq =
            30,colors=ccol,  scale=c(3,0.1)) 
#non ci dice molto, proveremo a rifarlo sugli hashtag




##############HASHTAG##########


#gli hashtag in questo dataset sono in un formato diverso da quello del 2020, 
#quindi facciamo un po' di pulizie per poter contare gli hashtag

hashtag19<-dati19$hashtags
lista_hashtag_19=list()
for (i in (1:1018)){
  hashtag19[i]<-gsub("\\[|]","",hashtag19[i])
  hashtag19[i]<-gsub("\\'","",hashtag19[i])
  hashtag19[i] <- str_split(hashtag19[i],", ")
  lista_hashtag_19[i]<-hashtag19[i]
}

matrice <- matrix(lista_hashtag_19, nrow = length(lista_hashtag_19))
df_hash_19 <-as.data.frame(matrice)

df_hash_19 %>% unnest(V1) %>% mutate(V1=tolower(V1)) %>%
  filter(V1!="#innovazione") %>%count(V1)%>%arrange(desc(n))%>%
  print(n=20)

library(wordcloud)
df_words_19 <- df_hash_19 %>% unnest(V1) %>% mutate(V1=tolower(V1)) %>%
  filter(V1!="#innovazione") %>%count(V1)%>%arrange(desc(n))
ccol = RColorBrewer::brewer.pal(8,"Set1")
wordcloud(words = df_words_19$V1,freq = df_words_19$n,min.freq =
            20,colors=ccol,  scale=c(3,0.6))






#############LDA#############

library(topicmodels)
library(lubridate)
library(tm)

#togliamo le parole anche da dati19b
dati19b<-dati19b%>% mutate(id=1:n())
dati19b$tweet=gsub("wwwurlwww", "", dati19b$tweet)
dati19b$tweet=gsub("innovazione", "", dati19b$tweet)
dati19b$tweet=gsub("non", "", dati19b$tweet)
dati19b$tweet=gsub("piã", "", dati19b$tweet)
dati19b$tweet=gsub("pic", "", dati19b$tweet)
dati19b$tweet=gsub("twitter", "", dati19b$tweet)
dati19b$tweet=gsub("com", "", dati19b$tweet)

#facciamo una serie di passaggi che ci servono per costruire un file di tipo dtm da dare in input all'LDA
tt_ww<- dati19b%>%select(tweet,id)%>%
  unnest_tokens(word, tweet)%>%
  group_by(id,word)%>% #raggruppiamo per documento e per parola
  mutate(freq=n())
tt_ww

dtm<-tt_ww%>% cast_dtm(document=id, term=word, value = freq)
inspect(dtm)

#mettiamo come soglia il 5%
tresh<-1-5/dim(dtm)[1]
#togliamo da dtm le parole che compaiono per meno del 5% nel documento
dtm<-removeSparseTerms(dtm,tresh) 

dim(dtm)
ui<-unique(dtm$i) #ui prende solo i valori diversi da zero
str(dtm)
#teniamo
dtm.new<-dtm[ui,]
dim(dtm.new)



set.seed(1)
library(topicmodels) #pacchetto per LDA
q_lda<- LDA(dtm.new,k=4, method = 'Gibbs',
            control=list(seed=1)) 
beta_topics<-tidy(q_lda, matrix='beta')
beta_topics

#filtriamo la matrice, prendiamo per ogni topic quali sono i termini con frequenza più elevata
termini<-beta_topics%>% group_by(topic)%>% 
  top_n(10,beta)%>% arrange(topic)  #arrange ci ordina rispetto a topic
termini
#abbiamo i primi 10 termini ordinati per frequenza
termini%>% top_n(3)
#3 termini per ogni topic, all'interno di ogni topic quali sono le tre parole più identificative



term_pl<-termini%>%ungroup()%>%
  mutate(term=reorder(term,beta)) #non vogliamo ordinarli rispetto al topic, lo ordiniamo rispetto ai valori di beta
term_pl

plot_base<- ggplot(data=term_pl, aes(term, beta, fill=factor(topic)))+ #fill ci dice rispetto a cosa coloriamo
  geom_col()+  #istogramma
  facet_wrap(~topic, scales= 'free')  #stratifichiamo per topic

plot_base+ coord_flip() 


gamma_doc<- tidy(q_lda, matrix='gamma')
#per ogni documento per ogni topic, ci dice qual è il valore di gamma ik con i indice documento, k indice topic
#tutti i documenti condividono gli stessi topic
gamma_doc%>% arrange(document)
#il primo topic è presente al 22% nel primo documento

#rappresentiamo graficamente
ggplot(gamma_doc)+ geom_density(aes(gamma,fill=factor(topic)))+
  facet_wrap(~topic) #stratifichiamo per topic
#sono quasi identici

#quali sono i documenti più informativi per ogni topic?
#sono quelli con gammaik più alto, ovvero quelli che sono più verosimilmente associati al topic kesimo

doc_best<-gamma_doc%>% group_by(document)%>%
  top_n(1,gamma)
#prendiamo per ogni documento, ci dice qual è il valore associato al coefficiente gammai più alto
doc_best%>% ungroup()%>% arrange(desc(gamma))
#ci restituisce l'indicatore del documento (document) 

#estraiamo questi documenti dal dataframe iniziale

#calcoliamo un indicatore che ci permette di selezionare questi documenti con valore di gamma più alto possibile per un dato topic

doc_id<-gamma_doc%>% group_by(topic)%>%
  arrange(gamma)%>%
  mutate(ord=1:n())
doc_id
#sono in ordine crescente, quindi prendiamo gli ultimi

doc_best<-doc_id%>% filter(ord>=n())
doc_best

#abbiamo un dataframe, che ci dice per ognuno dei topic, qual è il documento più interessante

#li prendiamo nel dataframe iniziale

id_best<-doc_best%>% mutate(id=as.numeric(document)) 
id_best
dati19<-dati19%>%mutate(id=1:n())
res<-dati19%>% select(username, tweet, id)%>%
  right_join(id_best, by='id')
res
#per ogni topic, i tweet che ha più prob di appartenere a quel topic
res$tweet
#controlliamo che i tweet rispecchino i topic che avevamo trovato
