library(rtweet)
library(remotes)
library(devtools)
library(TextWiller)
library(tidyverse)
library(tidytext)

#innovazione<-search_tweets(q="#innovazione AND lang:it", n=3000)
innovazione$text

#prima pulizia dei dati
innovazione$text = gsub("[^\x01-\x7FÀ-ú]", "",innovazione$text)
innovazione$text<- normalizzaTesti(innovazione$text)

summary(innovazione$created_at)
#dati estratti tra il 17-05 e il 25-05

#creiamo un tibble contenente solo testo e id del tweet
st= tibble(text=innovazione$text)%>% mutate(id=1:n())   

#separiamo i singoli token, contiamo le frequenze e ordiniamo dal più grande al più piccolo
st%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%arrange(desc(tot))
#rimuoviamo termini non utili
st$text=gsub("wwwurlwww", "", st$text)
st$text=gsub("innovazione", "", st$text)
st$text=gsub("non", "", st$text)
st$text=gsub("più", "", st$text)


#ci accorgiamo che tra le parole più comuni ci sono degli hashtag che non sono stati riconosciuti, per cui decidiamo di andare a rimuovere
#manualmente i vocaboli che hanno davanti # in modo da avere nell'analisi solo quelli che sono effettivi tokens


#############ANALISI PULENDO I DATI DA # E @ ###################


#Duplichiamo il dataset in modo da poter togliere le parole che iniziano con # e con @ 
#poichè da solo non riesce a riconoscere tutti gli hashtag
#e quindi li conta come parole (es. bastaprecariato di stato, mise_gov)
dati<-innovazione
dati$text<- gsub('#\\S+', '', dati$text)
dati$text<- gsub('@\\S+', '', dati$text)

#rifacciamo analisi parole con dati puliti da # e @

#ricreiamo il tibble contenente solo il testo e l'id
st2= tibble(text=dati$text)%>% mutate(id=1:n())  

#estraiamo i tokes
st2%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%arrange(desc(tot))
#rimuoviamo i tokens non utili per l'analisi
st2$text=gsub("wwwurlwww", "", st2$text)
st2$text=gsub("innovazione", "", st2$text)
st2$text=gsub("non", "", st2$text)
st2$text=gsub("più", "", st2$text)

#ricontrolliamo
st2%>% unnest_tokens(word,text)%>% 
  group_by(word)%>% summarise(tot=n())%>%
  arrange(desc(tot))%>%top_n(20)
 #tutto ok

#vediamo i bigrammi
st2%>% unnest_ngrams(big,text,n=2)%>%  #fa la stessa cosa ma su bigrammi
  count(big) %>% arrange(desc(n))



#visualizziamo le parole più usate tramite wordcloud
library(wordcloud)
#creiamo un oggetto contenente i singoli tokens
df_words<- st2%>% unnest_tokens(word, text)%>% count(word)%>% arrange(desc(n))
#oggetto contenente i bigrammi
df_big<- st2%>% unnest_ngrams(big, text, n=2)%>% count(big)%>% arrange(desc(n))

ccol = RColorBrewer::brewer.pal(8,"Set3")
df_words
df_big
wordcloud(words = df_words$word,freq = df_words$n,min.freq =
            90,colors=ccol, scale=c(3,0.9)) 
wordcloud(words = df_big$big,freq = df_big$n,min.freq = 60,colors=ccol, scale=c(1,0.2))
#i bigrammi potrebbero non essere utili



##############HASHTAG##########
#per l'estrazione degli hashtag riferirsi al file su python

# dati 2020
T <- innovazione$text
write.table(T, file = "text20.txt", sep='\n' ,row.names = FALSE, col.names = FALSE)
# passaggi su python
hashtag_innovazione_py <- read.csv('hashtag_innovazione20_postpy.csv',sep='\n',header = F,blank.lines.skip = F)
#riportiamo gli hashtag al formato originale
lista_hashtag<-list()
for (i in (1:dim(hashtag_innovazione_py)[1])){
  lista_hashtag[i]<- as.list(strsplit(hashtag_innovazione_py[i,], ","))
  
}

matrice <- (matrix(lista_hashtag,nrow = 2528))
df_hash <-as.data.frame(matrice)
df_hash %>% unnest(V1) %>% mutate(V1=tolower(V1)) %>% count(V1) %>%
  arrange(desc(n))%>%top_n(20)










#############LDA#############

library(topicmodels)
library(lubridate)
library(tm)

dati<-dati%>% mutate(id=1:n())
#dopo aver fatto le analisi, ci siamo accorti che 
#tra i termini più importanti c'erano sia 'nuovo','nuovi', 'nuove', 'nuova'.
#Si potrebbe fare quindi di fare una specie di stemming manuale, sostituendo tutti i termini con 'nuovo'
#ma decidiamo di non farla per non polarizzare le analisi su questo unico termine stemmizzato

#dati$text<- gsub('nuove', 'nuovo', dati$text)
#dati$text<- gsub('nuovi', 'nuovo', dati$text)
#dati$text<- gsub('nuova', 'nuovo', dati$text)

#creiamo questi oggetti per avere poi un dtm che serve per la funzione dell'LDA
tt_ww<- dati%>%select(text,id)%>%
  unnest_tokens(word, text)%>%
  group_by(id,word)%>% #raggruppiamo per documento e per parola
  mutate(freq=n())
tt_ww

#dtm contenente per ogni documento la frequenza della parola
dtm<-tt_ww%>% cast_dtm(document=id, term=word, value = freq)
inspect(dtm)

#rimuoviamo quelli presenti meno del 5% per documento
tresh<-1-5/dim(dtm)[1]
dtm<-removeSparseTerms(dtm,tresh) 

dim(dtm)
ui<-unique(dtm$i) #ui prende solo i valori diversi da zero
str(dtm)
dtm.new<-dtm[ui,]
dim(dtm.new)



set.seed(1)
library(topicmodels) #pacchetto per LDA
q_lda<- LDA(dtm.new,k=4, method = 'Gibbs',
            control=list(seed=1)) #in input: la matrice con i testi, n. di topic, metodo=Gibbs-> di default diverso(non l'abbiamo visto, usa gibbs)
str(q_lda)
beta_topics<-tidy(q_lda, matrix='beta')
beta_topics

#filtriamo la matrice, prendiamo per ogni topic quali sono i termini con frequenza più elevata
termini<-beta_topics%>% group_by(topic)%>% 
  top_n(10,beta)%>% arrange(topic)  #arrange ci ordina rispetto a topic
termini
#abbiamo i primi 10 termini ordinati per frequenza
termini%>% top_n(3)
#3 termini per ogni topic, all'interno di ogni topic quali sono le tre parole più identificative

#


term_pl<-termini%>%ungroup()%>%
  mutate(term=reorder(term,beta)) #non vogliamo ordinarli rispetto al topic, lo ordiniamo rispetto ai valori di beta
term_pl

plot_base<- ggplot(data=term_pl, aes(term, beta, fill=factor(topic)))+ #fill ci dice rispetto a cosa coloriamo
  geom_col()+  #istogramma
  facet_wrap(~topic, scales= 'free')  #stratifichiamo per topic


plot_base+ coord_flip() 

#estraiamo i gamma
gamma_doc<- tidy(q_lda, matrix='gamma')
#per ogni documento per ogni topic, ci dice qual è il valore di gamma ik con i indice documento, k indice topic
#tutti i documenti condividono gli stessi topic
gamma_doc%>% arrange(document)
#il primo topic è presente al 2% nel primo documento

#rappresentiamo graficamente
ggplot(gamma_doc)+ geom_density(aes(gamma,fill=factor(topic)))+
  facet_wrap(~topic) #stratifichiamo per topic



#quali sono i documenti più informativi per ogni topic?
#sono quelli con gammaik più alto, ovvero quelli che sono più verosimilmente associati al topic kesimo

doc_best<-gamma_doc%>% group_by(document)%>%
  top_n(1,gamma)
#prendiamo per ogni documento, ci dice qual è il valore associato al coefficiente gammai più alto
doc_best%>% ungroup()%>% arrange(desc(gamma))
#ci sono documenti con valori di gamma molto elevati (0.45)
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
res<-innovazione%>% select(screen_name, text, id)%>%
  right_join(id_best, by='id')
res
#per ogni topic, i tweet che ha più prob di appartenere a quel topic
res$text
#controlliamo che i tweet rispecchino i topic che avevamo trovato



