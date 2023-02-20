import csv

def conta_parole_0(lista): #una lista di stringhe
    i=0
    for parola in lista:
        if len(parola)==0:
            i+=1
    return(i)
############################
f=open('text19.txt','r')
contents=f.read()
#print(contents,type(contents))
contents=contents.split('\n')
# creo una lista di liste (tokens), ogni soto lista contiente un tweet
tokens=[]
for el in contents:
    el=el.strip(" \" ")
    token=el.split(" ")
    tokens.append(token)

while [''] in tokens:
    tokens.remove([''])

# print(tokens, len(tokens))

for el in tokens:
    while '' in el: el.remove('')  # in qualche tweet dopo aver tokenizzato
    while '#' in el: el.remove('#') # rimangono # e '', li tolgo qui


#tokens è una lista di liste di stinghe. in ogni sotto lista si trova un tweet, ogni stringa contiene una sola parola di un tweet

# tokens=[['#', 'elemento', 'centrale', 'rispondere', 'cambiamenti', 'atto',
# 'scenario', 'collaborazione', 'fra', '#ricerca', '#formazione', 'mondo',
# '#imprese', 'gioca', 'ruolo', 'primario', 'pietro', 'guindani', 'vp',
# '#assolombarda', 'oggi', '#17maggio', '@qn_giorno'],['ciao','#marghe'],
# ['colombo']]
#
# ############## funziona anche se qualche tweet non ha nessun #
# # invece se qualche tweet è vuoto non funziona, tolgo i tweet vuoti (solo l'ultimo)
#

# # print(tokens)
################### creo una lista di liste, ogni sotto lista contiene gli hashtag del relativo tweet
hashtag_list=[]
for tweet in tokens:
    hashtag_tweet=[]
    for parola in tweet:
        if parola[0]=='#':
            hashtag_tweet.append(parola)
    hashtag_list.append(hashtag_tweet)
print(hashtag_list,len(hashtag_list))
# ###########################


# esporto i dati


f = open('hashtag_innovazione19_postpy.csv', 'w')

with f:

    writer = csv.writer(f)
    writer.writerows(hashtag_list)
