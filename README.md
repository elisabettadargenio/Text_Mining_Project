# Text Mining Project

This group project was made in June 2020 during the Covid 19 pandemic.  
It tries to answer the following questions:
1. In which areas have we talked about innovation in the post covid19 scenario?
2. Can it be said that Covid 19 has stimulated innovation? In other words, have the areas in which we talked about it changed in 2020 compared to the previous year?   

Looking closely at the reality that surrounded us, one would expect some differences, for example the introduction of distance learning and of smart working, the spread of ecommerce and delivery.

To answer the analysis questions, the tweets from 17 to 25 May 2020, and the tweets from the same period of the previous year, are analyzed using text mining techniques.

In detail:
1. The frequencies of tokens and hashtags are analyzed in the two analysis periods
2. A Latent Dirichlet Allocation (LDA) analysis - the main model based on topics - is carried out in order to evaluate if different topics emerge between the two periods of analysis

Data for 2020 are extracted using the rtweets package, for the previous year's data, the twint tool in Python was used and the extraction is available as "hashtag_innovazione20_postpy.csv"
