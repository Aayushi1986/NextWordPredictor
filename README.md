# Next Word Predictor
The goal of this project is to build an application that predicts the most likely next word for an input of text. The model uses the concept of NLP and Tokenisation to achieve this. Shiny app has been used to provide user interactivity with the model.

# Data Preparation
The data used in this model has been collected from various sources of blogs, Tweets and news. 
The processed data is broken into tokens or n-grams. The model is built using trigrams (group of three words), bigrams (pair of words) and unigrams (single word).

# Model
Given a sequence of N-1 words, an N-gram model predicts the most probable word that might follow this sequence. Frequency of unigrams, bigrams and trigrams is calculated and stored in three different dataframes. These n-grams are then used to calculate the probability of the next word, given the previous word/words.
If n-grams do not work, the next word is predicted from the dictionary based on probability.

# Output
The algorithm displays a WordCloud of ten different words when input is provided.

# Web Application
The model is deployed on web using Shiny App. The web application contains a text box where the user can provide the input. A button displays the predicted word with the highest probability. The user can either click on the button to add the word to the sentence or type in manually. Also is displayed a WordCloud that shows ten most probable words.
(The application works even before you start typing.)

The web application can be accessed at https://aayushisaluja.shinyapps.io/NextWord/
