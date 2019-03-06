# -*- coding: utf-8 -*-
"""
Created on Wed Mar  6 16:21:18 2019

@author: Johnny
"""

#%%

##################################The Office - Word2Vec#######################################  

import pandas as pd 
import os
import nltk
import spacy
import numpy as np
import en_core_web_sm
nlp = en_core_web_sm.load()
#%%

from gensim.models.word2vec import Word2Vec
df=pd.read_csv('C:\\Users\\Johnny\\Documents\\UNH Notes and Documents 2018\\Spring 2018\\NLP and Time Series Analysis\\Lesson 6\\OfficeNLP_Final2.csv');
#%%
corpus= df['Name'] + ' ' + df['Line']
tok_corp= [nltk.word_tokenize(sent) for sent in corpus]
#%%

# Creating the model and setting values for the various parameters
num_features = 300  # Word vector dimensionality
min_word_count = 10 # Minimum word count
num_workers = 4     # Number of parallel threads
context = 10        # Context window size
epochs = 10           # number of epochs 
sg = 0            # skipgram model (if 0, then CBOW)  
#%%  
model = Word2Vec(tok_corp, min_count=min_word_count, size = num_features,workers=num_workers,
                 iter=epochs,window =context, sg= sg) 

#%%
# Saving the model for later use. Can be loaded using Word2Vec.load()
model_name = "office_w2vec"
model.save(model_name)

#%%
# This will print the most similar words present in the model
model.wv.most_similar("desk")
#%%
model.wv.most_similar("michael")
#%%
model.wv.most_similar("beets")
#%%
model.wv.most_similar("office")
#%%
model.wv.most_similar("scranton")
#%%
model.wv.most_similar("car")
#%%
model.wv.most_similar("fat")
#%%
model.wv.most_similar("meat")
#%%
model.wv.most_similar("corporate")

