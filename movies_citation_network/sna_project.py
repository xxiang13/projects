# -*- coding: utf-8 -*-
"""
Created on Wed May 27 21:57:09 2015

@author: apple
"""

import pandas as pd
import numpy as np
import os
#%%
os.chdir('/Users/apple/Documents/MSiA/Spring 2015/MSiA 490 Social Network/project')
myData = pd.read_csv('MovieCitation.csv')
#clean dataset: replace undersocre as space
#myData['InFilm']= myData['InFilm'].str.replace('_', ' ')
#myData['OutFilm']= myData['OutFilm'].str.replace('_', ' ')
#myData.to_csv('MovieCitation.csv',sep=',', header = True)
#%%
#assign lavels to movies
checkList = []
i = 0
a = 0
movieLabel = dict()
while i in range(len(myData.OutFilm)):
    if myData.InFilm[i] not in checkList:
        movieLabel[a] = myData.InFilm[i]
        checkList.append(myData.InFilm[i])
        a = a +1
    i = i+1

i = 0
a = a+1
while i in range(len(myData.OutFilm)):
    if myData.OutFilm[i] not in checkList:
        movieLabel[a] = myData.OutFilm[i]
        checkList.append(myData.OutFilm[i])
        a = a+1
    i = i+1
    
labels = list(movieLabel.keys())
movies = []
for i in movieLabel.keys():
    movies.append(movieLabel[i])
    
d = {'labels': labels, 'movies': movies}
oldest_100_df = pd.DataFrame(data = d)
oldest_100_df.to_csv('movies_labels.csv', sep=',', header = True)
#%%
checkList = []
i = 0
a = 0
labelMovie = dict()
while i <= len(myData.InFilm)-1:
    if myData.InFilm[i] not in checkList:
        labelMovie[myData.InFilm[i]] = a
        checkList.append(myData.InFilm[i])
        a = a +1
    i = i+1

i = 0
a = a+1
while i <= len(myData.OutFilm)-1:
    if myData.OutFilm[i] not in checkList:
        labelMovie[myData.OutFilm[i]] = a
        checkList.append(myData.OutFilm[i])
        a = a+1
    i = i+1
    
#%%
    
checkList = set()
i = 0
a = 0
labelMovie = dict()
while i <= len(myData.InFilm)-1:
    if myData.InFilm[i] not in checkList:
        labelMovie[myData.InFilm[i]] = a
        checkList.add(myData.InFilm[i])
        a = a +1
    i = i+1

i = 0
a = a+1
while i <= len(myData.OutFilm)-1:
    if myData.OutFilm[i] not in checkList:
        labelMovie[myData.OutFilm[i]] = a
        checkList.add(myData.OutFilm[i])
        a = a+1
    i = i+1


    
#%%
#find out first 100 oldest movies's index
movieList = []
findSubset = False
i = 0
while not findSubset:
    if len(movieList) < 100 and myData.InFilm[i] not in movieList:
        movieList.append(myData.InFilm[i])
    if len(movieList) >= 100:
        findSubset = True
    else:     
        i = i+1
    #return i
        
#%%
#replace movie name by lables
for i in range(len(myData.InFilm)):
    myData.InFilm[i] = labelMovie[myData.InFilm[i]]

for i in range(2):
    myData.OutFilm[i] = labelMovie[myData.OutFilm[i]]
    
    
#%%
#find out first 100 oldest citing movies's index
movieList2 = []
findSubset = False
a = len(myData.OutFilm)
while not findSubset:
    if len(movieList2) < 100 and myData.InFilm[i] not in movieList2:
        movieList2.append(myData.InFilm[i])
    if len(movieList2) >= 100:
        findSubset = True
    else:     
        a = a-1
    #return i
#%%
class subsetMovie:
    def __init__(self, name):      
        self.id = name
#%%
#first 100 oldest movies subset
oldest_100 = subsetMovie
oldest_100.inFilm = myData.InFilm[0:i]
oldest_100.inFilm_Year = myData.InFilm_Year[0:i]
oldest_100.outFilm = myData.OutFilm[0:i]
oldest_100.outFilm_Year = myData.OutFilm_Year[0:i]

d = {'inFilm': oldest_100.inFilm, 'inFilm_Year': oldest_100.inFilm_Year,
     'outFilm': oldest_100.outFilm, 'outFilm_Year': oldest_100.outFilm_Year,}
oldest_100_df = pd.DataFrame(data = d)
oldest_100_df.to_csv('oldest_100_movies.csv', sep=',', header = True)
#first 100 newest movie subset

print("ok")
