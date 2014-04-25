import pandas as pd
import numpy as np
#from __future__ import division
import re

def basic_text_statistics(text): 
  n_chars = text.map(len)                                	#-0.018
  n_words = text.map(lambda v: len(v.split(' ')))  		#-0.017
  avg_word = n_chars/n_words					#-0.004
  #pat = re.compile(r'([A-z|А-я][^\.!?]*[\.!?])', re.M)
  #n_sentences = text.map(lambda v: len(pat.findall(v)))         #-0.015
  #avg_sent = n_chars/n_sentences				# 0.002
  pat = re.compile(r'\.', re.M)
  n_punct_dots = text.map(lambda v: len(pat.findall(v)))       # 0.01
  pat = re.compile(r'!', re.M)
  n_punct_excl = text.map(lambda v: len(pat.findall(v)))	# -0.004
  pat = re.compile(r'\)', re.M)
  n_punct_smiles = text.map(lambda v: len(pat.findall(v)))	#  0.011
  pat = re.compile(r'\)\)', re.M)
  n_punct_cl_parenth = text.map(lambda v: len(pat.findall(v))) 	# 0.048
  pat = re.compile(r'\(\(', re.M)
  n_punct_op_parenth = text.map(lambda v: len(pat.findall(v))) 	# 0.02
  pat = re.compile(r'\?', re.M)
  n_punct_ques = text.map(lambda v: len(pat.findall(v)))	# -0.018
  pat = re.compile(r'Image', re.M)
  n_punct_Images = text.map(lambda v: len(pat.findall(v)))	# 0.108
  pat = re.compile(r'http', re.M)
  n_punct_http = text.map(lambda v: len(pat.findall(v)))	# -0.024
  pat = re.compile(r'\"', re.M)
  n_punct_apos = text.map(lambda v: len(pat.findall(v)))	# -0.011
  
  data = { 'n_chars': n_chars,'n_words': n_words, 'avg_word': avg_word, 'n_punct_excl':n_punct_excl, 'n_punct_cl_parenth': n_punct_cl_parenth, 'n_punct_Images' : n_punct_Images,
	   'n_punct_smiles':n_punct_smiles, 'n_punct_dots':n_punct_dots,#'n_words': n_words, 'avg_word':avg_word, 'n_sentences':n_sentences, 'avg_sent':avg_sent,
	   'n_punct_op_parenth': n_punct_op_parenth, 'n_punct_ques':n_punct_ques, 
	   'n_punct_http': n_punct_http, 'n_punct_apos':n_punct_apos}
  
  return pd.DataFrame(data, columns=['n_chars','n_words', 'avg_word', 'n_punct_excl', 'n_punct_cl_parenth','n_punct_Images', 'n_punct_smiles',
									'n_punct_dots', 'n_punct_op_parenth', 'n_punct_ques', 'n_punct_http', 'n_punct_apos'])
  
def extract_features2 (inputs, outputs):
  df = pd.read_csv(inputs)
  features = basic_text_statistics(df['text'])
  training_data_features = df.ix[:,0:3]
  training_data_features = training_data_features.join(features)
  training_data_features.to_csv(outputs, index = False)  


extract_features2("../data/processed/train_content.csv", "../data/processed/train_features.csv")
extract_features2("../data/processed/test_content.csv", "../data/processed/test_features.csv")
