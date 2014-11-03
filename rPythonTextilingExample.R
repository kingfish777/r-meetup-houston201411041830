#########################################################################
# Copyright (c) 2014 All Rights Reserved, Scott Alexander Malec
#
# This source is free to use and distribute so long as credit is provided.
# This code is provided "AS IS" without warranty of any kind, either
# expressed or implied, including but not limited to the implied
# warranties of merchantability and/or fitness for a particular purpose.
#
# Author: Scott Alexander Malec
# Email: scott [dot] malec [at] gmail [dot] com
# Date: 3/23/2014
#
# TITLE: rPython_NLTK.R
#
# Purpose: R Tools to interface with Python NLTK's implementation of Marti Heart's
# TextTiling algorithm
#
#########################################################################
# python nltk integration --- using Marti Hearst's textiling algorithm
# textiling uses a roving text window to identify breaks in the topical structure within a text
# http://people.ischool.berkeley.edu/~hearst/research/tiling.html
# http://clover.slavic.pitt.edu/sam/propp/praxis/results.html#final
# http://www.lrec-conf.org/proceedings/lrec2012/pdf/876_Paper.pdf
#########################################################################
#python nltk integration --- using Marti Hearst's textiling algorithm
# textiling uses a roving text window to identify breaks in the topical structure within a text
# http://people.ischool.berkeley.edu/~hearst/research/tiling.html
# http://clover.slavic.pitt.edu/sam/propp/praxis/results.html#final
# http://www.lrec-conf.org/proceedings/lrec2012/pdf/876_Paper.pdf
#########################################################################
# TO DO:
# * split apart sentences
# * useopenNLP to identify named entities
# * create 'the AutoPropp engine'
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
#
# O, the wretchedness: the elegance of Python meets, well, R
#
########################################################################
library(rPython)
library(tm)
library(RTextTools)
########################################################################
python.exec("import nltk")
python.exec("import nltk.corpus")
python.exec("from nltk.corpus import PlaintextCorpusReader")
python.exec("from urllib import urlopen")
python.exec("import numpy")
########################################################################
# deprecated junk
########################################################################
filePath <- "/amanuensis/data/raw_data/corpora/jordan/jordan/"
setwd(filePath)
fs <- list.files()
fs
########################
for (counter in 1:length(fs)) {
python.exec(paste("url = \"/amanuensis/data/raw_data/corpora/jordan/jordan/", fs[counter], "\"", sep=""))
python.exec("raw = urlopen(url).read()")
python.exec("ttt = nltk.tokenize.TextTilingTokenizer(w=7, k=3, smoothing_width = 6, smoothing_rounds = 10)")
python.get("raw")
python.exec(paste("tiles = ttt.tokenize(raw)"))
dataDumpFolderPath <- "/amanuensis/data/raw_data/corpora/textTiles/jordan/"
textTiles <- python.get("tiles")
print(textTiles)
print("####################")
print(fs[counter])
print(counter)
for (index in 1:length(textTiles)) {
index <= as.character(index)
#string_vector <- c(textTiles[index])
#lapply(string_vector, findOffendingCharacter)
#if (nchar(as.character(textTiles[index]), type = "chars") > 5) {
writeLines(textTiles[index], paste(dataDumpFolderPath, "textTile", as.character(index), "_", fs[counter], sep="")) #}
########################
# creepy "whisper" voice
########################
text <- paste("espeak -v+whisper -k20 -p 79 -s 225 -f \"", paste(dataDumpFolderPath, "textTile", as.character(index), "_", fs[counter], sep=""), "\"", sep="")
# http://espeak.sourceforge.net/commands.html
system(text)
}
}
