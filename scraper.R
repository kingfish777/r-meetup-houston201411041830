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
# Date: 1/23/2014
#
# TITLE: scraper.R
#
# Purpose: R tools to scrape Federal Reserve text collection
#
#########################################################################
#####################################################
#ecclesCorpus
#####################################################
#scrape gReenspan database
# http://fraser.stlouisfed.org/docs/historic/eccles/(e|E)ccles_ddddmmdd.pdf
#####################################################
library(XML) #htmlTreeParse
library(tm)
library(openNLP)
library(RCurl)
library(RWeka)
setwd("/home/hinckley/eccles2/")
homePath = "/home/hinckley/eccles2"
startYear = 1951
endYear= 1953
duration = endYear - startYear
baseURL <- list("http://fraser.stlouisfed.org/docs/historical/eccles/eccles_", "http://fraser.stlouisfed.org/docs/historical/eccles/Eccles_")
unlist(baseurl[1])
trim <- function(x) { gsub("\\s", "", x) }
monthMax = 12
dayMax = 31
duration = endYear - startYear
urlSuffix = list("", "a", "b", "c")
urlSuffix[3]
class(unlist(urlSuffix[2]))
for (y in 0:duration) {
for (month in 1:monthMax) {
for (day in 1:dayMax) {
for (suffix in urlSuffix) {
for (baseurl in baseURL) {
year = startYear + y
yearStr = as.character(year)
monthStr = as.character(month)
if (month < 10) { monthStr = trim(paste("0", monthStr)) }
dayStr = trim(as.character(day))
if (day <10) { dayStr = trim(paste("0", dayStr)) }
if (month == 12) { month = 1 }
if (day == 31) { day = 1 }
if (year == (duration + startYear)) { year = 9999999 }
url = paste(unlist(baseurl), yearStr, monthStr, dayStr, unlist(suffix), ".pdf", sep="")
#url2 = paste(baseurl2, yearStr, monthStr, dayStr, ".pdf", sep="")
print(url)
if (url.exists(url)) {
#
dest = as.character(paste("eccles_", yearStr, monthStr, dayStr, unlist(suffix), ".pdf", sep=""))
url = as.character(url)
pdf <- readPDF(PdftotextOptions="-layout")
print("found one!")
uri=url
dat <- download.file(url, dest, mode = "w")
txt_file_name <- sub(".pdf", ".txt", dest)
try(msg<-system(paste("pdftotext ", dest, txt_file_name, sep=" ")))
if (msg > 'NULL') {
print(msg) }
if (msg == "stderror")
{
system(paste("rm ", dest, sep=""));
print("WE GOT ONE!")
}
} else
print("bubble trouble in the Temple")
}
}
}
}
}
