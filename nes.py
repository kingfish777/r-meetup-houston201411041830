import nltk
import sys
from nltk.tokenize import RegexpTokenizer

sentence = "I am the walrus. I am the eggman. The man eats the apple. Ivan shot the bear."

for arg in sys.argv: 
    print arg
    sentences = nltk.sent_tokenize(arg)
    print(sentences)
    tokenizer = RegexpTokenizer(r'\w+')

for sentence in sentences: 
 #   tokens = nltk.word_tokenize(sentence) 
 #   print(tokens)
    tokens = tokenizer.tokenize(sentence)
    tagged = nltk.pos_tag(tokens)
    print(tagged)
    entities = nltk.ne_chunk(tagged)
    print(entities)
