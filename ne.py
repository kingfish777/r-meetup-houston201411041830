import nltk
import sys

# sentence = "I am the walrus. I am the eggman. The man eats the apple. Ivan shot the bear."
for arg in sys.argv: 
    print arg
tokens = nltk.word_tokenize(arg)
print(tokens)
tagged = nltk.pos_tag(tokens)
print(tagged)
entities = nltk.ne_chunk(tagged)
print(entities)
