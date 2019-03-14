# Compressing word2vec model

To build the executable:

```
ocamlbuild compress.native
```

To prepare a model from [GloVe](https://nlp.stanford.edu/projects/glove) with 50 dimensions and a vocabulary of size 100000:

```
wget "http://nlp.stanford.edu/data/glove.6B.zip"
unzip glove.6B.zip
head -n 100000 glove.6B.50d.txt | ./compress.native -d 50 > model-50d-100k.bin
```
