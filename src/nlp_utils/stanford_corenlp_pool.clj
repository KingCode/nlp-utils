(ns nlp-utils.stanford-corenlp-pool
    (:use nlp-utils.stanford-corenlp
          nlp-utils.stanford-corenlp-const
          nlp-utils.util))

(def SPLIT-PL (new-pipeline (props-for {CONFIG_ANN "tokenize, ssplit"})))
(def POS-PL (new-pipeline (props-for {CONFIG_ANN "tokenize, ssplit, pos"})))
(def NER-PL (new-pipeline (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner"})))
(def PARSE-PL (new-pipeline (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner, parse"})))
;;(def DCOREF-PL (new-pipeline (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner, parse, dcoref"}))
