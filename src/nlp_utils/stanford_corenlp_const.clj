(ns nlp-utils.stanford-corenlp-const
    (:use nlp-utils.util))

(def EMPTY_STR_ARRAY (make-array String 0))

(def CONFIG_ANN "annotators")
(def TOK_ATTR_SEP "=")

(def TOK_MAP { :txt "Text", :pos "PartOfSpeech", :ner "NamedEntityTag", :start "CharacterOffsetBegin"
               :end "CharacterOffsetEnd", :nner "NormalizedNamedEntityTag", :lemma "Lemma"
               :timex "Times", :tcase "TrueCase", :tcasetxt "TrueCaseText"})

(def TOK_KEYS (reverse-map TOK_MAP))

;;This should be renamed to TOKEN_ATTRS
(def TOKEN_IDS  (.toArray
                    '("Text", "PartOfSpeech", "Lemma", "Answer", "NamedEntityTag", "CharacterOffsetBegin",
                     "CharacterOffsetEnd", "NormalizedNamedEntityTag", "Timex", "TrueCase", "TrueCaseText")
                        EMPTY_STR_ARRAY))

(def NER_VALS (hash-map "MONEY" :MONEY, "ORGANIZATION" :ORG, "DATE" :DATE, "NUMBER" :NUMBER, "PERSON" :PERSON))

