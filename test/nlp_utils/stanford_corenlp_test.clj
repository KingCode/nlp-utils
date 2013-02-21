(ns nlp-utils.stanford-corenlp-test
  (:import (java.util Properties)
           (edu.stanford.nlp.ling CoreAnnotations)
           (edu.stanford.nlp.util CoreMap)
           (edu.stanford.nlp.trees Tree TreeCoreAnnotations))
  (:use clojure.test
        nlp-utils.stanford-corenlp
        nlp-utils.util))

(def TXT "GAAP diluted EPS for the fourth quarter was $0.10; Non-GAAP diluted EPS for the fourth quarter was $0.35.")
(def DATA_FIL "test/data/financial-lnkd.txt")

(deftest annotation-for-test
  (testing "Should build an annotation from text"
    (let [ ann (annotation-for TXT) ]
      (is (not (nil? ann))))))

(deftest annotated-pipeline-test
  (testing "Should construct and annotate a pipeline from an annotation"
    (let [ ann (annotation-for TXT)
           props (props-for {CONFIG_ANN "tokenize"})
           pln (annotated-pipeline ann props)
        ]
       (is (not (nil? pln)))
       #_(.prettyPrint pln ann System/out))))

(deftest annotated-pipeline-test-sentencesFromFile
  (testing "Should annotated multi-sentences text using only sentence annotator"
    (let [ txt (str-from-file DATA_FIL)
           ann (annotation-for txt)
           props (props-for {CONFIG_ANN "tokenize, ssplit"})
           pln (annotated-pipeline ann props) 
        ]
        (is (not (nil? pln)))
        #_(.prettyPrint pln ann System/out))))
                 
(deftest sentences-test
  (testing "Should parse entire document body and return a list of sentences"
    (let [ txt (str-from-file DATA_FIL)
           sents (sentences txt) ]
        (is (< 0 (count sents)))
        #_(println "SENTENCE 0: " (nth sents 0) "\nSENTENCE 1: " (nth sents 1)
                 "SENTENCE 2: " (nth sents 2) "\nSENTENCE 30: " (nth sents 30))

        (doall (map #(println "\n\nSENTENCE: \n" %) sents))
))) 
