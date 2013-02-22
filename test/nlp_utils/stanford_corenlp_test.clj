(ns nlp-utils.stanford-corenlp-test
  (:import (java.util Properties)
           (edu.stanford.nlp.ling CoreAnnotations)
           (edu.stanford.nlp.util CoreMap)
           (edu.stanford.nlp.trees Tree TreeCoreAnnotations))
  (:use clojure.test
        nlp-utils.stanford-corenlp
        nlp-utils.util
        nlp-utils.stanford-corenlp-test-const))

(defn hdr[ name msg ] (print-header (str name " START: ")  msg " ***************************** "))
(defn ftr[ name ] (print-header (str name " END") "" " ++++++++++++++++++++++++++++++++++++ "))

(defn show-sentences
[ txt grammar? test-name msg]
  (let [ sents (sentences txt grammar?) ] 
    (hdr test-name msg) 
    (if grammar?
      (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
      (doall (map #(println "\n\nSENTENCE: \n" %) sents)))
    (ftr test-name)))

(defn show-sentences-with-grammar
[ txt idx ]
  (let [ name (str "sentences-test-with-grammar-" idx)
         msg "Parsing one or a few sentences with grammar"
       ]
    (show-sentences txt true name msg)))

(deftest annotation-for-test
  (testing "Should build an annotation from text"
    (let [ ann (annotation-for TXT) ]
      (is (not (nil? ann))))))

(deftest annotated-pipeline-test
  (testing "Should construct and annotate a pipeline from an annotation"
    (let [ ann (annotation-for TXT)
           props (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner"})
           pln (annotated-pipeline ann props)
        ]
       (is (not (nil? pln)))
       (hdr "annotated-pipeline test" "tokens and text")
       (.prettyPrint pln ann System/out)
       (ftr "annotated-pipeline test")
)))

#_(deftest annotated-pipeline-test-sentencesFromFile
  (testing "Should annotated multi-sentences text using only sentence annotator"
    (let [ txt (str-from-file DATA_FIL)
           ann (annotation-for txt)
           props (props-for {CONFIG_ANN "tokenize, ssplit"})
           pln (annotated-pipeline ann props) 
        ]
        (hdr "annotated-pipeline-test-sentencesFromFile" "breaking up sentences from a document") 
        (is (not (nil? pln)))
        (.prettyPrint pln ann System/out)
        (ftr "annotated-pipeline-test-sentencesFromFile")
)))
                 
(deftest sentences-test
  (testing "Should parse entire document body and return a list of sentences"
    (let [ txt (str-from-file DATA_FIL)
           sents (sentences txt) ]
        (is (< 0 (count sents)))
        #_(println "SENTENCE 0: " (nth sents 0) "\nSENTENCE 1: " (nth sents 1)
                 "SENTENCE 2: " (nth sents 2) "\nSENTENCE 30: " (nth sents 30))

        (hdr "sentences breakup test" "Showing all sentences from a document") 
        (doall (map #(println "\n\nSENTENCE: \n" %) sents))
        (ftr "sentences breakup test")
)))

(deftest annotated-for-sentence-test
  (testing "Should produce a grammar from a small/single element sentence seq"
    (let [ sents (annotated-for-sentence TXT (new-pipeline))
           s (nth sents 0)
         ]
        (is (not (nil? s)))
        (hdr "annotated-for-sentence test" "full annotations, showing  grammar")
        (println (grammar-of s))
        (ftr "annotated-for-sentence test")
)))

(deftest sentences-test-with-grammar
  (testing "Should parse a sentence and provide both text and grammar tree"
   (let [  sents (sentences TXT true) ]
        (hdr "sentences-test-with-grammar" "Parsing a single sentence with grammar") 
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar")
)))

(deftest sentences-test-with-grammar-2
  (testing "Should parse a sentence and provide both text and grammar tree"
    (show-sentences-with-grammar  TXT-2 2)
))

(deftest sentences-test-with-grammar-3
  (testing "Should parse a sentence and provide both text and grammar tree"
  (show-sentences-with-grammar  TXT-3 3)
))

(deftest sentences-test-with-grammar-4
  (testing "Should parse a sentence and provide both text and grammar tree"
  (show-sentences-with-grammar  TXT-4 4)
))

(deftest sentences-test-with-grammar-5
  (testing "Should parse a sentence and provide both text and grammar tree"
  (show-sentences-with-grammar  TXT-5 5)
))

(deftest sentences-test-with-grammar-6
  (testing "Should parse a sentence and provide both text and grammar tree"
  (show-sentences-with-grammar  TXT-6 6)
))

;;Do not try this at home, lest you like to choke your machine
#_(deftest sentences-test-with-grammar-fromFile
  (testing "Should parse a document and provide sentence breakup AND grammar tree for each sentence"
    (let [ txt (str-from-file DATA_FIL)
           sents (sentences txt true) ]
        (hdr "sentences-test-with-grammar-fromFile" "Whoaw, the whole document with grammars, let's see this")
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar-fromFile")
))) 
