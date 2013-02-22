(ns nlp-utils.stanford-corenlp-test
  (:import (java.util Properties)
           (edu.stanford.nlp.ling CoreAnnotations)
           (edu.stanford.nlp.util CoreMap)
           (edu.stanford.nlp.trees Tree TreeCoreAnnotations))
  (:use clojure.test
        nlp-utils.stanford-corenlp
        nlp-utils.util))

(def TXT "GAAP diluted EPS for the fourth quarter was $0.10; Non-GAAP diluted EPS for the fourth quarter was $0.35.")

(def TXT-2 "MOUNTAIN VIEW, Calif., Feb. 7, 2013 (GLOBE NEWSWIRE) -- LinkedIn Corporation (NYSE:LNKD), the world's largest professional network on the Internet, with more than 200 million members, reported its financial results for the fourth quarter and full year ended December 31, 2012:

    Revenue for the fourth quarter was $303.6 million, an increase of 81% compared to $167.7 million in the fourth quarter of 2011.
")

(def TXT-3 "Net income for the fourth quarter was $11.5 million, compared to net income of $6.9 million for the fourth quarter of 2011.")

(def TXT-4 " Non-GAAP net income for the fourth quarter was $40.2 million, compared to $13.3 million for the fourth quarter of 2011.")

(def DATA_FIL "test/data/financial-lnkd.txt")

(defn hdr[ name msg ] (print-header (str name " START: ")  msg " ***************************** "))
(defn ftr[ name ] (print-header (str name " END") "" " ++++++++++++++++++++++++++++++++++++ "))

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
   (let [  sents (sentences TXT-2 true) ]
        (hdr "sentences-test-with-grammar-2" "Parsing a single sentence with grammar")
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar-2")
)))

(deftest sentences-test-with-grammar-3
  (testing "Should parse a sentence and provide both text and grammar tree"
   (let [  sents (sentences TXT-3 true) ]
        (hdr "sentences-test-with-grammar-3" "Parsing a single sentence with grammar")
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar-3")
)))

(deftest sentences-test-with-grammar-4
  (testing "Should parse a sentence and provide both text and grammar tree"
   (let [  sents (sentences TXT-4 true) ]
        (hdr "sentences-test-with-grammar-4" "Parsing a single sentence with grammar")
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar-4")
)))
;;Do not try this at home, lest you like to choke your machine
#_(deftest sentences-test-with-grammar-fromFile
  (testing "Should parse a document and provide sentence breakup AND grammar tree for each sentence"
    (let [ txt (str-from-file DATA_FIL)
           sents (sentences txt true) ]
        (hdr "sentences-test-with-grammar-fromFile" "Whoaw, the whole document with grammars, let's see this")
        (doall (map #(println "\n\nSENTENCE TEXT:\n" (first %) "\nSENTENCE PARSE: " (second %)) sents))
        (ftr "sentences-test-with-grammar-fromFile")
))) 
