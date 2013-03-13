(ns nlp-utils.stanford-corenlp-test
  (:import (java.util Properties)
           (edu.stanford.nlp.ling CoreAnnotations)
           (edu.stanford.nlp.util CoreMap)
           (edu.stanford.nlp.trees Tree TreeCoreAnnotations))
  (:use clojure.test
        nlp-utils.stanford-corenlp
        nlp-utils.stanford-corenlp-const
        nlp-utils.stanford-corenlp-pool
        nlp-utils.util
        nlp-utils.stanford-corenlp-test-const))

(defn hdr[ name msg ] (print-header (str name " START: ")  msg " ***************************** "))
(defn ftr[ name ] (print-header (str name " END") "" " ++++++++++++++++++++++++++++++++++++ "))


(def document (atom nil))

(defn get-document-sentences
([ s_atom filepath]
  (do
    (if (nil? @s_atom)
        (swap! s_atom (fn [ v ] (if (not (nil? v)) v
                                        (let [ txt (str-from-file filepath) ]
                                          (sentences txt SPLIT-PL false))))))
    @s_atom))
([ filepath ]
  (get-document-sentences (atom nil) filepath))
([]
  (get-document-sentences document DATA_FIL)))
                 
(defn show-sentences
[ txt grammar? test-name msg]
  (let [ 
         ppln (if grammar? PARSE-PL SPLIT-PL)
         sents (sentences txt ppln grammar?)    ] 
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


(defn show-fragments-with-grammar
[ txt idx ]
  (let [ name (str "fragments-test-with-grammar-" idx)
         msg "Parsing a sentence fragment" ]
    (show-sentences txt true name msg))) 

(deftest annotation-for-test
  (testing "Should build an annotation from text"
    (let [ ann (annotation-for TXT) ]
      (is (not (nil? ann))))))

(defn show-annotated-pipeline
[ txt label]
(let [ ann (annotation-for txt)
       props (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner"})
       pln (annotated-pipeline ann props) 
     ]
    (hdr (str "annotated-pipeline test-" label) "full report via .prettyPrint")
    (.prettyPrint pln ann System/out)
    (ftr "annotated-pipeline test")))



(deftest annotated-pipeline-test
  (testing "Should construct and annotate a pipeline from an annotation"
    (let [ ann (annotation-for TXT)
;;           props (props-for {CONFIG_ANN "tokenize, ssplit, pos, lemma, ner"})
;;         pln (annotated-pipeline ann props)
           pln NER-PL
        ]
       (is (not (nil? pln)))
       (hdr "annotated-pipeline test" "tokens and text")
       (.prettyPrint pln ann System/out)
       (ftr "annotated-pipeline test")
)))

;;(deftest annotated-pipeline-test-2


#_(deftest annotated-pipeline-test-sentencesFromFile
  (testing "Should annotated multi-sentences text using only sentence annotator"
    (let [ txt (str-from-file DATA_FIL)
           ann (annotation-for txt)
           ;;props (props-for {CONFIG_ANN "tokenize, ssplit"})
           ;;pln (annotated-pipeline ann props) 
           pln SPLIT-PL
        ]
        (hdr "annotated-pipeline-test-sentencesFromFile" "breaking up sentences from a document") 
        (is (not (nil? pln)))
        (.prettyPrint pln ann System/out)
        (ftr "annotated-pipeline-test-sentencesFromFile")
)))


(defn pr-sents 
[ sents ]
  (print-coll sents "\n\nSENTENCE: " "\n"))

                 
(deftest sentences-test
  (testing "Should parse entire document body and return a list of sentences"
    (let [ sents (get-document-sentences)
           size  (count sents)
            ]
        (is (< 0 size))
        #_(println "SENTENCE 0: " (nth sents 0) "\nSENTENCE 1: " (nth sents 1)
                 "SENTENCE 2: " (nth sents 2) "\nSENTENCE 30: " (nth sents 30))

        (hdr "sentences breakup test" (str "Showing all " size " sentences from document " DATA_FIL))
        (doall (map #(println (str "\n\nSENTENCE: " %2 "\n" %1)) sents (range 0 size)))
        (ftr "sentences breakup test")
)))

(deftest annotated-for-sentence-test
  (testing "Should produce a grammar from a small/single element sentence seq"
    (let [ txt TXT sents (annotated-for-sentence txt PARSE-PL)
           s (nth sents 0)
         ]
        (is (not (nil? s)))
        (hdr "annotated-for-sentence test" "full annotations, showing  grammar")
        (println "TEXT:\n" txt "\nGRAMMAR:\n\n" (grammar-of s))
        (ftr "annotated-for-sentence test")
)))

(deftest sentences-test-with-grammar
  (testing "Should parse a sentence and provide both text and grammar tree"
   (let [  sents (sentences TXT  PARSE-PL true) ]
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

(deftest sentences-test-with-grammar-7
  (testing "Show grammatical structure for first sentence (extract company & stock symbol)"
  (show-sentences-with-grammar  (nth (get-document-sentences) 0) 7)
))

(deftest fragments-teest-with-grammar
  (testing "Should parse a sentence fragment and provide text and grammar tree, still"
  (show-fragments-with-grammar PART 0)
))

(deftest fragments-teest-with-grammar-1
  (testing "Should parse a sentence fragment and provide text and grammar tree, still"
  (show-fragments-with-grammar PART-1 1)
))

(deftest fragments-teest-with-grammar-2
  (testing "Should parse a sentence fragment and provide text and grammar tree, still"
  (show-fragments-with-grammar PART-2 2)
))

(defn display-toks
[ txt toks] (println "TEXT:\n" txt "\nTOKENS:\n" (interpose "\n" toks)))

(deftest tokens-for-test
  (testing "Should parse a sentence and list essential (TOKEN_MIN) attributes for each token"
    (let [ txt TXT toks (tokens-for-sentxt txt TOKEN_MIN PARSE-PL) ]
      (hdr "tokens-for-test" "parsing minimal tokens on parsing pipeline")
      (display-toks txt toks)    
      (ftr "tokens-for-test"))))


(deftest tokens-for-test-with-NER
  (testing "Should parse a sentence and list only text, POS and NER attrs for each token" 
    (let [  txt TXT attrs (select-tokens :txt :pos :ner) 
            toks (tokens-for-sentxt txt attrs NER-PL) ]
      (hdr "tokens-for-test" "collecting TXT, POS and NER attrs from tokens")
      (display-toks txt toks)    
      (ftr "tokens-for-test"))))

;;Do not try this at home: parsing table of figures
#_(deftest irregular-sentence-table-with-grammar
  (testing "Should show parse tree for a financial table sentence"
    (let [ txt (str-from-file DATA_FIL)
           ;;sents (sentences txt) 
           sents (get-document-sentences)
           tab-sent (nth sents 80)
            ]
   (show-sentences tab-sent true "irregular-sentence-table-with-grammar" "Parsing a table of figures")
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
