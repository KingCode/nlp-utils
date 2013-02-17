 (ns nlp-utils.util-test
  (:use clojure.test
        nlp-utils.util))

(deftest sentences-of-test
  (testing "Detect the correct number of sentences and reproduce their content"
    (let [ text "This is the first sentence. Fido went to drink water out of the toilet." 
           sentences (sentences-of text) ] 
        (is (= 2 (count sentences))))))

(deftest parse-line-test
  (testing "Parse a line (sentence) and output the grammatical structure"
    (let [ text (str-from-file "test/data/financial-lnkd.txt")
           sentences (sentences-of text)
           lines (take 1 (drop 2 sentences))
           line (nth lines 0)
           model (chunking-parser-model)
           p (parser model)
           results (parse-line p line) 
        ]
        (is (not (empty? results)))
     )))

(deftest show-parse-test
  (testing "Show a parse result in  Penn Treebank format"
    (let [ text (str-from-file "test/data/financial-lnkd.txt")
           sentences (sentences-of text)
           lines (take 1 (drop 10 sentences))
           line (first lines)
           model (chunking-parser-model)
           p (parser model)
           pres (parse-line p line 5)
           input-1 (nth pres 0)
           input-2 (nth pres 1)
           input-3 (take 2 pres) 
           r-1 (show-parse input-1)
           r-2 (show-parse input-2)
           r-3 (show-parse input-3)
           r-4 (show-parse input-3 true "LINE")
         ]

   (println "SHOW=PARSE>\n" r-1) 
   (println "SHOW=PARSE>\n" r-2) 
   (println "SHOW=PARSE>\n" r-3)
   (println "SHOW-PARSE>\n" r-4) ))) 




