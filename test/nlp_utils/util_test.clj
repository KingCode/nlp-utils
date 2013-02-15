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
           model (chunking-parser-model)
           p (parser model)
           results (parse-line p (nth lines 0)) 
        ]
    (. (nth results 0) show))))

