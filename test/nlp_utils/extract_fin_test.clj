(ns nlp-utils.extract-fin-test
    (:use clojure.test
          nlp-utils.extract-fin
          [nlp-utils.stanford-corenlp-test :only [get-document-sentences hdr ftr]]))

(deftest filter-1-test
  (testing "Verifying pass one filtering"
    (let [ corpus (get-document-sentences)
           trimmed (filter-1 corpus)
           siz (count trimmed)
           ]
    (is (< siz (count corpus)))
    (hdr "filter-1-test" "Trimming down based on money and size")
    (println "INITIAL NUM SENTENCES: " (count corpus) "\nAFTER FILTER: " siz
             "\nRETAINED CONTENT:\n\n" (interpose "\n\n" trimmed))
    (ftr "filter-1-test"))))
    


