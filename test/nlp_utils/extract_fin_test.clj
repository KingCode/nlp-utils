(ns nlp-utils.extract-fin-test
    (:use clojure.test
          nlp-utils.extract-fin
          [nlp-utils.stanford-corenlp-test :only [get-document-sentences hdr ftr]]
          nlp-utils.stanford-corenlp-test-const))


(def goog_corp (atom nil))
(def gs_corp (atom nil))
(def td_corp (atom nil))
(def xom_corp (atom nil))

(defn test-filter-1
[ test-name msg corpus ]
  (let [ trimmed (filter-1 corpus)
         init-siz (count corpus)
         siz (count trimmed)
       ]
    (is (< siz (count corpus)))
    (hdr test-name msg)
    (println "INITIAL NUM SENTENCES: " init-siz "\nAFTER FILTER: " siz
;;             "\nRETAINED CONTENT:\n\n" (interpose "\n\n" trimmed))
             "\nRETAINED CONTENT:\n\n" (format-all trimmed "\n\n>>" "\n\t\t>"))
    (ftr test-name)))


(deftest filter-1-test_1
  (testing "Verifying pass one filtering 1"
    (test-filter-1 "filter-1-test_1" "Trimming LNKD" (get-document-sentences)))) 
    

(deftest filter-1-test_2
  (testing "Verifying pass one filtering 2"
    (test-filter-1 "filter-1-test_2" "Trimming GOOG" (get-document-sentences goog_corp DATA_FIL2))))

(deftest filter-1-test_3
  (testing "Verifying pass one filtering 3"
    (test-filter-1 "filter-1-test_3" "Trimming GS" (get-document-sentences gs_corp DATA_FIL3))))
  
(deftest filter-1-test_4
  (testing "Verifying pass one filtering 4"
    (test-filter-1 "filter-1-test_4" "Trimming TD" (get-document-sentences td_corp DATA_FIL4))))

(deftest filter-1-test_5
  (testing "Verifying pass one filtering 5"
    (test-filter-1 "filter-1-test_5" "Trimming XOM" (get-document-sentences xom_corp DATA_FIL5))))
