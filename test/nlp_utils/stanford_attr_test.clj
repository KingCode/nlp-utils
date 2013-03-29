(ns nlp-utils.stanford-attr-test
    (:use nlp-utils.stanford-attr)
    (:use nlp-utils.stanford-corenlp)
    (:use nlp-utils.util)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:use nlp-utils.test-util)
    (:use clojure.test))


(def gap-sents 
  (let [ txt (str-from-file DATA_FIL7)
         sents (sentences txt) ]
    [ (first sents) (second sents) ]))


(def ^String gap1 (first gap-sents))
(def ^String gap2 (second gap-sents))

(def ^String gap1-deps (get-collapsedCCDep gap1))
(def ^String gap2-deps (get-collapsedCCDep gap2))


(deftest money-to-test 
  (testing "Should return a monetary amount which is part of an 'to' preposition relationship"
    (is (= "$0.6" (money-to gap1-deps)))))
       

(deftest money-from-test 
  (testing "Should return a monetary amount which is part of an 'from' preposition relationship"
    (is (= "$0.5" (money-from gap1-deps)))))


(deftest date-in-money-from-test
  (testing "Should return a date 'prep_in' to a money entity in a 'prep_from' relationship"
    (is (= "2012" (date-in-money-from gap1-deps)))))


(deftest date-in-money-to-test
  (testing "Should return a date 'prep_in' to a money entity in a 'prep_to' relationship"
    (is (= "2013" (date-in-money-to gap1-deps)))))
