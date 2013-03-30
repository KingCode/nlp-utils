(ns nlp-utils.stanford-attr-test
    (:use nlp-utils.stanford-attr)
    (:use nlp-utils.stanford-corenlp)
    (:use nlp-utils.util)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:use nlp-utils.test-util)
    (:use clojure.test)
    (:import (edu.stanford.nlp.trees.semgraph SemanticGraph)))


(def gap-sents 
  (let [ txt (str-from-file DATA_FIL7)
         sents (sentences txt) ]
    [ (first sents) (second sents) ]))
(def ^String gap1 (first gap-sents))
(def ^String gap2 (second gap-sents))
(def ^SemanticGraph gap1-deps (get-collapsedCCDep gap1))
(def ^SemanticGraph gap2-deps (get-collapsedCCDep gap2))

(def hbhc-sents
  (let [ txt (str-from-file DATA_FIL8)
         sents (sentences txt) ]
    [ (first sents) (second sents) ]))
(def ^String hbhc1 (first hbhc-sents))
(def ^String hbhc2 (second hbhc-sents))
(def ^SemanticGraph hbhc1-deps (get-collapsedCCDep hbhc1))
(def ^SemanticGraph hbhc2-deps (get-collapsedCCDep hbhc2))

;;;;;;;;;;;;;;;;;;;;;;;; GAP ;;;;;;;;;;;;;;;
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


(deftest org-test-GAP
  (testing "GAP: Should return the stock symbol when found in the orginal text, or the company name from 
           the dependencies otherwise."
    (is (= "NYSE:GPS" (org gap1-deps gap1)))
    (is (= "Gap Inc. NYSE" (org gap1-deps)))))


(deftest dividend-test-GAP
  (testing "GAP: Should detect dividend node"
     (let [ div-nodes1 (dividend-nodes gap1-deps) 
            div-nodes2 (dividend-nodes gap1-deps) ]
       (is (not (nil? div-nodes1)))
       (is (not (nil? div-nodes2)))
       (is (= 1 (count div-nodes1)))
       (is (= 1 (count div-nodes2))))))
            
    
;;;;;;;;;;;;;;;;;;;;;;;;; HBHC ;;;;;;;;;;;;;;;;;
(deftest money-of-test
  (testing "Should return a monetary amount which is part of an 'of' preposition relationship"
    (is (= "$0.24" (money-of hbhc1-deps)))))

(deftest org-test-HCBHC
  (testing "HCBC: Should return the stock symbol when found in the orginal text, or the company name from 
           the dependencies otherwise."
    (is (= "Nasdaq:HBHC" (org hbhc1-deps hbhc1)))
    (is (= "Hancock Holding Company" (org hbhc1-deps)))))
 
(deftest dividend-test-HBHC
  (testing "HBHC: Should detect dividend node"
     (let [ div-nodes1 (dividend-nodes hbhc1-deps) 
            div-nodes2 (dividend-nodes hbhc2-deps) ]
       (is (not (nil? div-nodes1)))
       (is (not (nil? div-nodes2)))
       (is (= 1 (count div-nodes1)))
       (is (= 1 (count div-nodes2))))))
