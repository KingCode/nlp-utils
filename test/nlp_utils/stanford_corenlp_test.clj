(ns nlp-utils.stanford-corenlp-test
  (:use clojure.test
        nlp-utils.stanford-corenlp))

(def TXT "GAAP diluted EPS for the fourth quarter was $0.10; Non-GAAP diluted EPS for the fourth quarter was $0.35.")

(deftest annotation-for-test
  (testing "Should build an annotation from text"
    (let [ ann (annotation-for TXT) ]
      (is (not (nil? ann))))))

(deftest annotated-pipeline-test
  (testing "Should construct and annotate a pipeline from an annotation"
    (let [ ann (annotation-for TXT)
           pln (annotated-pipeline ann)
        ]
       (is (not (nil? pln))))))

    
