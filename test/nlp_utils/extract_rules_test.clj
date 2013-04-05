(ns nlp-utils.extract-rules-test
    (:use nlp-utils.extract-rules)
    (:use nlp-utils.util)
    (:use clojure.test))

(deftest analyze-document-basic-test
   (testing "Should output a report for all sentences in document"
        (let [ reports (analyze-document "/Users/jrompre/projects/nlp-utils/test/data/gap.txt") ]
            (is (< 0 (count reports)))
            (foreach [ r reports ] (println r "\n\n")))))

    
