 nlp-utils.util-test
  (:use clojure.test
        nlp-utils.util))

(deftest sentences-of-test
  (testing "Detect the correct number of sentences and reproduce their content"
    (let [ text "This is the first sentence. Fido went to drink water out of the toilet." 
           sentences (sentences-of text) ] 
        (is (= 2 (count sentences))))))



