(ns nlp-utils.util-test
    (:use clojure.test
          nlp-utils.util)
    (:import (java.util Properties)))

(deftest props-for-test
  (testing "Loads a Properties from a map"
    (let [ p (props-for {"a" "1" "b" "2"}) ]
      (is (= 2 (count p)))
      (is (= "1" (.getProperty p "a")))
      (is (= "2" (.getProperty p "b"))))))

