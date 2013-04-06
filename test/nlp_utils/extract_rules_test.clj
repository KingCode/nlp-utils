(ns nlp-utils.extract-rules-test
    (:use nlp-utils.extract-rules)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:use nlp-utils.util)
    (:use clojure.test))

(def GAP-reports (extract-reports GAP-FILE))
(def HBHC-reports (extract-reports HBHC-FILE))

(defn org-from [ reports ]
  (first reports))

(defn info-from [ reports ]
  (second reports))

(defn txt-from [ report ]
  (second report))

(defn map-from [ report ]
  (first report))

(defn formatted-reports [ reports ]
  (format-reports (info-from reports) (org-from reports)))

(defn report-info [ report ]
 { :txt (second report)
   :map (first report)
 })


(deftest analyze-document-test-GAP
   (testing "Should output a report for each  sentence in document"
          (let [ reports (formatted-reports GAP-reports)]
            (is (< 0 (count reports)))
            (foreach [ r reports ] (println r "\n\n")))))

(deftest analyze-document-test-accuracy-GAP    
  (testing "Should output dividend amount and specify quarterly if applicable, for GAP"
    (let [ rs (second GAP-reports)
           org (first GAP-reports)
           r1 (report-info (:map (first rs)))
           r2 (report-info (:map (second rs))) ] 
(println "GAP REPORTS:\n" r1 "\n\n" r2)
      (is (= "GPS" org))
      (is (= "$0.6" (:attr-val r1)))
      (is (= "dividend" (:attr r1)))
      (is (= "$0.15" (:attr-val r2)))
      (is (= "dividend" (:attr r2)))
      (is (= true (:qualifier-val r2)))
      (is (= "quarterly" (:qualifier r2))))))
