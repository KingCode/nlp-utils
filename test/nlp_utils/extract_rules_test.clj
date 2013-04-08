(ns nlp-utils.extract-rules-test
    (:use nlp-utils.extract-rules)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:use nlp-utils.util)
    (:use clojure.test))

(def GAP-reports (extract-reports GAP-FILE))
(def HBHC-reports (extract-reports HBHC-FILE))
(def FSTR-reports (extract-reports FSTR-FILE))
(def GEO-reports (extract-reports GEO-FILE))
(def APOG-reports (extract-reports APOG-FILE))

(defn org-from [ reports ]
  (first reports))

(defn info-from [ reports ]
  (second reports))

(deftest extract-reports-test-GAP
   (testing "Should output a report for each  sentence in document"
          (let [ reports (format-reports GAP-reports)]
            (is (< 0 (count reports)))
            (foreach [ r reports ] (println r "\n\n")))))

(deftest extract-reports-test-accuracy-GAP    
  (testing "Should output dividend amount and specify quarterly if applicable, for GAP"
    (let [ info (info-from GAP-reports)
           org (org-from GAP-reports)
           r1 (:result (first info))
           r2 (:result (second info)) ] 
;;(println "GAP REPORTS:\n" r1 "\n\n" r2)
      (is (= "NYSE:GPS" org))
      (is (= "$0.6" (:attr-val r1)))
      (is (= "dividend" (:attr r1)))
      (is (= "$0.15" (:attr-val r2)))
      (is (= "dividend" (:attr r2)))
      (is (= true (:qualifier-val r2)))
      (is (= "quarterly" (:qualifier r2))))))


(deftest extract-reports-test-HBHC
  (testing "Should output a report for each sentence in HBHC document"
     (let [ reports (format-reports HBHC-reports) ]
        (is (< 0 (count reports)))
        (foreach [ r reports ] (println r "\n\n")))))


(deftest extract-reports-test-accuracy-HBHC
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (let [ info (info-from HBHC-reports)
           org (org-from HBHC-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:HBHC" org))
      (is (= "$0.24" (:attr-val r)))
      (is (= true (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r))))))


(deftest extract-reports-test-GEO
   (testing "Should output a report for each sentence in GEO document"
     (let [ reports (format-reports GEO-reports) ]
        (is (< 0 (count reports)))
        (foreach [ r reports ] (println r "\n\n")))))

(deftest extract-reports-test-accuracy-GEO
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (let [ info (info-from GEO-reports)
           org (org-from GEO-reports)
           r (:result (first info)) ]
      (is (= "GEO" org))
      (is (= "$0.5" (:attr-val r)))
      (is (= true (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r))))))

(deftest extract-reports-test-FSTR
   (testing "Should output a report for each sentence in FSTR document"
     (let [ reports (format-reports FSTR-reports) ]
        (is (< 0 (count reports)))
        (foreach [ r reports ] (println r "\n\n")))))


(deftest extract-reports-test-accuracy-FSTR
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (let [ info (info-from FSTR-reports)
           org (org-from FSTR-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:FSTR" org))
      (is (= "$0.03" (:attr-val r)))
      (is (= true (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r))))))


(deftest extract-reports-test-APOG
   (testing "Should output a report for each sentence in APOG document"
     (let [ reports (format-reports APOG-reports) ]
        (is (< 0 (count reports)))
        (foreach [ r reports ] (println r "\n\n")))))


(deftest extract-reports-test-accuracy-APOG
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (let [ info (info-from APOG-reports)
           org (org-from APOG-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:APOG" org))
      (is (= "$0.09" (:attr-val r)))
      (is (= true (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r))))))
