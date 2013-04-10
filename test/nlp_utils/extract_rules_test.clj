(ns nlp-utils.extract-rules-test
    (:use nlp-utils.extract-rules)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:use nlp-utils.util)
    (:use clojure.test))


;;Whether to: run tests that don't do much other than print output
;;            run tests that only perform accuracy checking
;;            include empty/nil reports in print-demos
(def ^:private SETTINGS { :print-demo false
                          :show-empties false
                          :accuracy true
                          :show-ruleids true 
                })

(defn show-ruleids? [] (:show-ruleids SETTINGS))
(defn print-demo? [] (:print-demo SETTINGS))
(defn test-accuracy? [] (:accuracy SETTINGS))

(defn show-ruleids [ test-name r & rs ]
  (if (show-ruleids?)
  (let [ ids (apply str (interpose " " (cons (:rule-id r) (map #(:rule-id %) rs)))) ]
     (println "*** TEST " test-name " used rules " ids " ***"))))


(defn filter-analysis [ an ]
    (let [ 
           org (first an)
           reports (-> 
                        (if (:show-empties SETTINGS) 
                                (constantly true) 
                                #(and (not (nil? %)) (not (nil? (:result %)))))
                        (filter (second an)))  ]
        [org reports]))


(defn print-reports [ formatted-reports ]
  (do
  (foreach [ r formatted-reports ]
    (println r))
  (println "-----------------------------------------------------\n")))


(def GAP-reports (extract-reports GAP-FILE))
(def HBHC-reports (extract-reports HBHC-FILE))
(def FSTR-reports (extract-reports FSTR-FILE))
(def GEO-reports (extract-reports GEO-FILE))
(def APOG-reports (extract-reports APOG-FILE))
(def EE-reports (extract-reports EE-FILE))
(def CSP-reports (extract-reports CSP-FILE))
(def HSC-reports (extract-reports HSC-FILE))
(def KSS-reports (extract-reports KSS-FILE))
(def LEN-reports (extract-reports LEN-FILE))
(def LION-reports (extract-reports LION-FILE))
(def MWV-reports (extract-reports MWV-FILE))
(def PLL-reports (extract-reports PLL-FILE))
(def QUALCOMM-reports (extract-reports QUALCOMM-FILE))
(def ASTEC-reports (extract-reports ASTEC-FILE))
(def SEASPAN-reports (extract-reports SEASPAN-FILE))
(def SLB-reports (extract-reports SLB-FILE))
(def SMG-reports (extract-reports SMG-FILE))
(def SO-reports (extract-reports SO-FILE))
(def TXN-reports (extract-reports TXN-FILE))
(def VALU-reports (extract-reports VALU-FILE))
(def VNO-reports (extract-reports VNO-FILE))
(def WMB-reports (extract-reports WMB-FILE))


(defn org-from [ reports ]
  (first reports))

(defn info-from [ reports ]
  (second reports))

(deftest extract-reports-test-GAP
   (testing "Should output a report for each  sentence in document"
     (if (print-demo?)
          (let [ reports (->> GAP-reports (filter-analysis) (format-reports)) ] 
            (is (< 0 (count reports)))
            (print-reports reports)))))

(deftest extract-reports-test-accuracy-GAP    
  (testing "Should output dividend amount and specify quarterly if applicable, for GAP"
    (if (test-accuracy?) 
    (let [ info (info-from GAP-reports)
           org (org-from GAP-reports)
           r1 (:result (first info))
           r2 (:result (second info)) ] 
      (is (= "NYSE:GPS" org))
      (is (= "$0.6" (:attr-val r1)))
      (is (= "dividend" (:attr r1)))
      (is (= "$0.15" (:attr-val r2)))
      (is (= "dividend" (:attr r2)))
      (is (= true (:qualifier-val r2)))
      (is (= "quarterly" (:qualifier r2)))
      (show-ruleids "accuracy-GAP" r1 r2)))))


(deftest extract-reports-test-HBHC
  (testing "Should output a report for each sentence in HBHC document"
    (if (print-demo?)
     (let [ reports (->> HBHC-reports (filter-analysis) (format-reports)) ] 
        (is (< 0 (count reports)))
        (print-reports reports))))) 


(deftest extract-reports-test-accuracy-HBHC
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (if (test-accuracy?)
    (let [ info (info-from HBHC-reports)
           org (org-from HBHC-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:HBHC" org))
      (is (= "$0.24" (:attr-val r)))
      (is (= "quarter" (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r)))
      (show-ruleids "accuracy-HBHC" r)))))


(deftest extract-reports-test-GEO
   (testing "Should output a report for each sentence in GEO document"
     (if (print-demo?)
     (let [ reports (->> GEO-reports (filter-analysis) (format-reports)) ] 
        (is (< 0 (count reports)))
        (print-reports reports)))))

(deftest extract-reports-test-accuracy-GEO
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (if (test-accuracy?)
    (let [ info (info-from GEO-reports)
           org (org-from GEO-reports)
           r (:result (first info)) ]
      (is (= "GEO" org))
      (is (= "$0.5" (:attr-val r)))
      (is (= "first quarterly"(:qualifier-val r)))
      (show-ruleids "accuracy-GEO" r)))))

(deftest extract-reports-test-FSTR
   (testing "Should output a report for each sentence in FSTR document"
     (if (print-demo?)
     (let [ reports (->> FSTR-reports (filter-analysis) (format-reports)) ] 
        (is (< 0 (count reports)))
        (foreach [ r reports ] (println r "\n\n"))))))


(deftest extract-reports-test-accuracy-FSTR
  (testing "Should output dividend amount and specify quarterly if applicable, for HBHC"
    (if (test-accuracy?)
    (let [ info (info-from FSTR-reports)
           org (org-from FSTR-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:FSTR" org))
      (is (= "$0.03" (:attr-val r)))
      (is (not (nil? (:qualifier-val r))))
      (show-ruleids "accuracy-FSTR" r)))))


(deftest extract-reports-test-APOG
   (testing "Should output a report for each sentence in APOG document"
     (if (print-demo?)
     (let [ reports (->> APOG-reports (filter-analysis) (format-reports)) ] 
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-APOG
  (testing "Should output dividend amount and specify quarterly if applicable, for APOG"
    (if (test-accuracy?)
    (let [ info (info-from APOG-reports)
           org (org-from APOG-reports)
           r (:result (first info)) ]
      (is (= "Nasdaq:APOG" org))
      (is (= "$0.09" (:attr-val r)))
      (is (= true (:qualifier-val r)))
      (is (= "quarterly" (:qualifier r)))
      (show-ruleids "accuracy-APOG" r)))))


(deftest extract-reports-test-EE
   (testing "Should output a report for each sentence in EE document"
     (if (print-demo?)
     (let [ reports (->> EE-reports (filter-analysis) (format-reports)) ] 
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-EE
  (testing "Should output dividend amount and specify quarterly if applicable, for EE"
    (if (test-accuracy?)
    (let [ info (info-from EE-reports)
           org (org-from EE-reports)
           r (:result (first info)) ]
      (is (= "NYSE:EE" org))
      (is (= "$0.25" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (= "regular quarterly cash" (:qualifier-val r)))
      (show-ruleids "accuracy-EE" r)))))


(deftest extract-reports-test-CSP
   (testing "Should output a report for each sentence in CSP document"
     (if (print-demo?)
     (let [ reports (->> CSP-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-CSP
  (testing "Should output dividend amount and specify quarterly if applicable, for CSP" 
    (if (test-accuracy?)
    (let [ info (info-from CSP-reports)
           org (org-from CSP-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "Nasdaq:CSPI" org))
      (is (= "$0.03" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (or (= "quarterly" qval) (= true qval))) 
      (show-ruleids "accuracy-CSP" r)))))


(deftest extract-reports-test-HSC
   (testing "Should output a report for each sentence in HSC document"
     (if (print-demo?)
     (let [ reports (->> HSC-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-HSC
  (testing "Should output dividend amount and specify quarterly if applicable, for HSC" 
    (if (test-accuracy?)
    (let [ info (info-from HSC-reports)
           org (org-from HSC-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:HSC" org))
      (is (= "$0.205" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (or (= "quarterly cash" qval) (= true qval))) 
      (show-ruleids "accuracy-HSC" r)))))


(deftest extract-reports-test-KSS
   (testing "Should output a report for each sentence in KSS document"
     (if (print-demo?)
     (let [ reports (->> KSS-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-KSS
  (testing "Should output dividend amount and specify quarterly if applicable, for KSS" 
    (if (test-accuracy?)
    (let [ info (info-from KSS-reports)
           org (org-from KSS-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "Kohl's" org))
      (is (= "$0.35" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (or (= "quarterly cash" qval) (= true qval))) 
      (show-ruleids "accuracy-KSS" r)))))



(deftest extract-reports-test-LEN
   (testing "Should output a report for each sentence in LEN document"
     (if (print-demo?)
     (let [ reports (->> LEN-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-LEN
  (testing "Should output dividend amount and specify quarterly if applicable, for LEN" 
    (if (test-accuracy?)
    (let [ info (info-from LEN-reports)
           org (org-from LEN-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:LEN" org))
      (is (= "$0.04" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (or (= "quarterly cash" qval) (= true qval))) 
      (show-ruleids "accuracy-LEN" r)))))



