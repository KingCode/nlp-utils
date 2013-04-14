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

                          :org-count 23
                })

(def ruletab (ref { :tests {} :frequencies {} }))

(defn use-ruleids? [] (:show-ruleids SETTINGS))

(defn show-ruleids? [] (and (use-ruleids?)
                            (<= (:org-count SETTINGS) 
                                (count (:tests @ruletab)))))

(defn print-demo? [] (:print-demo SETTINGS))
(defn test-accuracy? [] (:accuracy SETTINGS))

(defn tablen 
([s tab]  
;;  (let [ len (.length s) tmod (mod len tab)] 
;;      (-> len (- tmod) (/ tab))))
    (-> (.length s) (/ tab)))
([ s ]
  (tablen s 4)))

(defn sep-tabs
[ s ]
  (if (-> (tablen s) (> 1)) ":\t" ":\t\t\t"))

(defn addto-usage-freqs [ ids freqs ]
  (loop [ is ids fs freqs ]
    (if (empty? is) fs
        (let [ f-is (first is) 
               uptd (assoc fs f-is 
                  (if-let [ f (get fs f-is) ]
                        (inc f) 1)) 
             ]
             (recur (next is) uptd)))))


(defn format-rule-freqs [ freqs ]
    (->> (keys freqs) 
         (sort #(let [ v1 (get freqs %1) v2 (get freqs %2) ] 
                                    (minus (compare v1 v2))))
         (map #(str % " (" (get freqs %) ")"))
         (interpose ", ")
         (apply concat)
         (apply str)))
                                 

(defn show-ruleids [ test-name r & rs ]
  (if (use-ruleids?)
    (let [ ids-raw
               (->> rs
               (map #(:rule-id %))
               (cons (:rule-id r)))
           ids-ints (filter #(not (nil? %)) ids-raw)
           ids (->> ids-raw 
                    (map #(if (nil? %) "*" %))
                    (interpose "\t")
                    (apply str))                ]
      (dosync 
         (ref-set ruletab { :tests 
                                (assoc (:tests @ruletab) test-name ids)
                            :frequencies 
                                (addto-usage-freqs ids-ints (:frequencies @ruletab))
                           }))
      (if (show-ruleids?)
         (let [ usage (->> (:tests @ruletab)
                           (map #(str "\n\t" (key %) (sep-tabs test-name) (val %)))
                           (apply concat)
                           (apply str))      
                used (->> (:tests @ruletab) (map val) 
                             (map #(->> (.split % "\t") (filter (fn [s] (not (= "*" %))))))
                             (map #(-> (fn [s] (Integer/valueOf s)) (map %)))
                             (apply concat)
                             (set))
                num-used (count used)
                used-str (->> (interpose " " used) (apply str))
                num-orgs (:org-count SETTINGS) 
                rules-dist (format-rule-freqs (:frequencies @ruletab))
               ] 
            (println "**************\nRULE usage distribution:\n" usage "\nUsed rules: " used-str 
                    "\n(" num-used " rules for " num-orgs " companies.)"
                    "\n Rule usage frequencies:\n\t" rules-dist
                      "\n**************"))))))


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

(def QUART_P #"(.*\s+)?quarter(ly)?(\s+.*)?")

(defn quarterly? [ result ]
  (if-let [ qval (:qualifier-val result) ]
    (or (->> (.matcher QUART_P qval) (.find)) (true? qval))))


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
      (is (quarterly? r2))
;;      (is (= true (:qualifier-val r2)))
;;      (is (= "quarterly" (:qualifier r2)))
      (show-ruleids "GAP" r1 r2)))))


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
;;      (is (= "quarter" (:qualifier-val r)))
;;      (is (= "quarterly" (:qualifier r)))
      (is (quarterly? r))
      (show-ruleids "HBHC" r)))))


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
;;      (is (= "first quarterly"(:qualifier-val r)))
      (is (quarterly? r))
      (show-ruleids "GEO" r)))))

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
;;      (is (not (nil? (:qualifier-val r))))
      (is (quarterly? r))
      (show-ruleids "FSTR" r)))))


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
;;      (is (= true (:qualifier-val r)))
;;      (is (= "quarterly" (:qualifier r)))
      (is (quarterly? r))
      (show-ruleids "APOG" r)))))


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
;;      (is (= "regular quarterly cash" (:qualifier-val r)))
      (is (quarterly? r))
      (show-ruleids "EE" r)))))


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
;;      (is (or (= "quarterly" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "CSP" r)))))


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
;;      (is (or (= "quarterly cash" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "HSC" r)))))


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
;;      (is (or (= "quarterly cash" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "KSS" r)))))


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
;;      (is (or (= "quarterly cash" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "LEN" r)))))


(deftest extract-reports-test-LION
   (testing "Should output a report for each sentence in LION document"
     (if (print-demo?)
     (let [ reports (->> LION-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-LION
  (testing "Should output dividend amount and specify quarterly if applicable, for LION" 
    (if (test-accuracy?)
    (let [ info (info-from LION-reports)
           org (org-from LION-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NASDAQ:LION" org))
      (is (= "one new share for every 100 shares" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "quarterly cash" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "LION" r)))))


(deftest extract-reports-test-MWV
   (testing "Should output a report for each sentence in MWV document"
     (if (print-demo?)
     (let [ reports (->> MWV-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-MWV
  (testing "Should output dividend amount and specify quarterly if applicable, for MWV" 
    (if (test-accuracy?)
    (let [ info (info-from MWV-reports)
           org (org-from MWV-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:MWV" org))
      (is (= "$0.25" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "regular quarterly" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "MWV" r)))))



(deftest extract-reports-test-PLL
   (testing "Should output a report for each sentence in PLL document"
     (if (print-demo?)
     (let [ reports (->> PLL-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-PLL
  (testing "Should output dividend amount and specify quarterly if applicable, for PLL" 
    (if (test-accuracy?)
    (let [ info (info-from PLL-reports)
           org (org-from PLL-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:PLL" org))
      (is (= "$0.25" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "regular quarterly" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "PLL" r)))))


(deftest extract-reports-test-QUALCOMM
   (testing "Should output a report for each sentence in QUALCOMM document"
     (if (print-demo?)
     (let [ reports (->> QUALCOMM-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-QUALCOMM
  (testing "Should output dividend amount and specify quarterly if applicable, for QUALCOMM" 
    (if (test-accuracy?)
    (let [ info (info-from QUALCOMM-reports)
           org (org-from QUALCOMM-reports)
           r1 (:result (first info)) 
           qval1 (:qualifier-val r1) 
           r2 (:result (second info)) 
           qval2 (:qualifier-val r2) ]
      (is (= "$0.35" (:attr-val r1)))
      (is (= "$0.25" (:attr-from-val r1)))
      (is (= "dividend" (:attr r1)))
;;      (is (or (= "quarterly" qval1) (= true qval1))) 
      (is (quarterly? r1))
      (is (= "$1.4" (:attr-val r2)))
      (is (= "annualized" qval2))
      (show-ruleids "QUALCOMM" r1 r2)))))


(deftest extract-reports-test-ASTEC
   (testing "Should output a report for each sentence in ASTEC document"
     (if (print-demo?)
     (let [ reports (->> ASTEC-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-ASTEC
  (testing "Should output dividend amount and specify quarterly if applicable, for ASTEC" 
    (if (test-accuracy?)
    (let [ info (info-from ASTEC-reports)
           org (org-from ASTEC-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "$0.1" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "quarterly cash" qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "ASTEC" r)))))


(deftest extract-reports-test-SEASPAN
   (testing "Should output a report for each sentence in SEASPAN document"
     (if (print-demo?)
     (let [ reports (->> SEASPAN-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-SEASPAN
  (testing "Should output dividend amount and specify quarterly if applicable, for SEASPAN" 
    (if (test-accuracy?)
    (let [ info (info-from SEASPAN-reports)
           org (org-from SEASPAN-reports)
           r1 (:result (first info)) 
           qval1 (:qualifier-val r1) 
           r2 (:result (second info))
           qval2 (:qualifier-val r2)
;;           r3 (:result (nth info 3))
;;           qval3 (:qualifier-val r3) 
          ]
      (is (= "$0.3125" (:attr-val r1)))
      (is (= "dividend" (:attr r1)))
;;      (is (or (= "quarterly common share" qval1) (= true qval1)))
      (is (quarterly? r1))

      (is (= "$0.0625" (:attr-val r2)))
      (is (= "dividend increase" (:attr r2)))
;;      (is (= "quarterly common share" qval2))
      (is (quarterly? r2))

      (show-ruleids "SEASPAN" r1 r2)))))


(deftest extract-reports-test-SLB
   (testing "Should output a report for each sentence in SLB document"
     (if (print-demo?)
     (let [ reports (->> SLB-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-SLB
  (testing "Should output dividend amount and specify quarterly if applicable, for SLB" 
    (if (test-accuracy?)
    (let [ info (info-from SLB-reports)
           org (org-from SLB-reports)
           r1 (:result (first info)) 
           qval1 (:qualifier-val r1) 
           r2 (:result (second info))
           qval2 (:qualifier-val r2)
        ]
      (is (= "NYSE:SLB" org))
      (is (= "13.6 %" (:attr-val r1)))
      (is (= "dividend increase" (:attr r1)))
;;      (is (= "quarterly" qval1))
      (is (quarterly? r1))
      (is (= "$0.3125" (:attr-val r2)))
      (is (= nil qval2))

      (show-ruleids "SLB" r1 r2)))))


(deftest extract-reports-test-SMG
   (testing "Should output a report for each sentence in SMG document"
     (if (print-demo?)
     (let [ reports (->> SMG-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-SMG
  (testing "Should output dividend amount and specify quarterly if applicable, for SMG" 
    (if (test-accuracy?)
    (let [ info (info-from SMG-reports)
           org (org-from SMG-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:SMG" org))
      (is (= "$0.325" (:attr-val r)))
      (is (= "dividend" (:attr r)))
      (is (or (= "cash" qval) (= false qval))) 
      (show-ruleids "SMG" r)))))


(deftest extract-reports-test-SO
   (testing "Should output a report for each sentence in SO document"
     (if (print-demo?)
     (let [ reports (->> SO-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-SO
  (testing "Should output dividend amount and specify quarterly if applicable, for SO" 
    (if (test-accuracy?)
    (let [ info (info-from SO-reports)
           org (org-from SO-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "Southern Company" org))
      (is (= "$0.49" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "regular quarterly"  qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "SO" r)))))


(deftest extract-reports-test-TXN
   (testing "Should output a report for each sentence in TXN document"
     (if (print-demo?)
     (let [ reports (->> TXN-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-TXN
  (testing "Should output dividend amount and specify quarterly if applicable, for TXN" 
    (if (test-accuracy?)
    (let [ info (info-from TXN-reports)
           org (org-from TXN-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NASDAQ:TXN" org))
      (is (= "$0.21" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "quarterly cash"  qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "TXN" r)))))


(deftest extract-reports-test-VALU
   (testing "Should output a report for each sentence in VALU document"
     (if (print-demo?)
     (let [ reports (->> VALU-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-VALU
  (testing "Should output dividend amount and specify quarterly if applicable, for VALU" 
    (if (test-accuracy?)
    (let [ info (info-from VALU-reports)
           org (org-from VALU-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NASDAQ:VALU" org))
      (is (= "$0.15" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "quarterly cash"  qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "VALU" r)))))


(deftest extract-reports-test-VNO
   (testing "Should output a report for each sentence in VNO document"
     (if (print-demo?)
     (let [ reports (->> VNO-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-VNO
  (testing "Should output dividend amount and specify quarterly if applicable, for VNO" 
    (if (test-accuracy?)
    (let [ info (info-from VNO-reports)
           org (org-from VNO-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:VNO" org))
      (is (= "$0.73" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "regular quarterly"  qval) (= true qval))) 
      (is (quarterly? r))
      (show-ruleids "VNO" r)))))


(deftest extract-reports-test-WMB
   (testing "Should output a report for each sentence in WMB document"
     (if (print-demo?)
     (let [ reports (->> WMB-reports (filter-analysis) (format-reports)) ]
        (is (< 0 (count reports)))
        (print-reports reports)))))


(deftest extract-reports-test-accuracy-WMB
  (testing "Should output dividend amount and specify quarterly if applicable, for WMB" 
    (if (test-accuracy?)
    (let [ info (info-from WMB-reports)
           org (org-from WMB-reports)
           r (:result (first info)) 
           qval (:qualifier-val r) ]
      (is (= "NYSE:WMB" org))
      (is (= "$0.33875" (:attr-val r)))
      (is (= "dividend" (:attr r)))
;;      (is (or (= "regular"  qval) (= false qval))) 
      (is (quarterly? r))
      (show-ruleids "WMB" r)))))

