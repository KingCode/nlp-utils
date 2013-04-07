(ns nlp-utils.extract-rules
    (:use (nlp-utils stanford-semgraph
                     stanford-attr
                     stanford-corenlp
                     (stanford-corenlp-pool :only [SPLIT-PL PARSE-PL])
                     util))
    (:import (edu.stanford.nlp.trees.semgraph SemanticGraph)
             (edu.stanford.nlp.ling IndexedWord)))


(def rules
"Rules which are used to extract information, each a map with one or more of the following key
pairs patterns: 
    :xyz-func maps to a (fn [^SemanticGraph g ^IndexWord w] ...) yielding the value of xyz
    :xyz      maps to a string identifying the value from invoking xyz-func
All functions are required to support the 2-arity signature above, whether both args are used
or not.
If provided map entries for :verifier and :formatter point to functions taking as input a map which must
contain the same key pattern as described above. The verifier is a predicate which returns true if the
argument map is a meaningful, or useful result. The formatter converts the result into user friendly
text. 
"
          [ { :attr-func dividend-money 
              :attr "dividend" 
              :qualifier-func quarterly-dividend? 
              :qualifier "quarterly"
              :verifier #(not (nil? (:attr-val %)))
              :formatter #(let [qstr (if (:qualifier-val %) (str (:qualifier %)  " ") "")]
                            (str qstr (:attr  %) " " ": " (:attr-val %)))
            },
            { :attr-func money-of 
              :attr "dividend" 
              :verifier #(not (nil? (:attr-val %)))
            } 
            { :attr-func money-to
              :attr "dividend"
              :verifier #(not (nil? (:attr-val %)))
            }
])


(defn ^clojure.lang.Keyword append-to-keyword
"Yields a new keyword ending with 'func' and prefixed with 'name-'."
[ ^clojure.lang.Keyword root ^String suffix]
  (keyword (str (keyname root) suffix)))


(defn ^clojure.lang.Keyword val-keyword
[ ^clojure.lang.Keyword root ]
  (append-to-keyword root "-val"))

(defn ^clojure.lang.Keyword func-keyword
[ ^clojure.lang.Keyword root ]
  (append-to-keyword root "-func"))


(defn only-attrs
"Filters out '-func', :verifier and :formatter keyed entries from rule map."
[^clojure.lang.IPersistentMap rule ] 
  (let [ attrs (filter #(not (.endsWith (keyname (key %)) "-func")) rule) ]
    (filter #(let [ k (key %) ] 
                (and (not (= :verifier k)) (not (= :formatter k)))) attrs)))


(defn only-utils
"Retains only :verfier, :formatter keyed entries from rule map."
[ ^clojure.lang.IPersistentMap rule ]
    (filter #(let [ k (key %) ] 
              (or (= :formatter k) (= :verifier k))) rule))
  

(defn except-utils
[ ^clojure.lang.IPersistentMap m]
   (filter #(let [ k (key %) ]
               (and (not (= :formatter k)) (not (= :verifier k)))) m))



(defn ^clojure.lang.IPersistentMap translate-keys
"Yields a new map with m's keys ending with -func renamed to <key>-val."
[ ^ clojure.lang.IPersistentMap m ]
  (let [ attrs (only-attrs m) 
         attrs-as-map (reduce #(conj %1 (apply hash-map %2)) {} attrs)
         utils (filter #(let [ k (key %) ] 
                          (or (= :formatter k) (= :verifier k))) m)
         attrs-vals (map  #(let [ val ((func-keyword (key %)) m)
                                  key (append-to-keyword (key %) "-val") ]
                             [key val]) attrs)  
         main (reduce #(conj %1 (apply hash-map %2)) attrs-as-map attrs-vals)
       ] 
    (reduce #(conj %1 (apply hash-map %2)) main utils)))
 


(defn ^clojure.lang.IPersistentMap prepare-report
"Assembles partial results into a fully populated report, using seqs of mappings
 fron rule with the :*-func keyed entries mapped to the output of the correponding rule function.
"
[ ^clojure.lang.ISeq attrs ^clojure.lang.ISeq func-returns ^clojure.lang.IPersistentMap rule]
  (let [ vals (filter #(not (nil? %)) func-returns) 
         utils (only-utils rule)
         utils-map (apply hash-map (apply concat utils)) ]
     (if (and (empty? vals) (= nil (:verifier rule))) nil
        (let [ vals-map (apply hash-map (apply concat vals)) ]
               (->> attrs 
                   (filter #(not (nil? 
                               ((val-keyword (key %)) vals-map))))
                  (apply concat)
                  (apply hash-map)
                  (conj vals-map) 
                  (conj vals-map)
                  (conj utils-map))))))



(defn ^clojure.lang.IPersistentMap process-rule
"Yields the result of processing each of the rule's functions on graph and node.
The output is a map with all key-func/key keyed entries for which key-func returns a non-nil
value, with the value replacing the function for key-func entry. See rules declaration for 
the expected structure of rule.
If none of the functions returns a non-nil value, nil is returned."
[ ^clojure.lang.IPersistentMap rule ^SemanticGraph g ^IndexedWord n ]
  (let [ attrs (only-attrs rule)
         vals (map #(let [ k (key %)
                           fkw (func-keyword k) 
                           f (fkw rule)
                           val (f g n)  ]
                       (if val [ (val-keyword k) val ] nil))  attrs) ]

     (prepare-report attrs vals rule)))


(defn verified?
[ ^clojure.lang.IPersistentMap report ]
  (if (and report (:verifier report) ((:verifier report) report)) true false))


(defn run-rules
"Runs extraction rules until a result is found, and outputs a map mirroring the key of the successful
rule, omitting all entry pairs xyz-* for which xyz-func returned nil."
([ ^SemanticGraph graph ^IndexedWord node ^clojure.lang.PersistentVector rules ]
  (if-let [ r (first rules) ]
     (let [ report (process-rule r graph node) ]
        (if
          (verified? report) report
          (recur graph node (next rules))))))
([ ^SemanticGraph graph ^clojure.lang.PersistentVector rules ]
  (run-rules graph nil rules)))


(defn get-graph
"Yields a dependencies graph from txt, which should be a sentence ending with a period."
[ ^String txt ]
   (let [ sent (first (annotated-for-sentence txt PARSE-PL)) ]
      (annotated-for-collapsedCCDep sent)))
  

          
(defn ^clojure.lang.IPersistentMap analyze-sent 
"Yields a report of (currently) dividend amount. If dividend is found to be quarterly, that is indicated."
[ ^SemanticGraph graph ]
 (let [ node (first (dividend-nodes graph)) ]
    (run-rules graph node rules))) 

(defn analyze-sent-txt
([ ^String txt ]
    (analyze-sent (get-graph txt))))



(defn format-report
"Yields a formatted string of an extracted attribute and its sub-attributes if any.
org defaults to the empty string and report-shell is part of the output of extract-reports.
"
([ ^String org ^clojure.lang.IPersistentMap report-shell] 
  (let [ report (:result report-shell)
         txt (:text report-shell)
         info 
           (cond (nil? report) "(nothing found)\n"
                 (:formatter report) ((:formatter report) report)
                 :else  
                    (->> (except-utils report) 
                      (keys) 
                      (sort)
                      (partition 2) 
                      (map #(let [ attr-name ((first %) report)
                                   figure ((second %) report) ] [attr-name figure]))
                      (map #(str (first %) ": " (second %)))
                      (interpose "\n\t")
                      (apply str))) 
        ]
      (str org "> " info "\nCONTEXT:\n" txt "\n")))
([ ^clojure.lang.IPersistentMap report-shell ]
  (format-report "" report-shell)))


(defn format-reports
"Yields extraction reports formatted into a reader friendly string.
Analysis is expected to be the output of extract-reports.
"
[ ^clojure.lang.ISeq analysis]
  (let [ org (first analysis) 
         reports (second analysis) ]
    (map #(format-report org %) reports)))


(defn extract-reports 
"Yields extracted data from doc, which can be either a text string or the path of a file thereof.
The extracted data is a 2 element vector , the first element of which is the organzation name,and 
the second a seq of maps with the following entries: 
   { :text -> the text being reported on, i.e. a sentence.
     :result -> the result map
   }
The :result entry is a map of the extraction results, and keyed according to the :<attr> and 
:<attr>-val pattern, none of which maps to nil intentionally.
"
[ doc ]
  (let [ txt (if (filepath? doc) (str-from-file doc) doc)
         sents (sentences txt SPLIT-PL false) 
         sent-one (first sents)
         corp (org (get-graph sent-one) sent-one) ]
;;    (map #(format-report % corp (analyze-sent-txt %)) sents)))
      [ corp (map #(let [ report (analyze-sent-txt %) ] 
                      { :text % :result report} ) sents)]))


(defn analyze-document
[ doc ]
    (->> (extract-reports doc)
        (format-reports)))
