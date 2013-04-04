(ns nlp-utils.extract-rules
    (:use (nlp-utils stanford-semgraph
                     stanford-attr
                     stanford-corenlp
                     (stanford-corenlp-pool :only [SPLIT-PL PARSE-PL])
                     util))
    (:import (edu.stanford.nlp.trees.semgraph SemanticGraph)
             (edu.stanford.nlp.ling IndexedWord)))

(def rules-with-node [ [dividend-money quarterly-dividend?]])
(def rules-no-node [ money-of money-to ])

(defn run-rules
"Runs extraction rules until a result is found, and outputs a report."
([ ^SemanticGraph graph ^IndexedWord node ^clojure.lang.PersistentVector rules ]
  (loop [ r (first rules) ]
     (if r
        (let [ attr (first r) 
               attr-res (apply attr graph node)
               mod-res (if-let [ mod (second r) ]
                            (apply mod graph node))  
               r-res (filter #(not (nil? %)) [ attr-res mod-res ]) ]
          (if (seq r-res) r-res
              (recur (next rules)))))))
([ ^SemanticGraph graph ^clojure.lang.PersistentVector rules ]
  (run-rules graph nil rules)))

          
(defn analyze-sentence 
"Yields a report of (currently) dividend amount. If dividend is found to be quarterly, that is indicated."
[ ^String txt ]
 (let [ sent (first (annotated-for-sentence txt PARSE-PL)) 
        graph (annotated-for-collapsedCCDep sent) 
        node (first (dividend-nodes graph)) ]
    (if node
        (let [ result-1 (run-rules graph node rules-with-node) ]
            (if (seq result-1) result-1 
                (run-rules graph rules-no-node)))))) 


(defn format-report
"Yields a formatted string of an extracted attribute"
[ ^String txt ^String attr ^clojure.lang.PersistentVector report ] 
  (let [ info (if-let [ value (first report) ]
                (if-let [ modifier (second report) ]
                    (str "(" modifier ")" attr ": " value)
                    (str attr ": " value)))
       ]
     (str "> " info "\n>(CONTEXT: " txt)))
           

(defn analyze-document
"Yields extracted data from the argument, which can be either a text string or the path of a file thereof."
[ doc ]
  (let [ txt (if (filepath? doc) (str-from-file doc) doc)
         sents (sentences txt SPLIT-PL false) ]
    (map #(format-report (analyze-sentence  %)) sents)))
