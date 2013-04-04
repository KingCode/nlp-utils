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
"
          [ { :attr-func dividend-money 
              :attr "dividend" 
              :qualifier-func quarterly-dividend? 
              :qualifier "quarterly"
            },
            { :attr-func money-of 
              :attr "dividend" 
            } ])


(defn ^clojure.lang.Keyword func-keyword
"Yields a new keyword ending with 'func' and prefixed with 'name-'."
[ ^clojure.lang.Keyword root ]
  (keyword (str (keyname root) "-func")))



(defn ^clojure.lang.IPersistentMap process-rule
"Yields the result of processing each of the rule's functions on graph and node.
The output is a map with all key-func/key keyed entries for which key-func returns a non-nil
value, with the value replacing the function for key-func entry. See rules declaration for 
the expected structure of rule.
If none of the functions returns a non-nil value, nil is returned."
[ ^clojure.lang.IPersistentMap rule ^SemanticGraph g ^IndexedWord n ^clojure.lang.IPersistentMap acc ]
  (let [ attrs (filter #(not (.endsWith (keyname %) "-func")) rule) 
         vals (map #(let [ fkw (func-keyword (.key %))
                           f (apply fkw rule)
                           val (apply f g n)  ]
                       (if val [ fkw val ] nil))  attrs)
         only-vals (filter #(not (nil? %)) vals) ]

     (if (empty? only-vals) nil
        (let [ vals-map (apply hash-map (apply concat only-vals))
               attrs-for-vals (filter 
                                #(not (nil? 
                                    (apply (func-keyword (.key %)) vals-map))) attrs) ]
            (apply conj vals-map (map #(hash-map (first %) (second %)) attrs-for-vals))))))


(defn run-rules
"Runs extraction rules until a result is found, and outputs a map mirroring the key of the successful
rule, omitting all entry pairs xyz-* for which xyz-func returned nil."
([ ^SemanticGraph graph ^IndexedWord node ^clojure.lang.PersistentVector rules ]
  (if-let [ r (first rules) ]
     (if-let [ report (process-rule r graph node) ]
        report
        (recur graph node (next rules)))))
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
