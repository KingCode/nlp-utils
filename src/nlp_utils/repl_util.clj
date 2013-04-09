(ns nlp-utils.repl-util
    (:use [clojure.set :only [difference]])
    (:use nlp-utils.extract-rules)
    (:use nlp-utils.util)
    (:use nlp-utils.stanford-attr)
    (:use nlp-utils.stanford-corenlp)
    (:use nlp-utils.stanford-corenlp-pool)
    (:use nlp-utils.stanford-corenlp-test)
    (:use nlp-utils.stanford-corenlp-test-const)
    (:import (edu.stanford.nlp.trees.semgraph SemanticGraphFormatter))
)

(defn cls
"Prints n-1 form feeds - default is 15. Intended to clear the REPL screen"
([ n ] (let [ ls (map (fn[_] (println \formfeed)) (range 1 n))
          _ (doall ls)   ] nil))
([] (cls 16)))


(defn show-cp
"Shows system classpath elements"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (println (apply str (interpose "\n" (. cp split ":"))))))


(defn show-vars
([ ns exclude]
  (let [ interned (ns-interns ns)
         selector #(let [ v (val %)
                          k-str (str (key %)) ] 
                     (and (not (in? exclude v)) 
                          (not (.startsWith k-str "user.proxy$java.lang.Object$"))))
         selected (filter selector interned) 
         items (map #(key %) selected) ]
     (foreach [it (sort items)] (println "\t" it))))
([ exclude ]
  (show-vars 'user exclude))
([] 
  (show-vars [ #'user/cdoc #'user/sourcery #'user/help #'user/defn
               #'user/clojuredocs #'user/set-signal-handler!])))


(defn- loading
"Outputs a string for a form which loads some needed modules and imports into current namespace.
 Used by loading-form.
"
[]
  "(do
     (use 'nlp-utils.util :reload)
     (use 'nlp-utils.stanford-attr :reload)
     (use 'nlp-utils.stanford-semgraph :reload)
     (use 'nlp-utils.stanford-corenlp-pool :reload)
     (use 'nlp-utils.stanford-corenlp-const :reload)
     (use 'nlp-utils.stanford-corenlp :reload)
     (use 'nlp-utils.stanford-corenlp-test-const :reload)
     (use 'nlp-utils.stanford-corenlp-test :reload)
     (import 
             (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation)
             (edu.stanford.nlp.trees.semgraph SemanticGraph)
             (edu.stanford.nlp.semgrex SemgrexPattern SemgrexMatcher)
             (edu.stanford.nlp.trees GrammaticalRelation)
             (edu.stanford.nlp.ling IndexedWord)))")


(defn loading-form
"Outputs a form which loads some needed modules and imports, ready to be evaluated in current namespace.
"
[]
  (let [l-str (loading) ]
    (read-string l-str)))

;;FORMATTER FOR SEM. GRAPHS
(defn semgraph-format
"Constructs a semantic graph formatter - see edu.stanford.nlp.trees.semgraph.SemanticGraphFormatter constructor.
 Defaults are 40 and 3 for width and indent and true for all others, except for showAnnos which has default false.
"
([ width indent smartIndent showRelns showTags showAnnos showIndices ]
  (SemanticGraphFormatter. width indent smartIndent showRelns showTags showAnnos showIndices))
([ width indent showAnnos ]
  (semgraph-format width indent true true true showAnnos true))
([]
  (semgraph-format 40 3 false)))


(defn graph-str
"Yields a string representation of the semgraph argument, according to fmt.
"
[ semgraph fmt ] (. fmt formatSemanticGraph semgraph))


(defn show-graph
"Prints to the console a formatted string of semgraph using fmt.
"
[ semgraph fmt ] 
  (let [ gstr (graph-str semgraph fmt)]
    (println gstr)))


(defn get-CCdeps
"Yields the CC collapsed dependencies for the argument sentence txt."
[ txt ]
  (let [ sent (first (annotated-for-sentence txt PARSE-PL)) ]
    (annotated-for-collapsedCCDep sent)))


;;DATA SETUPS
(defn get-div-CCdeps_SO
"Yields the CC collapsed dependencies from Southern data sample"
[]
  (let [ txt (nth (get-document-sentences DATA_FIL6) 0)
         sent (nth (annotated-for-sentence txt PARSE-PL) 0) ]
     (annotated-for-collapsedCCDep sent)))

(defn get-CCdeps-from-file
"Yields a seq of the CC collapsed dependencies for each parsed sentence of the text in 'file' filepath.
"
[ file ]
  (let [ all-txt (get-document-sentences file)
         all-sents (map #(annotated-for-sentence % PARSE-PL) all-txt) ]
    (map #(annotated-for-collapsedCCDep (first %)) all-sents)))

(defn get-div-CCdeps-GAP
[]
  (get-CCdeps-from-file DATA_FIL7)) 
