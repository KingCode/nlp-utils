(ns nlp-utils.repl-util)

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


(defn- loading
"Outputs a string for a form which loads some needed modules and imports into current namespace.
 Used by loading-form.
"
[]
  "(do
     (use 'nlp-utils.util :reload)
     (use 'nlp-utils.stanford-corenlp-pool :reload)
     (use 'nlp-utils.stanford-corenlp-const :reload)
     (use 'nlp-utils.stanford-corenlp :reload)
     (use 'nlp-utils.stanford-corenlp-test-const :reload)
     (use 'nlp-utils.stanford-corenlp-test :reload)
     (import (edu.stanford.nlp.trees.semgraph SemanticGraph)
             (edu.stanford.nlp.semgrex SemgrexPattern SemgrexMatcher)
             (edu.stanford.nlp.trees GrammaticalRelation)
             (edu.stanford.nlp.ling IndexedWord)))")


(defn loading-form
"Outputs a form which loads some needed modules and imports, ready to be evaluated in current namespace.
"
[]
  (let [l-str (loading) ]
    (read-string l-str)))

