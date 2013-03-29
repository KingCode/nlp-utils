(ns nlp-utils.test-util
  (:import (java.util Properties)
           (edu.stanford.nlp.trees.semgraph SemanticGraph))
  (:use clojure.test
        nlp-utils.stanford-corenlp
        nlp-utils.stanford-corenlp-const
        nlp-utils.stanford-corenlp-pool
        nlp-utils.util
        nlp-utils.stanford-corenlp-test-const))

(defn hdr[ name msg ] (print-header (str name " START: ")  msg " ***************************** "))
(defn ftr[ name ] (print-header (str name " END") "" " ++++++++++++++++++++++++++++++++++++ "))


(defn get-document-sentences
([ s_atom filepath]
  (do
    (if (nil? @s_atom)
        (swap! s_atom (fn [ v ] (if (not (nil? v)) v
                                        (let [ txt (str-from-file filepath) ]
                                          (sentences txt SPLIT-PL false))))))
    @s_atom))
([ filepath ]
  (get-document-sentences (atom nil) filepath)))


(defn ^SemanticGraph get-collapsedCCDep
[ ^String txt ]
  (let [ sentence (first (annotated-for-sentence txt PARSE-PL)) ]
    (annotated-for-collapsedCCDep sentence)))




