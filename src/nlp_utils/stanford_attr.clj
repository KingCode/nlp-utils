(ns nlp-utils.stanford-attr
  (:use nlp-utils.util)
  (:use nlp-utils.stanford-semgraph)
  (:import (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation)))
    
(defn attr-in-reln
"Yields the first-found monetary amount for attr matching attr-re from an edge bearing a relation
matching reln-re.
"
[ graph attr-re reln-re ]
  (let [ edges (get-edges graph reln-re) ] 
         (map #(first (nne-tags-from-edge % attr-re)) edges)))

