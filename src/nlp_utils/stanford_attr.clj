(ns nlp-utils.stanford-attr
  (:use nlp-utils.util)
  (:use nlp-utils.stanford-semgraph)
  (:import (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation)))
    
(defn attr-in-reln
"Yields the first-found node for attr matching attr-re from an edge bearing a relation
matching reln-re. The node's text is returned.
"
[ graph attr-re reln-re ]
  (let [ edges (get-edges graph reln-re)  
         attr-vals (map #(first (nne-tags-from-edge % attr-re)) edges) 
       ]
    (filter #(not (nil? %)) attr-vals)))

