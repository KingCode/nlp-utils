(ns nlp-utils.stanford-semgraph
(:import (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation))
)

(defn get-edges
"Yields the graph's edges bearing a relation matching reln-re and having node as the dependent 
if incoming? is true, or as governor otherwise.
" 
[ graph node reln-re incoming?]
    (let [ edges (if incoming? (.getIncomingEdgesSorted graph node) 
                               (.getOutEdgesSorted graph node) )    ]
       (filter #(.matches (.toString (.getRelation %)) reln-re) edges)))


(defn get-out-edges
"Yields the graph's edges bearing a relation matching reln-re and  having node as the governor." 
([ graph node reln-re ]
  (get-edges graph node reln-re false))
([ graph node ]
  (get-out-edges graph node ".*")))


(defn get-in-edges
"Yields the graph's edges bearing a relation matching reln-re and  having node as the dependent." 
([ graph node reln-re ]
  (get-edges graph node reln-re true))
([ graph node ]
  (get-in-edges graph node ".*")))


(defn money?
[ node ]
  (= "MONEY" (.ner node)))


(defn percent?
[ node ]
  (= "PERCENT" (.ner node)))


(defn date?
[ node ]
  (= "DATE" (.ner node)))


(defn nne-tag
"Yields the normalized named entity tag annotation, i.e. the 'complete' value of a money, percent, date etc.,
of which the token for node is part."
[ node ]
  (.get node CoreAnnotations$NormalizedNamedEntityTagAnnotation))


(defn get-money
"Yields the full monetary expression node is part of, using its NER attr." 
[ node ]
    (if (money? node)
      (nne-tag node)
        nil))

