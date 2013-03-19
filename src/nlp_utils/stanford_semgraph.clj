(ns nlp-utils.stanford-semgraph

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
[ graph node reln-re ]
  (get-edges graph node reln-re false))


(defn get-in-edges
"Yields the graph's edges bearing a relation matching reln-re and  having node as the dependent." 
[ graph node reln-re ]
  (get-edges graph node reln-re true))



