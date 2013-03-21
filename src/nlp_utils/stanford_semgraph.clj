(ns nlp-utils.stanford-semgraph
(:import (edu.stanford.nlp.ling CoreAnnotations 
                                CoreAnnotations$NormalizedNamedEntityTagAnnotation
                                CoreAnnotations$OriginalTextAnnotation
                                CoreAnnotations$TextAnnotation )
         (edu.stanford.nlp.semgrex SemgrexPattern SemgrexMatcher)))

(defn get-edges
"Yields the graph's edges bearing a relation matching reln-re and having node as the dependent 
if incoming? is true, or as governor otherwise. If node is not provided the same query is made 
against all edges in graph.
" 
([ graph node reln-re incoming?]
    (let [ edges (if incoming? (.getIncomingEdgesSorted graph node) 
                               (.getOutEdgesSorted graph node) )    ]
       (filter #(.matches (.toString (.getRelation %)) reln-re) edges)))
([ graph reln-re]
   (let [ edges (.getEdgeSet graph) ]
       (filter #(.matches (.toString (.getRelation %)) reln-re) edges))))


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
of which the token for node is part, if ner-re matches the NER tag for the node.
If ner-re is not provided the NNE tag annotation for the node is returned without condition.
"
([ node ner-re]
  (if (.matches (.ner node) ner-re)
    (.get node CoreAnnotations$NormalizedNamedEntityTagAnnotation)))
([ node ]
  (nne-tag node ".*")))


(defn get-money
"Yields the full monetary expression node is part of, using its NER attr." 
[ node ]
    (if (money? node)
      (nne-tag node)))


(defn nne-tags-from-edge
"Yields normalized named entities from edge's nodes if (.ner node) matches ner-re.
"
([ edge ner-re ]
  (let [ nodes [(.getGovernor edge) (.getDependent edge)] 
         sel-nodes (filter #(.matches (.ner %) ner-re) nodes) ]
    (map #(nne-tag %) sel-nodes)))
([ edge ]
  (nne-tags-from-edge edge ".*"))) 


(def ATTRS_RE "{lemma:dividend}")
(def ATTRS_P (SemgrexPattern/compile ATTRS_RE))

(defn matched-nodes
"Yields a seq of all nodes in graph whose text matches attributes according to ATTRS_RE."
([ matcher results]
  (if (not (.find matcher)) results
    (recur matcher (conj results (.getMatch matcher)))))
([ graph ]
  (matched-nodes (.matcher ATTRS_P graph) [])))
     
(defn nodes-text
"Yields the text annotation or if orig? is true, the original text from the node if a single node,
or each node in a seq if n is a collection. n must be either an IndexedWord or a seq thereof.
"
[ n orig? ]
  (let [func (if orig? #(.get % CoreAnnotations$OriginalTextAnnotation) 
                        #(.get % CoreAnnotations$TextAnnotation))]
    (if (coll? n) 
        (map func n) (func n)))) 


(defn matched-attrs
"Yields a seq of matched attrs in graph according to ATTRS_RE."
[ graph ]
    (let [ m (matched-nodes graph) ]
        (nodes-text m true)))

