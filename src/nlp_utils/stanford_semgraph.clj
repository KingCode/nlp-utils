(ns nlp-utils.stanford-semgraph
(:import (edu.stanford.nlp.ling CoreAnnotations 
                                CoreAnnotations$NormalizedNamedEntityTagAnnotation
                                CoreAnnotations$OriginalTextAnnotation
                                CoreAnnotations$TextAnnotation 
                                IndexedWord)
         (edu.stanford.nlp.trees.semgraph SemanticGraph)
         (edu.stanford.nlp.semgrex SemgrexPattern SemgrexMatcher)
         (java.util.regex Pattern)))


(defn compare-by-beginPos
"Yields the result of comparing a and b based on their beginPosition property value."
[ ^IndexedWord a ^IndexedWord b ]
    (compare (.beginPosition a) (.beginPosition b)))


(defn filter-edges-reln
"Filters edges, keeping only those having a grammatical relation matching reln-re."
[ edges reln-re ]
  (filter #(.matches (.toString (.getRelation %)) reln-re) edges))


(defn get-edges
"Yields the graph's edges bearing a relation matching reln-re and having node as the dependent 
if incoming? is true, or as governor otherwise. If incoming? is not provided, all edges matching reln-re
and containing node as both governor and dependent are returned. If node is not provided the same query is made 
against all edges in graph.
" 
([ ^SemanticGraph graph ^IndexedWord node ^String reln-re ^Boolean incoming?]
    (let [ edges (if incoming? (.getIncomingEdgesSorted graph node) 
                               (.getOutEdgesSorted graph node) )    ]
       (filter #(.matches (.toString (.getRelation %)) reln-re) edges)))
([ ^SemanticGraph graph ^IndexedWord node ^String reln-re ]
    (let [ in-edges (get-edges graph node reln-re true) 
           out-edges (get-edges graph node reln-re false) ]
        (concat in-edges out-edges)))
([ ^SemanticGraph graph ^String reln-re]
   (let [ edges (.getEdgeSet graph) ]
     (filter-edges-reln edges reln-re))))


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

(defn  org?
[ node ]
  (= "ORGANIZATION" (.ner node)))


(defn- ^SemgrexPattern compile-semgrex
"Yields a compiled pattern from semgrex-re."
[ ^String semgrex-re ]
  (SemgrexPattern/compile semgrex-re))


(def semgrex-pattern
"Yields a cached compiled pattern from semgrex-re.
 Memoizes nlp-utils.stanford-semgraph/compile-semgrex.
"
   (memoize compile-semgrex))


(defn ^String nne-tag
"Yields the normalized named entity tag annotation, i.e. the 'complete' value of a money, percent, date etc.,
of which the token for node is part, if ner-re matches the NER tag for the node.
If ner-re is not provided the NNE tag annotation for the node is returned without condition.
"
([ ^IndexedWord node ^String ner-re]
  (if (.matches (.ner node) ner-re)
    (.get node CoreAnnotations$NormalizedNamedEntityTagAnnotation)))
([ ^IndexedWord node ]
  (nne-tag node ".*")))


(defn get-nodes
"Yields all nodes in graph matching semgrex-re.
"
[ ^SemanticGraph graph ^String semgrex-re ]
  (let [ m (-> (semgrex-pattern semgrex-re) (.matcher graph)) ]
    (loop [ acc [] ]
       (if-not (.find m) acc 
         (recur (conj acc (.getMatch m)))))))
          

(defn format-nodes-text
"Yields a space separated string from concatenating each node's word by order
of appearance.
"
[ nodes ]
  ( ->> (sort compare-by-beginPos nodes) 
        (map #(.word %)) 
        (interpose " ") 
        (apply str)))


(defn get-nodes-as-text
"Yields formatted text from all nodes matching semgrex-re in graph.
The returned string is the word of each such node by order of appearance.
"
[ ^SemanticGraph graph ^String semgrex-re ]
  (if-let [ nodes (get-nodes graph semgrex-re) ]
    (format-nodes-text nodes)))


(defn ^IndexedWord get-node
"Yields the first node in graph matching semgrex-re.
"
[ ^SemanticGraph graph ^String semgrex-re ]
  (let [ m (-> (semgrex-pattern semgrex-re) (.matcher graph)) ]
    (if (.find m) (.getMatch m)))) 



(defn ^String get-money
"Yields the normalized monetary value for node, using its NER attr.
If instead graph and semgrex-re are provided, the same is obtained from the first node
resulting from a search on the graph using semgrex-re.
" 
([ ^IndexedWord node ]
    (if (money? node)
      (nne-tag node)))
([ ^SemanticGraph graph ^String semgrex-re ]
    (if-let [ node (get-node graph semgrex-re) ]
        (get-money node))))


(defmulti ^String get-word  
"Yields the word for node if it matches re, or nil.
If instead graph and semgrex-re are provided, the word is obtained from the first node
matching a search on the graph using semgrex-re.
"
 (fn [ arg re ] (class arg))) 

(defmethod get-word IndexedWord 
[ node ^Pattern re]
  (if-not (nil? node)
    (let [ w (.word node) ]
        (if (.matches w re) w))))

(defmethod get-word SemanticGraph 
[ graph ^String semgrex-re ]
    (get-word (get-node graph semgrex-re) ".*"))     

(defmethod get-word :default 
[ any any]
    nil)

(defn nodes-from-edge
"Yields one or both nodes in edge having named entities matching ner-re, or both nodes if 
ner-re is not provided."
([ edge ner-re ]
  (let [ nodes [(.getGovernor edge) (.getDependent edge)] ]
    (filter #(.matches (.ner %) ner-re) nodes)))
([ edge ]
  (nodes-from-edge edge ".*"))) 



(defn nne-tags-from-edge
"Yields normalized named entities from edge's nodes if (.ner node) matches ner-re.
"
([ edge ner-re ]
  (let [ sel-nodes (nodes-from-edge edge ner-re) ] 
    (map #(nne-tag %) sel-nodes)))
([ edge ]
  (nne-tags-from-edge edge ".*"))) 


(defn opposing-node
"Yields the node which is opposite node in edge, i.e. at the other end of the relation
between the two nodes. It is assumed that node is not nil and either the governor or dependent
of the relation in edge."
[ edge node ]
  (let [ gov (.getGovernor edge)
         dep (.getDependent edge) ]
    (if (.equals node gov) dep gov)))


(def DIVIDEND_RE "{lemma:dividend}")
(def DIVIDEND_P (SemgrexPattern/compile DIVIDEND_RE))

(def ORG_RE "{ner:ORGANIZATION}")
(def ORG_P (SemgrexPattern/compile ORG_RE))


(defn dividend-matcher
"Yields a dividend matcher for graph."
[ graph ]
  (.matcher DIVIDEND_P graph))


(defn matched-nodes
"Yields match results in matcher."
([ matcher results]
  (if (not (.find matcher)) results
    (recur matcher (conj results (.getMatch matcher)))))
([ matcher ]
  (matched-nodes matcher [])))


(defn matched-org
"Yields a seq of all nodes in graph having Organization as NER tag."
([ matcher results ]
  (if (not (.find matcher)) results
    (recur matcher (conj results (.getMatch matcher)))))
([ graph ]
  (matched-org (.matcher ORG_P graph) [])))
     

(defn nodes-text
"Yields the text annotation or if orig? is true, the original text from the node if a single node,
or each node in a seq if n is a collection. n must be either an IndexedWord or a seq thereof.
"
([ n orig? ]
  (let [func (if orig? #(.get % CoreAnnotations$OriginalTextAnnotation) 
                        #(.get % CoreAnnotations$TextAnnotation))]
    (if (coll? n) 
        (map func n) (func n)))) 
( [ n ]
  (nodes-text n true)))

(defn matched-attrs
"Yields a seq of matched attrs in graph according to ATTRS_RE."
[ graph ]
    (let [ m (matched-nodes graph) ]
        (nodes-text m true)))

