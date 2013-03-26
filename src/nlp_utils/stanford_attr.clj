(ns nlp-utils.stanford-attr
  (:use nlp-utils.util)
  (:use nlp-utils.stanford-semgraph)
  (:import 
  (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation)
  (edu.stanford.nlp.trees.semgraph SemanticGraph))) 


(defn nodes-in-reln
"Yields the nodes whose named entity matches ner-re from an edge bearing a relation
matching reln-re."
[ ^SemanticGraph graph ^String ner-re ^String reln-re ] 
  (let [ edges (get-edges graph reln-re)
         nodes (map #(first (nodes-from-edge % ner-re)) edges) ]
     (filter #(not (nil? %)) nodes))) 
              

(defn attr-in-reln
"Yields the nodes whose named entity matches attr-re from an edge bearing a relation
matching reln-re. The node's normalized text is returned.
"
[ ^SemanticGraph graph ^String attr-re ^String reln-re ]
  (let [ edges (get-edges graph reln-re)  
         attr-vals (map #(first (nne-tags-from-edge % attr-re)) edges) 
       ]
    (filter #(not (nil? %)) attr-vals)))


(defn ^String money-of
"Yields a normalized value for the first found money expression in a prepositional 'of' relationship"
[ ^SemanticGraph graph ]
  (first (attr-in-reln graph "MONEY" "prep_of")))


(defn ^String money-from
"Yields a normalized value for the first found money expression in a prepositional 'from' relationship"
[ ^SemanticGraph graph ]
  (first (attr-in-reln graph "MONEY" "prep_from")))


(defn ^String money-to
"Yields a normalized value for the first found money expression in a prepositional 'to' relationship"
[ ^SemanticGraph graph ]
  (first (attr-in-reln graph "MONEY" "prep_to")))


(defn ^String year-in-money_from-prototype
"Yields a normalized value for the first found year expression associated with a money expression, itself in a
'from' relationship."
[ ^SemanticGraph graph ]
  (let [ money-node (first (money-from graph))
         money-edges (get-edges graph money-node "prep_in")
         dates (map #(nne-tags-from-edge % "DATE") money-edges) ]
    (first dates)))


(defn ^String related-value
"Yields a normalized value for the first found node part of a relation reln-re in graph, and having 
its named entity matching related-ner-re, having a relation matching related-reln-re with 
the first found node having a named entity matching ner-re and part of a relation matching reln-re."
[ ^SemanticGraph graph ^String ner-re ^String reln-re ^String related-ner-re ^String related-reln-re]
  (let [ main-node (first (nodes-in-reln graph ner-re reln-re))
         related-edges (get-edges graph main-node related-reln-re)
         values (map #(nne-tags-from-edge % related-ner-re) related-edges) ]
     (first values)))


(defn ^String year-in-money-from
"Yields a normalized value for the first found year expression associated with a money expression, itself in a
'from' relationship."
[ ^SemanticGraph graph ]
  (related-value graph "MONEY" "prep_from" "DATE" "prep_in"))
