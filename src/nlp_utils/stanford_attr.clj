(ns nlp-utils.stanford-attr
  (:use nlp-utils.util)
  (:use nlp-utils.stanford-corenlp)
  (:import (edu.stanford.nlp.ling CoreAnnotations CoreAnnotations$NormalizedNamedEntityTagAnnotation)))
    
(defn get-money-as-prepof-dobj
"Retrieves money amount as preposition linked dependent of IndexedWord node, from CC collapsed dependencies."
[ graph node ]
  (if-let [ out-edges (.getOutEdgesSorted graph node) ]
     
      (if-let [ prep-edge (first (filter #(= "prep_of" (.toString (.getRelation %))) out-edges)) ]
          (let [ target (.getDependent prep-edge)
                 type (.ner target) ]
            (if (= "MONEY" type)
                (.get target CoreAnnotations$NormalizedNamedEntityTagAnnotation)
                nil))))) 
