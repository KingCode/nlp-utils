(ns nlp-utils.stanford-attr
  (:use nlp-utils.util)
  (:use nlp-utils.stanford-semgraph)
  (:import 
    (edu.stanford.nlp.ling IndexedWord
                           CoreAnnotations
                           CoreAnnotations$NormalizedNamedEntityTagAnnotation)
    (edu.stanford.nlp.trees.semgraph SemanticGraph)
    (java.util.regex Pattern))) 


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


(defn ^String related-value
"Yields a normalized value for the first found node part of a relation reln-re in graph, and having 
its named entity matching related-ner-re, having a relation matching related-reln-re with 
the first found node having a named entity matching ner-re and part of a relation matching reln-re."
[ ^SemanticGraph graph ^String ner-re ^String reln-re ^String related-ner-re ^String related-reln-re]
  (let [ main-node (first (nodes-in-reln graph ner-re reln-re))
         related-edges (get-edges graph main-node related-reln-re)
         values (mapcat #(nne-tags-from-edge % related-ner-re) related-edges) ]
     (first values)))


(defn ^String date-in-money-from
"Yields a normalized value for the first found year expression associated with a money expression, itself in a
'from' relationship."
[ ^SemanticGraph graph ]
  (related-value graph "MONEY" "prep_from" "DATE" "prep_in"))



(defn ^String date-in-money-to
"Yields a normalized value for the first found year expression associated with a money expression, itself in a
'to' relationship."
[ ^SemanticGraph graph ]
  (related-value graph "MONEY" "prep_to" "DATE" "prep_in"))

(def EXCH "(NYSE|Nyse|nyse|NASDAQ|Nasdaq|nasdaq|TSX|Tsx|tsx)")
(def STOCK "[A-Z0-9]{1,6}")
(def SEP "\\s?:\\s?")
(def L-PARN "\\(?")
(def R-PARN "\\)?")
(def XTRA "(\\s*\\[.*\\]\\s*)?")
(def STOCK_RE (str L-PARN "(" EXCH SEP STOCK "|" STOCK SEP EXCH ")"  XTRA R-PARN))
(def STOCK_P (Pattern/compile STOCK_RE))

(def SYM_P (Pattern/compile STOCK))
(def XTRA_P (Pattern/compile XTRA))
(def EXCH_P (Pattern/compile EXCH))

(defn ^String find-stock
"Returns the first item matching STOCK_RE regexp; coll can be either a seq of strings, or
continuous text."
[ c ]
  (if (= String (class c))
        (let [ m (.matcher STOCK_P c)
               _ (.find m) ]
            (.group m))
        (first (filter #(if-let [ candidate % ]
                            (let [matcher (. STOCK_P matcher candidate) ]
                               (.matches matcher))) c))))


(defn ^String txt-from-org-nodes
"Sorts the argument nodes by order of appearance of their corresponding text value
and returns their concatenated value."
[ nodes ]
  (let [ sorted-nodes (sort compare-by-beginPos nodes)
         as-sorted-txt (map #(nodes-text %) sorted-nodes)
         txt-and-seps (interpose " " as-sorted-txt) ]
     (apply str txt-and-seps)))


(defn ^String format-stock
"Formats stock symbol into EXCHANGE:SYMBOL"
[ stock ]
  (let [ extra-m (.matcher XTRA_P stock)
         _ (.find extra-m)
         extra (.group extra-m)

         exch-m (.matcher EXCH_P stock)
         _ (.find exch-m)
         exch (.group exch-m)

         cleansed-1 (.replace stock extra "")
         cleansed-2 (.replace cleansed-1 exch "")

    ;; we got rid of the exchange first, 
    ;;since it could match STOCK as well
    ;;
         sym-m (.matcher SYM_P cleansed-2)  
         _ (.find sym-m)
         sym (.group sym-m)   
       ]
    (str exch ":" sym)))

      

(defn ^String org
"Yields a normalized value for the first found organization expression. If provided, txt must be the source 
text for graph: it is used to extract the stock symbol if an organization is found in graph.
If no stock symbol if found (either with or without source text) all organization tokens are returned by order
of appearance."
([ ^SemanticGraph graph txt ]
  (let [ nodes (matched-org graph)
         as-txt (map #(nodes-text %) nodes) 
         stock (find-stock as-txt) ]
    (cond stock (format-stock stock)
          txt (if-let [ stock-from-text (find-stock txt) ] (format-stock stock-from-text)
                        (txt-from-org-nodes nodes))
          :else (txt-from-org-nodes nodes))))

([ ^SemanticGraph graph ]
  (org graph nil)))


(defn dividend-nodes
"Yields a seq of all nodes in graph whose text has 'dividend' as lemma."
[ graph ]
    (matched-nodes (dividend-matcher graph)))


;;(defn ^Boolean quarterly-dividend?
;;"Returns true if the dividend is found in graph (if divnode not provided) and has a quarterly modifier or
;;similar qualifier attached; false otherwise. If provided divnode must be the node bearing 'dividend' as its lemma
;;tag within graph."
;;([ ^SemanticGraph graph ^IndexedWord divnode ]
 ;;   (let [ node (if (nil? divnode) 
