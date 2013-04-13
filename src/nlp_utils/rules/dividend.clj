(ns nlp-utils.rules.dividend
    (:use nlp-utils.rules.util)
    (:use (nlp-utils (stanford-attr :only [ money-of money-to
                                            dividend-money quarterly-dividend?])
                     (stanford-semgraph :only [get-money get-word get-node 
                                              compare-by-beginPos
                                              get-nodes get-nodes-as-text])))
    (:use nlp-utils.util))

(def rules
"Rules which are used to extract information, each a map with one or more of the following key
pairs patterns:
    :xyz-func maps to a (fn [^SemanticGraph g ^IndexWord w] ...) yielding the value of xyz
    :xyz      maps to a string identifying the value from invoking xyz-func
All data extraction functions (i.e. keyed with :*-func) are required to support the 2-arity signature above,
whether both args are used or not.

META ENTRIES

Map entries for :verifier, :rating and :formatter point to functions taking as input a map which must
be structured according to a similar pattern as described above, except that :<attr>-func entries are instead
:<attr>-val and map to the value returned by the function: essentially the 'value' map is a mirror of this one
and results from processing the rule. The verifier is a predicate which returns true if the
argument map is a meaningful, or useful result. The formatter converts the result into user friendly
text.

Map entry :rule-id is a meta entry for tracking rule usage.

            **** To keep in mind when adding new rules ****
Of the meta entries, :rule-id  and :rating are compulsory, and :verifier and :formatter optional.
"
          [ {
              :rule-id 1
              :attr-func dividend-money
              :attr "dividend"
              :qualifier-func quarterly-dividend?
              :qualifier "quarterly"
              :verifier #(not (nil? (:attr-val %)))
              :formatter #(let [qstr (if (:qualifier-val %) (str (:qualifier %)  " ") "")]
                            (str qstr (:attr  %) ": " (:attr-val %)))
              :rating (make-rating {:core :attr-val, :aux [ :qualifier-val]})
              :weight 2
            },
            {
              :rule-id 2
              :attr-func money-of
              :attr "dividend"
              :verifier #(not (nil? (:attr-val %)))
              :rating (make-rating {:core :attr-val})
            },
            {
              :rule-id 3
              :attr-func money-to
              :attr "dividend"
              :verifier #(not (nil? (:attr-val %)))
              :rating (make-rating {:core :attr-val})
            },
           {
              :rule-id 4
              :attr-func (fn [g _] (get-money g "{ner:MONEY} <prep_of ({tag:VBD} >dobj {word:dividend})"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-word g "{lemma:quarter} < {word:dividend}"))
              :qualifier "quarterly"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter { :q :qualifier, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
            },
            {
              :rule-id 5
              :attr-func (fn [g _] (get-money g "{ner:MONEY} >prep_per {word:dividend}"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-word g "{lemma:quarter} < ({} > {word:dividend})"))
              :qualifier "quarterly"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter  { :q :qualifier, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
              :weight 3
            },
            {
              :rule-id 6
              :attr-func (fn [g _] (get-money g "{ner:MONEY} <dobj {tag:/VB./} > {word:dividend}"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-word g
                    "{word:/quarter(ly)?/} <amod ({word:dividend} < ({ner:MONEY} <dobj {tag:/VB./}))"))
              :qualifier "quarterly"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter  { :q :qualifier, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
              :weight 3
             },
           {
              :rule-id 7
              :attr-func (fn [g _] (get-money g
                            "{ner:MONEY} <prep_of ({tag:/NN(P)?/} <prep_as ({tag:/VB(D)?/} >dobj {word:dividend}))"))
              :attr "dividend"
              :qualifier "quarterly"
              :qualifier-func (fn [g _] (if-let [ nodes (get-nodes g
                  "{} <amod ({word:dividend} <dobj ({tag:/VB(D)?/} >prep_as ({tag:/NN(P)?/} >prep_of {ner:MONEY})))") ]
                    ( ->> (sort compare-by-beginPos nodes) (map #(.word %)) (interpose " ") (apply str))))
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
              :weight 5
            },
            {
              :rule-id 8
              :attr-func (fn [g _] (get-money g
                           "{ner:MONEY} </prep_(of|to)/ ({tag:NN} <prep_on ({tag:/VB(N|B)?/} >dobj {word:dividend}))"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-nodes-as-text g
                            "{} </amod|nn/ ({word:dividend} <dobj ({tag:/VB(N|D)?/} >prep_on ({tag:NN} >/prep_(of|to)/ {ner:MONEY})))"))
              :qualifier "dividend modifiers"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
              :weight 5
            },
            {   
              :rule-id 9
              :attr-func (fn [g _] (get-money g
                                    "{ner:MONEY} <prep_of ({tag:NN} <prep_on ({tag:/VB(N|D)?/} >dobj {word:dividend}))"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-nodes-as-text g
                        "{} </amod|nn/ ({word:dividend} <dobj ({tag:/VB(N|D)?/} >prep_on ({tag:NN} >prep_of {ner:MONEY})))"))
              :qualifier "dividend modifiers"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter {:q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating {:core :attr-val :aux [:qualifier-val]})
              :weight 5
             },  
             {
              :rule-id 10
              :attr-func (fn [g _] (get-money g
                    "{ner:MONEY} <prep_to ({tag:/VB[ND]?/} >nsubj {lemma:dividend} >prep_from {ner:MONEY})"))
              :attr "dividend"
              :attr-from-func (fn [g _] (get-money g
                    "{ner:MONEY} <prep_from ({tag:/VB[ND]?/} >nsubj {lemma:dividend} >prep_to {ner:MONEY})"))
              :attr-from "increased from"
              :qualifier-func (fn [g _] (get-word g (multiline
                     "{word:/quarter(ly)?/} <amod ({lemma:dividend} <prep_for ({} <conj_and ({tag:/VB[ND]?/}
                       >nsubj {lemma:dividend} >prep_from {ner:MONEY} >prep_to {ner:MONEY})))")))
              :qualifier "quarterly"
              :verifier (make-verifier :attr-val :qualifier-val)
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val :attr-from ]})
              :weight 10
             },
             {
              :rule-id 11
              :attr-func (fn [g _] (get-money g
                    "{ner:MONEY} <prep_to ({tag:/VB[ND]?/} >nsubj ({} >nn {lemma:dividend}))"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-nodes-as-text g (multiline
                    "{word:/annual(ized| basis)?|quarter(ly)?/} </prep_on|amod/ ({} >nn {lemma:dividend} <dobj
                       ({tag:/VB[ND]?/} >prep_to {ner:MONEY}))")))
              :qualifier "dividend qualifier"
              :verifier (make-verifier :attr-val :qualifier-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val]})
              :weight 8
             },
             {
              :rule-id 12
              :attr-func (fn [g _] (get-money g
                            "{ner:MONEY} <prep_to ({tag:/VB[ND]?/} >dobj ({} > {lemma:dividend}))"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-nodes-as-text g
                    "{} </amod|nn/ ({lemma:dividend} < ({} <dobj ({tag:/VB[ND]?/} >prep_to {ner:MONEY})))"))
              :qualifier "div qualifier"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val]})
              :weight 5
             },
             {
              :rule-id 13
              :attr-func (fn [g _] (get-money g
                            "{ner:MONEY} <nsubj {tag:/VB[NDZ]?/} > ({lemma:increase} > {lemma:dividend})"))
              :attr "dividend increase"
              :qualifier-func (fn [g _] (get-nodes-as-text g
                    "{} </amod|nn/ ({lemma:dividend} < ({lemma:increase} < ({ner:MONEY} <nsubj {tag:/VB[NDZ]?/})))"))
              :qualifier "div increase qualifier"
              :verifier (make-verifier :attr-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val ]})
              :weight 5
             },
             {
              :rule-id 14
              :attr-func (fn [g _] (get-nodes-as-text g "{ner:PERCENT} < ({lemma:increase} > {lemma:dividend})"))
              :attr "dividend increase"
              :qualifier-func (fn [g _] (get-nodes-as-text g "{} </amod|nn/ ({lemma:dividend} < {lemma:increase})"))
              :qualifier "div modifier"
              :verifier  #(and (not (empty? (:attr-val %))) (re-seq #"\d+(\.\d+)?\s*%" (:attr-val %)))
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val ]})
              :weight 5
             }(comment ,
             {
              :rule-id 15
              :attr-func (fn [g _] (get-money g "{ner:MONEY} <prep_of {lemma:dividend}"))
              :attr "dividend"
              :qualifier-func (fn [g _] (get-nodes-as-text g
                                "{word:/quarter(ly)?/} </amod|nn/ ({lemma:dividend} >prep_of {ner:MONEY})"))
              :qualifier "quarterly"
              :verifier  (make-verifier :attr-val :qualifier-val)
              :formatter (make-formatter { :q :qualifier-val, :qval :qualifier-val, :a :attr, :aval :attr-val})
              :rating (make-rating { :core :attr-val :aux [ :qualifier-val ]})
              :weight 5
             })

])


