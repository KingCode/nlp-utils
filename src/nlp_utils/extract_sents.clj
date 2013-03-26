(ns nlp-utils.extract-sents
    (:use nlp-utils.stanford-corenlp
          nlp-utils.stanford-corenlp-token
          nlp-utils.stanford-corenlp-pool
          nlp-utils.stanford-corenlp-const))


(defn get-sentences
"Breaks up unprocessed text into sentences of text using minimal configuration"
[ txt ]
  (sentences txt SPLIT-PL false))


(def NUMBER-ROW-RE #"((\s*(\p{Sc})?\s*)?\d+((\.|,)\d{2})?){3,}+")
(def ENUM-SENT-RE #"^[^:;]*:([^;]+;)+")

(defn has-table?
"Yields true if one or more rows of repeated (financial/currency) figures
are found within txt; false otherwise."
[ txt ]
  (< 0 (count (re-seq NUMBER-ROW-RE txt)))) 


(defn enum?
"Yields true iff the argument sentence is long and is an enumeration, based on criteria:
- the sentence contains more than 40 tokens
- (NOT IMPLEMENTED) the sentence starts with a complete grammatical (e.g. (NP VP)) ending with ':' 
- there are more than one ';' separators in the remaining tokens.
"
([ txt toks]
  (if (and (< 80 (count toks))
           (re-seq ENUM-SENT-RE txt)) true false))
([ txt ]
  (enum? txt (tokens-for-sentxt txt (select-tokens :txt) SPLIT-PL))))

(defn filter-1
"Filters a seq of sentences (document corpus) to retain only those having money figures or organization name,
and having both over 300 tokens and number + money tokens in greater amounts than one third.
"
[ sents ]
  (filter (fn [sent] (if (has-table? sent) false
                         (let [ toks (tokens-for-sentxt sent (select-tokens :txt :ner) NER-PL)
                            mrat (money-ratio toks)
                            nrat (number-ratio toks)
                            total (count toks)
                            monies (money-count toks)
                            nums (number-count toks)
                            figs (+ monies nums)
                            figsrat (/ figs total)
                           ]
                                (cond (= 0 monies) false
                                ;;(and (< 250 total) (< 1/3 figsrat)) false 
                                :else true))))
            sents))

                    
(defn format-sent
"Formats sentence txt. If it is an enumeration,
each enum item is prefixed with enum-sep."
[ txt enum-sep ]
  (if (enum? txt) 
        (let [ subs-raw (.split txt ";")  
               intro-and-first (.split (first subs-raw) ":")
               intro (str (first intro-and-first) ":")
               enum-first (second intro-and-first)
               subs-tail (cons enum-first (rest subs-raw))
               subs (cons intro subs-tail)
            ]               
          (apply str (interpose enum-sep subs)))
      txt ))

(defn format-all
"Takes a collection of sentences' text and formats it into a reader friendly string, 
prefixing each sentence with sent-sep. If a sentence is an enumeration, each enum 
item is prefixed with enum-sep.
"
[ sentxt sent-sep enum-sep ]
  (let [ fmt1 (map #(format-sent % enum-sep) sentxt) 
         fmt2 (interpose sent-sep fmt1) ]
    (apply str sent-sep fmt2)))
