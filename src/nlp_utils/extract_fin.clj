(ns nlp-utils.extract-fin
    (:use nlp-utils.stanford-corenlp
          nlp-utils.stanford-corenlp-token
          nlp-utils.stanford-corenlp-pool
          nlp-utils.stanford-corenlp-const))


(defn get-sentences
"Breaks up unprocessed text into sentences of text using minimal configuration"
[ txt ]
  (sentences txt SPLIT-PL false))

(defn filter-1
"Filters a seq of sentences (document corpus) to retain only those having money figures or organization name,
and having both over 300 tokens and number + money tokens in greater amounts than one third.
"
[ sents ]
  (filter (fn [sent] (let [ toks (tokens-for-sentxt sent (select-tokens :txt :ner) NER-PL)
                            mrat (money-ratio toks)
                            nrat (number-ratio toks)
                            total (count toks)
                            monies (money-count toks)
                            nums (number-count toks)
                            figs (+ monies nums)
                            figsrat (/ figs total)
                        ]
             (cond (= 0 monies) false
                   (and (< 250 total) (< 1/3 figsrat)) false 
                   :else true)))
    sents))
                    

