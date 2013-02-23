(ns nlp-utils.stanford-corenlp-token
    (:use nlp-utils.stanford-corenlp-const)
)


(defn- normalize-val
[ k v ]
  (if (= k :ner) (get NER_VALS v :OTHER) v)) 


(defn parse-token
"Breaks up a token string, returning a map keyed according to 
nlp-utils.stanford-corenlp-const/TOK_MAP, containing only the entries
found in token values, e.g. [Text=2011 NamedEntityTag=DATE] yields
{ :txt \"2011\" :ner :DATE}
"
[ tok ]
  (let [ attrs-raw (.split tok "\\s+")
         ;;get rid of the [ ]'s
         ftok (.substring (first attrs-raw) 1)
         ltok-raw (last attrs-raw)
         ltok (.substring ltok-raw 0 (dec (count ltok-raw)))
         rtoks (butlast (rest attrs-raw))
         toks-start (cons ftok rtoks)
         toks (conj toks-start ltok)
         kvs (map #(let [ sepIdx (.indexOf % TOK_ATTR_SEP) 
                          k-raw (.substring % 0 sepIdx)
                          k (get TOK_KEYS k-raw)                          
                          v-raw (.substring % (inc sepIdx))
                          v (normalize-val k v-raw)
                         ]
                     (list k v)) toks)
        ]
            (apply hash-map (flatten kvs))))


(defn parse-tokens
"Returns a seq of normalized token maps of the arg. raw (text) tokens."
[ tokens ]
    (map #(parse-token %) tokens))


(defn is-match
"Yields true if key is found in the token attrs and has value val.
 token can be either a raw token string or a map returned by (parse-token token)."
[ token key val]
  (let [ m  (if (= String (class token)) (parse-token token) token)]
    (= val (get m key))))


(defn is-money
"Yields true if the argument token NER is for money."
[ token ] (is-match token :ner :MONEY))


(defn is-org
"Yields true if the argument token NER is for an organization."
[ token ] (is-match token :ner :ORG))


(defn is-number
"Yields true if the argument token NER is for a number."
[ token ] (is-match token :ner :NUMBER))


(defn txt-and-occurs
"Yields a sorted seq of duples of token text and its number of occurences in the 
argument tokens matching attr and val"
[ tokens attr val ]
  (let [ tmaps (if (= String (class (first tokens)))
                        (parse-tokens tokens) tokens) 
         selected (filter #(is-match % attr val) tmaps)
         vlist (map #(:txt %) selected)
         vset (set vlist)
         occurs (map (fn [s] (let [ ct (count (filter #(= s %) vlist)) ]
                            (list s ct))) vset)
       ]
    (sort #(compare (second %1) (second %2)) occurs)))

(defn attr-count
"Yields the number of occurences of tokens with attribute attr having value val"
[ tokens attr val ]
  (count (filter #(is-match % attr val) tokens)))  

(defn attr-ratio
"Yields the ratio of tokens with attribute attr having value val relative 
to all tokens in the argument tokens coll."
[ tokens attr val]
    (let [ total (count tokens)
           sel (count (filter #(is-match % attr val) tokens)) ]
      (/ sel total)))


(defn number-ratio
"Yields the ratio of :NUMBER tokens relative to all tokens in the argument tokens coll."
[ tokens ]
    (attr-ratio tokens :ner :NUMBER))


(defn money-ratio
"Yields the ratio of :MONEY tokens relative to all tokens in the argument tokens coll."
[ tokens ]
    (attr-ratio tokens :ner :MONEY))


(defn num-orgs
"Yields the number of tokens identified as an organization (:ORG)"
[ tokens ]
    (attr-count tokens :ner :ORG))


(defn org-for
"Yields the organization with most occurences in tokens, or nil if none are found."
[ tokens ]
  (if (= 0 (num-orgs tokens)) nil
    (first (txt-and-occurs tokens :ner :ORG))))
