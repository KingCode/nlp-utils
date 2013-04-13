(ns nlp-utils.rules.util
    (:use nlp-utils.util))

(defn ^clojure.lang.IFn make-rating
"Yields a rating function which takes a result map and returns a ratio; config must be
a map with the following keys:
   :core -> the name of the key in the result map which points to the core attribute value,
            e.g. the money amount of a dividend. It is assigned one more than half the returned rating
            value if present.
   :aux (optional) -> a seq of other keys pointing at auxiliary attribute values, each one assigned one rating unit.

The ratio returned by the rating function can be used to calculate the quality of the result.
If two results have the same rating, the ratings' denominators can be compared to determine
the result with the most information.

For example, assuming two results A and B are being compared, A with 5 auxiliary attrs and B with 2 auxiliaries,
then they have maximum ratings off 11/11 and 5/5 respectively, and A can be considered to be higher quality.
Similarly, if A has all auxiliaries but is missing its core attribute value and B has its core but is missing all
its auxiliaries, then 5/11 < 3/5 and B is a better result.
"
[ ^clojure.lang.IPersistentMap config ]
  (let [ core (:core config) aux (or (:aux config) ()) ]
    (fn [ ^clojure.lang.IPersistentMap result]
      ;(do (println "RESULT: " result ", (core result)=" (core result))
        (let [ num-aux (count aux)
               denominator (-> num-aux (* 2) (+ 1))
               core-rval (if (core result) (inc num-aux) 0)
               aux-rval (reduce + (map #(if (% result) 1 0) aux))
               numerator (+ core-rval aux-rval) ]
            (make-ratio numerator denominator)))))


(defn make-verifier
"Yields a verifier function taking as input a result map and returning
true if (attr-key rmap) is not nil.
"
[ attr-key & attrs ]
  (fn [ result ]
    (let [vals (->> (cons attr-key attrs)
                   (map #(% result))) ]
             (not (some empty? vals)))))


(defn make-formatter
"Yields a formatter taking as input a result map.
config must be a map with  :a, :aval, [:q and :qval]  pointing to
qualifier, qualifier value, main attribute, and main attribute value resp.
The return value is in the pattern
    <qualifier> <attribute>: <attribute value>
all of which are taken respectively from :q, :a and :aval keyed values in
config applied to the result passed to the formatter.
"
[ ^clojure.lang.IPersistentMap config ]
  (let [ q (:q config) qval (:qval config) a (:a config) aval (:aval config) ]
     #(let [qstr (if (and q qval)
                     (if (qval %) (str (q %)  " ")
                                                "")
                     "")
            ]
      (str qstr (a %) ": " (aval %)))))


