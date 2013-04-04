(ns nlp-utils.util
  (:import (java.io File)
           (java.util Properties)
           (org.apache.commons.io FileUtils)))


(defn ^String keyname 
"Yields the string value of a keyword."
[ ^clojure.lang.Keyword key ] (name key))


(defn in?
"Yields true if x is in coll, false otherwise."
[coll x] (if (some #{x} coll) true false))


(defmacro foreach
"For each element of coll, executes body with sym bound to the element."
 [[sym coll] & body]
  `(loop [coll# ~coll]
      (when-let [[~sym & xs#] (seq coll#)]
                  ~@body
           (recur xs#))))

(defn file?
"Yields true if o is an instance of java.io.File; false otherwise"
[ o ] (= File (class o)))

;; Regex patterns for unix and windows file paths, but not dirs
(def WIN_FILEPATH_REP #"(([a-zA-Z]:)|([.a-zA-Z0-9_-]+))?(\\[.a-zA-Z0-9_-]+)+")
(def NIX_FILEPATH_REP #"([.a-zA-Z0-9_-]+)?(/[.a-zA-Z0-9_-]+)+")


(defn filepath?
"Yields true if the argument string is a file path (detects both windows and *nix)."
[ ^String s ]
  (let [ os (.getProperty (System/getProperties) "os.name")
         win? (.matches (.toLowerCase os) "^(.*\\s+)?windows(\\s+.*)?$") 
         re-ptn (if win? WIN_FILEPATH_REP NIX_FILEPATH_REP) ]
    (.matches (.matcher re-ptn s))))

     
(defn exists-file?
"Yields true if filepath is for an existing filename; false otherwise"
[ filepath ]
  (let [ f (File. filepath) ]
    (.exists f)))

(defn str-from-file
"Loads a text file's content into the returned string. The file path must be relative to the classpath"
[ filepath ]
    (FileUtils/readFileToString (File. filepath)))


(defn props-for
"Constructs and returns a java.util.Properties from the input map"
[ m ] (let [ p (Properties.) 
             ks (keys m)
             vs (vals m)
             _ (doall (map #(.setProperty p %1 %2) ks vs)) ]  p ))


(defn reverse-map
"Yields a hash map with m's values and keys inverted. Should not be used if
m is not one-to-one correpondance.
"
[ m ]
  (let [ vks (map #(list (val %) (key %)) m) ]
    (apply hash-map (flatten vks))))


(defn print-coll
" For each coll element, prints header followed the element 0-based position, then the
element itself"
[ coll header sep ]
  (doall (map #(println (str header %2 sep %1)) coll (range 0 (count coll))))) 

(defn print-header
"Outputs a header and message with surrounding padding, to the console."
[ hdr msg padding ]
  (println (str "\n" padding hdr msg padding "\n")))

(defn search
"Yields a seq of indexes of the elements in coll matching search according to f.
 f must be a binary predicate taking the collection element and search, resp.
"
[ coll search f]
  (let [ matches (map #(if (f %1 search) %2 nil) coll (range 0 (count coll)))
       ]
     (filter #(not (nil? %)) matches)))

(defn which-startWith
"Yields a seq of indexes of the strings in coll which start with what."
[ coll what ]
  (search coll what #(.startsWith %1 %2)))

(defn which-match
"Yields a seq of indexes of the strings in coll which have a match for regex."
[ coll regex ]
  (search coll regex #(re-seq %2 %1)))

(defn matches
"Yields a lazy seq of all elements of coll which match regex."
[ coll regex ]
  (let [ idx (which-match coll regex) ]
    (map #(nth coll %) idx))) 


