(ns nlp-utils.util
  (:import (java.io File)
           (java.util Properties)
           (org.apache.commons.io FileUtils)))

(defn file?
"Yields true if o is an instance of java.io.File; false otherwise"
[ o ] (= File (class o)))

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


(defn cls
"Prints n-1 form feeds - default is 15. Intended to clear the REPL screen"
([ n ] (let [ ls (map (fn[_] (println \formfeed)) (range 1 n)) 
          _ (doall ls)   ] nil))
([] (cls 16)))


(defn show-cp 
"Shows system classpath elements"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (println (apply str (interpose "\n" (. cp split ":"))))))


(defn reverse-map
"Yields a hash map with m's values and keys inverted. Should not be used if
m is not one-to-one correpondance.
"
[ m ]
  (let [ vks (map #(list (val %) (key %)) m) ]
    (apply hash-map (flatten vks))))


(defn print-header
"Outputs a header and message with surrounding padding, to the console."
[ hdr msg padding ]
  (println (str "\n" padding hdr msg padding "\n")))
