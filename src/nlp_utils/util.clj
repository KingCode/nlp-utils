(ns nlp-utils.util)

;;(defn cls
;;"Prints 20 form feeds - intended to clear the REPL screen"
;;[] (let [ ls (map (fn[_] (println \formfeed)) (range 1 21)) 
;;          _nil (doseq ls)   ] ""))

(defn show-cp 
"Shows system classpath elements"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (println (apply str (interpose "\n" (. cp split ":"))))))
