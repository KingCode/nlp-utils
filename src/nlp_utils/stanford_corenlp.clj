(ns nlp-utils.stanford-corenlp
  (:import 
    (java.io PrintWriter) 
    (java.util List)
    (edu.stanford.nlp.io IOUtils)
    (edu.stanford.nlp.ling CoreAnnotations)
    (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
    (edu.stanford.nlp.trees Tree)
    (edu.stanford.nlp.util CoreMap)))

(defn annotation-for
"Yields a new Annotation from the text argument, or filepath if file? is true"
([ arg file? ]
  (let [ txt (if file? (IOUtils/slurpFileNoExceptions arg) arg)
       ]
     (Annotation. txt)))
([ arg ]
    (annotation-for arg false)))

(defn annotated-pipeline
"Yields an annotated pipeline from annotation; propsPath is a classpath relative, 
propeties file name without the file extension.
"
([ annotation propsPath]
  (let [ pl (if propsPath (StanfordCoreNLP. propsPath) 
                          (StanfordCoreNLP.))
        ]
    (. pl annotate annotation)
    pl)) 
([ annotation ] (annotated-pipeline annotation nil)))   
