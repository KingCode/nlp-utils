(ns nlp-utils.opennlp-core
(:import (java.io File FileInputStream IOException)
        (opennlp.tools.cmdline.parser ParserTool)
        (opennlp.tools.parser Parser ParserFactory ParserModel)
        (opennlp.tools.sentdetect SentenceDetector SentenceDetectorME SentenceModel)
        (opennlp.tools.chunker ChunkerME ChunkerModel)
        (org.apache.commons.io FileUtils)
))

(defn str-from-file
"Loads a file content into the returned string. The file name must be relative to the classpath"
[ filename ]
    (FileUtils/readFileToString (File. filename)))

(def modelDirname "resources/opennlp/models")

(def chunkingParser-modelFilename "en-parser-chunking.bin")
(def chunker-modelFilename "en-chunker.bin")
(def sentence-modelFilename "en-sent.bin")

(defn get-modelStream [modelFile-name]
    (let [ modelFile (File. modelDirname modelFile-name)
           modelStream (FileInputStream. modelFile) ]
     modelStream ))

(defn sentence-model []
    (let [ ms (get-modelStream sentence-modelFilename) ]
        (SentenceModel. ms)))

(defn sentence-detector [] (SentenceDetectorME. (sentence-model)))


(defn sentences-of
"Yields a lazy seq of sentences collected from text" 
 [ text ] 
  (lazy-seq
    (let [ sd (sentence-detector) ]
        (. sd sentDetect text)))) 

(defn chunking-parser-model []
    (let [ms (get-modelStream chunkingParser-modelFilename)]
        (ParserModel. ms)))

(defn chunker-model []
    (let [ ms (get-modelStream chunker-modelFilename) ]
        (ChunkerModel. ms)))

(defn chunker-ME[] (ChunkerME. (chunker-model)))

(defn parser 
"Yields an initialized parser. Defaults are beamSize 20 and advancePercentage 0.95
"
([ model beamSize advancePercentage ]
    (ParserFactory/create model beamSize advancePercentage))
( [ model ]
    (parser model 20 0.95)))

(defn parse-line 
"Yields a list hierarchy of tokens preceded by a POS tag name at the
start of each list.
"
([ parser line maxres ]
    (ParserTool/parseLine line parser maxres))
([ parser line ]
    (parse-line parser line 1))) 

(defn not-nil
"Yields the argument if not nil. Otherwise default is returned.
If no default is provided the empty string is returned"
([ x default ]
  (if (nil? x) default x ))
([ x ]
  (not-nil x "")))

(defn show-parse
"Argument must be a opennlp.tools.parser.Parse or collection thereof, namely a Parse[] parse result.
Yields a string from the result of invoking Parse.show(). If a collection, a seq of such results
is returned, each one positioned at the same index as its counterpart in the argument.

If show-src? is true the source text corresponding to the parse format is shown first on a separate line,
followed by label if not nil.
"
([ pres show-src? label]
  (if (not (coll? pres))
    (let [ sb (StringBuffer.)
           _ (. pres show sb)
           prefix (if show-src? (str (. pres getText) "\n" (not-nil label) "\n") (str (not-nil label) "\n"))
         ]
        (str prefix (. sb toString) "\n"))

    (if (empty? pres) []
      (let [ c (count pres)
             dc (dec c)
             lbl (str label "\n" c)
             head (show-parse (first pres) show-src? lbl) 
           ] 
         (cons head (show-parse (rest pres) false nil))))))  
([ pres ]
    (show-parse pres false nil)))
