(ns nlp-utils.util
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

