(ns nlp-utils.util
(:import (java.io File FileInputStream IOException)
        (opennlp.tools.parser Parser ParserFactory ParserModel)
        (opennlp.tools.sentdetect SentenceDetector SentenceDetectorME SentenceModel)
        (opennlp.tools.chunker ChunkerME ChunkerModel)
))

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
    (let [ sd (sentenceDetector) ]
        (lazy-seq (. sd sentDetect text)))) 

(defn chunking-parser-model []
    (let [ms (get-modelStream chunkingParser-modelFilename)]
        (ParserModel. ms)))

(defn chunker-model []
    (let [ ms (get-modelStream chunker-modelFilename) ]
        (ChunkerModel. ms)))

(defn chunker-ME[] (ChunkerME. (chunker-model)))

(defn parser [ model beamSize advancePercentage ]
    (ParserFactory/create model beamSize advancePercentage))


