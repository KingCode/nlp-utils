(ns nlp-utils.stanford-corenlp
  (:use nlp-utils.util)
  (:import 
    (java.io PrintWriter) 
    (java.util List)
    (edu.stanford.nlp.io IOUtils)
    (edu.stanford.nlp.ling CoreAnnotations 
                           CoreAnnotations$TokensAnnotation
                           CoreAnnotations$TextAnnotation 
                           CoreAnnotations$SentencesAnnotation)
    (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
    (edu.stanford.nlp.trees Tree TreeCoreAnnotations TreeCoreAnnotations$TreeAnnotation)
    (edu.stanford.nlp.util CoreMap)))

(def EMPTY_STR_ARRAY (make-array String 0))
(def CONFIG_ANN "annotators")

(def TOK_MAP { :txt "Text", :pos "PartOfSpeech", :ner "NamedEntityTag", :start "CharacterOffsetBegin"
               :end "CharacterOffsetEnd", :nner "NormalizedNamedEntityTag", :lemma "Lemma"
               :timex "Times", :tcase "TrueCase", :tcasetxt "TrueCaseText"}) 

(def TOKEN_IDS  (.toArray 
                    '("Text", "PartOfSpeech", "Lemma", "Answer", "NamedEntityTag", "CharacterOffsetBegin", 
                     "CharacterOffsetEnd", "NormalizedNamedEntityTag", "Timex", "TrueCase", "TrueCaseText")
                        EMPTY_STR_ARRAY))

(defn select-tokens
"Constructs a String array of normalized token IDs for selected tokens we are interested in.
 arguments are looked up in TOK_MAP, so should be keys from it.
"
[ tkey & more ] 
  (let [ keys (conj more tkey) 
        toks (map #(get TOK_MAP % "Text") keys) ]
    (.toArray toks EMPTY_STR_ARRAY)))

(def TOKEN_MIN (select-tokens :text :pos :ner :nner))

(defn annotation-for
"Yields a new Annotation from the text argument, or filepath if file? is true"
([ arg ]
  (let [ txt (if (file? arg) (IOUtils/slurpFileNoExceptions arg) arg)
       ]
     (Annotation. txt))))

(defn new-pipeline
"Constructs a new StanfordCoreNLP pipeline with props if provided, or the default properties otherwise.
If provided props must be a Properties, or a classpath relative propeties file path without the file extension.
"
([ props ]
  (if props (StanfordCoreNLP. props) (StanfordCoreNLP.))) 
([]
  (new-pipeline nil)))

(defn annotated-pipeline
"Constructs a pipeline using fn new-pipeline with props and invokes pipeline.annotate( annotation).
Annotation is assumed to have been initialized. See new-pipeline for constraints on props.
"
([ annotation props ]
  (let [ pl (new-pipeline props) ] 
    (. pl annotate annotation)
    pl)) 
([ annotation ] (annotated-pipeline annotation nil)))   

(defn text-of
"Yields the text for the argument core map (edu.stanford.nlp.util.CoreMap)"
[ coremap ] 
  (do ;;(println "text-of: arg is" coremap)
  (.get coremap CoreAnnotations$TextAnnotation)))  


(defn annotated-for
"Yields a seq of core maps (edu.stanford.nlp.util.CoreMap) resuting from content annotated by pipeline.
The pipeline is assumed to be configured to annotate for a vtype-class annotation.
"
[ content pipeline atype-class]
  (let [ ann (annotation-for content)
         _ (.annotate pipeline ann) ]
    (.get ann atype-class)))


(defn annotated-for-sentence
"Yields a seq of sentence core maps (edu.stanford.nlp.util.CoreMap) from text."
[ txt pipeline ]
  (let [  ann-cl CoreAnnotations$SentencesAnnotation ]
         (annotated-for txt pipeline ann-cl))) 

(defn tokens-ann
"Yields a seq of token annotations from the argument sentence map"
[sentence]
 (let [ toktype CoreAnnotations$TokensAnnotation ]
    (.get sentence toktype)))

(defn tokens-for
"Yields a lazy seq of attributes for each token of the argument sentence/fragment map (edu.stanford.nlp.util.CoreMap).
 If none are specified, all available attributes are collected (as per TOKEN_IDS).
 If provided,  attrs must be a String[] with values found in TOK_MAP. 
"
([ sentence attrs-arg ]
  (let [ attrs (if (nil? attrs-arg) TOKEN_IDS attrs-arg)
         toktype CoreAnnotations$TokensAnnotation
         tokens (.get sentence toktype) ]
    (do (println "TOKENS-FOR: tokens are: " tokens)
    (map #(.toShorterString % attrs) tokens))))
([ sentence ]
    (tokens-for sentence nil)))

(defn tokens-for-sentxt
"Convenience for testing. Same as tokens-for, but argument is a single sentence text.
If provided pipeline must be configured to show required attrs.
"
([ txt attrs pipeline ]
  (let [ sent-ann (first (annotated-for-sentence txt pipeline)) ]
    (tokens-for sent-ann attrs)))
([ txt attrs ]
  (let [ ppln (new-pipeline (props-for { CONFIG_ANN "tokenize, ssplit, pos"})) ]
    (tokens-for-sentxt txt attrs ppln)))
([ txt ]
  (tokens-for-sentxt txt nil)))



(defn grammar-of
"Yields the grammatical structure for the argument sentence map (edu.stanford.nlp.util.CoreMap)."
[ sentence ]
  (let [ gr-ann-cl TreeCoreAnnotations$TreeAnnotation 
         gram (.get sentence gr-ann-cl) ]
        (do #_(println "TREE OBJ: " gram)
        (.pennString gram))))


(defn sentences
"Yields a seq of sentences resulting from annotating 'content' with pipeline. 
'content' must be either text or a classpath relative text file.
'pipeline' must be an  edu.stanford.pipeline.AnnotationPipeline initualized with annotators 
for at least 'tokenize, ssplit'. If not provided, a new pipeline is constructed.
"
([ content pipeline grammar?]
    (let [ 
           sent-ann-cl CoreAnnotations$SentencesAnnotation
           sents (annotated-for content pipeline sent-ann-cl) ]
      (if grammar? (map #(vector (text-of %) (grammar-of %)) sents)
                   (map text-of sents))))
([ content grammar?]
    (let [ 
            annotators (if grammar? "tokenize, ssplit, pos, lemma, ner, parse" "tokenize, ssplit")
            props (props-for {CONFIG_ANN annotators})
            pl (new-pipeline props) ] 
        (sentences content pl grammar?)))
([ content ]
    (sentences content false)))
    



