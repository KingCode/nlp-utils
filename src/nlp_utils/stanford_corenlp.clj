(ns nlp-utils.stanford-corenlp
  (:use nlp-utils.util)
  (:import 
    (java.io PrintWriter) 
    (java.util List)
    (edu.stanford.nlp.io IOUtils)
    (edu.stanford.nlp.ling CoreAnnotations 
                           CoreAnnotations$TextAnnotation 
                           CoreAnnotations$SentencesAnnotation)
    (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
    (edu.stanford.nlp.trees Tree TreeCoreAnnotations TreeCoreAnnotations$TreeAnnotation)
    (edu.stanford.nlp.util CoreMap)))

(def CONFIG_ANN "annotators")

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
            annotators (if grammar? "tokenize, ssplit, pos, lemma, ner, parse, dcoref" "tokenize, ssplit")
            props (props-for {CONFIG_ANN annotators})
            pl (new-pipeline props) ] 
        (sentences content pl grammar?)))
([ content ]
    (sentences content false)))
    



