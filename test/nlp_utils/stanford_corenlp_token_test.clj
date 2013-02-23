(ns nlp-utils.stanford-corenlp-token-test
    (:use nlp-utils.stanford-corenlp-token
          [nlp-utils.stanford-corenlp-pool :only [NER-PL]]
          [nlp-utils.stanford-corenlp-test :only [get-document-sentences hdr ftr]]))
