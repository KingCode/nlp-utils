(ns nlp-utils.stanford-corenlp-test-const)

(comment "

Naming Conventions: 
        DATA_FIL[-n] for text files
        TXT[-n] for sentences
        PART[-n] for fragments

")

(def DATA_FIL "test/data/financial-lnkd.txt")

(def PART "an increase of 81%")
(def PART-1 "compared to $167.7 million in the fourth quarter of 2011.")
(def PART-2 " and grew cumulative membership 39% year over year.") 

(def TXT "GAAP diluted EPS for the fourth quarter was $0.10; Non-GAAP diluted EPS for the fourth quarter was $0.35.")

(def TXT-2 "MOUNTAIN VIEW, Calif., Feb. 7, 2013 (GLOBE NEWSWIRE) -- LinkedIn Corporation (NYSE:LNKD), the world's largest professional network on the Internet, with more than 200 million members, reported its financial results for the fourth quarter and full year ended December 31, 2012:

    Revenue for the fourth quarter was $303.6 million, an increase of 81% compared to $167.7 million in the fourth quarter of 2011.
")

(def TXT-3 "Net income for the fourth quarter was $11.5 million, compared to net income of $6.9 million for the fourth quarter of 2011.")

(def TXT-4 " Non-GAAP net income for the fourth quarter was $40.2 million, compared to $13.3 million for the fourth quarter of 2011.")

(def TXT-5 "Fourth Quarter Highlights and Strategic Announcements

In the fourth quarter of 2012, LinkedIn:

    Passed the 200 million member milestone, ending the year with approximately 202 million members, and grew cumulative membership 39% year over year.
")


(def TXT-6 "We continue to add approximately two members per second, and over 64% of LinkedIn members now come from international markets.")

