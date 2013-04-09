(ns nlp-utils.stanford-corenlp-test-const)

(comment "

Naming Conventions: 
        DATA_FIL[-n] for text files
        TXT[-n] for sentences
        PART[-n] for fragments

")



(def DATA_FIL "test/data/financial-lnkd.txt")
(def DATA_FIL2 "test/data/financial-goog-2012-q4.txt")
(def DATA_FIL3 "test/data/financial-gs-2012-q4.txt")
(def DATA_FIL4 "test/data/financial-td-2012-q4.txt")
(def DATA_FIL5 "test/data/financial-xom-2012-q4.txt")
(def DATA_FIL6 "test/data/so.txt")
(def DATA_FIL7 "test/data/gap.txt")
(def DATA_FIL8 "test/data/hbhc.txt")
(def DATA_FIL9 "test/data/fstr.txt")
(def DATA_FIL10 "test/data/geo.txt")
(def DATA_FIL11 "test/data/apog.txt")


(def GAP-FILE DATA_FIL7) 
(def HBHC-FILE DATA_FIL8)
(def FSTR-FILE DATA_FIL9)
(def GEO-FILE DATA_FIL10)
(def APOG-FILE DATA_FIL11)
(def EE-FILE "test/data/ee.txt")
(def CSP-FILE "test/data/csp.txt")
(def HSC-FILE "test/data/hsc.txt")
(def KSS-FILE "test/data/kss.txt")
(def LEN-FILE "test/data/len.txt")
(def LION-FILE "test/data/lion.txt") 
(def MWV-FILE "test/data/mwv.txt")
(def PLL-FILE "test/data/pll.txt")
(def QUALCOMM-FILE "test/data/qualcomm.txt")
(def ASTEC-FILE "test/data/astec.txt")
(def SEASPAN-FILE "test/data/seaspan.txt")
(def SLB-FILE "test/data/slb.txt")
(def SMG-FILE "test/data/smg.txt")
(def SO-FILE "test/data/so.txt")
(def TXN-FILE "test/data/txn.txt")
(def VALU-FILE "test/data/valu.txt")
(def VNO-FILE "test/data/vno.txt")
(def WMB-FILE "test/data/wmb.txt")





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

