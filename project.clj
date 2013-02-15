(defproject nlp-utils "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.apache.opennlp/opennlp-tools "1.5.2-incubating"]
                 [commons-io/commons-io "2.4"]
                ]
  :resource-paths [ "resources/opennlp/models" ]
  :repositories ["snapshots" "https://repository.apache.org/content/repositories/snapshots"]
  :jvm-opts [ "-Xmx1024m"]
)
