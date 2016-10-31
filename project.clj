(defproject obi-assays "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.obolibrary/robot "0.0.1-SNAPSHOT"]]
  :repositories [["local_maven_repo"
                  {:url "file:local_maven_repo"
                   :username ""
                   :password ""}]]
  :main obi-assays.core
  :aot [obi-assays.core])
