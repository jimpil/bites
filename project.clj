(defproject bites "0.2.2-SNAPSHOT"
  :description "Utilities for converting stuff to/from bytes + NIO extensions to `clojure.java.io`."
  :url "https://github.com/jimpil/bites"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]]
  :profiles
  {:dev {:dependencies [;; for testing
                        [org.clojure/test.check "1.0.0"]
                        [commons-codec/commons-codec "1.14"]]}}
  :repl-options {:init-ns bites.core}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ;["vcs" "push"]
                  ]
  :deploy-repositories [["releases" :clojars]] ;; lein release :patch
  :signing {:gpg-key "jimpil1985@gmail.com"})
