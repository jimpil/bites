(defproject bites "0.3.5-SNAPSHOT"
  :description "Utilities for converting stuff to/from bytes + NIO extensions to `clojure.java.io` + CBOR support."
  :url "https://github.com/jimpil/bites"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1" :scope "provided"]
                 ;; for CBOR support
                 [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor "2.14.0"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]

  :profiles
  {:dev {:dependencies [;; for testing
                        [org.clojure/test.check "1.1.1"]
                        [commons-codec/commons-codec "1.15"]
                        [criterium "0.4.6"]]}}
  :repl-options {:init-ns bites.core}
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed}

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
