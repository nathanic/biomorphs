(defproject biomorphs "0.1.0-SNAPSHOT"
  :description "Biomorphs in ClojureScript"
  :url "http://github.com/nathanic/biomorphs/"
  :license {:name "BSD"
            :url "http://www.opensource.org/licenses/BSD-3-Clause"
            :distribution :repo }

  :dependencies [[org.clojure/clojure "1.5.1"]
                 ; TODO: austin
                 [com.cemerick/piggieback "0.0.4"]
                 [compojure "1.1.5"]
                 [ring "1.1.8"]
                 [rm-hull/monet "0.1.7"]
                 [jayq "2.4.0"]
                 [org.webjars/jquery "1.9.1"]
                 [core.async "0.1.0-SNAPSHOT"]
                 ;; [org.clojure/core.async "0.1.0-SNAPSHOT"]
                 ]

  ; using my own build from github with `lein install`
  ;; :repositories {"sonatype-staging"
  ;;                "https://oss.sonatype.org/content/groups/staging/"}

  :min-lein-version "2.1.2"
  :plugins [[lein-cljsbuild "0.3.2"]
            [lein-ring "0.8.3"]]
  :hooks [leiningen.cljsbuild]
  :repl-options {:timeout 240000
                 :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 ; why don't these work?
                 ;; :init-ns biomorphs.server
                 ;; :init (biomorphs.server/browser-repl)
                 }

  :source-paths ["src/clj"]
  :resource-paths ["resources" "target/resources"]

  :cljsbuild
  {:builds
   {:dev
    {:source-paths ["src/cljs" "src/cljs-repl"]
     :compiler {:output-to "target/resources/public/js/biomorphs_dev.js"}}
    }}

  :ring {:handler biomorphs.server/app}

  ; keep prod build in another lein profile so we don't need to wait on it all the time
  :profiles {:prod {:cljsbuild
                    {:builds
                     {:prod
                      {:source-paths ["src/cljs"]
                       :compiler {:output-to "target/resources/public/js/biomorphs.js"
                                  :optimizations :advanced
                                  :externs ["externs/jquery-1.9.js"]
                                  :pretty-print false}}
                      }}}}

  :main biomorphs.server)
