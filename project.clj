(defproject overtone-maschine "0.1.0-SNAPSHOT"
  :main overtone-maschine.core
  :description "Overtone maschine bindings."
  :url "http://github.com/johanatan/overtone-maschine"
  :jvm-opts ["-d64" "-Xms4G" "-Xmx12G"]
  :dependencies [[org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/clojure "1.8.0"]
                 [overtone "0.10.1"]])
