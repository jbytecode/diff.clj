(defproject com.github.jbytecode/diffclj "0.1.3"
  :description "Derivative of single variable functions and expression simplifier"
  :url "https://github.com/jbytecode/diff.clj"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot diffclj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
