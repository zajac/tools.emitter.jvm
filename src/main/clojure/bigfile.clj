(ns clojure.tools.emitter.temp
  (:refer-clojure :exclude
                  [eval macroexpand-1 macroexpand load])
  (:require [clojure.tools.analyzer.jvm :as a]
            [clojure.tools.emitter.jvm :as j]
            [clojure.tools.analyzer :refer [macroexpand-1 macroexpand]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.utils :refer [mmerge]]
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.tools.emitter.jvm.transform :as t]
            [clojure.tools.analyzer.passes
             [collect-closed-overs :refer [collect-closed-overs]]
             [trim :refer [trim]]]
            [clojure.tools.emitter.passes.jvm
             [collect :refer [collect]]
             [collect-internal-methods :refer :all]
             [clear-locals :refer [clear-locals]]
             [annotate-class-id :refer [annotate-class-id]]
             [annotate-internal-name :refer [annotate-internal-name]]
             [ensure-tag :refer [ensure-tag]]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers])
  (:import (clojure.lang IFn DynamicClassLoader Atom)))

(def bootstrap-invoke
  '(fn
    [^java.lang.invoke.MethodHandles$Lookup caller ^String methodName ^java.lang.invoke.MethodType methodType ^String varName]
    (if (identical? varName "x")
      (do
        (when (nil? huj.SampleNS/var_x_var)
          (set! huj.SampleNS/var_x_var
            (clojure.lang.Var. huj.SampleNS/ns (clojure.lang.Symbol/intern "x"))))
        (when (nil? huj.SampleNS/var_x_invoke_fallback_mh)
          (set! huj.SampleNS/var_x_invoke_fallback_mh
            (.findVirtual caller clojure.lang.Var "invoke" methodType)))
        (cond
          huj.SampleNS/var_x_invalid
          (java.lang.invoke.ConstantCallSite. huj.SampleNS/var_x_invoke_fallback_mh)

          (identical? huj.SampleNS/var_x_value huj.SampleNS/UNBOUND)
          (java.lang.invoke.ConstantCallSite. huj.SampleNS/var_x_invoke_fallback_mh)

          (some? huj.SampleNS/var_x_invoke_mh)
          (java.lang.invoke.ConstantCallSite. huj.SampleNS/var_x_invoke_mh)

          :else
          (do
            (when (nil? huj.SampleNS/var_x_switchPoint)
              (set! huj.SampleNS/var_x_switchPoint
                (java.lang.invoke.SwitchPoint.)))
            (if (identical? huj.SampleNS/var_x_value huj.SampleNS/UNINITIALIZED_FN)
              (java.lang.invoke.ConstantCallSite.
                (.guardWithTest huj.SampleNS/var_x_switchPoint
                                (.findStatic caller huj.MyLambda "invokeStatic" methodType)
                                huj.SampleNS/var_x_invoke_fallback_mh))

              (java.lang.invoke.ConstantCallSite.
                (.guardWithTest huj.SampleNS/var_x_switchPoint
                                (.bindTo
                                  (.findVirtual caller clojure.lang.IFn "invoke" methodType) huj.SampleNS/var_x_value)
                                huj.SampleNS/var_x_invoke_fallback_mh))))))
      (throw (java.lang.IllegalArgumentException.)))))

(def asst
  (binding [a/run-passes j/run-passes]
    (let [cs (-> (a/analyze bootstrap-invoke)
                 (e/emit-classes {:debug? true}))]
      cs)))

(clojure.pprint/pprint asst)

*e