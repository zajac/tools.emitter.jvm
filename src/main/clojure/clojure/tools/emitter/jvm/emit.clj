;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm.emit
  (:refer-clojure :exclude [cast])
  (:require [clojure.tools.analyzer.utils :as u]
            [clojure.tools.analyzer.jvm.utils :refer [primitive? numeric? box prim-or-obj] :as j.u]
            [clojure.string :as s]
            [clojure.tools.emitter.jvm.intrinsics :refer [intrinsic intrinsic-predicate]])
  (:import clojure.lang.Reflector))

(defmulti -emit (fn [{:keys [op]} _] op))
(defmulti -emit-set! (fn [{:keys [target]} _] (:op target)))

(def nil-expr
  {:op :const :type :nil :form nil :val nil})

(defn emit-box [tag box unchecked?]
  (if (and (primitive? tag)
           (not (primitive? box)))
    (cond
     (numeric? tag)
     [[:invoke-static [:clojure.lang.RT/box tag] :java.lang.Number]
      [:check-cast box]]
     (= Character/TYPE tag)
     [[:invoke-static [:clojure.lang.RT/box :char] :java.lang.Character]]
     (= Boolean/TYPE tag)
     [[:invoke-static [:clojure.lang.RT/box :boolean] :java.lang.Object]
      [:check-cast :java.lang.Boolean]])
    (when (primitive? box)
      (let [method (if (and (numeric? box) (or unchecked? *unchecked-math*))
                     (str "unchecked" (s/capitalize (.getName ^Class box)) "Cast")
                     (str (.getName ^Class box) "Cast"))
            tag (prim-or-obj tag)
            method-sig (str (.getMethod clojure.lang.RT method (into-array Class [tag])))]
        (if-let [ops (intrinsic method-sig)]
          (mapv (fn [op] [:insn op]) ops)
          [[:invoke-static [(keyword "clojure.lang.RT" method) tag] box]])))))

(defn emit-cast
  ([tag cast] (emit-cast tag cast false))
  ([tag cast unchecked?]
     (if (not (or (primitive? tag)
                (primitive? cast)))
       (when-not (#{Void Void/TYPE} cast)
         [[:check-cast cast]])
       (emit-box tag cast unchecked?))))

(defn emit-pop [tag]
  (if (#{Double/TYPE Long/TYPE} tag)
    [:pop2]
    [:pop]))

(def ^:dynamic *classes*)

(defn emit
  "(λ AST) → Bytecode
  (λ AST → Options) → Bytecode

  AST is an analyzed, macroexpanded t.a.jvm AST. Options is a map, the
  following values of which are significant. Returns a (potentially
  empty) sequence of bytecodes. *classes* must be bound before calling
  emit.

  Options
  -----------
  :debug? :- (Option bool)
    Controls development debug level printing throughout code generation."

  ([ast]
     (emit ast {}))

  ([{:keys [env o-tag tag op type unchecked?] :as ast} frame]
     (let [bytecode (-emit ast frame)
           statement? (isa? (:context env) :ctx/statement)
           m (meta bytecode)]
       (if statement?
         (if (:const m)
           []
           (into bytecode
                 (when (and (not (:untyped m))
                            (not (:container m))
                            (not= Void/TYPE tag))
                   [(emit-pop tag)])))
         (into bytecode
               `[~@(when (and (not (:container m))
                              (or (:untyped m)
                                  (= Void/TYPE tag)))
                     [[:insn :ACONST_NULL]])
                 ~@(when (and (not= tag o-tag)
                              (not= :const op))
                     (emit-cast o-tag tag unchecked?))])))))

(defn emit-classes
  "(λ AST) → (Seq Class-AST)
  (λ AST → Options) → (Seq Class-AST)

  Compiles the given AST into potentially several classes, returning a
  sequence of ASTs defining classes.

  Options
  -----------
  :debug :- (Option bool)
    Controls developlent debug level printing throughout code generation."

  ([ast]
     (emit-classes ast {}))

  ([ast opts]
     (binding [*classes* (atom {:classes []
                                :ids     #{}})]
       (emit ast opts)
       (:classes @*classes*))))

(defmethod -emit :import
  [{:keys [class]} frame]
  [[:get-static :clojure.lang.RT/CURRENT_NS :clojure.lang.Var]
   [:invoke-virtual [:clojure.lang.Var/deref] :java.lang.Object]
   [:check-cast :clojure.lang.Namespace]
   [:push class]
   [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
   [:invoke-virtual [:clojure.lang.Namespace/importClass :java.lang.Class] :java.lang.Class]])

(defmethod -emit :throw
  [{:keys [exception]} frame]
  `^:untyped
  [~@(emit (assoc exception :tag java.lang.Throwable) frame)
   [:throw-exception]])

(defmethod -emit :monitor-enter
  [{:keys [target]} frame]
  `^:untyped
  [~@(emit target frame)
   [:monitor-enter]])

(defmethod -emit :monitor-exit
  [{:keys [target]} frame]
  `^:untyped
  [~@(emit target frame)
   [:monitor-exit]])

(defn cast [to el]
  (if (numeric? to)
    (condp = (box to)
      Integer
      (.intValue ^Number el)

      Long
      (.longValue ^Number el)

      Double
      (.doubleValue ^Number el)

      Float
      (.floatValue ^Number el)

      Short
      (.shortValue ^Number el)

      Byte
      (.byteValue ^Number el))
    (clojure.core/cast to el)))

(defmethod -emit :const
  [{:keys [val id tag] :as ast} frame]
  ^:const
  [(case val
     (true false)
     (if (primitive? tag)
       [:push val]
       [:get-static (if val :java.lang.Boolean/TRUE :java.lang.Boolean/FALSE)
        :java.lang.Boolean])

     nil
     [:insn :ACONST_NULL]

     (if (or (primitive? tag)
             (string? val))
       [:push (cast (or (box tag) (class val)) val)]
       [:get-static (frame :class) (str "const__" id) tag]))])

(defmethod -emit :quote
  [{:keys [expr]} frame]
  (-emit expr frame))

(def ^:dynamic *vars-info*)

(defn var-sym [var]
  (let [{:keys [name ns]} (meta var)]
    (symbol (clojure.core/name (ns-name ns)) (clojure.core/name name))))

(defn loader-class [namespace-sym]
  (str (namespace-munge namespace-sym) "__init"))

(defn bootstrap-invoke-class [namespace-sym]
  (str (namespace-munge namespace-sym) "__bootstrap__invoke"))

(defn var-loader-class [var]
  (loader-class (namespace (var-sym var))))

(defn emit-var
  [{:keys [var id]} frame]
  ;; TODO: good place for laziness?
  ;; opt1: RT.var
  ;; opt2: NSLoader.var_x
  ;; opt3: invoke-dynamic: NSLoader.getVar("x")
  (let [var-sym (var-sym var)]
    [[:push (name (namespace var-sym))]
     [:push (name var-sym)]
     [:invoke-static [:clojure.lang.RT/var String String] clojure.lang.Var]]))

(defmethod -emit :var
  [{:keys [var] :as ast} frame]
  (let [var-sym (var-sym var)]
    [[:invoke-dynamic
      [(keyword (loader-class (namespace var-sym)) "bootstrapDeref") String]
      [["deref"] Object]
      [(name var-sym)]]]))

(defmethod -emit-set! :var
  [{:keys [target val] :as ast} frame]
  `[~@(emit-var target frame)
    ~@(emit val frame)
    [:invoke-virtual [:clojure.lang.Var/set :java.lang.Object] :java.lang.Object]])

(defmethod -emit :the-var
  [ast frame]
  (emit-var ast frame))

(declare emit-class)

(defn emit-fn-class [{:keys [variadic?] :as fn-ast} frame class-name]
  (let [super (if variadic? :clojure.lang.RestFn :clojure.lang.AFunction)
        ast (assoc fn-ast
                   :class-name class-name
                   :super super)]
    (emit-class ast frame)))

(defmethod -emit :def
  [{:keys [var meta init env] :as ast} frame]
  (let [var-info (get *vars-info* (var-sym var))
        var-kind (:var/kind var-info)]
    (cond
      (= :var.kind/static-fn var-kind)
      (let [expr (case (:op init)
                   :with-meta (:expr init)
                   :fn init
                   (throw (ex-info "can't handle init" init)))]
        (emit-fn-class expr frame (:var.static-fn/class-name var-info))
        [[:insn :ACONST_NULL]])

      (= :var.kind/constant var-kind)
      (let [loader-class (loader-class (namespace (var-sym var)))]
        (conj (emit init frame)
              [:put-static loader-class (:vat.constant/field-name var-info) Object]))

      :else
      `[~@(emit-var ast frame)
        ~@(when (u/dynamic? var)
           [[:push true]
            [:invoke-virtual [:clojure.lang.Var/setDynamic :boolean] :clojure.lang.Var]])
        ~@(when meta
           `[[:dup]
             ~@(emit meta frame)
             [:invoke-virtual [:clojure.lang.Var/setMeta :clojure.lang.IPersistentMap] :void]])
        ~@(when init
           `[[:dup]
             ~@(emit init frame)
             [:invoke-virtual [:clojure.lang.Var/bindRoot :java.lang.Object] :void]])
        ~@(when (u/macro? var)
           [[:dup]
            [:invoke-virtual [:clojure.lang.Var/setMacro] :void]])])))

(defmethod -emit :set!
  [ast frame]
  (-emit-set! ast frame))

(defn emit-as-array [list frame]
  `[[:push ~(int (count list))]
    [:new-array :java.lang.Object]
    ~@(mapcat (fn [i item]
                `[[:dup]
                  [:push ~(int i)]
                  ~@(emit item frame)
                  [:array-store :java.lang.Object]])
              (range) list)])

(defmethod -emit :map
  [{:keys [keys vals form]} frame]
  `[~@(emit-as-array (interleave keys vals) frame)
    ~@(if (sorted? form)
        [[:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
         [:invoke-static [:clojure.lang.PersistentTreeMap/create :clojure.lang.ISeq] :clojure.lang.PersistentTreeMap]]
        [[:invoke-static [:clojure.lang.RT/map :objects] :clojure.lang.IPersistentMap]])])

(defmethod -emit :vector
  [{:keys [items]} frame]
  (conj
   (emit-as-array items frame)
   [:invoke-static [:clojure.lang.RT/vector :objects] :clojure.lang.IPersistentVector]))

(defmethod -emit :set
  [{:keys [items form]} frame]
  `[~@(emit-as-array items frame)
    ~@(if (sorted? form)
        [[:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
         [:invoke-static [:clojure.lang.PersistentTreeSet/create :clojure.lang.ISeq] :clojure.lang.PersistentTreeSet]]
        [[:invoke-static [:clojure.lang.RT/set :objects] :clojure.lang.IPersistentSet]])])

(defmethod -emit :with-meta
  [{:keys [meta expr]} frame]
  (into
   (emit expr frame)
   `[~@(emit meta frame)
     [:invoke-interface [:clojure.lang.IObj/withMeta :clojure.lang.IPersistentMap]
      :clojure.lang.IObj]]))

(defmethod -emit :do
  [{:keys [statements ret]} frame]
  (with-meta
    (vec (mapcat #(emit % frame) (conj statements ret)))
    {:container true}))

(defn label []
  (keyword (gensym "label__")))

(defn local []
  (keyword (gensym "local__")))

(defn emit-try
  [{:keys [body catches finally env tag]} frame]
  (let [[start-label end-label ret-label finally-label] (repeatedly label)
        catches (mapv #(assoc %
                         :start-label (label)
                         :end-label (label)) catches)
        context (:context env)
        [ret-local finally-local] (repeatedly local)
        ret-local-tag (let [t (.getName ^Class tag)]
                        (if (= "void" t) "Object" t))]

    `^:container
    [[:mark ~start-label]
     ~@(emit body frame)
     ~@(if (isa? context :ctx/statement)
         [[:insn :ACONST_NULL]
          [:pop]]
         [[:var-insn (keyword ret-local-tag "ISTORE") ret-local]])
     [:mark ~end-label]
     ~@(when finally
         (emit finally frame))
     [:go-to ~ret-label]

     ~@(mapcat
        (fn [{:keys [body start-label end-label local]}]
          `[[:mark ~start-label]
            [:astore ~(:name local)]
            ~@(emit body frame)
            ~@(if (isa? context :ctx/statement)
                (when (not (#{Void Void/TYPE} (:tag body)))
                  [(emit-pop (:tag body))])
               [[:var-insn (keyword ret-local-tag "ISTORE") ret-local]])
            [:mark ~end-label]
            ~@(when finally
                (emit finally frame))
            [:go-to ~ret-label]])
        catches)
     ~@(when finally
         `[[:mark ~finally-label]
           [:astore ~finally-local]
           ~@(emit finally frame)
           [:aload ~finally-local]
           [:throw-exception]])

     [:mark ~ret-label]
     ~@(when-not (isa? context :ctx/statement)
         [[:var-insn (keyword ret-local-tag "ILOAD") ret-local]])
     [:mark ~(label)]

     ~@(for [c catches :let [^Class class (-> c :class :val)]]
         [:try-catch-block start-label end-label (:start-label c) class])

     ~@(when finally
         `[~[:try-catch-block start-label end-label finally-label nil]
           ~@(for [{:keys [start-label end-label] :as c} catches]
               [:try-catch-block start-label end-label finally-label nil])])

     ~@(for [{:keys [local start-label end-label body] :as c} catches]
         [:local-variable (:name local) Exception nil start-label end-label (:name local)])]))

(defn emit-line-number
  [{:keys [line]} & [l]]
  (when line
    (let [l (or l (label))]
      [[:mark l]
       [:line-number line l]])))

(defmethod -emit :static-field
  [{:keys [field o-tag class env]} frame]
  `^:const
  [~@(emit-line-number env)
   ~[:get-static class field o-tag]])

(defn dup [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2]
    [:dup]))

(defn dup-x1 [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2-x1]
    [:dup-x1]))

(defn dup-x2 [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2-x2]
    [:dup-x2]))

(defmethod -emit-set! :static-field
  [{:keys [target val env]} frame]
  (let [{:keys [o-tag class field]} target]
    `[~@(emit-line-number env)
      ~@(emit (assoc val :tag o-tag) frame)
      ~(dup o-tag)
      ~[:put-static class field o-tag]]))

(defmethod -emit :instance-field
  [{:keys [instance class field env o-tag]} frame]
  `^:const
  [~@(emit-line-number env)
   ~@(emit (assoc instance :tag class) frame)
   ~[:get-field class field o-tag]])

(defmethod -emit-set! :instance-field
  [{:keys [target val env]} frame]
  (let [{:keys [instance class field o-tag]} target]
    `[~@(emit-line-number env)
      ~@(emit (assoc instance :tag class) frame)
      ~@(emit (assoc val :tag o-tag) frame)
      ~(dup-x1 o-tag)
      ~[:put-field class field o-tag]]))

(defmethod -emit :keyword-invoke
  [{:keys [env target keyword args] :as ast} frame]
  (let [id (:id keyword)
        [end-label fault-label] (repeatedly label)]
    `[~@(emit-line-number env)
      [:get-static ~(name (frame :class)) ~(str "thunk__" id) :clojure.lang.ILookupThunk]
      [:dup]
      ~@(emit target frame)
      [:dup-x2]
      [:invoke-interface [:clojure.lang.ILookupThunk/get :java.lang.Object] :java.lang.Object]
      [:dup-x2]
      [:jump-insn :IF_ACMPEQ ~fault-label]
      [:pop]
      [:go-to ~end-label]

      [:mark ~fault-label]
      [:swap]
      [:pop]
      [:dup]
      [:get-static ~(name (frame :class)) ~(str "site__" id) :clojure.lang.KeywordLookupSite]
      [:swap]
      [:invoke-interface [:clojure.lang.ILookupSite/fault :java.lang.Object] :clojure.lang.ILookupThunk]
      [:dup]
      [:put-static ~(name (frame :class)) ~(str "thunk__" id) :clojure.lang.ILookupThunk]
      [:swap]
      [:invoke-interface [:clojure.lang.ILookupThunk/get :java.lang.Object] :java.lang.Object]
      [:mark ~end-label]]))

(defmethod -emit :new
  [{:keys [env class args validated?]} frame]
  (let [cname (.getName ^Class (:val class))]
    (if validated?
      `[[:new-instance ~cname]
        [:dup]
        ~@(mapcat #(emit % frame) args)
        [:invoke-constructor [~(keyword cname "<init>")
                              ~@(mapv :tag args)] :void]]
      `[[:push ~cname]
        [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
        ~@(emit-as-array args frame)
        [:invoke-static [:clojure.lang.Reflector/invokeConstructor :java.lang.Class :objects] :java.lang.Object]])))

(defn emit-intrinsic [{:keys [args method ^Class class false-label]}]
  (let [m (str (.getMethod class (name method) (into-array Class (mapv :tag args))))]
    (if false-label
      (when-let [ops (intrinsic-predicate m)]
        (with-meta (conj (mapv (fn [op] [:insn op]) (butlast ops))
                         [:jump-insn (last ops) false-label])
          {:intrinsic-predicate true}))
      (when-let [ops (intrinsic m)]
        (mapv (fn [op] [:insn op]) ops)))))

(defmethod -emit :static-call
  [{:keys [env o-tag validated? args method ^Class class false-label to-clear?] :as ast} frame]
  (if validated?
    (let [intrinsic (emit-intrinsic ast)]
      `^{:intrinsic-predicate ~(-> intrinsic meta :intrinsic-predicate)}
      [~@(emit-line-number env)
       ~@(mapcat #(emit % frame) args)
       ~@(or intrinsic
             `[~@(when to-clear?
                   [[:insn :ACONST_NULL]
                    [:var-insn :clojure.lang.Object/ISTORE 0]])
               [:invoke-static [~(keyword (.getName class) (str method))
                                ~@(mapv :tag args)] ~o-tag]])])
    `[[:push ~(.getName class)]
      [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      ~@(when to-clear?
          [[:insn :ACONST_NULL]
           [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-static [:clojure.lang.Reflector/invokeStaticMethod
                       :java.lang.Class :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :instance-call
  [{:keys [env o-tag validated? args method ^Class class instance to-clear?]} frame]
  (if validated?
    `[~@(emit-line-number env)
      ~@(emit (assoc instance :tag class) frame)
      ~@(mapcat #(emit % frame) args)
      ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:var-insn :clojure.lang.Object/ISTORE 0]])
      [~(if (.isInterface class)
          :invoke-interface
          :invoke-virtual)
       [~(keyword (.getName class) (str method)) ~@(mapv :tag args)] ~o-tag]]
    `[~@(emit instance frame)
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      ~@(when to-clear?
          [[:insn :ACONST_NULL]
           [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-static [:clojure.lang.Reflector/invokeInstanceMethod
                       :java.lang.Object :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :host-interop
  [{:keys [m-or-f target env]} frame]
  `[~@(emit target frame)
    [:push ~(str m-or-f)]
    [:invoke-static [:clojure.lang.Reflector/invokeNoArgInstanceMember :java.lang.Object :java.lang.String] :Object]])

(defmethod -emit-set! :host-interop
  [{:keys [target val env]} frame]
  `[~@(emit-line-number env)
    ~@(emit (:target target) frame)
    [:push ~(str (:m-or-f target))]
    ~@(emit val frame)
    [:invoke-static [:clojure.lang.Reflector/setInstanceField :java.lang.Object :java.lang.String :java.lang.Object] :java.lang.Object]])

(defmethod -emit :instance?
  [{:keys [target class]} frame]
  `[~@(emit target frame)
    ~[:instance-of (.getName ^Class class)]])

(defmethod -emit :if
  [{:keys [test then else env]} frame]
  (let [[null-label false-label end-label] (repeatedly label)
        test-expr (emit (assoc test :false-label false-label) frame)]
    `^:container
    [~@(emit-line-number env)
     ~@test-expr
     ~@(when (not (:intrinsic-predicate (meta test-expr)))
         (if (not= (:tag test) Boolean/TYPE)
           [[:dup]
            [:if-null null-label]
            [:get-static :java.lang.Boolean/FALSE :java.lang.Boolean]
            [:jump-insn :IF_ACMPEQ false-label]]
           [[:if-z-cmp :EQ false-label]]))
     ~@(emit then frame)
     [:go-to ~end-label]
     [:mark ~null-label]
     [:pop]
     [:mark ~false-label]
     ~@(emit else frame)
     [:mark ~end-label]]))

(defn emit-args [args {:keys [to-clear?] :as frame}]
  (let [frame (dissoc frame :to-clear?)]
    `[~@(mapcat #(emit % frame) (take 19 args))
      ~@(when-let [args (seq (drop 19 args))]
         (emit-as-array args frame))
      ~@(when to-clear?
         [[:insn :ACONST_NULL]
          [:var-insn :clojure.lang.Object/ISTORE 0]])]))

(defn emit-args-and-invoke
  ([args frame] (emit-args-and-invoke args frame false))
  ([args frame proto?]
   `[~@(emit-args args frame)
     [:invoke-interface [:clojure.lang.IFn/invoke ~@(repeat (min 20 (count args)) :java.lang.Object) ~@(when proto? [:java.lang.Object])] :java.lang.Object]]))

(defmethod -emit :invoke
  [{:keys [fn args env to-clear?]} frame]
  (if (= (:op fn) :var)
    (let [var-sym (var-sym (:var fn))]
      (assert (< (count args) 19) "TODO fix invoke-dynamic type signature")
      `[~@(emit-args args frame)
        [:invoke-dynamic
         [~(keyword (bootstrap-invoke-class (namespace var-sym)) "bootstrapInvoke") java.lang.String]
         [~(into ["invoke"] (repeat (count args) java.lang.Object)) java.lang.Object]
         [~(name var-sym)]]])
    `[~@(emit fn frame)
      [:check-cast :clojure.lang.IFn]
      ~@(emit-args-and-invoke args (assoc frame :to-clear? to-clear?))]))

(defmethod -emit :protocol-invoke
  [{:keys [protocol-fn target args env to-clear?]} frame]
  (let [[on-label call-label end-label] (repeatedly label)
        v (:var protocol-fn)
        id (:id protocol-fn)

        ^Class pinterface (:on-interface @(:protocol (meta v)))]
    `[~@(emit target frame)
      [:dup]
      [:invoke-static [:clojure.lang.Util/classOf :java.lang.Object] :java.lang.Class]

      [:get-static ~(name (frame :class)) ~(str "cached__class__" id) :java.lang.Class]
      [:jump-insn :IF_ACMPEQ ~call-label]

      [:dup]
      [:instance-of ~pinterface]
      [:if-z-cmp :NE ~on-label]

      [:dup]
      [:invoke-static [:clojure.lang.Util/classOf :java.lang.Object] :java.lang.Class]
      [:put-static ~(frame :class) ~(str "cached__class__" id) :java.lang.Class]

      [:mark ~call-label]
      ~@(emit-var protocol-fn frame)
      [:invoke-virtual [:clojure.lang.Var/getRawRoot] :java.lang.Object]
      [:swap]
      ~@(emit-args-and-invoke args (assoc frame :to-clear? to-clear?) true)
      [:go-to ~end-label]

      [:mark ~on-label]

      ~@(mapcat #(emit % frame) args)
      ~@(when to-clear?
          [[:insn :ACONST_NULL]
           [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-interface [~(keyword (.getName pinterface)
                                    (munge (name (:form protocol-fn))))
                          ~@(repeat (count args) :java.lang.Object)] :java.lang.Object]

      [:mark ~end-label]]))

(defmethod -emit :prim-invoke
  [{:keys [fn args ^Class prim-interface o-tag to-clear?]} frame]
  (if (and (= (:op fn) :var)
           (= (:var/kind (get *vars-info* (var-sym (:var fn)))) :var.kind/static-fn))
    (let [var-sym (var-sym (:var fn))]
      `[~@(mapcat #(emit % frame) args)
        [:invoke-dynamic
         [~(keyword (bootstrap-invoke-class (namespace var-sym)) "bootstrapInvoke") java.lang.String]
         [~(into ["invoke"] (map :tag) args) ~o-tag]
         [~(name var-sym)]]])
    `[~@(emit fn frame)
      [:check-cast ~prim-interface]
      ~@(mapcat #(emit % frame) args)
      ~@(when to-clear?
         [[:insn :ACONST_NULL]
          [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-interface [~(keyword (.getName prim-interface) "invokePrim")
                          ~@(mapv :tag args)] ~o-tag]]))

(defn emit-shift-mask
  [{:keys [shift mask]}]
  (when (not (zero? mask))
    [[:push (int shift)]
     [:insn :ISHR]
     [:push (int mask)]
     [:insn :IAND]]))

(defn emit-test-ints
  [{:keys [test test-type] :as ast} frame default-label]
  (cond
   (nil? (:tag test))
   ;; reflection warning
   `[~@(emit test frame)
     [:instance-of :java.lang.Number]
     [:if-z-cmp :EQ ~default-label]
     ~@(emit (assoc test :tag Integer/TYPE :unchecked? true) frame)
     ~@(emit-shift-mask ast)]

   (numeric? (:tag test))
   `[~@(emit (assoc test :tag Integer/TYPE :unchecked? true) frame)
     ~@(emit-shift-mask ast)]

   :else
   [[:go-to default-label]]))

(defn emit-test-hashes
  [{:keys [test] :as ast} frame]
  `[~@(emit test frame)
    [:invoke-static [:clojure.lang.Util/hash :java.lang.Object] :int]
    ~@(emit-shift-mask ast)])

(defn emit-then-ints
  [tag comp test then default-label mask frame]
  (cond
   (nil? tag)
   `[~@(emit comp frame)
     ~@(emit test frame)
     [:invoke-static [:clojure.lang.Util/equiv :java.lang.Object :java.lang.Object] :boolean]
     [:if-z-cmp :EQ ~default-label]
     ~@(emit then frame)]

   (= tag Long/TYPE)
   `[~@(emit (assoc test :tag Long/TYPE :unchecked? true) frame)
     ~@(emit (assoc comp :tag Long/TYPE :unchecked? true) frame)
     [:if-cmp :long :NE ~default-label]
     ~@(emit then frame)]

   (numeric? tag)
   `[~@(when (not (zero? mask))
         `[~@(emit (assoc test :tag Long/TYPE :unchecked? true) frame)
           ~@(emit (assoc comp :tag Long/TYPE :unchecked? true) frame)
           [:if-cmp :long :NE ~default-label]])
     ~@(emit then frame)]

   :else
   [[:go-to default-label]]))

(defn emit-then-hashes
  [comp test then test-type default-label frame]
  `[~@(emit comp frame)
    ~@(emit test frame)
    ~@(if (= :hash-identity test-type)
        [[:jump-insn :IF_ACMPNE default-label]]
        [[:invoke-static [:clojure.lang.Util/equiv :java.lang.Object :java.lang.Object] :boolean]
         [:if-z-cmp :EQ default-label]])
    ~@(emit then frame)])

(defmethod -emit :case
  [{:keys [test default tests thens shift mask low high switch-type test-type skip-check? env] :as ast} frame]
  (let [testc (count tests)
        tests (into (sorted-map) (zipmap (mapv :hash tests) (mapv :test tests)))
        thens (apply sorted-map (mapcat (juxt :hash :then) thens))
        [default-label end-label] (repeatedly label)
        tests-ks (keys tests)
        tests-vs (repeatedly (count tests-ks) label)
        labels (zipmap tests-ks tests-vs)]
    `^:container
    [~@(emit-line-number env)
     ~@(if (= :int test-type)
         (emit-test-ints ast frame default-label)
         (emit-test-hashes ast frame))
     ~(if (= :sparse switch-type)
        [:lookup-switch-insn default-label tests-ks tests-vs] ; to array
        [:table-switch-insn low high default-label
         (mapv (fn [i] (if (contains? labels i) (labels i) default-label)) (range low (inc high)))])
     ~@(mapcat (fn [[i label]]
                 `[[:mark ~label]
                   ~@(cond
                      (= :int test-type)
                      (emit-then-ints (:tag test) test (tests i) (thens i) default-label mask frame)

                      (contains? skip-check? i)
                      [(emit (thens i) frame)]

                      :else
                      (emit-then-hashes test (tests i) (thens i) test-type default-label frame))
                   [:go-to ~end-label]])
               labels)
     [:mark ~default-label]
     ~@(emit default frame)
     [:mark ~end-label]]))

(defn emit-bindings [bindings labels frame]
  (mapcat (fn [{:keys [init to-clear? tag name] :as binding} label]
            `[~@(emit init frame)
              ~(if to-clear?
                 (emit-pop tag)
                 [:var-insn (keyword (.getName ^Class tag) "ISTORE") name])
              ~@(when label
                  [[:mark label]])])
          bindings labels))

(defn emit-let
  [{:keys [op bindings body env]} frame]
  (let [loop? (= :loop op)
        [end-label loop-label & labels] (repeatedly (+ 2 (count bindings)) label)]
    (with-meta
      `[~@(emit-bindings bindings labels frame)
        [:mark ~loop-label]
        ~@(emit body (merge frame (when loop? {:loop-label loop-label
                                               :loop-locals bindings})))
        [:mark ~end-label]
        ~@(mapv (fn [{:keys [name tag]} label]
                  [:local-variable name tag nil label end-label name])
                (remove :to-clear? bindings) labels)]
      (if loop? {} {:container true}))))

(defmethod -emit :let
  [ast frame]
  (emit-let ast frame))

(defmethod -emit :loop
  [{:keys [closed-overs tag internal-method-name] :as ast} {:keys [class params] :as frame}]
  (let [locals (remove #(#{:arg :field} (:local %)) (vals closed-overs))
        method-sig (into [(keyword class (str internal-method-name))]
                         (into (mapv :tag params)
                               (mapv :o-tag locals)))]
    `[[:load-this]
      ~@(mapcat (fn [l] (-emit (assoc l :op :local) frame)) (concat params locals))
      ~[:invoke-virtual method-sig tag]]))

(defmethod -emit :try
  [{:keys [closed-overs tag internal-method-name env] :as ast} {:keys [class params] :as frame}]
  (let [locals (remove #(#{:arg :field} (:local %)) (vals closed-overs))
        method-sig (into [(keyword class (str internal-method-name))]
                         (into (mapv :tag params)
                               (mapv :o-tag locals)))
        statement? (isa? (:context env) :ctx/statement)
        tag (if statement? :void tag)]
    `^:container
    [[:load-this]
     ~@(mapcat (fn [l] (-emit (assoc l :op :local) frame)) (concat params locals))
     ~[:invoke-virtual method-sig tag]
     ~@(if (and (not statement?)
                (#{Void Void/TYPE} tag))
         [[:insn :ACONST_NULL]])]))

(defn emit-letfn-bindings [bindings class-names frame]
  (let [binds (set (mapv :name bindings))]
    (mapcat (fn [{:keys [init tag name]} class-name]
              (let [{:keys [closed-overs]} init]
                `[[:var-insn ~(keyword (.getName ^Class tag) "ILOAD") ~name]
                  [:check-cast ~class-name]

                  ~@(mapcat (fn [[k c]]
                              (when (binds k)
                                `[[:dup]
                                  ~@(emit (assoc c :op :local) frame)
                                  ~[:put-field class-name k (:tag c)]]))
                            closed-overs)

                  [:pop]]))
            bindings class-names)))


(defn emit-binds [bindings frame]
  (mapv
   (fn [{:keys [init tag name] :as binding}]
     (let [init (emit init frame)
           class-name (-> init first second)] ;; weak
       [class-name
        `[~@init
          [:var-insn ~(keyword (.getName ^Class tag) "ISTORE") ~name]]]))
   bindings))

(defmethod -emit :letfn
  [{:keys [bindings body env]} frame]
  (let [[loop-label end-label] (repeatedly label)]
    `^:container
    [~@(emit-bindings (mapv #(assoc % :init nil-expr) bindings) (repeat nil) frame)

     ~@(let [binds (emit-binds bindings frame)
             bindings-emit(mapcat second binds)
             class-names (mapv first binds)]
         `[~@bindings-emit
           ~@(emit-letfn-bindings bindings class-names frame)])

     [:mark ~loop-label]
     ~@(emit body frame)
     [:mark ~end-label]
     ~@(mapv (fn [{:keys [name tag]}]
               [:local-variable name tag nil loop-label end-label name])
             bindings)]))

(defmethod -emit :recur
  [{:keys [exprs]} {:keys [loop-label loop-locals] :as frame}]
  `[~@(mapcat (fn [arg] (emit arg frame)) exprs)
    ~@(rseq (mapv (fn [{:keys [local tag] :as arg} {:keys [name] :as binding}]
                    (if (= :arg local)
                      [:store-arg (:arg-id binding)]
                      [:var-insn (keyword (.getName ^Class tag) "ISTORE") name]))
                  exprs loop-locals))
    [:go-to ~loop-label]])

(defn emit-internal-methods [methods {:keys [class params] :as frame}]
  (mapv (fn [{:keys [closed-overs tag internal-method-name body] :as ast}]
          (let [locals (remove #(#{:arg :field} (:local %)) (vals closed-overs))
                [loop-label end-label] (repeatedly label)
                bc `[[:start-method]
                     [:local-variable :this :clojure.lang.AFunction nil ~loop-label ~end-label :this]
                     [:label ~loop-label]
                     ~@(mapv (fn [{:keys [name tag]}]
                               [:local-variable name tag nil loop-label end-label name])
                             params)
                     ~@(mapcat (fn [i {:keys [name o-tag] :as l}]
                                 `[[:load-arg ~i]
                                   ~[:local-variable name o-tag nil loop-label end-label name]
                                   ~[:var-insn (keyword (.getName ^Class o-tag) "ISTORE") name]])
                               (iterate inc (count params)) locals)
                     ~@(case (:op ast)
                         :loop
                         (emit-let ast frame)
                         :try
                         (emit-try ast frame))
                     ~@(emit-cast (prim-or-obj tag) tag)
                     [:label ~end-label]
                     [:return-value]
                     [:end-method]]
                method-sig (into [(keyword internal-method-name)]
                                 (into (mapv :tag params)
                                       (mapv :o-tag locals)))]

            {:op :method
             :attr #{:private}
             :method [method-sig (if (isa? (-> body :env :context) :ctx/statement)
                                     :void
                                     tag)]
             :code bc}))
        methods))

(defmethod -emit :fn-method
  [{:keys [params tag fixed-arity static? variadic? body env internal-methods]}
   {:keys [class] :as frame}]
  (let [arg-tags               (mapv (comp prim-or-obj :tag) params)
        return-type            (prim-or-obj tag)
        tags                   (conj arg-tags return-type)
        prim-interface         (j.u/prim-interface tags)

        primitive?             (some primitive? tags)

        code-method-name            (cond
                                      variadic? :doInvoke
                                      primitive? :invokePrim
                                      static? :invokeStatic
                                      :else :invoke)

        ;; arg-types
        [loop-label end-label] (repeatedly label)

        code `[[:start-method]
               [:local-variable :this :clojure.lang.AFunction nil ~loop-label ~end-label :this]
               ~@(mapcat (fn [{:keys [name arg-id o-tag tag]}]
                           `[~[:local-variable name tag nil loop-label end-label name]
                             ~@(when-not (= tag o-tag)
                                [[:load-arg arg-id]
                                 [:check-cast tag]
                                 [:store-arg arg-id]])])
                  params)
               [:mark ~loop-label]
               ~@(emit-line-number env loop-label)
               ~@(emit body (assoc frame
                                   :loop-label  loop-label
                                   :loop-locals params
                                   :params      params))
               [:mark ~end-label]
               [:return-value]
               [:end-method]]
        delegate-invoke-to-primitive (when primitive?
                                       `[[:start-method]
                                         [:load-this]
                                         ~@(mapcat (fn [{:keys [tag]} id]
                                                     `[~[:load-arg id]
                                                       ~@(emit-cast Object tag)])
                                            params (range))
                                         ~[:invoke-virtual (into [(keyword class "invokePrim")] arg-tags) return-type]
                                         ~@(emit-cast return-type Object)
                                         [:return-value]
                                         [:end-method]])
        delegate-prim-to-static (when static?
                                  `[[:start-method]
                                    ~@(map-indexed (fn [idx arg] [:load-arg idx]) params)
                                    ~[:invoke-static (into [(keyword class "invokeStatic")] arg-tags) return-type]
                                    [:return-value]
                                    [:end-method]])
        delegate-invoke-to-static (when static?
                                    `[[:start-method]
                                      ~@(mapcat (fn [{:keys [tag]} id]
                                                  `[~[:load-arg id]
                                                    ~@(emit-cast Object tag)])
                                         params (range))
                                      ~[:invoke-static (into [(keyword class "invokeStatic")] arg-tags) return-type]
                                      ~@(emit-cast return-type Object)
                                      [:return-value]
                                      [:end-method]])]

    ;; should emit typed only when there's an interface, otherwise it's useless

    `[~{:op     :method
        :attr   #{:public}
        :method [(into [:invoke] (repeat (count params) :java.lang.Object))
                 :java.lang.Object]
        :code   (cond
                  static? delegate-invoke-to-static
                  primitive? delegate-invoke-to-primitive
                  :else code)}
      ~@(when static?
         [{:op :method
           :attr #{:static :public}
           :method [(into [:invokeStatic] arg-tags) return-type]
           :code code}])
      ~@(when primitive?
         [{:op        :method
           :attr      #{:public}
           :interface prim-interface
           :method    [(into [:invokePrim] arg-tags) return-type]
           :code      (if static? delegate-prim-to-static code)}])
      ~@(when internal-methods
          (emit-internal-methods internal-methods (assoc frame :params params)))]))

;; addAnnotations
(defmethod -emit :method
  [{:keys [this params name bridges tag fixed-arity variadic? body env internal-methods]}
   {:keys [class] :as frame}]

  (let [method-name name
        return-type tag
        arg-types (mapv :tag params)
        [loop-label end-label] (repeatedly label)

        code
        `[[:start-method]
          ~[:local-variable (:name this) class nil loop-label end-label (:name this)]
          ~@(mapv (fn [{:keys [tag name]}]
                    [:local-variable name tag nil loop-label end-label name])
                  params)
          [:mark ~loop-label]
          ~@(emit-line-number env loop-label)
          ~@(emit (assoc body
                    :tag   return-type
                    :o-tag (or (:tag body) Object))
                  (assoc frame
                    :loop-label  loop-label
                    :loop-locals params
                    :params      params))
          [:mark ~end-label]
          [:return-value]
          [:end-method]]]

    `[~{:op     :method
        :attr   #{:public}
        :method [(into [method-name] arg-types) return-type]
        :code   code}
      ~@(let [target [(into [(keyword class (str method-name))] arg-types) return-type]]
          (for [{:keys [name parameter-types return-type]} bridges]
            {:op :method
             :attr #{:public :bridge}
             :method [(into [method-name] parameter-types) return-type]
             :code `[[:start-method]
                     [:load-this]
                     [:load-args]
                     [:invoke-virtual ~@target]
                     [:return-value]
                     [:end-method]]}))
      ~@(when internal-methods
          (emit-internal-methods internal-methods (assoc frame :params params)))]))

(defmethod -emit :local
  [{:keys [to-clear? local name tag o-tag arg-id]}
   {:keys [closed-overs class] :as frame}]
  (let [to-clear? (and to-clear?
                       (not (primitive? o-tag)))]
    (cond
     (closed-overs name)
     `[[:load-this]
       ~[:get-field class name o-tag]
       ~@(when to-clear?
           [[:load-this]
            [:insn :ACONST_NULL]
            [:put-field class name o-tag]])]

     (= :arg local)
     `[[:load-arg ~arg-id]
       ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:store-arg arg-id]])]

     (= :fn local)
     [[:var-insn :clojure.lang.AFunction/ILOAD 0]]

     (= :this local)
     [[:var-insn :clojure.lang.Object/ILOAD 0]]

     :else
     `[~[:var-insn (keyword (.getName ^Class o-tag) "ILOAD") name]
       ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:var-insn (keyword (.getName ^Class o-tag) "ISTORE") name]])])))

(defmethod -emit-set! :local
  [{:keys [target val env]} {:keys [class] :as frame}]
  (let [{:keys [o-tag name]} target]
    `[~@(emit-line-number env)
     [:load-this]
     ~@(emit (assoc val :tag Object) frame)
     ~[:put-field class name Object]
     ~@(-emit target frame)]))

(defmulti -emit-value (fn [type value] type))

(defn emit-value [t o]
  `[~@(-emit-value t o)
    ~@(when-let [m (and (u/obj? o)
                        (meta o))]
        `[[:check-cast :clojure.lang.IObj]
          ~@(-emit-value :map m)
          [:check-cast :clojure.lang.IPersistentMap]
          [:invoke-interface [:clojure.lang.IObj/withMeta :clojure.lang.IPersistentMap]
           :clojure.lang.IObj]])])

(defmethod -emit-value :nil [_ _]
  [[:insn :ACONST_NULL]])

(defmethod -emit-value :string [_ s]
  [[:push s]])

(defmethod -emit-value :bool [_ b]
  [[:get-static (if b :java.lang.Boolean/TRUE :java.lang.Boolean/FALSE)
    :java.lang.Boolean]])

(defmethod -emit-value :number [_ n]
  (cond
   (instance? Long n)
   [[:push n]
    [:invoke-static [:java.lang.Long/valueOf :long] :java.lang.Long]]

   (instance? Integer n)
   [[:push n]
    [:invoke-static [:java.lang.Integer/valueOf :int] :java.lang.Integer]]

   (instance? Double n)
   [[:push n]
    [:invoke-static [:java.lang.Double/valueOf :double] :java.lang.Double]]

   (instance? Float n)
   [[:push n]
    [:invoke-static [:java.lang.Float/valueOf :float] :java.lang.Float]]

   (instance? Byte n)
   [[:push n]
    [:invoke-static [:java.lang.Byte/valueOf :byte] :java.lang.Byte]]

   (instance? Short n)
   [[:push n]
    [:invoke-static [:java.lang.Short/valueOf :short] :java.lang.Short]]

   :else
   (-emit-value :default n)))

(defmethod -emit-value :class [_ c]
  (if (primitive? c)
    [[:get-static (box c) "TYPE" :java.lang.Class]]
    [[:push (.getName ^Class c)]
     [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]]))

(defmethod -emit-value :symbol [_ s]
  [[:push (namespace s)]
   [:push (name s)]
   [:invoke-static [:clojure.lang.Symbol/intern :java.lang.String :java.lang.String]
    :clojure.lang.Symbol]])

(defmethod -emit-value :keyword [_ k]
  [[:push (namespace k)]
   [:push (name k)]
   [:invoke-static [:clojure.lang.Keyword/intern :java.lang.String :java.lang.String]
    :clojure.lang.Keyword]])

(defmethod -emit-value :var [_ ^clojure.lang.Var v]
  [[:push (str (ns-name (.ns v)))]
   [:push (name (.sym v))]
   [:invoke-static [:clojure.lang.RT/var :java.lang.String :java.lang.String]
    :clojure.lang.Var]])

(defn emit-values-as-array [list]
  `[[:push ~(int (count list))]
    [:new-array :java.lang.Object]
    ~@(mapcat (fn [i item]
                `[[:dup]
                  [:push ~(int i)]
                  ~@(emit-value (u/classify item) item)
                  [:array-store :java.lang.Object]])
              (range) list)])

(defmethod -emit-value :map [_ m]
  (let [arr (mapcat identity m)
        sorted? (sorted? m)
        hmap? (= clojure.lang.PersistentHashMap (class m))]
    (if (empty? m)
      [(cond
        sorted?
        [:get-static :clojure.lang.PersistentTreeMap/EMPTY :clojure.lang.PersistentTreeMap]
        hmap?
        [:get-static :clojure.lang.PersistentHashMap/EMPTY :clojure.lang.PersistentHashMap]
        :else
        [:get-static :clojure.lang.PersistentArrayMap/EMPTY :clojure.lang.PersistentArrayMap])]
      `[~@(emit-values-as-array arr)
        ~@(cond
           sorted?
           [[:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
            [:invoke-static [:clojure.lang.PersistentTreeMap/create :clojure.lang.ISeq] :clojure.lang.PersistentTreeMap]]
           (and hmap? (<= (count m) 8))
           [[:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
            [:invoke-static [:clojure.lang.PersistentHashMap/create :clojure.lang.ISeq] :clojure.lang.PersistentHashMap]]
           (and (= clojure.lang.PersistentArrayMap (class m)) (> (count m) 8))
           [[:invoke-static [:clojure.lang.PersistentArrayMap/createAsIfByAssoc :objects] :clojure.lang.PersistentArrayMap]]
           :else
           [[:invoke-static [:clojure.lang.RT/map :objects] :clojure.lang.IPersistentMap]])])))

(defmethod -emit-value :vector [_ v]
  (if (empty? v)
    [[:get-static :clojure.lang.PersistentVector/EMPTY :clojure.lang.PersistentVector]]
    `[~@(emit-values-as-array v)
      [:invoke-static [:clojure.lang.RT/vector :objects] :clojure.lang.IPersistentVector]]))

(defmethod -emit-value :set [_ s]
  (let [sorted? (sorted? s)]
    (if (empty? s)
      [(if sorted?
          [:get-static :clojure.lang.PersistentTreeSet/EMPTY :clojure.lang.PersistentTreeSet]
          [:get-static :clojure.lang.PersistentHashSet/EMPTY :clojure.lang.PersistentHashSet])]
      `[~@(emit-values-as-array s)
        ~@(if sorted?
            [[:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
             [:invoke-static [:clojure.lang.PersistentTreeSet/create :clojure.lang.ISeq] :clojure.lang.PersistentTreeSet]]
            [[:invoke-static [:clojure.lang.RT/set :objects] :clojure.lang.IPersistentSet]])])))

(defmethod -emit-value :seq [_ s]
  (if (empty? s)
    [[:get-static :clojure.lang.PersistentList/EMPTY :clojure.lang.PersistentList$EmptyList]]
   `[~@(emit-values-as-array s)
     [:invoke-static [:java.util.Arrays/asList :objects] :java.util.List]
     [:invoke-static [:clojure.lang.PersistentList/create :java.util.List]
      :clojure.lang.IPersistentList]]))

(defmethod -emit-value :char [_ c]
  [[:push c]
   [:invoke-static [:java.lang.Character/valueOf :char] :java.lang.Character]])

(defmethod -emit-value :regex [_ r]
  `[~@(emit-value :string (str r))
    [:invoke-static [:java.util.regex.Pattern/compile :java.lang.String]
     :java.util.regex.Pattern]])

(defmethod -emit-value :class [_ c]
  (if (primitive? c)
    [[:get-static (box c) "TYPE" :java.lang.Class]]
    [[:push (.getName ^Class c)]
     [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]]))

(defmethod -emit-value :record [_ r]
  (let [r-class (.getName (class r))]
    `[~@(emit-value :map r)
      ~[:invoke-static [(keyword r-class "create") :clojure.lang.IPersistentMap] r-class]]))

(defmethod -emit-value :type [_ t]
  (let [t-class (.getName (class t))
        fields (Reflector/invokeStaticMethod t-class "getBasis" (object-array []))]
    `[[:new-instance ~t-class]
      [:dup]
      ~@(mapcat (fn [field]
                  (let [val (Reflector/getInstanceField t (name field))]
                    (emit-value (u/classify val) val))) fields)
      [:invoke-constructor [~(keyword t-class "<init>")
                            ~@(mapv (comp j.u/maybe-class :tag meta) fields)]
       :void]]))

(defmethod -emit-value :default [_ o]
  (try
    (let [s (binding [*print-dup* true] (pr-str o))]
      (when (or (not (seq s))
                (= "#<" (subs s 0 2)))
        (throw (ex-info "Can't embed unreadable object in code"
                        {:object o})))
      [[:push s]
       [:invoke-static [:clojure.lang.RT/readString :java.lang.String] :java.lang.Object]])
    (catch Exception e
      (if (instance? clojure.lang.ExceptionInfo e)
        (throw e)
        (throw (ex-info "Can't embed object in code"
                        {:object o}))))))

(defn emit-constants [{:keys [class constants]}]
  (mapcat (fn [{:keys [val id tag type]}]
            `[~@(emit-value (or type (u/classify val)) val)
              [:check-cast ~tag]
              ~[:put-static class (str "const__" id) tag]])
          (vals constants)))

(defn emit-keyword-callsites
  [{:keys [keyword-callsites constants class]}]
  (mapcat (fn [k]
            (let [{:keys [id]} (constants {:form k :tag clojure.lang.Keyword :meta nil})]
              `[[:new-instance :clojure.lang.KeywordLookupSite]
                [:dup]
                ~@(emit-value :keyword k)
                [:invoke-constructor [:clojure.lang.KeywordLookupSite/<init> :clojure.lang.Keyword] :void]
                [:dup]
                ~[:put-static class (str "site__" id) :clojure.lang.KeywordLookupSite]
                ~[:put-static class (str "thunk__" id) :clojure.lang.ILookupThunk]]))
          keyword-callsites))


;; TODO: generalize this for deftype/reify: needs  mutable field handling + altCtor + annotations
;; add smap

(defn emit-class
  [{:keys [class-name meta methods variadic? constants closed-overs keyword-callsites
           protocol-callsites env annotations super interfaces op fields class-id]
    :as ast}
   {:keys [debug? class-loader] :as frame}]
  (let [old-frame          frame

        constants          (into {}
                                 (remove #(let [{:keys [tag type]} (val %)]
                                            (or (primitive? tag)
                                                (#{:string :bool} type)))
                                         constants))

        consts             (vals constants)
        constant-table     (zipmap (mapv :id consts) consts)

        frame              (merge frame
                                  {:class              class-name
                                   :constants          constants
                                   :constant-table     constant-table
                                   :closed-overs       closed-overs
                                   :keyword-callsites  keyword-callsites
                                   :protocol-callsites protocol-callsites})

        consts             (mapv (fn [{:keys [id tag]}]
                                   {:op   :field
                                    :attr #{:public :final :static}
                                    :name (str "const__" id)
                                    :tag  tag})
                                 consts)

        meta-field         (when meta
                             [{:op   :field
                               :attr #{:public :final}
                               :name "__meta"
                               :tag  clojure.lang.IPersistentMap}])

        keyword-callsites  (mapcat (fn [k]
                                     (let [{:keys [id]} (constants {:form k :tag clojure.lang.Keyword :meta nil})]
                                       [{:op   :field
                                         :attr #{:public :final :static}
                                         :name (str "site__" id)
                                         :tag  clojure.lang.KeywordLookupSite}
                                        {:op   :field
                                         :attr #{:public :final :static}
                                         :name (str "thunk__" id)
                                         :tag  clojure.lang.ILookupThunk}]))
                                   keyword-callsites)

        protocol-callsites (mapcat (fn [p]
                                     (let [{:keys [id]} (constants {:form p :tag clojure.lang.Var :meta (clojure.core/meta p)})]
                                       [{:op   :field
                                         :attr #{:private :static}
                                         :name (str "cached__class__" id)
                                         :tag  java.lang.Class}]))
                                   protocol-callsites)

        deftype?           (= op :deftype)
        defrecord?         (contains? closed-overs '__meta)

        closed-overs       (mapv (fn [{:keys [name local o-tag tag mutable] :as l}]
                                   (merge l
                                          {:op   :field
                                           :attr (when deftype?
                                                   (if mutable
                                                     #{mutable}
                                                     #{:public :final}))
                                           :tag  o-tag}))
                                 (if deftype?
                                   fields ;; preserve order
                                   (vals closed-overs)))

        ctor-types         (into (if meta [:clojure.lang.IPersistentMap] [])
                                 (mapv (if deftype? (comp prim-or-obj :tag) :tag) closed-overs))

        class-ctors        [{:op     :method
                             :attr   #{:public :static}
                             :method [[:<clinit>] :void]
                             :code   `[[:start-method]
                                       ~@(emit-line-number env)
                                       ~@(when (seq constants)
                                           (emit-constants frame))
                                       ~@(when (seq keyword-callsites)
                                           (emit-keyword-callsites frame))
                                       [:return-value]
                                       [:end-method]]}
                            (let [[start-label end-label] (repeatedly label)]
                              {:op     :method
                               :attr   #{:public}
                               :method `[[:<init> ~@ctor-types] :void]
                               :code   `[[:start-method]
                                         ~@(emit-line-number env)
                                         [:label ~start-label]
                                         [:load-this]
                                         [:invoke-constructor [~(keyword (name super) "<init>")] :void]
                                         ~@(when meta
                                             [[:load-this]
                                              [:load-arg 0]
                                              [:put-field class-name :__meta :clojure.lang.IPersistentMap]])
                                         ~@(mapcat
                                            (fn [{:keys [name tag]} t id]
                                              `[[:load-this]
                                                ~[:load-arg id]
                                                ~@(emit-cast t tag)
                                                ~[:put-field class-name name tag]])
                                            closed-overs ctor-types (if meta (rest (range)) (range)))

                                         [:label ~end-label]
                                         [:return-value]
                                         [:end-method]]})]

        defrecord-ctor     (when defrecord?
                             [{:op     :method
                               :attr   #{:public}
                               :method `[[:<init> ~@(drop-last 2 ctor-types)] :void]
                               :code   `[[:start-method]
                                         [:load-this]
                                         [:load-args]
                                         [:insn :ACONST_NULL]
                                         [:insn :ACONST_NULL]
                                         [:invoke-constructor [~(keyword class-name "<init>") ~@ctor-types] :void]
                                         [:return-value]
                                         [:end-method]]}])

        variadic-method    (when variadic?
                             (let [required-arity (->> methods (filter :variadic?) first :fixed-arity)]
                               [{:op     :method
                                 :attr   #{:public}
                                 :method [[:getRequiredArity] :int]
                                 :code   `[[:start-method]
                                           [:push ~(int required-arity)]
                                           [:return-value]
                                           [:end-method]]}]))

        meta-methods       (when meta
                             [{:op     :method
                               :attr   #{:public}
                               :method `[[:<init> ~@(rest ctor-types)] :void]
                               :code   `[[:start-method]
                                         [:load-this]
                                         [:insn :ACONST_NULL]
                                         [:load-args]
                                         [:invoke-constructor [~(keyword class-name "<init>")
                                                               ~@ctor-types] :void]
                                         [:return-value]
                                         [:end-method]]}
                              {:op     :method
                               :attr   #{:public}
                               :method`[[:meta] :clojure.lang.IPersistentMap]
                               :code   [[:start-method]
                                        [:load-this]
                                        [:get-field class-name :__meta :clojure.lang.IPersistentMap]
                                        [:return-value]
                                        [:end-method]]}
                              {:op     :method
                               :attr   #{:public}
                               :method`[[:withMeta :clojure.lang.IPersistentMap] :clojure.lang.IObj]
                               :code   `[[:start-method]
                                         [:new-instance ~class-name]
                                         [:dup]
                                         [:load-arg 0]
                                         ~@(mapcat
                                            (fn [{:keys [name tag]}]
                                              [[:load-this]
                                               [:get-field class-name name tag]])
                                            closed-overs)
                                         [:invoke-constructor [~(keyword class-name "<init>")
                                                               ~@ctor-types] :void]
                                         [:return-value]
                                         [:end-method]]}])

        deftype-fields     (vec (remove '#{__meta __extmap} (mapv :form closed-overs)))

        deftype-methods    (when deftype?
                             `[~{:op     :method
                                 :attr   #{:public :static}
                                 :method [[:getBasis] :clojure.lang.IPersistentVector]
                                 :code   `[[:start-method]
                                           ~@(emit-value :vector (mapv munge deftype-fields))
                                           [:return-value]
                                           [:end-method]]}
                               ~@(when defrecord?
                                   [{:op     :method
                                     :attr   #{:public :static}
                                     :method [[:create :clojure.lang.IPersistentMap] class-name]
                                     :code   `[[:start-method]
                                               ~@(mapcat
                                                  (fn [field id]
                                                    `[[:load-arg 0]
                                                      ~@(emit-value :keyword field)
                                                      [:insn :ACONST_NULL]
                                                      [:invoke-interface [:clojure.lang.IPersistentMap/valAt :java.lang.Object :java.lang.Object] :java.lang.Object]
                                                      [:astore ~id]
                                                      [:load-arg 0]
                                                      ~@(emit-value :keyword field)
                                                      [:invoke-interface [:clojure.lang.IPersistentMap/without :java.lang.Object] :clojure.lang.IPersistentMap]
                                                      [:store-arg 0]])
                                                  deftype-fields (rest (range)))
                                               [:new-instance ~class-name]
                                               [:dup]
                                               ~@(for [i (rest (range (inc (count deftype-fields))))]
                                                   [:var-insn :java.lang.Object/ILOAD i])
                                               [:insn :ACONST_NULL]
                                               [:load-arg 0]
                                               [:invoke-static [:clojure.lang.RT/seqOrElse :java.lang.Object] :java.lang.Object]
                                               [:invoke-constructor [~(keyword class-name "<init>")
                                                                     ~@ctor-types] :void]
                                               [:return-value]
                                               [:end-method]]}])])

        jvm-ast            {:op          :class
                            :debug?      debug?
                            :attr        #{:public :super :final}
                            :annotations annotations
                            :class-name  class-name
                            :name        class-name
                            :super       super
                            :interfaces  interfaces
                            :fields      (concat consts keyword-callsites
                                                 meta-field closed-overs protocol-callsites)
                            :methods     (concat class-ctors defrecord-ctor deftype-methods
                                                 variadic-method meta-methods
                                                 (mapcat #(-emit % frame) methods))}]

    (when-not (get-in @*classes* [:ids class-id])
      (swap! *classes* update-in [:classes] conj jvm-ast)
      (when class-id
        (swap! *classes* update-in [:ids] conj class-id)))

    (if deftype?
      [[:insn :ACONST_NULL]]
      `[[:new-instance ~class-name]
        [:dup]
        ~@(when meta
            [[:insn :ACONST_NULL]])
        ~@(mapcat #(emit (assoc % :op :local) old-frame)
                  closed-overs)
        [:invoke-constructor [~(keyword class-name "<init>")
                              ~@ctor-types] :void]])))

(defmethod -emit :reify
  [{:keys [class-name] :as ast}
   frame]
  (let [class-name (.getName ^Class class-name)
        ast (assoc ast
              :class-name class-name
              :super :java.lang.Object
              :meta {})]
    (emit-class ast frame)))

(defmethod -emit :deftype
  [{:keys [class-name] :as ast}
   frame]
  (let [class-name (.getName ^Class class-name)
        ast (assoc ast
              :class-name class-name
              :super :java.lang.Object)]
    (with-meta
      (emit-class ast frame)
      {:untyped true})))

(defmethod -emit :fn
  [{:keys [form internal-name variadic? class-name] :as ast} frame]
  (let [class-name (or class-name
                     (str (namespace-munge *ns*)
                          "$"
                          (munge internal-name)))]
    (emit-fn-class ast frame class-name)))
