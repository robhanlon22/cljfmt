(ns cljfmt.core
  #?(:clj (:refer-clojure :exclude [reader-conditional?]))
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z])
  #?(:clj (:import java.util.regex.Pattern)
     :cljs (:require-macros [cljfmt.core :refer [read-resource]])))

#?(:clj (def read-resource* (comp read-string slurp io/resource)))
#?(:clj (defmacro read-resource [path] `'~(read-resource* path)))

(def includes?
  #?(:clj  (fn [^String a ^String b] (.contains a b))
     :cljs str/includes?))

#?(:clj
   (defn- find-all [zloc p?]
     (loop [matches []
            zloc zloc]
       (if-let [zloc (z/find-next zloc z/next* p?)]
         (recur (conj matches zloc)
                (z/next* zloc))
         matches))))

(defn- edit-all
  ([zloc p? f]
   (edit-all zloc p? f z/next*))
  ([zloc p? f nextf]
   (loop [zloc (if (p? zloc) (f zloc) zloc)]
     (if-let [zloc (z/find-next zloc nextf p?)]
       (recur (f zloc))
       zloc))))

(defn- transform [form zf & args]
  (z/root (apply zf (z/of-node form) args)))

(defn root? [zloc]
  (nil? (z/up* zloc)))

(defn- top? [zloc]
  (some-> zloc z/up root?))

(defn- root [zloc]
  (if (root? zloc) zloc (recur (z/up zloc))))

(defn- clojure-whitespace? [zloc]
  (z/whitespace? zloc))

(defn- unquote? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :unquote)))

(defn- deref? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :deref)))

(defn- unquote-deref? [zloc]
  (and (deref? zloc)
       (unquote? (z/up* zloc))))

(defn- comment? [zloc]
  (some-> zloc z/node n/comment?))

(defn- surrounding-whitespace? [zloc]
  (and (not (top? zloc))
       (clojure-whitespace? zloc)
       (or (and (nil? (z/left* zloc))
                ;; don't convert ~ @ to ~@
                (not (unquote-deref? (z/right* zloc)))
                ;; ignore space before comments
                (not (comment? (z/right* zloc))))
           (nil? (z/skip z/right* clojure-whitespace? zloc)))))

(defn remove-surrounding-whitespace [form]
  (transform form edit-all surrounding-whitespace? z/remove*))

(defn- element? [zloc]
  (and zloc (not (z/whitespace-or-comment? zloc))))

(defn- reader-macro? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :reader-macro)))

(defn- namespaced-map? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :namespaced-map)))

(defn- missing-whitespace? [zloc]
  (and (element? zloc)
       (not (reader-macro? (z/up* zloc)))
       (not (namespaced-map? (z/up* zloc)))
       (element? (z/right* zloc))))

(defn insert-missing-whitespace [form]
  (transform form edit-all missing-whitespace? z/insert-space-right))

(defn- space? [zloc]
  (= (z/tag zloc) :whitespace))

(defn- line-comment? [zloc]
  (and (comment? zloc) (re-matches #"(?s);;([^;].*)?" (z/string zloc))))

(defn- comma? [zloc]
  (some-> zloc z/node n/comma?))

(defn- line-break? [zloc]
  (or (z/linebreak? zloc) (comment? zloc)))

(defn- skip-whitespace [zloc]
  (z/skip z/next* space? zloc))

(defn- skip-whitespace-and-commas
  ([zloc] (skip-whitespace-and-commas zloc z/next*))
  ([zloc f] (z/skip f (some-fn space? comma?) zloc)))

(defn- skip-clojure-whitespace
  ([zloc] (skip-clojure-whitespace zloc z/next*))
  ([zloc f] (z/skip f clojure-whitespace? zloc)))

(defn- count-newlines [zloc]
  (loop [zloc' zloc, newlines 0]
    (if (z/linebreak? zloc')
      (recur (-> zloc' z/right* skip-whitespace-and-commas)
             (-> zloc' z/string count (+ newlines)))
      (if (comment? (skip-clojure-whitespace zloc z/left*))
        (inc newlines)
        newlines))))

(defn- final-transform-element? [zloc]
  (nil? (skip-clojure-whitespace (z/next* zloc))))

(defn- consecutive-blank-line? [zloc]
  (and (> (count-newlines zloc) 2)
       (not (final-transform-element? zloc))))

(defn- remove-clojure-whitespace [zloc]
  (if (clojure-whitespace? zloc)
    (recur (z/remove* zloc))
    zloc))

(defn- replace-consecutive-blank-lines [zloc]
  (let [zloc-elem-before (-> zloc
                             skip-clojure-whitespace
                             z/prev*
                             remove-clojure-whitespace)]
    (-> zloc-elem-before
        z/next*
        (z/insert-left* (n/newlines (if (comment? zloc-elem-before) 1 2))))))

(defn remove-consecutive-blank-lines [form]
  (transform form edit-all consecutive-blank-line? replace-consecutive-blank-lines))

(defn- indentation? [zloc]
  (and (line-break? (z/left* zloc)) (space? zloc)))

(defn- comment-next? [zloc]
  (-> zloc z/next* skip-whitespace comment?))

(defn- comment-next-other-than-line-comment? [zloc]
  (when-let [znext (-> zloc z/next* skip-whitespace)]
    (and (comment? znext) (not (line-comment? znext)))))

(defn- should-indent? [zloc opts]
  (and (line-break? zloc)
       (if (:indent-line-comments? opts)
         (not (comment-next-other-than-line-comment? zloc))
         (not (comment-next? zloc)))))

(defn- should-unindent? [zloc opts]
  (and (indentation? zloc)
       (if (:indent-line-comments? opts)
         (not (comment-next-other-than-line-comment? zloc))
         (not (comment-next? zloc)))))

(defn unindent
  ([form]
   (unindent form {}))
  ([form opts]
   (transform form edit-all #(should-unindent? % opts) z/remove*)))

(def ^:private start-element
  {:meta "^", :meta* "#^", :vector "[",       :map "{"
   :list "(", :eval "#=",  :uneval "#_",      :fn "#("
   :set "#{", :deref "@",  :reader-macro "#", :unquote "~"
   :var "#'", :quote "'",  :syntax-quote "`", :unquote-splicing "~@"
   :namespaced-map "#"})

(defn- prior-line-string [zloc]
  (loop [zloc     zloc
         worklist '()]
    (if-let [p (z/left* zloc)]
      (let [s            (str (n/string (z/node p)))
            new-worklist (cons s worklist)]
        (if-not (includes? s "\n")
          (recur p new-worklist)
          (apply str new-worklist)))
      (if-let [p (z/up* zloc)]
        ;; newline cannot be introduced by start-element
        (recur p (cons (start-element (n/tag (z/node p))) worklist))
        (apply str worklist)))))

(defn- last-line-in-string ^String [^String s]
  (subs s (inc (.lastIndexOf s "\n"))))

(defn- margin [zloc]
  (-> zloc prior-line-string last-line-in-string count))

(defn- whitespace [width]
  (n/whitespace-node (apply str (repeat width " "))))

(defn- coll-indent [zloc]
  (-> zloc z/leftmost* margin))

(defn- uneval? [zloc]
  (= (z/tag zloc) :uneval))

(defn- index-of [zloc]
  (->> (iterate z/left zloc)
       (remove uneval?)
       (take-while identity)
       (count)
       (dec)))

(defn- meta? [zloc]
  (#{:meta :meta*} (z/tag zloc)))

(defn- skip-meta [zloc]
  (if (meta? zloc)
    (-> zloc z/down z/right recur)
    zloc))

(defn- cursive-two-space-list-indent? [zloc]
  (-> zloc z/leftmost* skip-meta z/tag #{:vector :map :list :set} not))

(defn- zprint-two-space-list-indent? [zloc]
  (-> zloc z/leftmost* z/tag #{:token :list}))

(defn two-space-list-indent? [zloc context]
  (case (:function-arguments-indentation context)
    :community false
    :cursive (cursive-two-space-list-indent? zloc)
    :zprint (zprint-two-space-list-indent? zloc)))

(defn- list-indent [zloc context]
  (if (> (index-of zloc) 1)
    (-> zloc z/leftmost* z/right margin)
    (cond-> (coll-indent zloc)
      (two-space-list-indent? zloc context) inc)))

(def indent-size 2)

(defn- indent-width [zloc]
  (case (z/tag zloc)
    :list indent-size
    :fn   (inc indent-size)))

(defn pattern? [v]
  (instance? #?(:clj Pattern :cljs js/RegExp) v))

#?(:clj
   (defn- top-level-form [zloc]
     (->> zloc
          (iterate z/up)
          (take-while (complement root?))
          last)))

(defn- token? [zloc]
  (= (z/tag zloc) :token))

(defn- ns-token? [zloc]
  (and (token? zloc)
       (= 'ns (z/sexpr zloc))))

(defn- ns-form? [zloc]
  (and (top? zloc)
       (= (z/tag zloc) :list)
       (some-> zloc z/down ns-token?)))

(defn- token-value [zloc]
  (let [zloc (skip-meta zloc)]
    (when (token? zloc) (z/sexpr zloc))))

(defn- reader-conditional? [zloc]
  (and (reader-macro? zloc) (#{"?" "?@"} (-> zloc z/down token-value str))))

(defn- find-next-keyword [zloc]
  (z/find zloc z/right #(n/keyword-node? (z/node %))))

(defn- first-symbol-in-reader-conditional [zloc]
  (when (reader-conditional? zloc)
    (when-let [key-loc (-> zloc z/down z/right z/down find-next-keyword)]
      (when-let [value-loc (-> key-loc z/next skip-meta)]
        (when (token? value-loc)
          (z/sexpr value-loc))))))

(defn- form-symbol [zloc]
  (let [zloc (z/leftmost zloc)
        zloc (if (uneval? zloc) (z/skip z/right uneval? zloc) zloc)
        sym  (or (token-value zloc)
                 (first-symbol-in-reader-conditional zloc))]
    (when (symbol? sym) sym)))

(defn- index-matches-top-argument? [zloc depth idx]
  (and (> depth 0)
       (= (inc idx) (index-of (nth (iterate z/up zloc) depth)))))

(defn- qualify-symbol-by-alias-map [possible-sym alias-map]
  (when-let [ns-str (namespace possible-sym)]
    (symbol (get alias-map ns-str ns-str) (name possible-sym))))

(defn- qualify-symbol-by-refer-map [possible-sym refer-map]
  (when-not (namespace possible-sym)
    (when-let [ns-str (get refer-map (str possible-sym))]
      (symbol ns-str (name possible-sym)))))

(defn- qualify-symbol-by-ns-name [sym ns-name]
  (when ns-name (symbol (name ns-name) (name sym))))

(defn- fully-qualified-symbol [sym context]
  (or (qualify-symbol-by-alias-map sym (:alias-map context))
      (qualify-symbol-by-refer-map sym (:refer-map context))
      (qualify-symbol-by-ns-name sym (:ns-name context))))

(defn- string-matches-key-part? [s key-part]
  (if (pattern? key-part) (re-find key-part s) (= s (name key-part))))

(defn- parts-match-vector-key? [sym-ns sym-name [ns-key name-key]]
  (and (string-matches-key-part? sym-ns ns-key)
       (string-matches-key-part? sym-name name-key)))

(defn- form-matches-key? [zloc key context]
  (when-some [sym (form-symbol zloc)]
    (let [full-sym (fully-qualified-symbol sym context)
          sym-name (name sym)
          sym-ns   (or (some-> full-sym namespace) (namespace sym))]
      (cond
        (vector? key)           (parts-match-vector-key? sym-ns sym-name key)
        (pattern? key)          (re-find key sym-name)
        (qualified-symbol? key) (= key full-sym)
        (symbol? key)           (= (name key) sym-name)))))

(defn- inner-indent [zloc key depth idx context]
  (let [top (nth (iterate z/up zloc) depth)]
    (when (and (z/left zloc)
               (form-matches-key? top key context)
               (or (nil? idx) (index-matches-top-argument? zloc depth idx)))
      (let [zup (z/up zloc)]
        (+ (margin zup) (indent-width zup))))))

(defn- nth-form [zloc n]
  (reduce (fn [z f] (when z (f z)))
          (z/leftmost zloc)
          (repeat n z/right)))

(defn- first-form-in-line? [zloc]
  (and (some? zloc)
       (if-let [zloc (z/left* zloc)]
         (if (space? zloc)
           (recur zloc)
           (or (z/linebreak? zloc) (comment? zloc)))
         true)))

(defn- block-indent [zloc key idx context]
  (when (form-matches-key? zloc key context)
    (let [zloc-after-idx (some-> zloc (nth-form (inc idx)))]
      (if (and (or (nil? zloc-after-idx) (first-form-in-line? zloc-after-idx))
               (> (index-of zloc) idx))
        (inner-indent zloc key 0 nil context)
        (list-indent zloc context)))))

(def default-indents
  (merge (read-resource "cljfmt/indents/clojure.clj")
         (read-resource "cljfmt/indents/compojure.clj")
         (read-resource "cljfmt/indents/fuzzy.clj")))

(def default-aligned-forms
  (read-resource "cljfmt/aligned_forms/clojure.clj"))

(def blank-line-forms
  (read-resource "cljfmt/blank_line_forms/clojure.clj"))

(def default-options
  {:alias-map                             {}
   :align-binding-columns?                false
   :align-map-columns?                    false
   :align-reader-conditionals?            false
   :align-single-column-lines?            false
   :aligned-forms                         default-aligned-forms
   :max-column-alignment-gap              nil
   :max-column-alignment-width            nil
   :blank-line-forms                      blank-line-forms
   :blank-lines-separate-alignment?       false
   :extra-aligned-forms                   {}
   :extra-blank-line-forms                {}
   :extra-indents                         {}
   :function-arguments-indentation        :community
   :indent-line-comments?                 false
   :indentation?                          true
   :indents                               default-indents
   :normalize-newlines-at-file-end?       false
   :insert-missing-whitespace?            true
   :remove-blank-lines-in-forms?          false
   :remove-consecutive-blank-lines?       true
   :remove-multiple-non-indenting-spaces? false
   :remove-surrounding-whitespace?        true
   :remove-trailing-whitespace?           true
   :sort-ns-references?                   false
   :split-keypairs-over-multiple-lines?   false})

(defmulti ^:private indenter-fn
  (fn [_sym _context [type & _args]] type))

(defmethod indenter-fn :inner [sym context [_ depth idx]]
  (fn [zloc] (inner-indent zloc sym depth idx context)))

(defmethod indenter-fn :block [sym context [_ idx]]
  (fn [zloc] (block-indent zloc sym idx context)))

(defmethod indenter-fn :default [sym context [_]]
  (fn [zloc]
    (when (form-matches-key? zloc sym context)
      (list-indent zloc context))))

(defn- make-indenter [[key opts] context]
  (apply some-fn (map (partial indenter-fn key context) opts)))

(defn- indent-order [[key specs]]
  (let [get-depth (fn [[type depth]] (if (= type :inner) depth 0))
        max-depth (transduce (map get-depth) max 0 specs)
        key-order  (cond
                     (qualified-symbol? key) 0
                     (simple-symbol? key)    1
                     (pattern? key)          2)]
    [(- max-depth) key-order (str key)]))

(defn- custom-indent [zloc indenter context]
  (or (when indenter (indenter zloc))
      (list-indent zloc context)))

(defn- indent-amount [zloc indenter context]
  (let [tag (-> zloc z/up z/tag)
        gp  (-> zloc z/up z/up)]
    (cond
      (reader-conditional? gp) (coll-indent zloc)
      (#{:list :fn} tag)       (custom-indent zloc indenter context)
      (= :meta tag)            (indent-amount (z/up zloc) indenter context)
      :else                    (coll-indent zloc))))

(defn- indent-line [zloc indenter context]
  (let [width (indent-amount zloc indenter context)]
    (if (> width 0)
      (z/insert-right* zloc (whitespace width))
      zloc)))

(defn- find-namespace [zloc]
  (some-> zloc root z/down (z/find z/right ns-form?) z/down z/next z/sexpr))

(defn indent
  ([form]
   (indent form default-indents default-options))
  ([form indents]
   (indent form indents default-options))
  ([form indents opts]
   (let [ns-name (or (::ns-name opts) (find-namespace (z/of-node form)))
         sorted-indents (sort-by indent-order indents)
         context (merge (select-keys opts [:function-arguments-indentation
                                           :alias-map :refer-map])
                        {:ns-name ns-name})
         indenter (some->> (seq sorted-indents)
                           (map #(make-indenter % context))
                           (apply some-fn))]
     (transform form edit-all #(should-indent? % opts)
                #(indent-line % indenter context)))))

(defn- map-key? [zloc]
  (and (z/map? (z/up zloc))
       (even? (index-of zloc))
       (not (uneval? zloc))
       (not (z/whitespace-or-comment? zloc))))

(defn- preceded-by-line-break? [zloc]
  (loop [previous (z/left* zloc)]
    (cond
      (line-break? previous)
      true
      (z/whitespace-or-comment? previous)
      (recur (z/left* previous)))))

(defn- map-key-without-line-break? [zloc]
  (and (map-key? zloc) (z/left zloc) (not (preceded-by-line-break? zloc))))

(defn- insert-newline-left [zloc]
  (z/insert-left* zloc (n/newlines 1)))

(defn split-keypairs-over-multiple-lines [form]
  (transform form edit-all map-key-without-line-break? insert-newline-left))

(defn reindent
  ([form]
   (indent (unindent form)))
  ([form indents]
   (indent (unindent form) indents))
  ([form indents opts]
   (indent (unindent form opts) indents opts)))

(defn final? [zloc]
  (and (nil? (z/right* zloc)) (root? (z/up* zloc))))

(defn- trailing-whitespace? [zloc]
  (and (space? zloc)
       (or (z/linebreak? (z/right* zloc)) (final? zloc))))

(defn remove-trailing-whitespace [form]
  (transform form edit-all trailing-whitespace? z/remove*))

(defn normalize-newlines-at-file-end [s]
  (cond-> (str/trimr s)
    (not (str/blank? s)) (str "\n")))

(defn- replace-with-one-space [zloc]
  (z/replace* zloc (whitespace 1)))

(defn- non-indenting-whitespace? [zloc]
  (and (space? zloc)
       (not (indentation? zloc))
       (not (comment? (z/right* zloc)))))

(defn remove-multiple-non-indenting-spaces [form]
  (transform form edit-all non-indenting-whitespace? replace-with-one-space))

(def ^:private ns-reference-symbols
  #{:import :require :require-macros :use})

(defn- ns-reference? [zloc]
  (and (z/list? zloc)
       (some-> zloc z/up ns-form?)
       (-> zloc z/sexpr first ns-reference-symbols)))

(defn- re-indexes [re s]
  (let [matcher    #?(:clj  (re-matcher re s)
                      :cljs (js/RegExp. (.-source re) "g"))
        next-match #?(:clj  #(when (.find matcher)
                               [(.start matcher) (.end matcher)])
                      :cljs #(when-let [result (.exec matcher s)]
                               [(.-index result) (.-lastIndex matcher)]))]
    (take-while some? (repeatedly next-match))))

(defn- re-seq-matcher [re charmap coll]
  {:pre (every? charmap coll)}
  (let [s (apply str (map charmap coll))
        v (vec coll)]
    (for [[start end] (re-indexes re s)]
      {:value (subvec v start end)
       :start start
       :end   end})))

(defn- find-elements-with-comments [nodes]
  (re-seq-matcher #"(CNS*)*E(S*C)?"
                  #(case (n/tag %)
                     (:whitespace :comma) \S
                     :comment \C
                     :newline \N
                     \E)
                  nodes))

(defn- splice-into [coll splices]
  (letfn [(splice [v i splices]
            (when-let [[{:keys [value start end]} & splices] (seq splices)]
              (lazy-cat (subvec v i start) value (splice v end splices))))]
    (splice (vec coll) 0 splices)))

(defn- add-newlines-after-comments [nodes]
  (mapcat #(if (n/comment? %) [% (n/newlines 1)] [%]) nodes))

(defn- remove-newlines-after-comments [nodes]
  (mapcat #(when-not (and %1 (n/comment? %1) (n/linebreak? %2)) [%2])
          (cons nil nodes)
          nodes))

(defn- sort-node-arguments-by [f nodes]
  (let [nodes  (add-newlines-after-comments nodes)
        args   (rest (find-elements-with-comments nodes))
        sorted (sort-by f (map :value args))]
    (->> sorted
         (map #(assoc %1 :value %2) args)
         (splice-into nodes)
         (remove-newlines-after-comments))))

(defn- update-children [zloc f]
  (let [node (z/node zloc)]
    (z/replace zloc (n/replace-children node (f (n/children node))))))

(defn- nodes-string [nodes]
  (apply str (map n/string nodes)))

(defn- remove-node-metadata [nodes]
  (mapcat #(if (= (n/tag %) :meta)
             (rest (n/children %))
             [%])
          nodes))

(defn- node-sort-string [nodes]
  (-> (remove (some-fn n/comment? n/whitespace?) nodes)
      (remove-node-metadata)
      (nodes-string)
      (str/replace #"[\[\]\(\)\{\}]" "")
      (str/trim)
      (str/lower-case)))

(defn sort-arguments [zloc]
  (update-children zloc #(sort-node-arguments-by node-sort-string %)))

(defn sort-ns-references [form]
  (transform form edit-all ns-reference? sort-arguments))

(defn- end-of-column-group? [zloc]
  (and (line-break? zloc) (> (count-newlines zloc) 1)))

(defn- skip-column-separators [zloc step]
  (z/skip step (some-fn space? comma? uneval?) zloc))

(defn- reduce-columns
  ([zloc f init]
   (reduce-columns zloc f init false))
  ([zloc f init separate?]
   (loop [zloc zloc, col 0, acc init]
     (if-some [zloc (skip-column-separators zloc z/right*)]
       (cond
         (and separate? (end-of-column-group? zloc)) acc
         (line-break? zloc) (recur (z/right* zloc) 0 acc)
         :else (recur (z/right* zloc) (inc col) (f zloc col acc)))
       acc))))

(defn- count-columns [zloc]
  (inc (reduce-columns zloc #(max %2 %3) 0)))

(defn- trailing-commas [zloc]
  (let [right (z/right* zloc)]
    (if (and right (comma? right))
      (-> right z/node n/string)
      "")))

(defn- node-end-position [zloc]
  (let [lines (str (last-line-in-string (prior-line-string zloc))
                   (n/string (z/node zloc))
                   (trailing-commas zloc))]
    (transduce (comp (remove #(str/starts-with? % ";"))
                     (map count))
               max 0 (str/split lines #"\r?\n"))))

(defn- column-end-position [zloc]
  ;; Discarded forms between columns still occupy space on the line.
  (loop [zloc zloc, end (node-end-position zloc)]
    (let [right (skip-whitespace-and-commas (z/right* zloc) z/right*)]
      (if (uneval? right)
        (recur right (max end (node-end-position right)))
        end))))

(defn- single-column-line? [zloc start]
  (and (let [right (skip-column-separators (z/right* zloc) z/right*)]
         (or (nil? right) (line-break? right)))
       (let [left (skip-column-separators (z/left* zloc) z/left*)]
         (or (identical? zloc start) (line-break? left)))))

(defn- node-str-length [zloc]
  (-> zloc z/node n/string count))

(defn- update-space-left [zloc delta]
  (let [left (z/left* zloc)]
    (cond
      (space? left) (let [n (max 0 (+ delta (node-str-length left)))]
                      (z/right* (z/replace* left (n/spaces n))))
      (pos? delta)  (z/insert-space-left zloc delta)
      :else         zloc)))

(defn- pad-after-line-break [zloc padding opts]
  (let [right (z/right* zloc)
        next (if (space? right) (z/right* right) right)]
    (cond
      (z/linebreak? next) [zloc 0]
      (and (comment? next)
           (not (and (:indent-line-comments? opts) (line-comment? next)))) [zloc 0]
      (space? right) (let [old (node-str-length right)
                           new (max 0 (+ padding old))]
                       [(if (pos? new)
                          (-> right (z/replace* (n/spaces new)) z/left*)
                          (z/remove* right))
                        (- new old)])
      (pos? padding) [(z/insert-space-right zloc padding) padding]
      :else [zloc 0])))

(defn- pad-inside-node [zloc padding opts]
  ;; Return the location and the change in its ending column. Literal newlines
  ;; reset that change; direct child line breaks use the parent's padding.
  (if (zero? padding)
    [zloc 0]
    (if-let [child (z/down* zloc)]
      (loop [child child, shift padding]
        (let [[child shift] (if (line-break? child)
                              (pad-after-line-break child padding opts)
                              (pad-inside-node child shift opts))]
          (if-let [right (z/right* child)]
            (recur right shift)
            [(z/up* child) shift])))
      [zloc (if (includes? (z/string zloc) "\n") 0 padding)])))

(defn- pad-node [zloc padding]
  (if (zero? padding)
    zloc
    (-> (update-space-left zloc padding)
        (pad-inside-node padding {})
        first)))

(defn- count-spaces [zloc]
  (if (space? zloc) (node-str-length zloc) 0))

(defn- pad-continuation-lines [zloc padding opts realign]
  (if (zero? padding)
    [zloc 0]
    (let [[zloc shift] (pad-inside-node zloc padding opts)]
      (if (and (:max-column-alignment-width opts) (n/inner? (z/node zloc)))
        ;; Moving a form can change which columns fall within an absolute limit.
        (let [before (count (last-line-in-string (z/string zloc)))
              zloc (realign zloc)
              after (count (last-line-in-string (z/string zloc)))]
          [zloc (+ shift (- after before))])
        [zloc shift]))))

(defn- pad-to-position
  [zloc start-position {max-gap     :max-column-alignment-gap
                        align-width :max-column-alignment-width
                        :as opts} realign]
  {:pre [(or (nil? max-gap) (pos-int? max-gap))
         (or (nil? align-width) (pos-int? align-width))]}
  (let [old-gap (count-spaces (z/left* zloc))
        delta   (- start-position (margin zloc))
        new-gap (+ old-gap delta)
        padding (if (or (< new-gap 1)
                        (and max-gap (> new-gap max-gap)))
                  (- 1 old-gap)
                  delta)]
    (if (zero? padding)
      [zloc 0]
      (-> zloc
          (update-space-left padding)
          (pad-continuation-lines padding opts realign)))))

(defn- edit-column [zloc column position opts realign]
  (loop [zloc zloc, col 0, shift 0]
    (if-some [current (skip-whitespace-and-commas zloc z/right*)]
      (if (and (:blank-lines-separate-alignment? opts) (end-of-column-group? current))
        current
        (let [discarded? (uneval? current)
              [current shift] (cond
                                (line-break? current) [current 0]
                                (and (= col column) (not discarded?))
                                (pad-to-position current position opts realign)
                                :else (pad-continuation-lines current shift opts realign))
              col (cond
                    (line-break? current) 0
                    discarded? col
                    :else (inc col))]
          (if-some [right (z/right* current)]
            (recur right col (long shift))
            current)))
      zloc)))

(defn- column-start-position
  [start col {align-width    :max-column-alignment-width
              align-singles? :align-single-column-lines?
              separate?      :blank-lines-separate-alignment?}]
  (let [collector (fn [zloc c position]
                    (if (and (= c (dec col))
                             (or align-singles?
                                 (not (single-column-line? zloc start))))
                      (let [end (column-end-position zloc)]
                        (if (or (nil? align-width) (<= end align-width))
                          (max position end)
                          position))
                      position))]
    (inc (reduce-columns start collector 0 separate?))))

(defn- first-aligned-form [zloc skip]
  (loop [zloc (z/down zloc), skip skip]
    (cond
      (nil? zloc) nil
      (uneval? zloc) (recur (z/right zloc) skip)
      (pos? skip) (recur (z/right zloc) (dec skip))
      :else zloc)))

(defn- align-one-column
  [zloc col skip {:keys [blank-lines-separate-alignment?] :as opts} realign]
  (if-some [start (first-aligned-form zloc skip)]
    (loop [start start]
      (let [position (column-start-position start col opts)
            end (edit-column start col position opts realign)]
        (if-let [next-group (when (and blank-lines-separate-alignment?
                                       (end-of-column-group? end))
                              (z/right* end))]
          (recur next-group)
          (z/up end))))
    zloc))

(defn- align-columns
  ([zloc opts]
   (align-columns zloc 0 opts identity))
  ([zloc skip opts realign]
   (if (meta? zloc)
     (-> zloc z/down z/right (align-columns skip opts realign) z/up)
     (reduce #(align-one-column %1 %2 skip opts realign)
             zloc
             (-> zloc (first-aligned-form skip) count-columns range rest)))))

(defn- last-descendant [zloc depth]
  (if-let [child (z/down* zloc)]
    (recur (z/rightmost* child) (inc depth))
    [zloc depth]))

(defn- align-form-tree [form opts alignment-offset]
  ;; Measure parents after their children have reached their final widths.
  (letfn [(align-subtree [start]
            (loop [[zloc depth] (last-descendant start 0)]
              (let [zloc (if-some [skip (when (n/inner? (z/node zloc))
                                          (alignment-offset zloc))]
                           (align-columns zloc skip opts align-subtree)
                           zloc)]
                (if (zero? depth)
                  zloc
                  (if-let [left (z/left* zloc)]
                    (recur (last-descendant left depth))
                    (recur [(z/up* zloc) (dec depth)]))))))]
    (-> form z/of-node root align-subtree z/root)))

(defn align-map-columns
  ([form]
   (align-map-columns form default-options))
  ([form opts]
   (align-form-tree form opts #(when (z/map? %) 0))))

(defn- matching-form-index? [zloc [k indexes] context]
  (if (= :all indexes)
    (and (or (z/list? zloc)
             (= (z/tag zloc) :fn))
         (form-matches-key? (z/down zloc) k context))
    (and (z/list? (z/up zloc))
         (form-matches-key? zloc k context)
         (contains? (set indexes) (dec (index-of zloc))))))

(defn- matching-form? [zloc form-indexes context]
  (and (or (z/list? zloc)
           (= (z/tag zloc) :fn)
           (z/list? (z/up zloc)))
       (some #(matching-form-index? zloc % context) form-indexes)))

(defn- form-alignment-offset [zloc aligned-forms context]
  (when (or (#{:list :fn} (z/tag zloc)) (z/list? (z/up zloc)))
    (some (fn [[k rule]]
            (if (or (= :all rule) (= :block (first rule)))
              (when (and (#{:list :fn} (z/tag zloc))
                         (form-matches-key? (z/down zloc) k context))
                (inc (if (= :all rule) 0 (second rule))))
              (when (matching-form-index? zloc [k rule] context) 0)))
          aligned-forms)))

(defn- alignment-context [form opts]
  {:alias-map (:alias-map opts)
   :refer-map (:refer-map opts)
   :ns-name (or (::ns-name opts) (find-namespace (z/of-node form)))})

(defn align-form-columns [form aligned-forms opts]
  (let [context (alignment-context form opts)]
    (align-form-tree form opts #(form-alignment-offset % aligned-forms context))))

(defn- reader-conditional-body? [zloc]
  (and (z/list? zloc) (reader-conditional? (z/up* zloc))))

(defn align-reader-conditionals
  ([form]
   (align-reader-conditionals form default-options))
  ([form opts]
   (align-form-tree form opts #(when (reader-conditional-body? %) 0))))

(defn- align-selected-columns [form aligned-forms opts]
  (let [context (alignment-context form opts)
        offset (fn [zloc]
                 (or (when (and (:align-map-columns? opts) (z/map? zloc)) 0)
                     (when (and (:align-reader-conditionals? opts)
                                (reader-conditional-body? zloc))
                       0)
                     (when (:align-form-columns? opts)
                       (form-alignment-offset zloc aligned-forms context))))]
    (align-form-tree form opts offset)))

(defn realign-form
  "Realign a rewrite-clj form such that the columns line up into columns."
  ([form]
   (realign-form form default-options))
  ([form opts]
   (-> form z/of-node (align-columns opts) z/root)))

(defn- unalign-from-space [zloc]
  (pad-node (z/right* zloc) (- 1 (node-str-length zloc))))

(defn unalign-form
  "Remove any consecutive non-indenting whitespace within the form."
  [form]
  (-> form z/of-node z/down
      (edit-all non-indenting-whitespace? unalign-from-space z/right*)
      z/root))

(defn- blank-line-in-form? [zloc blank-line-forms context]
  (and (z/linebreak? zloc)
       (> (count-newlines zloc) 1)
       (not (z/map? (z/up zloc)))
       (not (root? (z/up zloc)))
       (not (matching-form? (z/up zloc) blank-line-forms context))))

(defn- replace-with-single-newline [zloc]
  (z/replace zloc (n/newline-node "\n")))

(defn remove-blank-lines-in-forms [form blank-line-forms opts]
  (let [ns-name     (or (::ns-name opts) (find-namespace (z/of-node form)))
        context     {:alias-map (:alias-map opts)
                     :refer-map (:refer-map opts)
                     :ns-name ns-name}
        blank-line? #(blank-line-in-form? % blank-line-forms context)]
    (transform form edit-all blank-line? replace-with-single-newline)))

#?(:clj
   (defn- ns-require-form? [zloc]
     (and (some-> zloc top-level-form ns-form?)
          (some-> zloc z/child-sexprs first (= :require)))))

#?(:clj
   (defn- as-keyword? [zloc]
     (and (= :token (z/tag zloc))
          (= :as (z/sexpr zloc)))))

#?(:clj
   (defn- refer-keyword? [zloc]
     (and (= :token (z/tag zloc))
          (= :refer (z/sexpr zloc)))))

#?(:clj
   (defn- symbol-node? [zloc]
     (some-> zloc z/node n/symbol-node?)))

#?(:clj
   (defn- leftmost-symbol [zloc]
     (some-> zloc z/leftmost (z/find (comp symbol-node? skip-meta)))))

#?(:clj (defn- ns-require-form-parent [grandparent-node]
          (when-not (ns-require-form? grandparent-node)
            (when (or (z/vector? grandparent-node)
                      (z/list? grandparent-node))
              (some-> (z/find (-> grandparent-node z/down skip-meta)
                              (comp skip-meta z/right)
                              symbol-node?)
                      z/sexpr)))))

#?(:clj (defn- join-ns-str [parent-namespace current-ns]
          (if parent-namespace
            (format "%s.%s" parent-namespace current-ns)
            (str current-ns))))

#?(:clj
   (defn- refer-zloc->refer-mapping [refer-zloc]
     (let [refers           (some-> refer-zloc
                                    (z/find-next (comp skip-meta z/right)
                                                 (some-fn z/vector? z/list?))
                                    z/sexpr)
           current-ns       (some-> refer-zloc leftmost-symbol z/sexpr)
           grandparent-node (some-> refer-zloc
                                    (z/find-next z/up (complement meta?))
                                    (z/find-next z/up (complement meta?)))
           parent-ns        (ns-require-form-parent grandparent-node)]
       (when (and (sequential? refers) (symbol? current-ns))
         (let [ns-str (join-ns-str parent-ns current-ns)]
           (->> refers (map (fn [sym] [(str sym) ns-str])) (into {})))))))

#?(:clj (defn- refer-map-for-form [form]
          (when-let [req-zloc (-> form z/of-node (z/find z/next ns-require-form?))]
            (->> (find-all req-zloc refer-keyword?)
                 (map refer-zloc->refer-mapping)
                 (apply merge)))))

#?(:clj
   (defn- as-zloc->alias-mapping [as-zloc]
     (let [alias            (some-> as-zloc
                                    (z/find-next (comp skip-meta z/right)
                                                 symbol-node?)
                                    z/sexpr)
           current-ns       (some-> as-zloc leftmost-symbol z/sexpr)
           grandparent-node (some-> as-zloc
                                    (z/find-next z/up (complement meta?))
                                    (z/find-next z/up (complement meta?)))
           parent-ns        (ns-require-form-parent grandparent-node)]
       (when (and (symbol? alias) (symbol? current-ns))
         {(str alias) (join-ns-str parent-ns current-ns)}))))

#?(:clj
   (defn- alias-map-for-form [form]
     (when-let [req-zloc (-> form z/of-node (z/find z/next ns-require-form?))]
       (->> (find-all req-zloc as-keyword?)
            (map as-zloc->alias-mapping)
            (apply merge)))))

(defn- stringify-map [m]
  (into {} (map (fn [[k v]] [(str k) (str v)])) m))

(defn reformat-form
  "Reformats a rewrite-clj form data structure. Accepts a map of
  [formatting options][1]. See also: [[reformat-string]].

  [1]: https://github.com/weavejester/cljfmt#formatting-options"
  ([form]
   (reformat-form form {}))
  ([form options]
   (let [opts      (merge default-options options)
         indents   (merge (:indents opts) (:extra-indents opts))
         aligned   (merge (:aligned-forms opts) (:extra-aligned-forms opts))
         blank     (merge (:blank-line-forms opts)
                          (:extra-blank-line-forms opts))
         alias-map #?(:clj  (merge (alias-map-for-form form)
                                   (stringify-map (:alias-map opts)))
                      :cljs (stringify-map (:alias-map opts)))
         refer-map #?(:clj  (merge (refer-map-for-form form)
                                   (stringify-map (:refer-map opts)))
                      :cljs (stringify-map (:refer-map opts)))
         ns-name   (find-namespace (z/of-node form))
         opts      (assoc opts :refer-map refer-map :alias-map alias-map
                          ::ns-name ns-name)]
     (-> form
         (cond-> (:sort-ns-references? opts)
           sort-ns-references)
         (cond-> (:split-keypairs-over-multiple-lines? opts)
           split-keypairs-over-multiple-lines)
         (cond-> (:remove-consecutive-blank-lines? opts)
           remove-consecutive-blank-lines)
         (cond-> (:remove-surrounding-whitespace? opts)
           remove-surrounding-whitespace)
         (cond-> (:insert-missing-whitespace? opts)
           insert-missing-whitespace)
         (cond-> (:remove-multiple-non-indenting-spaces? opts)
           remove-multiple-non-indenting-spaces)
         (cond-> (:indentation? opts)
           (reindent indents opts))
         (cond-> (or (:align-map-columns? opts)
                     (:align-form-columns? opts)
                     (:align-reader-conditionals? opts))
           (align-selected-columns aligned opts))
         (cond-> (:remove-trailing-whitespace? opts)
           remove-trailing-whitespace)
         (cond-> (:remove-blank-lines-in-forms? opts)
           (remove-blank-lines-in-forms blank opts))))))

(defn reformat-string
  "Reformat a string. Accepts a map of [formatting options][1].

  [1]: https://github.com/weavejester/cljfmt#formatting-options"
  ([form-string]
   (reformat-string form-string {}))
  ([form-string options]
   (-> (p/parse-string-all form-string)
       (reformat-form options)
       n/string
       (cond-> (:normalize-newlines-at-file-end? options)
         normalize-newlines-at-file-end))))

(def default-line-separator
  #?(:clj (System/lineSeparator) :cljs \newline))

(defn normalize-newlines [s]
  (str/replace s #"\r\n" "\n"))

(defn replace-newlines [s sep]
  (str/replace s #"\n" sep))

(defn find-line-separator [s]
  (or (re-find #"\r?\n" s) default-line-separator))

(defn wrap-normalize-newlines [f]
  (fn [s]
    (let [sep (find-line-separator s)]
      (-> s normalize-newlines f (replace-newlines sep)))))
