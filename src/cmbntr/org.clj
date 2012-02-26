(ns  ^{:doc "org-mode outlining utility"
       :author "Michael Locher <cmbntr@gmail.com>"}
  cmbntr.org
  (:require [clojure.zip :as zip]))

;; Body

(defn make-body [& contents]
  (with-meta
    (vec contents)
    {:type :outline}))

;; Outline-Zipper

(defprotocol OutlineZippable
  (zip-branch? [node])
  (zip-children [node])
  (zip-make-node [node children]))

(extend-protocol OutlineZippable
  clojure.lang.Sequential
  (zip-branch?   [s] true)
  (zip-children  [s] (seq s))
  (zip-make-node [_ children] (make-body children)))

(defn outline-zip [root]
  (zip/zipper zip-branch? zip-children zip-make-node root))

;; Printing protocol

(def ^:dynamic *outline-level* 0)

(defprotocol OutlinePrintable
  (emit-outline [x]))

(defn print-outline [o]
  (emit-outline o))

(extend-protocol OutlinePrintable

  clojure.lang.Sequential
  (emit-outline [s]
    (doseq [x s]
      (emit-outline x)))

  java.lang.CharSequence
  (emit-outline [cs]
    (println cs))
  
  nil
  (emit-outline [_]))

;; Headings

(defprotocol OutlineHeading
  (heading-title [h])
  (heading-tags [h])
  (heading-content [h]))

(def ^:private stars
  (memoize (fn [n] (apply str (repeat n \*)))))

(def ^:dynamic *tag-cleaner* #(.replace % \- \_))

(defn- tags-str [tags]
  (->> tags
       (map name)
       (map *tag-cleaner*)
       (interpose \:)
       (apply str)
       (format ":%s:")))

(defn- print-headline [h]
  (binding [*outline-level* (inc *outline-level*)]
    (print (stars *outline-level*))
    (print \space)
    (print (heading-title h))
    (if-let [tags (seq (heading-tags h))]
      (println \space (tags-str tags))
      (println))
    (emit-outline (heading-content h))))

(defrecord Heading [title tags content]

  OutlineZippable
  (zip-branch? [this] true)
  (zip-children [this] content)
  (zip-make-node [this children] (Heading. title tags (make-body children)))

  OutlineHeading
  (heading-title [this] (if (sequential? title)
                          (->> title (map name) (interpose \space) (apply str))
                          (name title)))
  (heading-tags [this] tags)
  (heading-content [this] content)
  
  OutlinePrintable
  (emit-outline [this] (print-headline this)))

;; Drawers

(defn- print-drawer [name lines]
  (println (str \: name \:))
  (doseq [l lines]
    (println l))
  (println ":END:"))

(defrecord PropertiesDrawer [props]
  OutlinePrintable
  (emit-outline [this]
    (print-drawer "PROPERTIES"
                  (for [[k v] props] (format ":%s: %s" (name k) v)))))

;; Checklists

(defn- print-checklist [checklist]
  (doseq [[k v] (:checks checklist)]
    (println (format " [%s] %s"
                     (if v \X \space)
                     (name k)))))

(defrecord Checklist [checks]
  OutlinePrintable
  (emit-outline [this]
    (print-checklist this)))

;; Constructor functions

(defn make-outline [& contents]
  (outline-zip (apply make-body contents)))

(defn make-checklist [checks]
  (Checklist. checks))

(defn make-properties-drawer [props]
  (PropertiesDrawer. props))

(defn make-heading
  ([title] (make-heading title nil nil))
  ([title tags] (make-heading title tags nil))
  ([title tags content] (Heading. title tags content))
  ([title tags props content]
     (make-heading title tags
                   (cons (make-properties-drawer props) content))))

(defn make-settings [heading-title props]
  (let [settings (-> (make-heading heading-title [:noexport])
                     (outline-zip))]
    
    (-> (reduce (fn [s [k v]] (zip/append-child s (format "#+%S: %s" (name k) v)))
                settings props)
        zip/root)))

(comment

  (-> (make-outline)
      (zip/append-child (make-heading "EPIC"))
      (zip/down)
      (zip/rightmost)

      (zip/append-child (make-heading [:TODO "Story 1"]))
      (zip/down)
      (zip/rightmost)
      (zip/append-child (make-properties-drawer {:foo 99, :bar 42}))
      (zip/append-child (make-checklist [[:Foo true] [:Bar nil] [:Baz  11]]))
      (zip/append-child (make-heading "Task 1"))
      (zip/up)
      
      (zip/append-child (make-heading "Story 2" #{:foo} {:a 3 :b 8} ["Hello World"]))
      (zip/down)
      (zip/rightmost)
      (zip/append-child "some text")
      (zip/append-child (make-heading "Task 2"))

      (zip/up)
      (zip/up)
      (zip/append-child
       (make-settings "Settings"
                      (partition 2
                                 [:title "outline title"
                                  :startup "showall logdone"
                                  :options "^:nil creator:nil email:nil d:t"
                                  :todo "TODO(o) | DONE(d)"
                                  :todo "PENDING(p) | DISCARDED(x) SUBMITTED(s)"])))
      (zip/root)
      emit-outline)

  )