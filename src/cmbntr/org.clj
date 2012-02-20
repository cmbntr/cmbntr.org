(ns cmbntr.org
  (:require [clojure.zip :as zip]))

(defprotocol OutlineZippable
  (zip-branch? [node])
  (zip-children [node])
  (zip-make-node [node children]))

(defn outline-zip [root]
  (zip/zipper zip-branch? zip-children zip-make-node root))

(extend-protocol OutlineZippable
  clojure.lang.Sequential
  (zip-branch?   [s] true)
  (zip-children  [s] (seq s))
  (zip-make-node [_ children] (vec children)))

(defn make-outline []
  (outline-zip []))

(defprotocol OutlinePrintable
  (emit-outline [x]))

(def ^:dynamic *outline-level* 0)

(defn print-outline [o]
  (emit-outline o))

(defn- println-outline [cs]
  (println cs))

(extend-protocol OutlinePrintable

  clojure.lang.Sequential
  (emit-outline [s]
    (doseq [x s]
      (emit-outline x)))

  java.lang.CharSequence
  (emit-outline [cs]
    (println-outline cs))
  
  nil
  (emit-outline [_]))

(defprotocol OutlineHeading
  (heading-title [h])
  (heading-tags [h])
  (heading-content [h]))

(defn- stars [n]
  (if (pos? n)
    (str \* (stars (dec n)))))

(defn- print-headline [h]
  (binding [*outline-level* (inc *outline-level*)]
    (print (stars *outline-level*))
    (print \space)
    (println (heading-title h))
    (emit-outline (heading-content h))))


(defrecord Heading [title tags content]

  OutlineZippable
  (zip-branch? [this] true)
  (zip-children [this] content)
  (zip-make-node [this children] (Heading. title tags (vec children)))

  OutlineHeading
  (heading-title [this] title)
  (heading-tags [this] tags)
  (heading-content [this] content)
  
  OutlinePrintable
  (emit-outline [this] (print-headline this)))

(defn make-heading [title tags content]
  (Heading. title tags content))

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

(defn- print-checklist [checklist]
  (doseq [[k v] (:checks checklist)]
    (println (format " [%s] %s"
                     (if v \X \space)
                     (name k)))))

(defrecord Checklist [checks]
  OutlinePrintable
  (emit-outline [this]
    (print-checklist this)))

(comment

  (-> (make-outline)
      (zip/append-child (Heading. "EPIC" nil nil))
      (zip/down)
      (zip/rightmost)

      (zip/append-child (Heading. "Story 1" nil nil))
      (zip/down)
      (zip/rightmost)
      (zip/append-child (PropertiesDrawer. {:foo 99, :bar 42}))
      (zip/append-child (Checklist. [[:Foo true] [:Bar nil] [:Baz  11]]))
      (zip/append-child (Heading. "Task 1" nil nil))
      (zip/up)
 
      (zip/append-child (Heading. "Story 2" nil nil))
      (zip/down)
      (zip/rightmost)
      (zip/append-child "some text")
      (zip/append-child (Heading. "Task 2" nil nil))
      (zip/up)
      
      zip/root
      emit-outline)
  
)