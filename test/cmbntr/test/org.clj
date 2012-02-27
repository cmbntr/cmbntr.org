(ns cmbntr.test.org
  (:use [cmbntr.org]
        [clojure.test]))

;; Body

(deftest body-wrap-unwrap-zipper
  (is (= [23 42] (top (make-body 23 42)))))

(deftest body-append-child
  (let [b (-> (make-body 0 1)
              (append-child 2))]
    (is (= [0 1 2] (top b)))))

(deftest body-editing
  (let [b (-> (make-body)
              (append-child  1)
              (prepend-child 0)
              (append-child  2))]
    (is (= [0 1 2] (top b)))))

;; Headings

(deftest heading-wrap-unwrap-zipper
  (let [h (make-heading "foo")]
    (is (= h (top h)))))

(deftest heading-append-child
  (let [h (make-heading "parent")
        c "child"
        edited (-> h
                   (append-child c)
                   top)]
    (is (= [c] (heading-content edited)))))