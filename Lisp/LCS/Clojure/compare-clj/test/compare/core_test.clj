(ns compare.core-test
  "clojure.test suite for the Compare port, mirroring compare-test.lisp."
  (:require [clojure.test :refer [deftest is testing]]
            [compare.test :as ct]
            [compare.face :as face]
            [compare.body :as body]
            [compare.delta :as delta]
            [compare.lab.dynamic :as dynamic]
            [compare.lab.space :as space]
            [compare.lab.form :as form]))

(deftest diagnose-compare-suite
  (testing "The full diagnose-compare suite passes"
    (is (nil? (ct/diagnose-compare)))))

(deftest hunt-szymanski-example
  (testing "The Hunt-Szymanski (1977) worked example"
    (is (= "abbd" (face/common-sequence "abcbdda" "badbabd" :symmetry nil)))))

(deftest symmetric-example
  (testing "The symmetric example"
    (is (= "oooo oooo"
           (face/common-sequence "aooboocoo dooeoo" "foogoo hooioo")))))

(deftest normalizer-options
  (testing ":ignore-whitespace and :ignore-case-and-style"
    (is (= '("zero" "one" "two " "Three")
           (face/common-sequence
            '("zero" "ONE" " two" " THREE ")
            '("zero" "one" "two " "Three")
            :ignore-whitespace true :ignore-case-and-style true)))
    (is (= '("zero")
           (face/common-sequence
            '("zero" "ONE" " two" " THREE ")
            '("zero" "one" "two " "Three")
            :ignore-whitespace nil :ignore-case-and-style nil)))))

(deftest affix-intervals
  (testing "Affix options yield the expected correspondence/difference"
    (is (= '((d) () ())
           (mapv first (face/correspondence-sequences
                        '(m a d e i n) '(t a i w a n) :prefix -1 :suffix 1))))
    (is (= '((a) (e i) (n))
           (mapv first (face/difference-sequences
                        '(m a d e i n) '(t a i w a n) :prefix -1 :suffix 1))))))

(deftest lab-algorithms-agree
  (testing "Hunt-Szymanski, DP, and Hirschberg agree on LCS length"
    (let [a "abcbdda" b "badbabd"
          main (:lcs-length (body/basic-common-pairs a b :method :any))
          dyn (:lcs-length (dynamic/basic-common-pairs a b :method :any))
          hir (:lcs-length (space/basic-common-pairs a b :method :any))]
      (is (= 4 main dyn hir)))))

(deftest form-tokenizer-roundtrip
  (testing "Form tokenizer round-trips nested forms"
    (is (= '(+ 1 (* 2 3))
           (first (form/form-tokens (form/tokenize-form '(+ 1 (* 2 3)))))))
    (is (= '(a (b (c d)) e)
           (first (form/form-tokens (form/tokenize-form '(a (b (c d)) e))))))))

(deftest delta-formatter
  (testing "Delta formatter emits a delta-file form with an edit"
    (let [sw (java.io.StringWriter.)]
      (delta/compare-as-delta true "abc" "abd" :output-stream sw)
      (let [form (read-string (str sw))]
        (is (= :delta-file (first form)))
        (is (= :delta-edit (ffirst (second form))))))))

(deftest common-pairs-shape
  (testing "common-pairs returns matched pairs and length pair"
    (let [{:keys [pairs length-pair]} (face/common-pairs "abc" "abc")]
      (is (= [[0 0] [1 1] [2 2]] pairs))
      (is (= [3 3] length-pair)))))

(deftest improbable-absorption-deterministic
  (testing "A distinct-alphabet short sequence is exactly the LCS of itself and a supersequence"
    (let [short (vec (range 20))
          long (ct/supersequence short 800 (range 100 120))
          forward (face/common-sequence short long :symmetry nil)
          reverse (face/common-sequence long short :symmetry nil)]
      (is (= 800 (count long)))
      (is (= short forward))
      (is (= short reverse)))))
