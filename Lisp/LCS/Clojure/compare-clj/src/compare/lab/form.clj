(ns compare.lab.form
  "Tokenize a Form for Sequential Comparison.

  Port of `lab/compare-form.lisp`.  The CL tokens `|(|` and `|)|` become
  symbols named \"(\" and \")\"."
  (:refer-clojure :exclude [subseq]))

(def lparen (symbol "("))
(def rparen (symbol ")"))

(defn tokenize-form
  "Tokenize the form."
  [form]
  (if (sequential? form)
    (concat [lparen] (apply concat (map tokenize-form form)) [rparen])
    (list form)))

(defn basic-form-tokens
  "Build a sub-form out of a list of tokens.
  Returns `[form remaining-tokens]`."
  [tokens]
  (loop [token-remains (seq tokens)
         form '()]
    (if (empty? token-remains)
      [(reverse form) '()]
      (let [token (first token-remains)]
        (cond
          (= token lparen)
          (let [[subform rest-tokens] (basic-form-tokens (next token-remains))]
            ;; `rest-tokens` still includes the closing `)`; consume it, as the
            ;; CL `do` step form does.
            (recur (next rest-tokens) (cons subform form)))

          (= token rparen)
          [(reverse form) token-remains]

          :else
          (recur (next token-remains) (cons token form)))))))

(defn form-tokens
  "Build a form out of a list of tokens.
  Returns `[form remaining-tokens]`."
  [tokens]
  (let [[form remainder] (basic-form-tokens tokens)
        form-length (count form)]
    (if (and (empty? remainder) form-length (= form-length 1))
      [(first form) remainder]
      [form remainder])))
