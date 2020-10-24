;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session1.txt when OutOfMemoryException occurred

(require 'clojure-rte.rte-tester)
(clojure-rte.rte-tester/-main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session2.txt when no OutOfMemoryException
;; occurred when calling canonicalize-pattern on the same input that
;; caused the OOM during repl-session1.txt, but did not when that was
;; the only parameter ever given to canonicalize-pattern.

(require 'clojure-rte.rte-tester)
(require '[clojure-rte.rte-core :as co])
(def maybe-troublesome-pattern '(:cat :epsilon (:not (:and :empty-set (:cat :epsilon))) (:or clojure.lang.IMeta :empty-set (:* (:or (:and)))) (:and :sigma (:+ :sigma) (:cat (:+ (:cat)) :sigma))))

(def prev-stats (atom nil))
(def same-stats (atom 0))

(defn repeat-same-call [pattern n]
  (reset! prev-stats nil)
  (reset! same-stats 0)
  (dotimes [i n]
    (co/reset-stats!)
    (doall (co/canonicalize-pattern pattern))
    (let [s (co/get-stats)]
      (if (= @prev-stats s)
        (swap! same-stats inc)
        (do
          (println "i=" i " stats different than previous:")
          (co/print-stats pattern)
          (reset! prev-stats s)))))
  (println "same stats count=" @same-stats))

(repeat-same-call maybe-troublesome-pattern 500)
