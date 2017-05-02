;; Download from https://clojure.org/community/downloads
;; alias clj="java -cp ~/Code/clojure-1.8.0/clojure-1.8.0.jar:. clojure.main"
;; $ clj -m envelopes envelopes.clj
;; or 
;; $ brew install leiningen
;; $ lein repl
;; user=> (load-file "envelopes.clj")
;; user=> (envelopes/-main [])

(ns envelopes)

(def NUM_TRIALS 10000)
(def PRIOR_LOWER_MAX 100)

(defn get-trial-values 
  "Helper to generate the [picked, other] list for a given run."
  [envelope lower_value]
    (let [values [lower_value (* 2 lower_value)]]
      (if (== envelope 0) values (reverse values))))

;; Returns the result of a single trial. We switch if the value is below the
;; cutoff.
(defn single-trial [cutoff]
  (let [values (get-trial-values (rand-int 2) (rand PRIOR_LOWER_MAX))]
    (if (>= (first values) cutoff) (first values) (second values))))

;; Returns the total value of all of the trials for a given cutoff level.       
(defn get-multi-trial-total [cutoff]
  (loop [trial 0 total 0]
    (if (>= trial NUM_TRIALS)
      total
      (recur (+ trial 1) (+ total (single-trial cutoff))))))
    
;; Computes the average value among many trials.  
(defn multi-trial [cutoff] (/ (get-multi-trial-total cutoff) NUM_TRIALS))

;; Prints the expected value for each possible cutoff.
(defn -main [& args]
  (loop [cutoff 0]
    (if (> cutoff (* 2 PRIOR_LOWER_MAX))
      ()
      (let [expected_value (multi-trial cutoff)]
        (printf "cutoff=%s, expected_value=%s\n" cutoff expected_value)
        (recur (+ cutoff 1))))))
