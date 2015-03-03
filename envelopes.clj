(def NUM_TRIALS 10000)
(def PRIOR_LOWER_MAX 100)

(defn single-trial [cutoff]
    (let [lower_value (rand PRIOR_LOWER_MAX)
          higher_value (* 2 lower_value)
          envelope (rand-int 2)]
          (if (== envelope 0)
              (if (>= lower_value cutoff) lower_value higher_value)
              (if (>= higher_value cutoff) higher_value lower_value))))
        
(defn multi-trial [cutoff]
    (/ 
        (loop [trial 0 total 0]
            (if (>= trial NUM_TRIALS)
                total
                (recur (+ trial 1) (+ total (single-trial cutoff)))))
        NUM_TRIALS))
        
(defn main []
    (loop [cutoff 0]
        (if (> cutoff (* 2 PRIOR_LOWER_MAX))
            ()
            (let [expected_value (multi-trial cutoff)]
                (printf "cutoff=%s, expected_value=%s\n" cutoff expected_value)
                (recur (+ cutoff 1))))))
                
(main)
