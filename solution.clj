;; solution.clj
(def sample_histories [[0 3 6 9 12 15] [1 3 6 10 15 21] [10 13 16 21 30 45]])

(def history0 (get sample_histories 0))

(defn except_first [v] ;; return a subvector excluding the first element
  (subvec v 1 (count v))
)


(defn calc_diffs [history]
  (def diff_pairs (mapv vector (except_first history) history))
;;  (def diffs (mapv #(reduce - %) diff_pairs))
;; (println diffs)
  (mapv #(reduce - %) diff_pairs)
)

(defn find_next_value [history]
  (cond
    (every? zero? history) 0
    :else (+ (last history) (find_next_value (calc_diffs history)))
  )
)

(defn find_prev_value [history]
  (cond
    (every? zero? history) 0
    :else (- (first history) (find_prev_value (calc_diffs history)))
  )
)

;; Sample Solution
(print "[SAMPLE] The sum of the extrapolated values is: part1: ")
(print (reduce + (map find_next_value sample_histories)))
(print " part2: ")
(println (reduce + (map find_prev_value sample_histories)))


(defn line_to_vector [numbers_string]
       (vec (map read-string (clojure.string/split numbers_string #" ")))
)
;; PART 1 - Read in the whole puzzle input

(defn process-line [line next?]
  (cond 
    (true? next?) (find_next_value (line_to_vector line))
    :else (find_prev_value (line_to_vector line))    
  ))

(defn process-file [file-path next?]
  (->> (clojure.java.io/reader file-path)
       (line-seq)
       (map #(process-line % next?))))

(def next_values (process-file "puzzle_input.txt" true))
(print "[PART1] The sum of the extrapolated values is: ")
(println (reduce + next_values))


;; PART 2
(def prev_values (process-file "puzzle_input.txt" false))
(print "[PART2] The sum of the extrapolated values is: ")
(println (reduce + prev_values))


