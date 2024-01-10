;; solution_optimized.clj

(defn calc_diffs [history] 
  (def diff_pairs (mapv vector (rest history) history)) ;; for a vector [a b c d], rest will return [b c d] i.e. the subvector excl the first element a/the head.
  ;; by mapping to vector we find the difference pairs [ [a b], [b c], [c d] ]; this is similar to zip in python.
  ;; we can then just apply reduce on each pair to find (b-a, c-b, d-c)
  ;;(def diffs (mapv #(reduce - %) diff_pairs))
 ;;(println diffs)
  (map #(reduce - %) diff_pairs))


(defn predict_value_after [history]
  (loop [hist history accum 0]
    ;;(println accum) 
    (if (every? zero? hist) accum ;; base case all elements eq to zero, return the sum of tails.
      (recur (calc_diffs hist)  (+ accum (last hist))) ;; the extrapolated value is equal to the sum of recursive tails;
    )
  )
)

(defn predict_value_before [history]
  (loop [hist history accum 0 op +] ;; op is the operation to be done in this recusion instance;
;    (println accum)
    (if (every? zero? hist) accum
      (recur (calc_diffs hist)  (op accum (first hist)) (if (= op +) - +)) ;; the extrapolated value is equal to the sum with alternating signs of the recursive heads.
    )
  )
)


;; I/O Functions

(defn line_to_vector [numbers_string] ;; read line into a vector of numbers.
       (vec (map read-string (clojure.string/split numbers_string #" "))))

(defn process_puzzle_line [line last?];; extrapolate head or tail depending on the last? boolean. If true, tail value is extrapolated.
  ((if (true? last?) predict_value_after predict_value_before)(line_to_vector line)))

(defn process_puzzle_file [file-path last?] ;; lazily read the puzzle input line by line and extrapolate values for each line saving them in a sequence. Return that sequence.
  (->> (clojure.java.io/reader file-path)
       (line-seq)
       (mapv #(process_puzzle_line % last?))))



;; Sample Solution
(def sample_histories [[0 3 6 9 12 15] [1 3 6 10 15 21] [10 13 16 21 30 45]])

;;(print (predict_value_after (get sample_histories 2)))
(print "[SAMPLE] The sum of the extrapolated values is: Part 1: ")
(print (reduce + (map predict_value_after sample_histories)))

(print "\t Part 2: ")
(println (reduce + (map predict_value_before sample_histories)))
;;(println (map predict_value_before sample_histories))
;;(println (predict_value_before (get sample_histories 0)))
;;(println (find_prev_value (get sample_histories 0)))


;; PART 1  
(def next_values (process_puzzle_file "puzzle_input.txt" true))
(print "[PART1] The sum of the extrapolated values is: ")
(println (reduce + next_values))


;; PART 2
(def prev_values (process_puzzle_file "puzzle_input.txt" false))
(print "[PART2] The sum of the extrapolated values is: ")
(println (reduce + prev_values))


