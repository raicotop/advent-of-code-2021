(ns advent-of-code-2021.day-10)

(def task
  (->> (slurp "resources/day-10.txt")
       (clojure.string/split-lines)))

(def pairs
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(def score-1
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def score-2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn total-score-2
  [xs]
  (->> (map score-2 xs)
       (reduce (fn [a b] (+ (* a 5) b)) 0)))

(defn rows->score
  [empty-fn unexpected-ch-fn rows]
  (map (fn [in]
         (loop [to-process in
                expected []]
           (if (empty? to-process)
             (empty-fn expected)
             (let [ch (first to-process)
                   ch-opening? (contains? (set (keys pairs)) ch)
                   to-process' (drop 1 to-process)]
               (if ch-opening?
                 (recur to-process' (conj expected (pairs ch)))
                 (let [expected-ch (peek expected)]
                   (if (= ch expected-ch)
                     (recur to-process' (pop expected))
                     (unexpected-ch-fn ch)))))))) rows))

(defn sol-1
  [task]
  (->> task
       (rows->score (fn [_] 0) score-1)
       (reduce +)))

(defn sol-2
  [task]
  (->> task
       (rows->score (fn [expected] (total-score-2 (reverse expected))) (fn [_] 0))
       (remove #{0})
       (sort)
       (#(nth % (/ (- (count %) 1) 2)))))

(comment
  (sol-1 task) ;=> 436497
  (sol-2 task) ;=> 2377613374
  )