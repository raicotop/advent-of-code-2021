(ns advent-of-code-2021.day-14)

(def task
  (->> (slurp "resources/day-14.txt")
       (clojure.string/split-lines)
       (partition-by #{""})
       ((fn [[a _ b]]
          (vector
           (first a)
           (->> (map #(clojure.string/split % #" -> ") b)
                (map #(hash-map (seq (first %)) (first (seq (second %)))))
                (reduce merge)))))))

(defn sol
  [task steps]
  (let [rules (second task)
        letter-combs (keys rules)
        fs (loop [step 0
                  res [(->> letter-combs
                            (map #(hash-map % (frequencies %)))
                            (reduce merge))]]
             (if (= steps step)
               (last res)
               (let [res' (->> letter-combs
                               (map (fn [x] (->> (vector [(first x) (rules x)] [(rules x) (second x)])
                                                 (map #(get (last res) %))
                                                 (reduce (fn [a b] (merge-with + a b)))
                                                 (merge-with + {(rules x) -1})
                                                 (hash-map x))))
                               (reduce merge))]
                 (recur (inc step) (conj res res')))))]
    (->> (first task)
         (partition 2 1)
         (map fs)
         (reduce (fn [a b] (merge-with + a b)))
         (merge-with + (->> (first task)
                            (rest)
                            (drop-last)
                            (frequencies)
                            (map (fn [[k v]] {k (- v)}))
                            (apply merge)))
         (vals)
         (#(vector (reduce max %) (reduce min %)))
         (reduce -))))

(defn sol-1
  [task]
  (sol task 10))

(defn sol-2
  [task]
  (sol task 40))

(comment
  (sol-1 task) ;=> 2170
  (sol-2 task) ;=> 2422444761283
  )