(ns advent-of-code-2021.day-06)

(def task
  (->> (slurp "resources/day-06.txt")
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (frequencies)))

(defn sol
  [task day-limit]
  (loop [day 0
         population task]
    (if (= day day-limit)
      (->> (vals population)
           (reduce +))
      (let [population' (->> (range 9)
                             (map #(hash-map % (if (= 8 %)
                                                 (get population 0 0)
                                                 (if (= 6 %)
                                                   (+ (get population 7 0)
                                                      (get population 0 0))
                                                   (get population (inc %) 0)))))
                             (reduce into))]
        (recur (inc day) population')))))

(defn sol-1
  [task]
  (sol task 80))

(defn sol-2
  [task]
  (sol task 256))

(comment
  (sol-1 task) ;=> 345793
  (sol-2 task) ;=> 1572643095893 
  )