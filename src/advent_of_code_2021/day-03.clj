(ns advent-of-code-2021.day-03)

(def task
  (->> (slurp "resources/day-03.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #""))))

(defn- compute-result
  [xs]
  (->> (map #(reduce str %) xs)
       (map #(Integer/parseInt % 2))
       (reduce *)))

(defn sol-1
  [task]
  (let [column-freqs (->> (range (count (first task)))
                          (map #(map (fn [bits] (nth bits %)) task))
                          (map (fn [bits] (frequencies bits))))
        pick (fn [pred] (map #(if (pred (get % "0") (get % "1")) "0" "1") column-freqs))
        gamma (pick >)
        epsilon (pick <)]
    (compute-result [gamma epsilon])))

(defn sol-2
  [task]
  (let [pick-or (fn [pred default]
                  (loop [n 0
                         result task]
                    (if (= 1 (count result))
                      (first result)
                      (let [nth-bits (->> (map #(nth % n) result)
                                          (frequencies))
                            bit (if (reduce = (vals nth-bits))
                                  default
                                  (if (pred (get nth-bits "0") (get nth-bits "1")) "0" "1"))]
                        (recur (inc n) (filter #(= bit (nth % n)) result))))))
        oxygen (pick-or > "1")
        co2 (pick-or < "0")]
    (compute-result [oxygen co2])))

(comment
  (sol-1 task) ;=> 1071734
  (sol-2 task) ;=> 6124992
  )