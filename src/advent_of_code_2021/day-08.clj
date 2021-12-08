(ns advent-of-code-2021.day-08)

(def task
  (->> (slurp "resources/day-08.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map #(remove #{"|"} %))))

(defn sol-1
  [task]
  (->> (map #(take-last 4 %) task)
       (map #(filter (fn [segments] (#{2 3 4 7} (count segments))) %))
       (map count)
       (reduce +)))

(defn map-to-numbers
  [digits]
  (let [digits-set (map set digits)
        segments-freqs (->> (reduce str digits)
                            (frequencies))
        map-seqs-freqs-to-nums {42 0
                                17 1
                                34 2
                                39 3
                                30 4
                                37 5
                                41 6
                                25 7
                                49 8
                                45 9}
        numbers (->> digits-set
                     (map (fn [segments]
                            (->> (map segments-freqs segments)
                                 (reduce +))))
                     (map map-seqs-freqs-to-nums))]
    (->> (range 10)
         (map #(hash-map (nth digits-set %) (nth numbers %)))
         (reduce merge))))

(defn sol-2
  [task]
  (->> task
       (map (fn [in]
              (let [m (map-to-numbers (take 10 in))
                    nums-set (map set (take-last 4 in))]
                (->> (map m nums-set)
                     (map str)
                     (reduce str)
                     (#(Integer/parseInt %))))))
       (reduce +)))

(comment
  (sol-1 task) ;=> 530 
  (sol-2 task) ;=> 1051087
  )