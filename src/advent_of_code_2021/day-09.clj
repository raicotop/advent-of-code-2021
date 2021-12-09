(ns advent-of-code-2021.day-09)

(def task
  (->> (slurp "resources/day-09.txt")
       (clojure.string/split-lines)
       (map #(map (fn [c] (Byte/parseByte (str c))) %))))

(defn adjacents
  [x y task]
  (->> [[(dec x) y]
        [x (dec y)]
        [(inc x) y]
        [x (inc y)]]
       (map (fn [[x' y']]
              (nth (nth task x' nil) y' nil)))
       (filter identity)))

(defn sol-1
  [task]
  (->> (for [x (range (count task))
             y (range (count (first task)))]
         [x y])
       (map (fn [[x y]]
              (let [n (nth (nth task x) y)
                    adjs (adjacents x y task)
                    extreme? (->> (map #(> % n) adjs)
                                  (reduce (fn [a b] (and a b))))]
                (when extreme? n))))
       (filter identity)
       (map inc)
       (reduce +)))

(defn basin
  [x y task]
  (loop [queue #{[x y]}
         res #{[x y]}]
    (if (empty? queue)
      (count res)
      (let [[x' y'] (first queue)
            n (nth (nth task x') y')
            basin-points (->> [[(dec x') y']
                               [x' (dec y')]
                               [(inc x') y']
                               [x' (inc y')]]
                              (map (fn [[x'' y'']]
                                     [[x'' y''] (nth (nth task x'' nil) y'' nil)]))
                              (filter (fn [[_ v]]
                                        (identity v)))
                              (filter (fn [[_ v]]
                                        (and (< v 9) (> v n))))
                              (map first)
                              (remove #(contains? res %)))
            queue' (apply conj (disj queue (first queue)) basin-points)
            res' (apply conj res basin-points)]
           (recur queue' res')))))

(defn sol-2
  [task]
  (->> (for [x (range (count task))
             y (range (count (first task)))]
         [x y])
       (map (fn [[x y]]
              (let [n (nth (nth task x) y)
                    adjs (adjacents x y task)
                    extreme? (->> (map #(> % n) adjs)
                                  (reduce (fn [a b] (and a b))))]
                (when extreme? (basin x y task)))))
       (filter identity)
       (sort >)
       (take 3)
       (reduce *)))

(comment
  (sol-1 task) ;=> 512
  (sol-2 task) ;=> 1600104
  )