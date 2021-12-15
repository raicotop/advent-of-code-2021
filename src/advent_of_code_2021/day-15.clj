(ns advent-of-code-2021.day-15
  (:use [clojure.data.finger-tree :as ft]))

(def task
  (->> (slurp "resources/day-15.txt")
       (clojure.string/split-lines)
       (map seq)
       (map (fn [x] (map #(Integer/parseInt (str %)) x)))))

(defn map-to-num
  [n]
  (if (< n 10)
    n
    (rem n 9)))

(def task-extended
  (->> task
       (map (fn [xs]
              (reduce concat (->> (range 5)
                                  (map (fn [n]
                                         (map #(map-to-num (+ n %)) xs)))))))
       ((fn [xs]
          (->> (range 5)
               (map (fn [n]
                      (map (fn [ys]
                             (map #(map-to-num (+ n %)) ys)) xs)))
               (reduce concat))))))

(defn nxt
  [task visited [x y]]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (fn [[x' y']] [(+ x x') (+ y y')]))
       (remove visited)
       (map (fn [[x' y']] (hash-map (nth (nth task x' nil) y' nil) #{[x' y']})))
       (map #(dissoc % nil))
       (reduce (fn [a b] (merge-with into a b)) {})))

(defn sol
  [task]
  (let [end-pos (map dec [(count task) (count (first task))])]
    (loop [queue (->> (range 9)
                      (map (fn [_] #{}))
                      (#(conj % #{[0 0]})))
           visited #{}
           steps 0]
      (let [to-process (first queue)]
        (if (contains? to-process end-pos)
          steps
          (let [add-to-process (->> (map #(nxt task visited %) to-process)
                                    (reduce (fn [m1 m2] (merge-with into m1 m2)) {}))
                queue' (->> (range 10)
                            (map #(into (nth queue %) (get add-to-process %)))
                            (rest)
                            (apply ft/double-list)
                            (#(conj % #{})))
                visited' (into visited to-process)]
            (recur queue' visited' (inc steps))))))))

(comment
  (sol task) ;=> 441
  (sol task-extended) ;=> 2849
  )