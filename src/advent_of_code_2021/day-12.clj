(ns advent-of-code-2021.day-12)

(def task
  (->> (slurp "resources/day-12.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"-"))
       (map (fn [[a b]] {a #{b} b #{a}}))
       (reduce (fn [a b] (merge-with into a b)))))

(defn lower-case?
  [str]
  (= str (clojure.string/lower-case str)))

(defn recursive
  [task path limit]
  (let [last (peek path)]
    (if (= "end" last)
      (clojure.string/join "," path)
      (if (or (->> (filter lower-case? path)
                   (frequencies)
                   ((fn [freqs] (= (+ limit (count freqs))
                                   (reduce + (vals freqs))))))
              (= (get (frequencies path) "start" 0) 2))
        nil
        (->> (for [next (task last)]
               (recursive task (conj path next) limit))
             (remove #(= nil (clojure.core/last %))))))))

(defn sol-limit
  [task limit]
  (->> (recursive task ["start"] limit)
       (flatten)
       (count)))

(defn sol-1
  [task]
  (sol-limit task 1))

(defn sol-2
  [task]
  (sol-limit task 2))

(comment
  (sol-1 task) ;=> 3738
  (sol-2 task) ;=> 120506
  )