(ns advent-of-code-2021.day-04)

(def task
  (->> (slurp "resources/day-04.txt")
       (clojure.string/split-lines)
       (remove empty?)
       (#(hash-map
          :numbers 
          (->> (clojure.string/split (first %) #",")
               (map (fn [num-str] (Long/parseLong num-str))))
          :cards
          (->> (rest %)
               (map (fn [row-string]
                      (->> (clojure.string/split row-string #" ")
                           (remove empty?)
                           (map (fn [num-str] (Long/parseLong num-str))))))
               (partition 5))))))

(defn card-rows->rows-and-columns
  [rows]
  (let [columns (->> (range 5)
                     (map (fn [i] 
                            (map #(nth % i) rows))))]
  (->> [rows columns]
       (reduce into)
       (map set))))

(defn sol
  [task cards-left]
  (let [all-cards (map card-rows->rows-and-columns (:cards task))]
    (loop [cards all-cards
           numbers (:numbers task)]
      (let [number (first numbers)
            cards-new (->> cards
                           (map (fn [card]
                                  (map #(disj % number) card))))
            winner (->> cards-new
                        (filter #(contains? (set %) #{}))
                        (first))]
        (if winner
          (if (= cards-left (count cards))
            (->> (reduce into winner)
                 (reduce +)
                 (* number))
            (recur (remove (fn [card] (contains? (set card) #{})) cards-new) (rest numbers)))
          (recur cards-new (rest numbers)))))))

(defn sol-1
  [task]
  (sol task 100))

(defn sol-2
  [task]
  (sol task 1))

(comment
  (sol-1 task) ;=> 58412
  (sol-2 task) ;=> 10030
  )