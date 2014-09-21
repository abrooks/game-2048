(ns game-2048.core)

(def direction {:left  0
                :right 2
                :up    1
                :down  3})

(defn new-board
  ([] (new-board 4))
  ([size]
     (into (with-meta {} {:size size})
           (for [x (range size)
                 y (range size)]
             [[x y] 0]))))


(defn print-board [board]
  (let [size (-> board meta :size)]
    (doseq [y (range size)]
      (doseq [x (range size)]
        (print " " (get board [x (- size (inc y))])))
      (println))))

(defn add-random
  ([board] (add-random board 0.9))
  ([board prob]
     (let [blanks (filter (fn [[p v]]
                            (zero? v))
                          board)
           [loc _] (rand-nth blanks)
           val (if (< (rand) prob)
                 2
                 4)]
       (assoc board loc val))))

(defn rotate-board [board n]
  (if (zero? (rem n 4 #_rotations))
    board
    (rotate-board
     (into board
           (map (fn [[[x y] v]]
                  (let [Y (- (-> board meta :size) (inc y))]
                    [[Y x] v]))
                board))
     (dec n))))

(defn collapse-row [row]
  (->> row
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn collapse-board [board]
  (let [size (-> board meta :size)]
    (->>
     (for [y (range size)]
       (map-indexed #(vector [% y] %2)
                    (collapse-row
                     (for [x (range size)
                           :let [v (get board [x y])]
                           :when (not (zero? v))]
                       v))))
     (apply concat)
     (into (new-board size)))))

(defn move [board dir]
  (-> board
      (rotate-board (direction dir))
      (collapse-board)
      (rotate-board (- 4 #_rotations (direction dir)))))

(defn init-board
  ([] (init-board 4))
  ([size]
     (->
      (new-board size)
      (add-random)
      (add-random))))
