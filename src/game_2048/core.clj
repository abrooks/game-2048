(ns game-2048.core)

(def direction {:left  0
                :up    1
                :right 2
                :down  3})

(defn new-board
  ([] (new-board 4))
  ([size]
     (with-meta
       (vec (repeat size (vec (repeat size 0))))
       {:size size})))

(defn add-random
  ([board] (add-random board 0.9))
  ([board prob]
     (let [size (-> board meta :size)
           blanks (for [x (range size), y (range size)
                        :when (zero? (get-in board [x y]))]
                    [x y])
           loc (rand-nth blanks)
           val (if (< (rand) prob)
                 2
                 4)]
       (assoc-in board loc val))))

(defn rotate-board [board n]
  (if (zero? (rem n (count direction)))
    board
    (rotate-board
     (->
      (apply mapv vector (reverse board))
      (with-meta (meta board)))
     (dec n))))

(defn collapse-row [row]
  (->> row
       (remove zero?)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn collapse-board [board]
  (->
   (mapv #(->> (concat (collapse-row %) (repeat 0))
               (take (-> board meta :size))
               vec)
         board)
   (with-meta (meta board))))

(defn move [board dir]
  (-> board
      (rotate-board (direction dir))
      (collapse-board)
      (rotate-board (- (count direction) (direction dir)))))

(defn init-board
  ([] (init-board 4))
  ([size]
     (->
      (new-board size)
      (add-random)
      (add-random))))

(defn print-board [board]
  (let [size (-> board meta :size)]
    (doseq [x (reverse (range size))]
      (doseq [y (range size)]
        (printf "%5d" (get-in board [x y])))
      (println ))))
