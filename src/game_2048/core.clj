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

(defn blanks [board]
  (let [size (-> board meta :size)]
    (for [x (range size), y (range size)
          :when (zero? (get-in board [x y]))]
      [x y])))

(defn add-random
  ([board] (add-random board 0.9))
  ([board prob]
     (let [loc (rand-nth (blanks board))
           val (if (< (rand) prob)
                 2
                 4)]
       (assoc-in board loc val))))

(defn rotate-board [board n]
  (if (zero? n)
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
      (println ))
    (println "-----")
    board))

(defn detect-end [board]
  (if (apply = board (map (partial move board)
                          (keys direction)))
    (println "No more moves!")
    board))

(defn play-round [board dir]
  (-> board
      (move dir)
      (as-> newboard
        (if (not= board newboard)
          (add-random newboard)
          newboard))
      (print-board)
      (detect-end)))

(defn score [board]
  (apply max (flatten board)))

(defn auto-play [board player args]
  (let [dir (player board args)
        _ (prn dir)
        res (play-round board dir)]
    (if (not res)
      (score board)
      (recur res player args))))

(defn random-player [board args]
  (rand-nth (keys direction)))

(defn noop? [board dir]
  (= board (move board dir)))

(defn game-score [board]
  (reduce + (map #(* % %) (flatten board))))

(defn monty-player [board [tries depth]]
  (let [dirs (->
              (for [d (keys direction)]
                (future (->
                         (for [t (range tries)
                               :let [p (repeatedly (- depth 1) #(rand-nth (keys direction)))
                                     r (reduce move (move board d) p)]]
                           (game-score r))
                         (->> (reduce +))
                         vector
                         (with-meta {:dir d}))))
              doall
              (->> (map deref)))]
    (if (apply = dirs)
      (or (first (remove (partial noop? board) (keys direction))) :left)
      (-> dirs sort last meta :dir))))

(comment
  (def b (atom (init-board)))
  (swap! b play-round :left)
  (swap! b play-round :up)
  (auto-play (init-board) random-player nil)
  (auto-play (init-board) monty-player [512 16])
  )
