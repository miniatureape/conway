(ns conway.core
  :gen-class)

(defn empty-board
  "Creates a rectangular empty board of the specified width
  and height"
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] 
    [(+ dx x ) (+ dy y)]))

(defn count-neighbors 
  [board loc]
  (count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
  "Yields the next state of the board, using indeices to determine neigbors, livenews etc"
  [board] (let [w (count board)
                h (count (first board))]
            (loop [new-board board x 0 y 0]
              (cond
                (>= x w) new-board
                (>= y h) (recur new-board (inc x) 0)
                :else
                (let [new-liveness
                         (case (count-neighbors board [x y])
                           2 (get-in board [x y])
                           3 :on
                            nil)]
                    (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(get-in [[01] [0 3] [0 2]] [1 1])
(assoc-in [[01] [0 3] [0 2]] [1 1] 5)

(defn -main
  "I run the glider game"
  [& args]
  (-> (iterate indexed-step glider) (nth 8) pprint))
