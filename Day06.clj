(defn step [v]
  (let [len (count v)
        inc* (fn [x] (if (= len (inc x)) 0 (inc x)))
        m (apply max v)
        idx (.indexOf v m)
        ]
    (loop [v (assoc v idx 0)
           cnt m
           idx (inc* idx)]
      (if (= cnt 0)
          v
          (recur (assoc v idx (inc (v idx)))
                 (dec cnt)
                 (inc* idx))))))

(defn find-loop [v]
  (loop [visited #{v}
         v (step v)
         cnt 1]
    (if (contains? visited v)
        cnt
        (recur (conj visited v)
               (step v)
               (inc cnt)))))

(defn find-loop-length [v]
  (loop [visited {v 0}
         v (step v)
         cnt 1]
    (if (contains? visited v)
        (- cnt (visited v))
        (recur (conj visited [v cnt])
               (step v)
               (inc cnt)))))

(= (step [0 2 7 0])
   [2 4 1 2])
(= (step [2 4 1 2])
   [3 1 2 3])
(= (step [3 1 2 3])
   [0 2 3 4])
(= (step [0 2 3 4])
   [1 3 4 1])
(= (step [1 3 4 1])
   [2 4 1 2])

(= (find-loop [0 2 7 0]) 5)
(= (find-loop-length [0 2 7 0]) 4)
(find-loop [14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4])
(find-loop-length [14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4])
