(def dirs (list (list [0 1] (fn [xy n] (= (second xy) n)))
                (list [-1 0] (fn [xy n] (= (first xy) (- n))))
                (list [0 -1] (fn [xy n] (= (second xy) (- n))))
                (list [1 0] (fn [xy n] (= (first xy) (inc n))))))

(def adjacents '([1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]))

(defn total-adjacents [m x]
  (->> adjacents
       (map #(map + x %))
       (map #(get-in m % 0))
       (apply +)
       (max 1)))

(defn inc-prev [m x]
  (->> adjacents
       (map #(map + x %))
       (map #(get-in m % 0))
       (apply max)
       inc))

(defn l1-distance [mp xy]
  (apply + (map #(if (pos? %) % (- %)) xy)))

(defn iter [top f ret]
  (loop [mp {}
         xy [0 0]
         dirs-loc (list (last dirs))
         n 0]
    (let [val (f mp xy)
          new-xy (map + xy (ffirst dirs-loc))
          [new-dirs-loc new-n] (if ((second (first dirs-loc)) new-xy n)
                                   (if (= 1 (count dirs-loc))
                                       [dirs (inc n)]
                                       [(rest dirs-loc) n])
                                   [dirs-loc n])]
      (if (>= val top)
          (ret mp xy)
          (recur (assoc-in mp xy val) new-xy new-dirs-loc new-n
             )))))

(= 0 (iter 1 inc-prev l1-distance))
(= 3 (iter 12 inc-prev l1-distance))
(= 2 (iter 23 inc-prev l1-distance))
(= 31 (iter 1024 inc-prev l1-distance))
(iter 325489 inc-prev l1-distance)

(iter 325489 total-adjacents total-adjacents)
