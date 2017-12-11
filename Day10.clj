(defn mod-add [n & ls] (mod (apply + ls) n))

(defn part-rev [vs n pos len]
  (if (< len 2)
      vs
      (let [i (mod-add n pos len -1)]
        (recur (assoc vs pos (vs i) i (vs pos))
             n
             (mod-add n pos 1)
             (- len 2)))))

(= (part-rev [0 1 2 3 4] 5 0 3) [2 1 0 3 4])
(= (part-rev [2 1 0 3 4] 5 3 4) [4 3 0 1 2])
(= (part-rev [4 3 0 1 2] 5 3 1) [4 3 0 1 2])
(= (part-rev [4 3 0 1 2] 5 1 5) [3 4 2 1 0])

(defn knot-hash-step
  ([n lx] (knot-hash-step n (vec (range n)) 0 0 lx))
  ([n ls pos skip lx]
      (loop [ls ls
             pos pos
             skip skip
             x (first lx)
             lx (next lx)]
        (if (nil? x)
            (list ls pos skip)
            (recur (part-rev ls n pos x)
                   (mod-add n pos skip x)
                   (inc skip)
                   (first lx)
                   (next lx))))))

(defn knot-hash-simple [lx] (apply * (take 2 (first (knot-hash-step 256 lx)))))


(= '(2 1 0 3 4) (first (knot-hash-step 5 '(3))))
(= '(4 3 0 1 2) (first (knot-hash-step 5 '(3 4))))
(= '(4 3 0 1 2) (first (knot-hash-step 5 '(3 4 1))))
(= '(3 4 2 1 0) (first (knot-hash-step 5 '(3 4 1 5))))

(knot-hash-simple '(76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229))

(defn knot-hash-advanced [msg]
  (let [lx (concat (map int msg) '(17 31 73 47 23))
        ret (loop [i 64
                   ls (vec (range 256))
                   pos 0
                   skip 0]
              (if (= i 0)
                  ls
                  (let [[ls pos skip] (knot-hash-step 256 ls pos skip lx)]
                    (recur (dec i) ls pos skip))))
        ret (loop [in ret
                   ret []]
              (if (= '() in)
                  ret
                  (recur (nthrest in 16)
                         (conj ret (apply bit-xor (take 16 in))))))]
    (clojure.string/join "" (map #(format "%02x" %) ret))))

(= (knot-hash-advanced "") "a2582a3a0e66e6e86e3812dcb672a272")
(= (knot-hash-advanced "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd")
(= (knot-hash-advanced "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d")
(= (knot-hash-advanced "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e")

(knot-hash-advanced "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229")
