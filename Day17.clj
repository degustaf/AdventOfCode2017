(defn spinlock [n step]
  (loop [ls '(0)
         idx 0
         i 1]
    (if (> i n)
        ls
        (let [loc (mod (+ idx step) i)
              end (take-last (- i loc 1) ls)
              start (take (inc loc) ls)]
          (recur
            (apply conj (conj end i) (reverse start))
            (inc loc)
            (inc i))))))

(defn follow [n step]
  (second (drop-while #(not= % n) (spinlock n step))))

(defn spinlock2 [n step]
  (loop [sec 0
         idx 0
         i 1]
    (if (> i n)
        sec
        (let [loc (mod (+ idx step) i)]
          (recur
            (if (= loc 0) i sec)
            (inc loc)
            (inc i))))))

(= (spinlock 1 3) '(0 1))
(= (spinlock 2 3) '(0 2 1))
(= (spinlock 3 3) '(0 2 3 1))
(= (spinlock 4 3) '(0 2 4 3 1))
(= (spinlock 5 3) '(0 5 2 4 3 1))
(= (spinlock 6 3) '(0 5 2 4 3 6 1))
(= (spinlock 7 3) '(0 5 7 2 4 3 6 1))
(= (spinlock 8 3) '(0 5 7 2 4 3 8 6 1))
(= (spinlock 9 3) '(0 9 5 7 2 4 3 8 6 1))

(follow 9 3)

(follow 2017 382)

(= (spinlock2 1 3) 1)
(= (spinlock2 2 3) 2)
(= (spinlock2 3 3) 2)
(= (spinlock2 4 3) 2)
(= (spinlock2 5 3) 5)
(= (spinlock2 6 3) 5)
(= (spinlock2 7 3) 5)
(= (spinlock2 8 3) 5)
(= (spinlock2 9 3) 9)

; This runs Out of Memory ?!?!?!
(spinlock2 50000000 382)
