(defn snd [x]
  (try
    (let [n (Integer. x)]
      (fn [[m & ls]]
        (conj ls (update (assoc m :rcv n) :idx inc))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]]
        (conj ls (update (assoc m :rcv (get m x 0)) :idx inc))))))

(defn set [x y]
  (try
    (let [n (Integer. y)]
      (fn [[m & ls]]
        (conj ls (update (assoc m x n) :idx inc))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]]
        (conj ls (update (assoc m x (get m y 0)) :idx inc))))))

(defn add [x y]
  (try
    (let [n (Integer. y)]
      (fn [[m & ls]]
        (conj ls (update (update m x + n) :idx inc))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]]
        (conj ls (update (update m x + (get m y 0)) :idx inc))))))

(defn mul [x y]
  (try
    (let [n (Integer. y)]
      (fn [[m & ls]]
        (conj ls (assoc m x (* (get m x 0) n) :idx (inc (m :idx))))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]]
        (conj ls (update (update m x * (get m y 0)) :idx inc))))))

(defn mod [x y]
  (try
    (let [n (Integer. y)]
      (fn [[m & ls]]
        (conj ls (update (update m x clojure.core/mod n) :idx inc))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]]
        (conj ls (update (update m x clojure.core/mod (get m y 0)) :idx inc))))))

(defn rcv [x]
  (try
    (let [n (Integer. x)]
      (if (> n 0)
          (fn [[m & ls]] (get m :rcv 0))
          (fn [[m & ls]] (conj ls (update m :idx inc)))))
    (catch
      NumberFormatException ex
      (fn [[m & ls]] (if (> (get m x) 0)
                  (get m :rcv 0)
                  (conj ls (update m :idx inc)))))))

(defn jgz [x y]
  (try
    (let [n (Integer. x)]
      (if (> n 0)
          (try
            (let [val (Integer. y)]
              (fn [[m & ls]] (conj ls (update m :idx + val))))
            (catch
              NumberFormatException ex
              (fn [[m & ls]] (conj ls (update m :idx + (get m y))))))
          (fn [[m & ls]] (conj ls (update m :idx inc)))))
    (catch
      NumberFormatException ex
      (try
        (let [val (Integer. y)]
          (fn [[m & ls]] (if (> (get m x 0) 0)
                      (conj ls (update m :idx + val))
                      (conj ls (update m :idx inc)))))
        (catch
          NumberFormatException ex
          (fn [[m & ls]] (if (> (get m x 0) 0)
                      (conj ls (update m :idx + (get m y 0)))
                      (conj ls (update m :idx inc)))))))))

(defn compile [cmd]
  (let [[inst & args] (clojure.string/split cmd #"\s+")]
    (apply (resolve (symbol inst)) args)))

(defn run [data]
  (let [prog (vec (map compile (clojure.string/split-lines data)))
        len (count prog)]
    (loop [m (list {:idx 0})]
      (if (list? m)
          (if (< ((first m) :idx) len)
              (recur ((prog ((first m) :idx)) m))
              (first m))
          m))))

(= ((compile "set a 1") (list {:idx 0}))
   (list {"a" 1 :idx 1}))

(= ((compile "add a 2") (list {"a" 1 :idx 1}))
   (list {"a" 3 :idx 2}))

(= ((compile "mul a a") (list {"a" 3 :idx 2}))
   (list {"a" 9 :idx 3}))

(= ((compile "mod a 5") (list {"a" 9 :idx 3}))
   (list {"a" 4 :idx 4}))

(= ((compile "snd a") (list {"a" 4 :idx 4}))
   (list {"a" 4 :idx 5 :rcv 4}))

(= ((compile "set a 0") (list {"a" 4 :idx 5 :rcv 4}))
   (list {"a" 0 :idx 6 :rcv 4}))

(= ((compile "rcv a") (list {"a" 0 :idx 6 :rcv 4}))
   (list {"a" 0 :idx 7 :rcv 4}))

(= ((compile "jgz a -1") (list {"a" 0 :idx 7 :rcv 4}))
   (list {"a" 0 :idx 8 :rcv 4}))

(= ((compile "set a 1") (list {"a" 0 :idx 8 :rcv 4}))
   (list {"a" 1 :idx 9 :rcv 4}))

(= ((compile "jgz a -2") (list {"a" 1 :idx 9 :rcv 4}))
   (list {"a" 1 :idx 7 :rcv 4}))

(= ((compile "jgz a -1") (list {"a" 1 :idx 7 :rcv 4}))
   (list {"a" 1 :idx 6 :rcv 4}))

(= ((compile "rcv a") (list {"a" 1 :idx 6 :rcv 4}))
   4)

(= (run "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2") 4)

(def data
  "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 735
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19")

(run data)

(defn snd [x]
  (try
    (let [n (Integer. x)]
      (fn [[m q-snd q-rcv]]
        (list (update m :idx inc) (conj q-snd n) q-rcv)))
    (catch
      NumberFormatException ex
      (fn [[m q-snd q-rcv]]
        (list (update m :idx inc) (conj q-snd (get m x 0)) q-rcv)))))

(defn rcv [x]
  (fn [[m q-snd q-rcv]]
    (if (empty? q-rcv)
        (list m q-snd q-rcv)
        (list (assoc m x (peek q-rcv) :idx (inc (m :idx))) q-snd (pop q-rcv)))))

(defn rcv-instructions [data]
  (clojure.core/set (keep-indexed
                      #(if (= (first (clojure.string/split %2 #"\s+")) "rcv") %1)
                      (clojure.string/split-lines data))))

(defn run [data]
  (let [prog (vec (map compile (clojure.string/split-lines data)))
        rcv-inst (rcv-instructions data)
        len (count prog)]
    (loop [m0 {:idx 0 "p" 0}
           m1 {:idx 0 "p" 1}
           q0to1 clojure.lang.PersistentQueue/EMPTY
           q1to0 clojure.lang.PersistentQueue/EMPTY
           cnt 0]
      (if (and (empty? q0to1)
               (empty? q1to0)
               (contains? rcv-inst (m0 :idx))
               (contains? rcv-inst (m1 :idx)))
          cnt
          (if (or (= (m0 :idx) len)
                  (= (m1 :idx) len))
              cnt
              (let [[m0 q0to1 q1to0] ((prog (m0 :idx)) (list m0 q0to1 q1to0))
                    [m1 new-q1to0 q0to1] ((prog (m1 :idx)) (list m1 q1to0 q0to1))
                    cnt (if (= q1to0 new-q1to0) cnt (inc cnt))]
                (recur m0 m1 q0to1 new-q1to0 cnt)))))))

(= ((compile "snd a")
             (list {"a" 4 :idx 4}
                   clojure.lang.PersistentQueue/EMPTY
                   clojure.lang.PersistentQueue/EMPTY))
   (list {"a" 4 :idx 5}
         (conj clojure.lang.PersistentQueue/EMPTY 4)
         clojure.lang.PersistentQueue/EMPTY))

(= ((compile "rcv a")
             (list {"a" 0 :idx 6}
                   clojure.lang.PersistentQueue/EMPTY
                   (conj clojure.lang.PersistentQueue/EMPTY 4)))
   (list {"a" 4 :idx 7}
         clojure.lang.PersistentQueue/EMPTY
         clojure.lang.PersistentQueue/EMPTY))

(= ((compile "rcv a")
             (list {"a" 0 :idx 6}
                   clojure.lang.PersistentQueue/EMPTY
                   clojure.lang.PersistentQueue/EMPTY))
   (list {"a" 0 :idx 6}
         clojure.lang.PersistentQueue/EMPTY
         clojure.lang.PersistentQueue/EMPTY))

(rcv-instructions "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(run "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(run "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(run data)
