(defn snd [x]
  (try
    (let [n (Integer. x)]
      (fn [m]
        (update (assoc m :rcv n) :idx inc)))
    (catch
      NumberFormatException ex
      (fn [m]
        (update (assoc m :rcv (get m x 0)) :idx inc)))))

(defn set [x y]
  (try
    (let [n (Integer. y)]
      (fn [m]
        (update (assoc m x n) :idx inc)))
    (catch
      NumberFormatException ex
      (fn [m]
        (update (assoc m x (get m y 0)) :idx inc)))))

(defn add [x y]
  (try
    (let [n (Integer. y)]
      (fn [m]
        (update (update m x + n) :idx inc)))
    (catch
      NumberFormatException ex
      (fn [m]
        (update (update m x + (get m y 0)) :idx inc)))))

(defn mul [x y]
  (try
    (let [n (Integer. y)]
      (fn [m]
        (assoc m x (* (get m x 0) n) :idx (inc (m :idx)))))
    (catch
      NumberFormatException ex
      (fn [m]
        (update (update m x * (get m y 0)) :idx inc)))))

(defn mod [x y]
  (try
    (let [n (Integer. y)]
      (fn [m]
        (update (update m x clojure.core/mod n) :idx inc)))
    (catch
      NumberFormatException ex
      (fn [m]
        (update (update m x clojure.core/mod (get m y 0)) :idx inc)))))

(defn rcv [x]
  (try
    (let [n (Integer. x)]
      (if (> n 0)
          (fn [m] (get m :rcv 0))
          (fn [m] (update m :idx inc))))
    (catch
      NumberFormatException ex
      (fn [m] (if (> (get m x) 0)
                  (get m :rcv 0)
                  (update m :idx inc))))))

(defn jgz [x y]
  (try
    (let [n (Integer. x)]
      (if (> n 0)
          (try
            (let [val (Integer. y)]
              (fn [m] (update m :idx + val)))
            (catch
              NumberFormatException ex
              (fn [m] (update m :idx + (get m y)))))
          (fn [m] (update m :idx inc))))
    (catch
      NumberFormatException ex
      (try
        (let [val (Integer. y)]
          (fn [m] (if (> (get m x 0) 0)
                      (update m :idx + val)
                      (update m :idx inc))))
        (catch
          NumberFormatException ex
          (fn [m] (if (> (get m x 0) 0)
                      (update m :idx + (get m y 0))
                      (update m :idx inc))))))))

(defn compile [cmd]
  (let [[inst & args] (clojure.string/split cmd #"\s+")]
    (apply (resolve (symbol inst)) args)))

(defn run [data]
  (let [prog (vec (map compile (clojure.string/split-lines data)))
        len (count prog)]
    (loop [m {:idx 0}]
      (if (map? m)
          (if (< (m :idx) len)
              (recur ((prog (m :idx)) m))
              m)
          m))))

(= ((compile "set a 1") {:idx 0})
   {"a" 1 :idx 1})

(= ((compile "add a 2") {"a" 1 :idx 1})
   {"a" 3 :idx 2})

(= ((compile "mul a a") {"a" 3 :idx 2})
   {"a" 9 :idx 3})

(= ((compile "mod a 5") {"a" 9 :idx 3})
   {"a" 4 :idx 4})

(= ((compile "snd a") {"a" 4 :idx 4})
   {"a" 4 :idx 5 :rcv 4})

(= ((compile "set a 0") {"a" 4 :idx 5 :rcv 4})
   {"a" 0 :idx 6 :rcv 4})

(= ((compile "rcv a") {"a" 0 :idx 6 :rcv 4})
   {"a" 0 :idx 7 :rcv 4})

(= ((compile "jgz a -1") {"a" 0 :idx 7 :rcv 4})
   {"a" 0 :idx 8 :rcv 4})

(= ((compile "set a 1") {"a" 0 :idx 8 :rcv 4})
   {"a" 1 :idx 9 :rcv 4})

(= ((compile "jgz a -2") {"a" 1 :idx 9 :rcv 4})
   {"a" 1 :idx 7 :rcv 4})

(= ((compile "jgz a -1") {"a" 1 :idx 7 :rcv 4})
   {"a" 1 :idx 6 :rcv 4})

(= ((compile "rcv a") {"a" 1 :idx 6 :rcv 4})
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

(run "set i 31
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
