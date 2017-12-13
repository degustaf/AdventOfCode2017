(defn parse-data [data]
  (map (fn [s] (map #(Integer. %) (clojure.string/split s #": ")))
       (clojure.string/split-lines data)))

(defn traverse-firewall [start data]
  (reduce (fn [acc [loc rng]] (if (= (mod (+ loc start) (* 2 (dec rng))) 0)
                                  (+ acc (* loc rng))
                                  acc))
          0
          (parse-data data)))

(defn safe-traversal [data]
  (first (reduce (fn [ret [loc rng]]
                     (filter #(not= 0 (mod (+ loc %) (* 2 (dec rng)))) ret))
                 (range)
                 (parse-data data))))

(= 24 (traverse-firewall 0
"0: 3
1: 2
4: 4
6: 4"))

(= 0 (traverse-firewall 10
"0: 3
1: 2
4: 4
6: 4"))

(= 10 (safe-traversal
"0: 3
1: 2
4: 4
6: 4"))

(def data
"0: 4
1: 2
2: 3
4: 5
6: 6
8: 6
10: 4
12: 8
14: 8
16: 9
18: 8
20: 6
22: 6
24: 8
26: 12
28: 12
30: 12
32: 10
34: 8
36: 8
38: 10
40: 12
42: 12
44: 12
46: 14
48: 14
50: 14
52: 14
54: 12
56: 12
58: 12
60: 12
62: 14
64: 14
66: 14
68: 14
70: 14
80: 14
82: 14
86: 14
88: 17
94: 30
98: 18")

(traverse-firewall 0 data)
;(safe-traversal data)
