(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))
  
(defn my-last [coll]
  (if (and (not (empty? coll)) (not (singleton? coll)))
    (my-last (rest coll))
    (first coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
         seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
      (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
      :else
       true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
     ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
     ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq)) 
    :else a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)) 
    :else false))       

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) ()
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) '()
    (<= up-to 1) '(0)
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [a-seq n]
  (if (zero? n)
    '()
    (let [rotated (concat (rest a-seq) [(first a-seq)])]
      (cons a-seq (rotations-helper rotated (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn my-unfrequencies-helper [unfreqs a-map]
  (if (empty? a-map)
    unfreqs
    (let [new-unfreqs (concat (repeat (last (first a-map)) (first (first a-map))) unfreqs)]
      (my-unfrequencies-helper new-unfreqs (rest a-map)))))

(defn un-frequencies [a-map]
  (my-unfrequencies-helper () a-map))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [nhalve (int (/ (count a-seq) 2))]
    (vector (my-take nhalve a-seq) (my-drop nhalve a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (>= (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    ))

(defn merge-sort [a-seq]
  (let [[left right] (halve a-seq)]
    (if (or (empty? a-seq) (= 1 (count a-seq)))
      a-seq
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

