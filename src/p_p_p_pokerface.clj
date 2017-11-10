(ns p-p-p-pokerface)

(def face-cards
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get face-cards fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)(pair? hand)))

(defn two-pairs? [hand]
  (let [rank-count (reverse (sort (vals (frequencies (map rank hand)))))]
    (or
      (and (= 2 (first rank-count))
           (= 2 (second rank-count)))
      (= 4 (first rank-count)))))

(defn straight? [hand]
  (let [high-ace-hand (sort (map rank hand))
        low-ace-hand (sort(replace {14 1} (map rank hand)))
        high-ace-range (range (first high-ace-hand) (+ (first high-ace-hand) 5))
        low-ace-range (range (first low-ace-hand) (+ (first low-ace-hand) 5))]
   (or (= high-ace-hand high-ace-range)
       (= low-ace-hand low-ace-range))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands (map (fn [x] (if ((first x) hand) (second x) nil)) checkers)
        values (filter (fn [x] (not (nil? x))) hands)]
    (apply max values)))