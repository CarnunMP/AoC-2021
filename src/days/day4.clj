(ns days.day4
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]

            [utils :refer [read-string-seq]]))

;; Part 1

(def dummy-input '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
                    ""
                    "22 13 17 11  0"
                    "8  2 23  4 24"
                    "21  9 14 16  7"
                    "6 10  3 18  5"
                    "1 12 20 15 19"
                    ""
                    "3 15  0  2 22"
                    "9 18 13 17  5"
                    "19  8  7 25 23"
                    "20 11 10 24  4"
                    "14 21 16 12  6"
                    ""
                    "14 21 17 24  4"
                    "10 16 15  9 19"
                    "18  8 23 26 20"
                    "22 11 13  6  5"
                    "2  0 12  3  7"))

(defn- parse-input [input]
  {:draws (str/split (first input) #",")
   :boards (->> (rest input)
                (partition-by str/blank?)
                (remove #(= 1 (count %)))
                (map (fn [board]
                       (vec (map #(->> (str/split % #"\s+")
                                       (remove str/blank?)
                                       vec)
                                 board))))
                vec)})

(defn assoc-draw->board-id->position [{:keys [boards] :as m}]
  (assoc m :draw->board-id->position
         (->> boards
              (map-indexed (fn [id board]
                             (map-indexed (fn [i row]
                                            (map-indexed (fn [j n]
                                                           [n [id #{[i j]}]]) row))
                                          board)))
              (apply concat)
              (apply concat)
              (group-by first)
              (map (fn [[n n->positions]]
                     [n (into {} (map second n->positions))]))
              (into {}))))

(comment
  (pprint (parse-input dummy-input))
  ;{:draws
  ; ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24" "10" "16"
  ;  "13" "6" "15" "25" "12" "22" "18" "20" "8" "19" "3" "26" "1"],
  ; :boards
  ; [[["22" "13" "17" "11" "0"]
  ;   ["8" "2" "23" "4" "24"]
  ;   ["21" "9" "14" "16" "7"]
  ;   ["6" "10" "3" "18" "5"]
  ;   ["1" "12" "20" "15" "19"]]
  ;  [["3" "15" "0" "2" "22"]
  ;   ["9" "18" "13" "17" "5"]
  ;   ["19" "8" "7" "25" "23"]
  ;   ["20" "11" "10" "24" "4"]
  ;   ["14" "21" "16" "12" "6"]]
  ;  [["14" "21" "17" "24" "4"]
  ;   ["10" "16" "15" "9" "19"]
  ;   ["18" "8" "23" "26" "20"]
  ;   ["22" "11" "13" "6" "5"]
  ;   ["2" "0" "12" "3" "7"]]]}
  (pprint (assoc-draw->board-id->position (parse-input dummy-input)))
  ;:draw->board-id->position
  ;{"9" {0 #{[2 1]}, 1 #{[1 0]}, 2 #{[1 3]}},
  ; "3" {0 #{[3 2]}, 1 #{[0 0]}, 2 #{[4 3]}},
  ; "22" {0 #{[0 0]}, 1 #{[0 4]}, 2 #{[3 0]}},
  ; "26" {2 #{[2 3]}},
  ; "4" {0 #{[1 3]}, 1 #{[3 4]}, 2 #{[0 4]}},
  ; "8" {0 #{[1 0]}, 1 #{[2 1]}, 2 #{[2 1]}},
  ; "14" {0 #{[2 2]}, 1 #{[4 0]}, 2 #{[0 0]}},
  ; "21" {0 #{[2 0]}, 1 #{[4 1]}, 2 #{[0 1]}},
  ; "20" {0 #{[4 2]}, 1 #{[3 0]}, 2 #{[2 4]}},
  ; "19" {0 #{[4 4]}, 1 #{[2 0]}, 2 #{[1 4]}},
  ; "17" {0 #{[0 2]}, 1 #{[1 3]}, 2 #{[0 2]}},
  ; "25" {1 #{[2 3]}},
  ; "15" {0 #{[4 3]}, 1 #{[0 1]}, 2 #{[1 2]}},
  ; "7" {0 #{[2 4]}, 1 #{[2 2]}, 2 #{[4 4]}},
  ; "5" {0 #{[3 4]}, 1 #{[1 4]}, 2 #{[3 4]}},
  ; "18" {0 #{[3 3]}, 1 #{[1 1]}, 2 #{[2 0]}},
  ; "12" {0 #{[4 1]}, 1 #{[4 3]}, 2 #{[4 2]}},
  ; "13" {0 #{[0 1]}, 1 #{[1 2]}, 2 #{[3 2]}},
  ; "24" {0 #{[1 4]}, 1 #{[3 3]}, 2 #{[0 3]}},
  ; "6" {0 #{[3 0]}, 1 #{[4 4]}, 2 #{[3 3]}},
  ; "1" {0 #{[4 0]}},
  ; "0" {0 #{[0 4]}, 1 #{[0 2]}, 2 #{[4 1]}},
  ; "11" {0 #{[0 3]}, 1 #{[3 1]}, 2 #{[3 1]}},
  ; "2" {0 #{[1 1]}, 1 #{[0 3]}, 2 #{[4 0]}},
  ; "16" {0 #{[2 3]}, 1 #{[4 2]}, 2 #{[1 1]}},
  ; "10" {0 #{[3 1]}, 1 #{[3 2]}, 2 #{[1 0]}},
  ; "23" {0 #{[1 2]}, 1 #{[2 4]}, 2 #{[2 2]}}}
  )

(defn- winning-board? [[id positions]]
  (let [rows (vals (group-by first positions))
        cols (vals (group-by second positions))]
    (when (or (some #(= 5 (count %)) rows)
              (some #(= 5 (count %)) cols))
      [id positions])))

(comment
  (winning-board? [0 #{[0 0] [0 1] [0 2] [0 3] [0 4]}]) ; => [0 #{[0 0] [0 3] [0 2] [0 4] [0 1]}]
  (winning-board? [0 #{[0 0] [1 0] [2 0] [3 0] [4 0]}]) ; => [0 #{[0 0] [1 0] [3 0] [2 0] [4 0]}]
  (winning-board? [0 #{[0 1] [0 2] [0 3] [0 4]}])) ; => nil

(defn get-winning-board-ids+positions [{:keys [draws draw->board-id->position boards] :as m} & {:keys [part] :or {part 1}}]
  (reduce
    (fn [{:keys [drawn winning-board-ids board-id->positions] :as acc} draw]
      (let [board-id->positions (apply dissoc (merge-with set/union board-id->positions (set (get draw->board-id->position draw)))
                                       (flatten winning-board-ids))
            winning-ids-and-positions (when (<= 5 (count drawn))
                                             (filter winning-board? board-id->positions))
            acc (cond-> acc
                  true (update :drawn conj draw)
                  true (assoc :board-id->positions board-id->positions)
                  (seq winning-ids-and-positions) (update :winning-board-ids conj (map first winning-ids-and-positions))
                  (seq winning-ids-and-positions) (update :winning-positions merge (into {} winning-ids-and-positions)))]
        (if (or (and (= 1 part) (seq winning-ids-and-positions))
                (and (= 2 part) (or (= (count (flatten (:winning-board-ids acc))) (count boards))
                                    (= (count (:drawn acc)) (count draws)))))
          (reduced (merge m acc))
          acc)))
    {:drawn []
     :winning-board-ids []
     :winning-positions {}
     :board-id->positions {}}
    draws))

(defn score [{:keys [boards drawn winning-board-ids winning-positions]}]
  (let [winning-board-id (first (peek winning-board-ids))
        positions (set (for [row (range 5)
                             col (range 5)]
                         [row col]))
        unmarked-num-positions (apply disj positions (get winning-positions winning-board-id))
        sum (reduce
              (fn [score [row col]]
                (+ score (Integer/parseInt (-> boards
                                               (get winning-board-id)
                                               (get row)
                                               (get col)))))
              0
              unmarked-num-positions)]
    (* sum (Integer/parseInt (peek drawn)))))

(comment
  (def a (update (assoc-draw->board-id->position (parse-input dummy-input))
                 :draws #(->> % (take 11) vec)))
  (get-winning-board-ids+positions a)
  ;{:drawn ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21"],
  ; :winning-board-id nil,
  ; :board-id->positions {2 #{[2 2] [0 0] [3 4] [4 1] [1 3] [0 2] [0 4] [3 1] [4 4] [0 1] [4 0]},
  ;                       1 #{[2 2] [1 0] [3 4] [4 1] [1 4] [1 3] [0 3] [2 4] [0 2] [3 1] [4 0]},
  ;                       0 #{[2 2] [1 1] [3 4] [1 3] [0 3] [2 4] [0 2] [2 0] [0 4] [2 1] [1 2]}}}
  (get-winning-board-ids+positions (update a :draws conj "24"))
  ;{:draws ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24"],
  ; :boards [[["22" "13" "17" "11" "0"] ["8" "2" "23" "4" "24"] ["21" "9" "14" "16" "7"] ["6" "10" "3" "18" "5"] ["1" "12" "20" "15" "19"]]
  ;          [["3" "15" "0" "2" "22"] ["9" "18" "13" "17" "5"] ["19" "8" "7" "25" "23"] ["20" "11" "10" "24" "4"] ["14" "21" "16" "12" "6"]]
  ;          [["14" "21" "17" "24" "4"] ["10" "16" "15" "9" "19"] ["18" "8" "23" "26" "20"] ["22" "11" "13" "6" "5"] ["2" "0" "12" "3" "7"]]],
  ; :draw->board-id->position {"9" {0 #{[2 1]}, 1 #{[1 0]}, 2 #{[1 3]}},
  ;                            "3" {0 #{[3 2]}, 1 #{[0 0]}, 2 #{[4 3]}},
  ;                            "22" {0 #{[0 0]}, 1 #{[0 4]}, 2 #{[3 0]}},
  ;                            "26" {2 #{[2 3]}},
  ;                            "4" {0 #{[1 3]}, 1 #{[3 4]}, 2 #{[0 4]}},
  ;                            "8" {0 #{[1 0]}, 1 #{[2 1]}, 2 #{[2 1]}},
  ;                            "14" {0 #{[2 2]}, 1 #{[4 0]}, 2 #{[0 0]}},
  ;                            "21" {0 #{[2 0]}, 1 #{[4 1]}, 2 #{[0 1]}},
  ;                            "20" {0 #{[4 2]}, 1 #{[3 0]}, 2 #{[2 4]}},
  ;                            "19" {0 #{[4 4]}, 1 #{[2 0]}, 2 #{[1 4]}},
  ;                            "17" {0 #{[0 2]}, 1 #{[1 3]}, 2 #{[0 2]}},
  ;                            "25" {1 #{[2 3]}},
  ;                            "15" {0 #{[4 3]}, 1 #{[0 1]}, 2 #{[1 2]}},
  ;                            "7" {0 #{[2 4]}, 1 #{[2 2]}, 2 #{[4 4]}},
  ;                            "5" {0 #{[3 4]}, 1 #{[1 4]}, 2 #{[3 4]}},
  ;                            "18" {0 #{[3 3]}, 1 #{[1 1]}, 2 #{[2 0]}},
  ;                            "12" {0 #{[4 1]}, 1 #{[4 3]}, 2 #{[4 2]}},
  ;                            "13" {0 #{[0 1]}, 1 #{[1 2]}, 2 #{[3 2]}},
  ;                            "24" {0 #{[1 4]}, 1 #{[3 3]}, 2 #{[0 3]}},
  ;                            "6" {0 #{[3 0]}, 1 #{[4 4]}, 2 #{[3 3]}},
  ;                            "1" {0 #{[4 0]}},
  ;                            "0" {0 #{[0 4]}, 1 #{[0 2]}, 2 #{[4 1]}},
  ;                            "11" {0 #{[0 3]}, 1 #{[3 1]}, 2 #{[3 1]}},
  ;                            "2" {0 #{[1 1]}, 1 #{[0 3]}, 2 #{[4 0]}},
  ;                            "16" {0 #{[2 3]}, 1 #{[4 2]}, 2 #{[1 1]}},
  ;                            "10" {0 #{[3 1]}, 1 #{[3 2]}, 2 #{[1 0]}},
  ;                            "23" {0 #{[1 2]}, 1 #{[2 4]}, 2 #{[2 2]}}},
  ; :drawn ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24"],
  ; :winning-board-ids [(2)],
  ; :winning-positions {2 #{[2 2] [0 0] [3 4] [4 1] [1 3] [0 3] [0 2] [0 4] [3 1] [4 4] [0 1] [4 0]}},
  ; :board-id->positions {2 #{[2 2] [0 0] [3 4] [4 1] [1 3] [0 3] [0 2] [0 4] [3 1] [4 4] [0 1] [4 0]},
  ;                       1 #{[2 2] [1 0] [3 3] [3 4] [4 1] [1 4] [1 3] [0 3] [2 4] [0 2] [3 1] [4 0]},
  ;                       0 #{[2 2] [1 1] [3 4] [1 4] [1 3] [0 3] [2 4] [0 2] [2 0] [0 4] [2 1] [1 2]}}}

  (-> dummy-input
      parse-input
      assoc-draw->board-id->position
      get-winning-board-ids+positions
      score) ; => 4512
  )

(def input (read-string-seq "day4"))

(defn result [input & {:keys [part] :or {part 1}}]
  (-> input
      parse-input
      assoc-draw->board-id->position
      (get-winning-board-ids+positions :part part)
      score))

(result input)


;; Part 2

(result dummy-input :part 2) ; => 1924
(result input :part 2)
