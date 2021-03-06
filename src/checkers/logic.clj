(ns checkers.logic
  (:require [checkers.utils :refer :all]
            [checkers.audio :as audio]))

(defn calc-square-at [x y]
  (+ (quot (- x MARGIN) SQUARE_WIDTH)
     (* 8 (quot (- y MARGIN) SQUARE_WIDTH))))

;;user iterate on this maybe??
(defn go-direction [dir pos]
  (case dir
    :top-left
    (when (not (zero? (mod pos 8)))
      (- pos 9))
    :top-right
    (when (not (zero? (mod (inc pos) 8)))
      (- pos 7))
    :bottom-left
    (when (not (zero? (mod pos 8)))
      (+ pos 7))
    :bottom-right
    (when (not (zero? (mod (inc pos) 8)))
      (+ pos 9))))

(defn get-valid-jumps [{:keys [board turn] :as state} pos]
  (when (not= EMPTY (get-nth board pos))
    (letfn [(jumpable? [dir p]
              (let [something (if (= turn :black) #{RED RED_KING} #{BLACK BLACK_KING})]
                (and (some->> p (go-direction dir) (get-nth board) something)
                     (some->> p (go-direction dir) (go-direction dir) (get-nth board) (= EMPTY)))))]
      (seq
        (if (#{BLACK RED} (get-nth board pos))
          (cond-> []
            (jumpable? :top-right pos)
            (conj (->> pos (go-direction :top-right) (go-direction :top-right)))

            (jumpable? :top-left pos)
            (conj (->> pos (go-direction :top-left) (go-direction :top-left))))
          ;kings
          (cond-> []
            (jumpable? :top-right pos)
            (conj (->> pos (go-direction :top-right) (go-direction :top-right)))

            (jumpable? :top-left pos)
            (conj (->> pos (go-direction :top-left) (go-direction :top-left)))

            (jumpable? :bottom-right pos)
            (conj (->> pos (go-direction :bottom-right) (go-direction :bottom-right)))

            (jumpable? :bottom-left pos)
            (conj (->> pos (go-direction :bottom-left) (go-direction :bottom-left)))))))))

(defn get-valid-moves [board pos]
  (when (not= EMPTY (get-nth board pos))
    (->> (if (#{BLACK RED} (get-nth board pos))
           (cond-> []
             (some->> (go-direction :top-right pos) (get-nth board) (= EMPTY))
             (conj (go-direction :top-right pos))

             (some->> (go-direction :top-left pos) (get-nth board) (= EMPTY))
             (conj (go-direction :top-left pos)))
           ;kings
           (cond-> []
             (some->> (go-direction :top-right pos) (get-nth board) (= EMPTY))
             (conj (go-direction :top-right pos))

             (some->> (go-direction :top-left pos) (get-nth board) (= EMPTY))
             (conj (go-direction :top-left pos))

             (some->> (go-direction :bottom-right pos) (get-nth board) (= EMPTY))
             (conj (go-direction :bottom-right pos))

             (some->> (go-direction :bottom-left pos) (get-nth board) (= EMPTY))
             (conj (go-direction :bottom-left pos))))
         (remove nil?)
         (seq))))

(defn is-game-tied? [board turn]
  (loop [[n & r] (range 64)]
    (cond
      (and
        (contains? (if (= :black turn) #{BLACK BLACK_KING} #{RED RED_KING}) (get-nth board n))
        (or (get-valid-moves board n)
            (get-valid-jumps {:board board :turn turn} n))) false
      (empty? r) true
      :else (recur r))))

(defn is-playable-piece? [{:keys [board turn player multiplayer] :as state} pos]
  (and (if multiplayer (= turn player) true)
       (or
         (get-valid-jumps state pos) ;;our piece has valid jumps
         (and
           ;;there isn't another jumpable piece
           (not (seq (reduce (fn [c p]
                               (if (= turn (if (#{BLACK BLACK_KING} (get board p)) :black :red))
                                 (into c (get-valid-jumps state p))
                                 c))
                             [] (range 64))))
           (get-valid-moves board pos)) ;; this piece can move
         )))

(defn find-jumped-square [s e]
  (when (> (Math/abs ^Integer (- s e)) 9)
    ;(println "JUMPED: " s e (/ (+ s e) 2))
    (/ (+ s e) 2)))

(defmulti perform-action (fn [_ [k _]] k))

(defmethod perform-action :move [state [_ {:keys [release-square piece square]}]]
  (-> state
      (assoc-in [:board release-square]
                (if (< release-square 8)
                  (condp = piece RED RED_KING BLACK BLACK_KING piece)
                  piece))
      (assoc-in [:board square] EMPTY)
      (update :actions (fn [actions]
                         (if-let [jumped-square (find-jumped-square square release-square)]
                           (conj actions [:jump {:jumped-square jumped-square :release-square release-square}])
                           (conj actions [:turn-switch] [:play-sound {:clip ::audio/move}]))))
      (dissoc :selected :jumping-piece)))

(defmethod perform-action :jump [state [_ {:keys [release-square jumped-square]}]]
  (let [new-state (-> state
                      (update :actions conj [:play-sound {:clip ::audio/jump}])
                      (assoc-in [:board jumped-square] EMPTY))]
    (if (seq (get-valid-jumps new-state release-square))
      (assoc new-state :jumping-piece release-square)
      (update new-state :actions conj [:turn-switch]))))

(defmethod perform-action :play-sound [state [_ action]]
  (when (:sound-on? state)
    (audio/play-sound (:clip action)))
  state)

(defmethod perform-action :turn-switch [state _]
  (cond-> state
    true (update :actions conj [:check-board-state])
    true (update :turn #(if (= % :red) :black :red))
    true (assoc :last-turn-seconds 0)
    true (dissoc :selected)
    (not (:multiplayer state)) (update :board (comp vec reverse))))

(defmethod perform-action :check-board-state [{:keys [board turn player] :as state} _]
  (let [new-game-state (cond
                         (not (seq (filter #{BLACK BLACK_KING} board))) :red-wins
                         (not (seq (filter #{RED RED_KING} board))) :black-wins
                         (and (= turn player) (is-game-tied? board turn)) :tied
                         :else :in-progress)]
    (cond-> (assoc state :game-state new-game-state)
      (#{:red-wins :black-wins :tied} new-game-state)
      (update :actions conj [:play-sound
                             {:clip
                              (if (= :red (:player state))
                                (if (= new-game-state :red-wins) ::audio/won ::audio/lost)
                                (if (= new-game-state :black-wins) ::audio/won ::audio/lost))}]))))

(defmethod perform-action :reset-game [state _]
  (println "reset game")
  (-> state
      (merge default-game-state)
      (assoc :new-menu :main-menu)))

(defmethod perform-action :default [state _] state)
