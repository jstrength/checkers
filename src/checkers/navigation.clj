(ns checkers.navigation
  (:require [checkers.utils :refer :all]
            [quil.core :as q]
            [checkers.draw :as draw]
            [checkers.server :as server])
  (:import (java.util Date)))

(defn get-next-item [item items]
  (nth items (mod (inc (.indexOf items item)) (count items))))

(defn get-previous-item [item items]
  (nth items (mod (dec (.indexOf items item)) (count items))))

(defn default-action [state _] state)

(defn selected-item? [state item-id]
  (= item-id (get-in state [:current-menu :selected-item])))

(defn enter-menu? [{:keys [key-code button] :as event}]
  (or (#{right-key enter-key} key-code)
      (= :left button)))

(def back-text "⬅Back")

(declare settings-menu multiplayer-menu pause-menu board-menu) ;;todo use lookup

(def main-menu
  (let [display-order [:start-game :multiplayer :settings :quit]]
    {:display-order display-order
     :items {:start-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Start Game" (selected-item? state :start-game)))
              :action (fn [state _]
                        (assoc state :game-state :in-progress))}
             :multiplayer
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Multiplayer" (selected-item? state :multiplayer)))
              :action (fn [state _] (assoc state :current-menu multiplayer-menu))}
             :settings
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Settings" (selected-item? state :settings)))
              :action (fn [state _] (assoc state :current-menu settings-menu))}
             :quit
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Quit" (selected-item? state :quit)))
              :action (fn [_ _] (System/exit 0))}}
     :action (fn [{:keys [current-menu] :as state} event]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (cond-> state
                   (enter-menu? event)
                   (item-action-fn event))))
     :selected-item (first display-order)}))

(def settings-menu
  (let [display-order [:back :timer :sound :board]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state event]
                (cond-> state
                  (enter-menu? event)
                  (assoc :current-menu
                         (assoc
                           (if (= :paused (:game-state state))
                             pause-menu
                             main-menu)
                           :selected-item :settings))))}
             :timer
             {:draw (fn [state x y]
                      (draw/draw-menu-item
                        x y
                        (format "Timer: %s" (if (zero? (:timer state)) "Off" (:timer state)))
                        (selected-item? state :timer)))
              :action (fn [state {:keys [key-code] :as event}]
                        (update state :timer
                                (fn [timer]
                                  (condp = key-code
                                    left-key (if (zero? timer)
                                               timer
                                               (- timer 5))
                                    right-key (if (= 60 timer)
                                                timer
                                                (+ timer 5))
                                    timer))))}
             :sound
             {:draw (fn [state x y]
                      (draw/draw-menu-item
                        x y
                        (format "Sound: %s" (if (:sound-on? state) "ON" "OFF"))
                        (selected-item? state :sound)))
              :action (fn [state _] (update state :sound-on? not))}
             :board
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Board" (selected-item? state :board)))
              :action (fn [state event]
                        (cond-> state
                          (enter-menu? event)
                          (assoc :current-menu board-menu)))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def board-menu
  (let [display-order [:back :dark-piece :light-piece :dark-square :light-square]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state event]
                (cond-> state
                  (enter-menu? event)
                  (assoc :current-menu (assoc settings-menu :selected-item :board))))}
             :dark-piece
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Dark:" (selected-item? state :dark-piece))
                      (draw/draw-static-piece (+ x (q/text-width "Dark: ")) (- y 10) (:dark-color state)))
              :action (fn [state {:keys [key-code] :as event}]
                        (cond-> state
                          (enter-menu? event)
                          (update :dark-color
                                  (if (= key-code left-key)
                                    get-previous-item
                                    get-next-item)
                                  (vec (keys dark-colors)))))}
             :light-piece
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Light:" (selected-item? state :light-piece))
                      (draw/draw-static-piece (+ x (q/text-width "Light: ")) (- y 10) (:light-color state)))
              :action (fn [state {:keys [key-code] :as event}]
                        (cond-> state
                          (enter-menu? event)
                          (update :light-color
                                  (if (= key-code left-key)
                                    get-previous-item
                                    get-next-item)
                                  (vec (keys light-colors)))))}
             :dark-square
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Dark:" (selected-item? state :dark-square))
                      (draw/draw-square (+ x (q/text-width "Dark:")) (- y (- SQUARE_WIDTH 15)) (:dark-square-color state)))
              :action (fn [state {:keys [key-code] :as event}]
                        (cond-> state
                          (enter-menu? event)
                          (update :dark-square-color
                                  (if (= key-code left-key)
                                    get-previous-item
                                    get-next-item)
                                  (vec (keys dark-colors)))))}

             :light-square
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Light:" (selected-item? state :light-square))
                      (draw/draw-square (+ x (q/text-width "Light:")) (- y (- SQUARE_WIDTH 15)) (:light-square-color state)))
              :action (fn [state {:keys [key-code] :as event}]
                        (cond-> state
                          (enter-menu? event)
                          (update :light-square-color
                                  (if (= key-code left-key)
                                    get-previous-item
                                    get-next-item)
                                  (vec (keys light-colors)))))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def host-game-menu
  (let [display-order [:back :your-ip :port :host]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state event]
                (cond-> state
                  (enter-menu? event)
                  (assoc :current-menu (assoc multiplayer-menu :selected-item :host-game))))}
             :your-ip
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x (- y 50) (format "Your IP:\n%s" your-ip)
                                     (selected-item? state :your-ip)))}
             :port
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y (format "Port:%s" port) (selected-item? state :port)))}
             :host
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Host!" (selected-item? state :host)))
              :action (fn [state event]
                        (server/host-game! state))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def join-game-menu
  (let [display-order [:back :ip :port :join]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state event]
                (cond-> state
                  (enter-menu? event)
                  (assoc :current-menu (assoc multiplayer-menu :selected-item :join-game))))}
             :ip
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x (- y 50) (format "IP:\n%s" (:ip state))
                                     (selected-item? state :ip)))
              :action
              (fn [state {:keys [key key-code] :as event}]
                (cond-> state
                  key-code
                  (cond->
                    (= backspace-key key-code) (update :ip (fn [ip] (cond-> ip (< 0 (count ip)) (subs 0 (dec (count ip))))))
                    :else (update :ip (fn [ip k] (re-find #"[0-9\.]{0,15}" (str ip k))) (name key)))))}
             :port
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y (format "Port: %s" port) (selected-item? state :port)))}
             :join
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y "Join!" (selected-item? state :join)))
              :action (fn [state event]
                        (server/join-game! state)
                        ;;todo start game if we can connect, else show error message
                        )}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def multiplayer-menu
  (let [display-order [:back :host-game :join-game]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state event]
                (cond-> state
                  (enter-menu? event)
                  (assoc :current-menu (assoc main-menu :selected-item :multiplayer))))}
             :host-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Host Game" (selected-item? state :host-game)))
              :action (fn [state event]
                        (cond-> state
                          (enter-menu? event)
                          (assoc :current-menu host-game-menu)))}
             :join-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Join Game" (selected-item? state :join-game)))
              :action (fn [state event]
                        (cond-> state
                          (enter-menu? event)
                          (assoc :current-menu join-game-menu)))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def pause-menu
  (let [display-order [:resume-game :settings :quit]]
    {:display-order display-order
     :items {:resume-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Resume Game" (selected-item? state :resume-game)))
              :action (fn [state _]
                        (assoc state :game-state :in-progress
                                     :start-time (.getTime (Date.))))}
             :settings
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Settings" (selected-item? state :settings)))
              :action (fn [state _] (assoc state :current-menu settings-menu))}
             :quit
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Quit" (selected-item? state :quit)))
              :action (fn [_ _] (System/exit 0))}}
     :action (fn [{:keys [current-menu] :as state} event]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (cond-> state
                   (enter-menu? event)
                   (item-action-fn event))))
     :selected-item (first display-order)}))

(def play-again-menu
  (let [display-order [:yes :no]]
    {:display-order display-order
     :items {:yes
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Play again?" false)
                      (draw/draw-menu-item x (+ y 100) "YES" (selected-item? state :yes)))
              :action (fn [state _] ;todo use action system instead???
                        (merge state default-game-state {:game-state :in-progress}))}
             :no
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "No" (selected-item? state :no)))
              :action (fn [state _]
                        (merge state default-game-state {:current-menu main-menu}))}}
     :action (fn [{:keys [current-menu] :as state} event]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (cond-> state
                   (enter-menu? event)
                   (item-action-fn event))))
     :selected-item (first display-order)}))

(def player-disconnected-menu
  (let [display-order [:ok]]
    {:display-order display-order
     :items {:ok
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Player\ndisconnected." false)
                      (draw/draw-menu-item (+ 20 x) (+ y 200) "Ok" (selected-item? state :ok)))}}
     :action (fn [state event]
               (if (enter-menu? event)
                 (merge default-game-state {:game-state :menu :current-menu main-menu})
                 state))
     :selected-item (first display-order)}))

(def menu-kw->menu
  {:main-menu main-menu
   :player-disconnected-menu player-disconnected-menu
   })

(defn update-selected-item [{:keys [current-menu] :as state} {:keys [y] :as event}]
  (let [menus-count (count (:display-order current-menu))
        item-heights (/ 400 menus-count)
        selected-item-idx (q/constrain (quot (- y 40) item-heights) 0 (min (dec menus-count)))]
    (assoc-in state [:current-menu :selected-item] (nth (:display-order current-menu) selected-item-idx))))

(defn handle-nav [{:keys [current-menu new-menu] :as state} event]
  (let [state (if new-menu
                (-> state
                    (dissoc :new-menu)
                    (assoc :current-menu (menu-kw->menu new-menu)))
                state)]
    (condp = (:key-code event)
      up-key
      (update-in state [:current-menu :selected-item] get-previous-item (:display-order current-menu))
      down-key
      (update-in state [:current-menu :selected-item] get-next-item (:display-order current-menu))
      ((:action current-menu) state event))))
