(ns checkers.server
  (:require [checkers.utils :refer :all]
            [clojure.edn :as edn])
  (:import (java.net InetSocketAddress ConnectException StandardSocketOptions)
           (java.nio.channels ServerSocketChannel SocketChannel)
           (java.nio ByteBuffer)
           (java.io IOException)))

(defonce server-chan (atom nil))
(defonce sock-chan (atom nil))
(def BUFFER_SIZE (int 64))

(defn ^:private set-socket-config [socket]
  (doto socket
    (.configureBlocking false)
    (.setOption StandardSocketOptions/SO_KEEPALIVE true)
    (.setOption StandardSocketOptions/SO_SNDBUF BUFFER_SIZE)
    (.setOption StandardSocketOptions/SO_RCVBUF BUFFER_SIZE)))

(defn byte-buffer->msg [buffer]
  (clojure.string/trim (apply str (map char (.array buffer)))))

(defn msg->byte-buffer [msg]
  (ByteBuffer/wrap (byte-array (map byte (subs (format (str "%" BUFFER_SIZE "s") msg) 0 BUFFER_SIZE)))))

(defn ^:private close-connection! []
  (when @server-chan
    (.close @server-chan)
    (reset! server-chan nil))
  (when @sock-chan
    (.close @sock-chan)
    (reset! sock-chan nil)))

(defn ^:private read-msgs! []
  (loop [msgs []]
    (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
      (if (pos? (.read @sock-chan buffer))
        (recur (conj msgs (byte-buffer->msg buffer)))
        msgs))))

(defn ^:private write-msg! [msg]
  (println "write: " msg)
  (try
    (.write @sock-chan (msg->byte-buffer msg))
    (catch IOException e (do (println (ex-data e))
                             (close-connection!)))))

(defn is-connected? []
  (and (not (nil? @sock-chan))
       (.isOpen @sock-chan)))

(defn start-server! []
  (reset! server-chan (doto (ServerSocketChannel/open) (.bind (InetSocketAddress. port))))
  (println "Waiting on client to connect...")
  (future (reset! sock-chan (set-socket-config (.accept @server-chan)))
          (println "Client connected!")))

(defn connect-to-server! [host]
  (try
    (reset! sock-chan (set-socket-config (SocketChannel/open (InetSocketAddress. ^String host ^Integer port))))
    (println "Connected")
    (catch ConnectException e (println "Unable to connect to" port))))

(defn host-game! [state]
  (start-server!)
  (assoc state :player :red :multiplayer true :hosting? true))

(defn join-game! [state]
  (connect-to-server! (:ip state))
  (assoc state :player :black :multiplayer true :hosting? false))

(defn send-move! [player release-square square]
  (let [msg {:move {:from (get-algebraic-notation player square)
                    :to (get-algebraic-notation player release-square)}}]
    ))

(defmulti handle-msg (fn [state {:keys [type]}] type))
(defmethod handle-msg :moved [state msg] state)
(defmethod handle-msg :quit [state _]
  (reset! sock-chan nil)
  (assoc state :game-state :opponent-resigned))

(defn update-state [state]
  (reduce
    handle-msg
    (read-msgs!)
    state))

(comment
  (start-server!)
  (connect-to-server! "localhost")
  (is-connected?)
  (write-msg! "HELLO :)")
  (write-msg! "OMG")
  (write-msg! "OKAYLY DOKALY")
  (write-msg! "HELLO :)")
  (write-msg! {:msg "WOW"})
  (read-msgs!)

  (close-connection!)

  )
