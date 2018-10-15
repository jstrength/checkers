(ns checkers.server
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! put!]])
  (:import (java.net InetSocketAddress ConnectException)
           (java.nio.channels ServerSocketChannel SocketChannel)
           (java.nio ByteBuffer)))

(def ^:static BUFFER_SIZE 64)

(defn byte-buffer->msg [buffer]
  (clojure.string/trim (apply str (map char (.array buffer)))))

(defn msg->byte-buffer [msg]
  (ByteBuffer/wrap (byte-array (map byte (subs (format "%10s" msg) 0 BUFFER_SIZE)))))

(defn server-channel [port read-ch write-ch]
  (go
    (let [server-chan (.bind (ServerSocketChannel/open) (InetSocketAddress. port))
          _ (println "Waiting on client to connect...")
          sock-chan (.accept server-chan)]
      (println "Client connected!")
      (go-loop []
        (println "Server reading")
        (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
          (.read sock-chan buffer)
          (let [msg (byte-buffer->msg buffer)]
            (println "Server read: " msg)
            (put! read-ch msg)))
        (recur))
      (go-loop []
        (let [msg (<! write-ch)]
          (println "Server write: " msg)
          (.write sock-chan (msg->byte-buffer msg)))
        (recur)))))

(defn client-channel [port read-ch write-ch]
  (try
    (let [sock-chan (SocketChannel/open (InetSocketAddress. port))]
      (println "Connected")
      (go-loop []
        (println "Client reading")
        (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
          (.read sock-chan buffer)
          (let [msg (byte-buffer->msg buffer)]
            (println "Client read: " msg)
            (put! read-ch msg)))
        (recur))
      (go-loop []
        (let [msg (<! write-ch)]
          (println "Client write: " msg)
          (.write sock-chan (msg->byte-buffer msg)))
        (recur)))
    (catch ConnectException e (println "Unable to connect to" port))))

(comment
  (def s-read-c (async/chan))
  (def s-write-c (async/chan))
  (def c-read-c (async/chan))
  (def c-write-c (async/chan))
  (server-channel 1525 s-read-c s-write-c)
  (client-channel 1525 c-read-c c-write-c)
  (async/put! c-write-c "YI! wow hi :)")
  (async/put! s-write-c "hey guy :)")
  (go (<! s-read-c))
  (async/put!)
  (async/take! s-read-c println)
  async/>!!
  async/>!

  )