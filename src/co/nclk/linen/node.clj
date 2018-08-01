(ns co.nclk.linen.node
  (:require [clojure.tools.logging :as logging]
            [clj-ssh.ssh :as ssh])
  (:import com.jcraft.jsch.JSch
           com.jcraft.jsch.Logger
           clj_ssh.ssh.SshLogger)
  (:gen-class))


(JSch/setLogger (SshLogger. Logger/ERROR))

(defprotocol PNode
  (proxy [self checkpoint])
  (create [self node full-name runid])
  (destroy [self failed?])
  (clone [self]))


(defprotocol PNodeManager
  (get-node [self node runid])
  (invoke [self node-label checkpoint])
  (full-node-name [self node]))


(def ^:dynamic *log* false)
(def ^:dynamic *level* :info)


(defn node-log-str
  [node-name ip runid message]
  (str "[" node-name (if ip (str " (" ip ")") "") ": " runid "] " message))


(defn log*
  ([callbacks level msg]
    (log* callbacks level nil nil nil msg))
  ([callbacks level node-name ip runid & more]
    (if-let [handler (-> callbacks :log)]
      (if-not (and node-name runid)
        (handler level nil (apply str more))
        (handler level runid (node-log-str node-name ip runid (apply str more))))
      (logging/log level (node-log-str node-name ip runid (apply str more))))))


(def ^:dynamic log
  (partial log* nil))


(defn node-error
  [body node]
  (clojure.string/join "\n"
    [body
     (str "Node: " (:name node))
     (str "Hostname: " (:public_ip node))]))


(defn resolve-connector
  [namesp]
  (locking ::lock
    (-> namesp symbol require)
    (-> namesp (str "/connector") symbol resolve)))


(defrecord LocalNode []
  PNode
  (create [self node full-name runid] self)
  (destroy [self failed?] nil)
  (clone [self] self))


(defn log-result
  [out err exit node-name ip runid]
  (when-not (clojure.string/blank? out)
    (log :debug node-name ip runid "stdout: " (clojure.string/trim out)))
  (when-not (clojure.string/blank? err)
    (log :debug node-name ip runid "stderr: " (clojure.string/trim err))))

(defn tempfile-name
  [& [local]]
  (str (when-not (false? local)
         (str (System/getProperty "java.io.tmpdir")
              "/"))
       ".linen-temp-script-"
       (java.util.UUID/randomUUID)))

(defn invocation-string
  [invocation tmpfile-name]
  (if invocation
    (if (string? invocation)
      (str invocation " " tmpfile-name)
      (if-let [t (:template invocation)]
        (if-not (clojure.string/blank? (:match invocation))
          (clojure.string/replace
            t
            (re-pattern (str (:match invocation)))
            tmpfile-name)
          (str t " " tmpfile-name))))
    tmpfile-name))


(defn heredoc-src
  [source terminator]
  (str
    \newline
    (-> (clojure.string/trim source)
        (clojure.string/replace "$" "\\$")
        (clojure.string/replace #"\\n" "\n")
        (clojure.string/replace #"\\\n" "\n"))
    \newline
    terminator
    \newline))


(defn- invoke-local
  [checkpoint & [argv]]
  (binding [*log* (not (false? (:log checkpoint)))]
    (let [tmpfile-name (tempfile-name)
          proxy? (not (nil? argv))
          argv (or argv
                   (remove clojure.string/blank?
                     (flatten
                       [(if-let [u (:user checkpoint)] ["sudo" "-u" u])
                        (if-let [d (:invocation checkpoint)]
                          (clojure.string/split
                            (invocation-string d tmpfile-name)
                            #" ")
                          tmpfile-name
                          )])))]

      (when-not proxy?
        (spit tmpfile-name (:source checkpoint))
        (-> (java.io.File. tmpfile-name) (.setExecutable true))
        ;; XXX: Not sure why this was necessary before... seems to be working without it.
        ;;(Thread/sleep 500))
        )

      (loop []
        (let [result
              (try
                (let [proc (-> (Runtime/getRuntime)
                             (.exec (into-array String argv)))
                      stdout (clojure.java.io/reader (.getInputStream proc))
                      stderr (clojure.java.io/reader (.getErrorStream proc))]

                  (when *log*

                    (when (:display checkpoint)
                      (log :info "local" nil (:runid checkpoint)
                           (clojure.string/trim (:display checkpoint))))

                    (log :debug "local" nil (:runid checkpoint)
                         (clojure.string/join " " argv))

                    (when-not proxy?
                      (log :debug "local" nil (:runid checkpoint)
                           "Contents of "
                           tmpfile-name
                           ":\n"
                           (:source checkpoint))))

                  (let [[out err]
                        (for [stream [stdout stderr]]
                          (future
                            (loop [lines []]
                              (let [line (.readLine stream)]
                                (if (nil? line)
                                  lines
                                  (do
                                    (log :debug "local" nil (:runid checkpoint) line)
                                    (recur (conj lines line))))))))
                        exit (.waitFor proc)
                        out (clojure.string/join "\n" @out)
                        err (clojure.string/join "\n" @err)
                        result
                        (assoc checkpoint
                               :stdout {:keys (:stdout checkpoint) :value out}
                               :stderr {:keys (:stderr checkpoint) :value err}
                               :exit {:keys (:exit checkpoint) :value exit}
                               ;; See `core/assert-checkpoint`. `:success` `:value` is
                               ;; determined there:
                               :success {:keys (:success checkpoint) :value nil})]
                    (when-not proxy? (clojure.java.io/delete-file tmpfile-name))
                    (when-not (zero? exit)
                      (log-result nil nil exit "local" nil (:runid checkpoint)))
                    result))
                (catch java.io.IOException ioe
                  ;; FIXME: really catch all ioexceptions?
                  ;; Should we attempt to retry, or can't assume idempotent
                  (log :warn (.getMessage ioe))
                  nil))]
            (if (nil? result)
              (recur)
              result)
            )))))


(defn- invoke-remote
  [checkpoint node]
  (if (:proxy checkpoint)
    (invoke-local checkpoint (proxy node checkpoint))
    (let [node (:data node)]
      (if-not (:source checkpoint)
        (assoc checkpoint :public_ip (:public_ip @node)
                          :stdout {:keys (:stdout checkpoint)
                                   :value ""}
                          :stderr {:keys (:stderr checkpoint)
                                   :value ""}
                          :exit {:keys (:exit checkpoint)
                                 :value 0}
                          :success {:keys (:success checkpoint)
                                    :value nil})
        (binding [*log* (not (false? (:log checkpoint)))
                  *level* (or (:log checkpoint) :info)]
          (let [agent (:ssh-agent @node)
                total-attempts 3]
            (loop [remaining-attempts total-attempts]
              (if (zero? remaining-attempts)
                (assoc checkpoint :stdout {:keys (:stdout checkpoint) :value ""}
                                  :stderr {:keys (:stderr checkpoint)
                                           :value (str "linen: " total-attempts
                                                       " attempts to ssh to " (:name @node)
                                                       " failed.")}
                                  :exit {:keys (:exit checkpoint) :value 1}
                                  :success {:keys (:success checkpoint) :value nil})
                (if-let [resolved-checkpoint
                         (try
                           (let [session (ssh/session
                                           agent
                                           (:public_ip @node)
                                           {:strict-host-key-checking :no
                                            :log-level :verbose
                                            :user-known-hosts-file
                                            (or (-> @node
                                                    :options
                                                    :user-known-hosts-file)
                                                "/dev/null")
                                            :username (:ssh-user @node)})]

                             (when (and *log*
                                        (not (clojure.string/blank?
                                               (:display checkpoint))))
                               (log :info (:short-name @node)
                                          (:public_ip @node)
                                          (:runid checkpoint)
                                          (clojure.string/trim
                                            (:display checkpoint))))

                             (try

                               (when-not (ssh/connected? session)
                                 (ssh/connect session
                                              (or (-> @node :options :timeout)
                                                  (* 60 1000))))

                               (when *log*
                                 (log :debug (:short-name @node)
                                             (:public_ip @node)
                                             (:runid checkpoint)
                                             (clojure.string/trim
                                               (:source checkpoint))))

;;                               (let [result (ssh/ssh session {:cmd
;;                                                              (str "sudo su "
;;                                                                   (or (:user checkpoint)
;;                                                                       "root")
;;                                                                   " - ")
;;                                                              :in (:source checkpoint)})]
                               (let [tmpfile-name (str "./" (tempfile-name false))
                                     terminator (:runid checkpoint)
                                     instr (str "cat <<-" terminator " > " tmpfile-name
                                                (heredoc-src (:source checkpoint) terminator)
                                                "chmod a+x " tmpfile-name
                                                "; " (invocation-string
                                                       (:invocation checkpoint)
                                                       tmpfile-name)
                                                "; rc=$?"
                                                "; rm " tmpfile-name
                                                "; exit $rc")
                                     ;;_ (do (println "CHESTER: " instr)) ;; (System/exit 0))
                                     result (ssh/ssh session {:cmd
                                                              (str "sudo su "
                                                                   (or (:user checkpoint)
                                                                       "root")
                                                                   " - ")
                                                              :in instr})]
                                 (when *log*
                                   (log-result (:out result)
                                               (:err result)
                                               (:exit result)
                                               (:short-name @node)
                                               (:public_ip @node)
                                               (:runid checkpoint)))

                                 (assoc checkpoint :stdout {:keys (:stdout checkpoint)
                                                            :value (:out result)}
                                                   :stderr {:keys (:stderr checkpoint)
                                                            :value (:err result)}
                                                   :exit {:keys (:exit checkpoint)
                                                          :value (:exit result)}
                                                   :success {:keys (:success checkpoint)
                                                             :value nil}))

                               (finally (ssh/disconnect session))))
                           (catch Exception se
                             (log :error (:short-name @node) (:public_ip @node) (:runid checkpoint)
                                  (apply str
                                    (flatten [(.getMessage se)
                                              (map #(str "\n\t" %) (.getStackTrace se))]))
                                  ". "
                                  remaining-attempts
                                  " attempts remaining.")
                             nil))]
                  (assoc resolved-checkpoint :public_ip (:public_ip @node))
                  (let [remaining-attempts (dec remaining-attempts)
                        sleep-time (if (zero? remaining-attempts) 0 2000)]
                    (Thread/sleep sleep-time)
                    (recur remaining-attempts))))))
                  )))))

(defn short-name
  [node]
  (cond
    (string? node)
    node

    (or (nil? (:name node))
        (= (name (:name node)) "local"))
    "local"

    :else (:name node)))


(defrecord NodeManager [nodes effective version context]
  PNodeManager
  (get-node [self node runid]
    (cond

      ;; If you're only passing a string, the only outcome can be
      ;; retrieving an existing node (or nil).
      (string? node)
      (-> @nodes (get (full-node-name self node)))

      (map? node)
      (or ;; If a node already exists with the node name, just return it.
          (-> @nodes (get (full-node-name self node)))
          ;; If there's no connector, then it's assumed that the client code thought it
          ;; already existed, so we throw an exception because they thought wrong.
          (and (nil? (-> node :connector))
               (throw (RuntimeException. (str "Unknown node: " (full-node-name self node)))))

          ;; Else, create the node and add it to the list of managed nodes.
          (let [ctor (-> node :connector resolve-connector)
                ;;_ (println ctor context node)
                n (-> (ctor context) (create node (full-node-name self node) runid))
                agent (ssh/ssh-agent {})]
            (ssh/add-identity agent {:name (:name @(:data @n))
                                     :private-key (-> @(:data @n) :private-key)})

            (swap! (:data @n) #(assoc % :ssh-agent agent
                                        :short-name (short-name node)))
            ;; The `node` returned from the constructor is a promise.
            ;; The new node record has some `:data` with a `:name` entry
            ;; which is the full node name. We add the node to the manager's
            ;; nodes with this full name as the key.
            (swap! nodes #(assoc % (-> @(:data @n) :name)
                                   @n))
            @n))))
  (invoke [self checkpoint node]
    (let [res (future
                (let [ts (java.util.Date.)]
                  (assoc (if (= "local" (full-node-name self node))
                           (invoke-local checkpoint)
                           (let [node (get-node self node (:runid checkpoint))]
                             ;; TODO: log checkpoint started here, instead
                             (if (nil? (:data node))
                               (invoke-local checkpoint)
                               (invoke-remote checkpoint node))))
                         :started (.getTime ts)
                         :finished (.getTime (java.util.Date.)))))
          checkpoint (when-not (:abandon checkpoint) @res)]
      (if (and checkpoint
               (false? (:log checkpoint)))
        ;; disable these keys for checkpoint recording, too, if :log is false
        (dissoc checkpoint :source :stdout :stderr)
        checkpoint)
    ))
  (full-node-name [self node]
    (cond
      (string? node)
      node

      (or (clojure.string/blank? (:name node))
          (= (name (:name node)) "local"))
      "local"

      :else (str (name (:name node)) "-" (.getTime (:effective self)) "-" (:version self))))
  )


(defn node-manager
  [effective callbacks & [version]]
  (alter-var-root #'log (fn [_] (partial log* callbacks)))
  (NodeManager. (atom {"local" (LocalNode.)})
                effective
                (or version 0)
                (atom {})))

