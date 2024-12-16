(ns helper)

(defn get-image-name+tag [ns n tag]
  (format "%s/%s:%s" (name ns) (name n) (name tag)))

(defn get-datomic-url [protocol host port db-name username password]
  (format "%s://%s:%s?jdbc:postgresql://%s:5432/%s?user=%s&password=%s"
          protocol
          db-name
          port
          host
          db-name
          username
          password))
