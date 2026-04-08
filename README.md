[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.bzg/fetch-imap.svg)](https://clojars.org/org.clojars.bzg/fetch-imap)

# fetch-imap

A minimal, read-only Clojure library for fetching and parsing IMAP email.

Built on [Eclipse Angus Mail](https://eclipse-ee4j.github.io/angus-mail/) (the modern successor to JavaMail), `fetch-imap` provides a data-oriented API: maps in, maps out.

## Status

Early development — API may change before 1.0.

## Installation

deps.edn:

```clojure
org.clojars.bzg/fetch-imap {:mvn/version "0.1.0"}
```

Leiningen:

```clojure
[org.clojars.bzg/fetch-imap "0.1.0"]
```

## Quick start

```clojure
(require '[fetch-imap.core :as imap]
         '[fetch-imap.fetch :as fetch])

;; Connect and fetch the 10 most recent messages
(imap/with-connection [conn {:host "imap.example.com"
                             :user "me@example.com"
                             :password "secret"}]
  (fetch/messages conn "INBOX" {:limit 10}))
```

Each message is returned as a Clojure map:

```clojure
{:uid            12345
 :message-id     "<abc@example.com>"
 :from           [{:name "Alice" :address "alice@example.com"}]
 :to             [{:name "Bob" :address "bob@example.com"}]
 :cc             []
 :bcc            nil
 :reply-to       [{:name "Alice" :address "alice@example.com"}]
 :subject        "Hello from fetch-imap"
 :date-sent      #inst "2025-02-15T10:30:00.000-00:00"
 :date-received  #inst "2025-02-15T10:30:02.000-00:00"
 :content-type   "multipart/alternative; boundary=..."
 :flags          #{:seen}
 :body           {:text "Plain text body"
                  :html "<p>HTML body</p>"
                  :attachments [{:filename "doc.pdf"
                                 :content-type "application/pdf"
                                 :size 14023
                                 :data #object[byte[] ...]}]}
 :headers        {"Subject" "Hello from fetch-imap" ...}}
```

## API

### Connection (`fetch-imap.core`)

```clojure
;; Connect
(def conn (imap/connect {:host "imap.example.com"
                         :port 993
                         :ssl true
                         :user "me@example.com"
                         :password "secret"}))

;; Check connection
(imap/connected? conn) ;; => true

;; Disconnect
(imap/disconnect conn)

;; Or use the macro for automatic cleanup
(imap/with-connection [conn {...}]
  ...)
```

OAuth2 is supported — pass `:oauth2-token` instead of `:password`.

### Fetching messages (`fetch-imap.fetch`)

```clojure
;; Fetch recent messages
(fetch/messages conn "INBOX" {:limit 20})

;; Fetch by date range
(fetch/messages conn "INBOX" {:since "2025-01-01" :before "2025-02-01"})

;; Fetch by UID (e.g. relative to the last UID you saw)
(fetch/by-uid conn "INBOX" [12345 12346])

;; Fetch everything newer than a known UID
(fetch/by-uid-range conn "INBOX" 12347 jakarta.mail.UIDFolder/LASTUID)

;; Lightweight fetch (skip body parsing)
(fetch/messages conn "INBOX" {:limit 100
                               :body? false
                               :headers? false})
```

### Folders (`fetch-imap.folder`)

```clojure
(require '[fetch-imap.folder :as folder])

(folder/list-folders conn)
;; => [{:name "INBOX" :full-name "INBOX" :type :holds-messages
;;      :message-count 1042 :unread-count 3} ...]

(folder/message-count conn "INBOX")  ;; => 1042
(folder/unread-count conn "INBOX")   ;; => 3
```

### IDLE / push notifications (`fetch-imap.idle`)

```clojure
(require '[fetch-imap.idle :as idle])

;; Blocking — run in a future or thread
(def idle-thread
  (idle/idle-async conn "INBOX"
    (fn [msg]
      (println "New message:" (:subject msg)))))

;; Stop with:
(.interrupt idle-thread)
```

## Design principles

- **Read-only** — no sending, writing, moving, or deleting
- **Data-oriented** — all inputs and outputs are plain Clojure maps
- **Minimal API surface** — fewer functions means less to maintain and learn
- **Thin wrapper** — for advanced use cases, interop with Jakarta Mail directly
- **Zero extra dependencies** — only Eclipse Angus Mail, nothing else

## Building & deploying

```bash
# Run tests
clj -X:test cognitect.test-runner.api/test

# Build jar
clj -T:build jar

# Install locally
clj -T:build install

# Deploy to Clojars
clj -T:build deploy
```

## Contributing

- Send a [bug report](mailto:~bzg/dev@lists.sr.ht) with `[BUG] fetch-imap: <SHORT EXPLICIT BUG DESCRIPTION>`{.verbatim}
- Send a [patch](mailto:~bzg/dev@lists.sr.ht) with `[PATCH] fetch-imap: <COMMIT SUMMARY>`{.verbatim}
- Send a [feature request](mailto:~bzg/dev@lists.sr.ht) with `[FR] fetch-imap: <FEATURE REQUEST>`{.verbatim}
- Share any [other question or idea](mailto:~bzg/dev@lists.sr.ht)

You can also [send me an email](mailto:bzg@bzg.fr) and support my work
on [liberapay](https://liberapay.com/bzg/).


## Support the Clojure(script) ecosystem

If you like Clojure(script), please consider supporting maintainers by
donating to [clojuriststogether.org](https://clojuriststogether.org).

## License

Copyright © 2026 Bastien Guerry

Distributed under the Eclipse Public License 2.0.
