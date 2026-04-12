;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.source
  "MailSource protocol: the unified, read-only interface shared by every
  backend (IMAP, Maildir).

  Consumers should use the functions in the top-level `mailseq` namespace
  rather than calling protocol methods directly. The protocol exists to
  let backends plug into that public API.")

(defprotocol MailSource
  "Read-only view over a mail source.

  A source owns a fixed, explicit map of *logical folder names* to
  backend-specific locations. Logical names are the only identifiers the
  public API accepts — there is no folder discovery."

  (-list-folders [this]
    "Return a vector of the logical folder names known to this source.
    Order follows the configuration's insertion order when available.")

  (-messages [this folder-name opts]
    "Return a vector of message maps from the given logical folder.
    `opts` is restricted to `mailseq.filter/allowed-option-keys`.")

  (-list-ids [this folder-name]
    "Return a vector of stable id strings for every message in
    `folder-name`, without reading or parsing message content.
    For IMAP, ids are UID strings; for Maildir, filename prefixes.")

  (-by-id [this folder-name id opts]
    "Return a single message map (or nil) from `folder-name` identified
    by the backend's stable id (long, string, or backend-specific).")

  (-by-ids [this folder-name ids opts]
    "Return a vector of message maps for the given set of ids.
    More efficient than repeated `-by-id` calls: a single filesystem
    or server round-trip.")

  (-watch [this folder-name on-message opts]
    "Start watching `folder-name` for new messages, calling `on-message`
    with each parsed message map. Blocks the current thread.
    For IMAP, delegates to IDLE; for Maildir, delegates to WatchService.")

  (-watch-async [this folder-name on-message opts]
    "Like `-watch` but starts in a new daemon thread. Returns a Thread.
    Call `(.interrupt thread)` to stop.")

  (-by-id-range [this folder-name start-id end-id opts]
    "Return a vector of message maps whose ids fall in [start-id, end-id].
    For IMAP, delegates to UID-range fetch. For Maildir, filters `list-ids`
    by lexicographic comparison. `end-id` may be nil to mean \"to the end\".")

  (-close [this]
    "Release any resources held by this source. Idempotent."))
