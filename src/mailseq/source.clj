;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.source
  "MailSource protocol: the unified, read-only interface shared by every
  backend (IMAP, Maildir, Mbox).

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
    `opts` is restricted to the contract described in plan §14.")

  (-by-id [this folder-name id opts]
    "Return a single message map (or nil) from `folder-name` identified
    by the backend's stable id. For IMAP, `id` is a UID (long).")

  (-close [this]
    "Release any resources held by this source. Idempotent."))
