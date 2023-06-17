;; extends
; Conceal shortcut links
; (query
;   name: "wikilink"
;   captures: ["link", "alias"]
;   body:
;     (seq
;       (text "[")
;       (optional
;         (seq
;           (text "[")
;           (capture "link"
;             (text anything))
;           (text "]")
;         )
;       )
;       (text "|")
;       (capture "alias"
;         (text anything))
;       (text "]")
;     )
; )

; Conceal shortcut links
; Conceal shortcut links
; (shortcut_link
;   [
;     "["
;     "]"
;   ] @conceal
;   (#set! conceal "")))

; [
;  "|"
;  "]"
;  "]"
;  ] @link_alias

; (shortcut_link
;   [
;     "["
;     "]"
;   ] @conceal
;   (#set! conceal ""))

; (shortcut_link
;   (link_text) @full_link
;   (#match? @full_link "[A-Z]\\|") @conceal
;   ; (#match? @link ".*") @conceal
;   (#set! conceal ""))
