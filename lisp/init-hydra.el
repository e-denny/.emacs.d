;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------------

(use-package hydra)

;; ----------------------------------------------------------------------
;; hydras
;; ----------------------------------------------------------------------

(with-eval-after-load 'hydra
  (defhydra hydra-spelling (:color blue)
    "
  ^Spelling^        ^Errors^            ^Checker^
  ^────────^────────^──────^────────────^───────^───────
  [_q_] quit        [_<_] previous      [_c_] correction
  ^^                [_>_] next          [_d_] dictionary
  ^^                [_f_] check         [_m_] mode
  "
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" ispell)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer :color pink)
    ("m" flyspell-mode))


  (defhydra hydra-info (:hint nil)
    "
   Info-mode:
   [_j_] forward   [_l_] last     [_u_] up         [_f_] follow reference  [_T_] TOC
   [_k_] backward  [_r_] return   [_m_] menu       [_i_] index             [_d_] directory
   [_n_] nex       [_H_] history  [_g_] goto       [_,_] next index item   [_c_] copy node name
   [_p_] prev      [_<_] top      [_b_] beginning  [_I_] virtual index     [_C_] clone buffer
   [_s_] search    [_>_] final    [_e_] end        ^^                      [_a_] apropos

   [_1_] .. [_9_] Pick first .. ninth item in the node's menu.
  "

    ("j"   Info-forward-node)
    ("k"   Info-backward-node)
    ("n"   Info-next)
    ("p"   Info-prev)
    ("s"   Info-search)
    ("S"   Info-search-case-sensitively)

    ("l"   Info-history-back)
    ("r"   Info-history-forward)
    ("H"   Info-history)
    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("u"   Info-up)
    ("^"   Info-up)
    ("m"   Info-menu)
    ("g"   Info-goto-node)
    ("b"   beginning-of-buffer)
    ("e"   end-of-buffer)

    ("f"   Info-follow-reference)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("h"   Info-help "Info help")
    ("q"   Info-exit "Info exit")
    ("." nil "cancel" :color blue))


  (define-key Info-mode-map "." 'hydra-info/body)


  (defhydra hydra-next-error
    (global-map "C-x")
    "
  Compilation errors:
  _j_: next error        _h_: first error    _q_uit
  _k_: previous error    _l_: last error
  "
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))
  )

(provide 'init-hydra)
;;; init-hydra.el ends here
