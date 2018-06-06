;;----------------------------------------------------------------------------
;; hydra setting
;;----------------------------------------------------------------------------

;; Major-mode binds to one specific file type
;; Minor-mode shared among all file types
;; Hydra groups related commands together to act like a temporary minor mode
(require-package 'hydra)

;; Don't treat 0-9 as digit-argument.
(after-load 'hydra
  (setq hydra-base-map (make-sparse-keymap)))

(defhydra hydra-multiple-cursors (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_p_]   Next          [_n_]   Next             [_a_] All
[_P_]   Skip          [_N_]   Skip             [_e_] Edit
[_M-p_] Unmark        [_M-n_] Unmark           [_q_] Quit
^ ^                   ^ ^
"
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("e" mc/edit-lines)
  ("q" nil))
(global-set-key (kbd "C-x m")  'hydra-multiple-cursors/body)

(defhydra hydra-window (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_s_] split-window    [_d_] dedicate-current-window
[_p_] winner-undo     [_n_] winner-redo
[_q_] quit
"
  ("s" sanityinc/split-window)
  ("d" sanityinc/toggle-current-window-dedication :exit t)
  ("p" winner-undo)
  ("n" winner-redo)
  ("q" nil))
(global-set-key (kbd "C-x w") 'hydra-window/body)

(defhydra hydra-quickness (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_a_] Swiper-at-point [_s_] Counsel-git-grep   [_g_] Counsel-git     [_l_] Counsel-locate
[_u_] Update-GTAGS    [_c_] Mode-Compile       [_C_] Compile         [_r_] Recompile
[_e_] Eww-Open-File   [_w_] Google-Search-Word [_k_] Google-Lucky    [_p_] Previous-mark
[_n_] Next-mark       [_i_] Pyim               [_q_] Quit
"
  ("a" smart/swiper-at-point :exit t)
  ("s" counsel-git-grep :exit t)
  ("g" counsel-git :exit t)
  ("l" counsel-locate :exit t)
  ("u" ggtags-update-tags :exit t)
  ("c" mode-compile :exit t)
  ("C" compile :exit t)
  ("e" modi/eww-browse-url-of-file :exit t)
  ("w" modi/eww-search-words :exit t)
  ("k" eww-im-feeling-lucky :exit t)
  ("r" recompile :exit t)
  ("i" hydra-pyim-start :exit t)
  ("p" pop-to-mark-command)
  ("n" unpop-to-mark-command)
  ("q" nil))
(global-set-key (kbd "C-x q")  'hydra-quickness/body)


(provide 'init-hydra)
;;; init-hydra.el ends here
