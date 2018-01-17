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
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_q_] Quit
[_P_]   Skip    [_N_]   Skip
[_M-p_] Unmark  [_M-n_] Unmark
^ ^             ^ ^
"
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))
(global-set-key (kbd "C-x m")  'hydra-multiple-cursors/body)

(defhydra hydra-window (:hint nil)
   "
    ^Move^        ^Split^             ^Switch^          ^Other^
-------------------------------------------------------------
[_h_] ←       [_v_] vertical      [_b_] buffer      [_q_] quit
[_j_] ↓       [_x_] horizontal    [_f_] find files
[_k_] ↑       [_z_] undo          [_S_] save
[_l_] →       [_Z_] reset         [_d_] delete
[_F_] follow  [_o_] only this
"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("S" save-buffer)
   ("d" delete-window)
   ("o" delete-other-windows)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo)))
   ("Z" winner-redo)
   ("q" nil))
(global-set-key (kbd "C-x w") 'hydra-window/body)

(defhydra hydra-quickness (:hint nil)
  "
                 ^Commands^
-----------------------------------------
[_a_] Swiper-at-point [_s_] Counsel-git-grep   [_g_] Counsel-git     [_l_] Counsel-locate
[_u_] Update-GTAGS    [_c_] Mode-Compile       [_C_] Compile         [_r_] Recompile
[_e_] Eww-Open-File   [_w_] Google-Search-Word [_k_] Google-Lucky    [_p_] Pyim
[_q_] Quit
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
  ("p" hydra-pyim-start :exit t)
  ("q" nil))
(global-set-key (kbd "C-x q")  'hydra-quickness/body)


(provide 'init-hydra)
