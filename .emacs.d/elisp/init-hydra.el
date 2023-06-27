;; init-hydra.el --- Initialize hydra configurations.   -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Nice looking hydras.
;;

;;; Code:

;; Major-mode binds to one specific file type
;; Minor-mode shared among all file types
;; Hydra groups related commands together to act like a temporary minor mode
(require-package 'hydra)


;; Don't treat 0-9 as digit-argument.
(with-eval-after-load 'hydra
  (setq hydra-base-map (make-sparse-keymap)))


(defhydra hydra-multiple-cursors (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_p_]   Previous      [_n_]   Next          [_a_] Match-All
[_P_]   Skip-Prev     [_N_]   Skip-Next     [_e_] Edit-All
[_M-p_] Unmark-Prev   [_M-n_] Unmark-Next   [_q_] Quit
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
(global-set-key (kbd "C-x m") #'hydra-multiple-cursors/body)


(defhydra hydra-window (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_s_] swap-window        [_o_] other-window       [_]_] enlarge-window-horizontally    [_)_] enlarge-window-vertically
[_p_] winner-undo        [_n_] winner-redo        [_[_] shrink-window-horizontally     [_(_] shrink-window-vertically
[_k_] delete-window      [_v_] preview-window     [_q_] quit
"
  ("s" transpose-windows)
  ("]" enlarge-window-horizontally)
  (")" enlarge-window-vertically)
  ("p" winner-undo)
  ("n" winner-redo)
  ("[" shrink-window-horizontally)
  ("(" shrink-window-vertically)
  ("v" sanityinc/split-window)
  ("o" other-window)
  ("k" delete-window)
  ("q" nil))
(global-set-key (kbd "C-x w") #'hydra-window/body)


(require 'org-ivy-search)
(defhydra hydra-quickness (:hint nil)
  "
                 ^Commands^
--------------------------------------------
[_a_] Counsel-Ag         [_g_] Counsel-Git-Grep [_f_] Counsel-Git      [_l_] Counsel-Locate     [_P_] Move-Text-Up
[_u_] Update-GTAGS       [_c_] Compile          [_C_] Mode-Compile     [_r_] Recent-File        [_N_] Move-Text-Down
[_p_] Previous-Mark      [_n_] Next-Mark        [_s_] Mate-Term        [_d_] Remove-Duplicate   [_o_] Org-Search-View
[_i_] Pin-Yin            [_w_] Google-Word      [_h_] Hs-Hide-Block    [_H_] Hs-Show-Block      [_t_] ChatGPT
[_S_] Sort-Lines         [_q_] Quit
  "
  ("a" smart/counsel-ag :exit t)
  ("g" counsel-git-grep :exit t)
  ("f" counsel-git :exit t)
  ("l" counsel-locate :exit t)
  ("u" ggtags-update-tags :exit t)
  ("c" compile :exit t)
  ("C" mode-compile :exit t)
  ("r" counsel-recentf :exit t)
  ("p" pop-to-mark-command)
  ("n" unpop-to-mark-command)
  ("s" my-exwm-exterm :exit t)
  ("d" delete-duplicate-lines :exit t)
  ("P" move-text-up)
  ("N" move-text-down)
  ("h" hs-hide-block)
  ("H" hs-show-block)
  ("o" org-ivy-search-view :exit t)
  ("i" hydra-pyim-start :exit t)
  ("w" modi/eww-search-words :exit t)
  ("t" gptel :exit t)
  ("S" sort-lines :exit t)
  ("q" nil))
(global-set-key (kbd "C-x q") #'hydra-quickness/body)


;; pretty-hydra
(when (maybe-require-package 'pretty-hydra)
  (use-package pretty-hydra
    :bind ("C-x C-h" . toggles-hydra/body)
    :init
    (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                        &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face `(:foreground ,(face-background 'highlight))))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0)))
        (concat (propertize title 'face face))))

    ;; Global toggles
    (with-no-warnings
      (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on" :v-adjust -0.1)
                                                 :color amaranth :quit-key "q")
        ("Play"
         (("p b" bongo "Bongo" :exit t)
          ("p c" open-calendar "Calendar" :exit t)
          ("p k" keyfreq-show "Keyfreq" :exit t)
          ("p n" newsticker-show-news "Newsticker" :exit t)
          ("p s" stock-tracker-start "Stock" :exit t)
          ("p w" wttrin "Weather" :exit t))
         "Gist"
         (("g s" gist-region-or-buffer-private "Gist-Share" :exit t)
          ("g l" gist-list "Gist-List" :exit t))
         "Misc"
         (("m r" everlasting-scratch-restore "Restore-Scratch" :exit t)
          ("m c" wandbox "Online-Compile" :exit t)
          ("m d" download-region-as-url "Download-Region" :exit t)
          ("m D" download-region-cancel "Download-Cancel" :exit t)
          ;; ("m s" sublimity-mode "Sublimity-Mode" :exit t)
          )
         "Ipc"
         (("i c" ipc-udp-client-send "Ipc-Udp-Client-Send" :exit t)
          ("i C" ipc-udp-client-stop "Ipc-Udp-Client-Stop" :exit t)
          ("i s" ipc-udp-server-start "Ipc-Udp-Server-Start" :exit t)
          ("i S" ipc-udp-server-stop "Ipc-Udp-Server-Stop" :exit t))
         "System"
         (("s p" proced "List-Process" :exit t)
          ("s m" memory-report "List-Memory" :exit t)
          ("s n" netstat "List-Network" :exit t)
          ("s e" list-environment "List-Environment" :exit t)
          ("s d" daemons "List-Daemons" :exit t)
          ("s c" tldr "Command-Cheatsheet" :exit t)))))))


(provide 'init-hydra)
;;; init-hydra.el ends here
