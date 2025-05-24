;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent" :scale 2.0 :zoom 1.0
                 :page-width 0.6 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-preview-live '(inline block edit-special)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t :extend nil)))
 '(secondary-selection ((t :background "#ffe8e8" :foreground unspecified :extend nil)))
 '(typst-ts-subscript-face ((t :height 1.0)))
 '(typst-ts-superscript-face ((t :height 1.0))))
