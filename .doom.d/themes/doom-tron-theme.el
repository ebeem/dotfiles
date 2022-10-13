;;; doom-tron-theme.el --- inspired by Tron Legacy -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021-2022 Ibraheem Almarhoon
;;
;; Author: Ibraheem Almarhoon <https://github.com/ebeem>
;; Created: October 13, 2022
;; Version: 1.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Inspired by Tron Legacy's color scheme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tron-theme nil
  "Options for the `doom-tron' theme."
  :group 'doom-themes)

(defcustom doom-tron-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-tron-theme
  :type 'boolean)

(defcustom doom-tron-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-tron-theme
  :type 'boolean)

(defcustom doom-tron-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-tron-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tron
  "A dark theme inspired by Tron Legacy."

  ;; name        default   256           16
  ((bg         '("#0e1724" "black"       "black"  ))
   (fg         '("#E6FFFF" "#bfbfbf"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#101b2a" "black"       "black"        ))
   (fg-alt     '("#E6FFFF" "#2d2d2d"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#0C141F" "black"       "black"        ))
   (base1      '("#0B1724" "#1e1e1e"     "brightblack"  ))
   (base2      '("#0F1B28" "#2e2e2e"     "brightblack"  ))
   (base3      '("#121F2E" "#262626"     "brightblack"  ))
   (base4      '("#2E3C4A" "#3f3f3f"     "brightblack"  ))
   (base5      '("#4A5A68" "#525252"     "brightblack"  ))
   (base6      '("#62717E" "#6b6b6b"     "brightblack"  ))
   (base7      '("#8B98A4" "#979797"     "brightblack"  ))
   (base8      '("#D3E0EC" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#B62D66" "#DF740C" "red"          ))
   (orange     '("#DF740C" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#DF740C" "#FFE64D" "yellow"       ))
   (blue       '("#6FC3DF" "#6FC3DF" "brightblue"   ))
   (dark-blue  '("#6FC3DF" "#6FC3DF" "blue"         ))
   (magenta    '("#6FC3DF" "#c678dd" "brightmagenta"))
   (violet     '("#6FC3DF" "#a9a1e1" "magenta"      ))
   (cyan       '("#6FC3DF" "#6FC3DF" "brightcyan"   ))
   (dark-cyan  '("#6FC3DF" "#6FC3DF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-tron-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-tron-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (hl-line        cyan)
   (region         `(,(doom-lighten (car bg) 0.25) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-tron-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg 0.1)))
   (modeline-bg-alt          (if doom-tron-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-tron-padded-modeline
      (if (integerp doom-tron-padded-modeline) doom-tron-padded-modeline 4))))


  ;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-tron-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-tron-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-tron-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())
;;; doom-tron-theme.el ends here
