;;; modus-alucard-theme.el --- Elegant, highly legible theme with a night sky background -*- lexical-binding:t -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'modus-themes t))
    (require 'modus-themes))

;;;###theme-autoload
  (deftheme modus-alucard
    "Elegant, highly legible theme with a night sky background.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard)."
    :background-mode 'dark
    :kind 'color-scheme
    :family 'modus)

  (defconst modus-alucard-palette
    '(

;; custom colors
      (th-rosewater 	"#f4dbd6")
      (th-flamingo 		"#f0c6c6")
      (th-pink 			"#f5bde6")
      (th-mauve 		"#c6a0f6")
      (th-red 			"#ed8796")
      (th-maroon 		"#ee99a0")
      (th-peach 		"#f5a97f")
      (th-yellow 		"#eed49f")
      (th-green 		"#a6da95")
      (th-teal 			"#8bd5ca")
      (th-sky 			"#91d7e3")
      (th-sapphire 		"#7dc4e4")
      (th-blue 			"#8aadf4")
      (th-lavender 		"#b7bdf8")
      (th-text 			"#cad3f5")
      (th-subtext1 		"#b8c0e0")
      (th-subtext0 		"#a5adcb")
      (th-overlay2 		"#939ab7")
      (th-overlay1 		"#8087a2")
      (th-overlay0 		"#6e738d")
      (th-surface2 		"#5b6078")
      (th-surface1 		"#494d64")
      (th-surface0 		"#363a4f")
      (th-base 			"#24273a")
      (th-mantle 		"#1e2030")
      (th-crust 		"#181926")

      (th-accent 		th-mauve)

;;; Basic values

      (bg-main          th-base)
      (bg-dim           th-crust)
      (fg-main          th-text)
      (fg-dim           th-subtext0)
      (fg-alt           th-accent)
      (bg-active        th-surface1)
      (bg-inactive      th-surface0)
      (border           th-accent)

;;; Common accent foregrounds
      (red             th-red)
      (red-warmer      "#bd6c78")
      (red-cooler      "#f09fab")
      (red-faint       "#8e515a")
      (red-intense     "#f4b7c0")
      (green           th-green)
      (green-warmer    "#84ae77")
      (green-cooler    "#b7e1aa")
      (green-faint     "#638259")
      (green-intense   "#c9e8bf")
      (yellow          th-yellow)
      (yellow-warmer   "#bea97f")
      (yellow-cooler   "#f1dcb2")
      (yellow-faint    "#8e7f5f")
      (yellow-intense  "#f4e5c5")
      (blue            th-blue)
      (blue-warmer     "#6e8ac3")
      (blue-cooler     "#a1bdf6")
      (blue-faint      "#a1bdf6")
      (blue-intense    "#b8cdf8")
      (magenta         th-mauve)
      (magenta-warmer  "#9e80c4")
      (magenta-cooler  "#d1b3f7")
      (magenta-faint   "#766093")
      (magenta-intense "#dcc6f9")
      (cyan            th-sky)
      (cyan-warmer     "#74acb5")
      (cyan-cooler     "#a6dfe8")
      (cyan-faint      "#a6dfe8")
      (cyan-intense    "#bde7ee")

;;; Uncommon accent foregrounds

      (rust       th-peach)
      (gold       th-yellow)
      (olive      th-green)
      (slate      th-teal)
      (indigo     magenta)
      (maroon     th-maroon)
      (pink       th-pink)

;;; Common accent backgrounds

      (bg-red-intense     "#9d1f1f")
      (bg-green-intense   "#2f822f")
      (bg-yellow-intense  "#7a6100")
      (bg-blue-intense    "#1640b0")
      (bg-magenta-intense "#7030af")
      (bg-cyan-intense    "#2266ae")

      (bg-red-subtle      "#620f2a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#4a4000")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#552f5f")
      (bg-cyan-subtle     "#004065")

      (bg-red-nuanced     "#3a0c14")
      (bg-green-nuanced   "#092f1f")
      (bg-yellow-nuanced  "#381d0f")
      (bg-blue-nuanced    "#12154a")
      (bg-magenta-nuanced "#2f0c3f")
      (bg-cyan-nuanced    "#042837")

;;; Uncommon accent backgrounds

      (bg-ochre    "#442c2f")
      (bg-lavender "#38325c")
      (bg-sage     "#0f3d30")

;;; Graphs

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#0fed00")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#f1e00a")
      (bg-graph-yellow-1  "#b08940")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special purpose

      (bg-completion       th-surface1)
      (bg-hover            "#45605e")
      (bg-hover-secondary  "#654a39")
      (bg-hl-line          th-surface1)
      (bg-region           "#555a66")
      (fg-region           "#ffffff")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

      (bg-mode-line-active        th-crust)
      (fg-mode-line-active        th-text)
      (border-mode-line-active    th-accent)
      (bg-mode-line-inactive      th-crust)
      (fg-mode-line-inactive      th-overlay2)
      (border-mode-line-inactive  th-crust)

      (modeline-err     th-red)
      (modeline-warning th-yellow)
      (modeline-info    th-sapphire)

      (bg-tab-bar      "#2c3045")
      (bg-tab-current  "#0d0e1c")
      (bg-tab-other    "#4a4f6a")

;;; Diffs

      (bg-added           "#003a2f")
      (bg-added-faint     "#002922")
      (bg-added-refine    "#035542")
      (bg-added-fringe    "#23884f")
      (fg-added           "#a0e0a0")
      (fg-added-intense   "#80e080")

      (bg-changed         "#363300")
      (bg-changed-faint   "#2a1f00")
      (bg-changed-refine  "#4a4a00")
      (bg-changed-fringe  "#8f7a30")
      (fg-changed         "#efef80")
      (fg-changed-intense "#c0b05f")

      (bg-removed         "#4f1127")
      (bg-removed-faint   "#380a19")
      (bg-removed-refine  "#781a3a")
      (bg-removed-fringe  "#b81a26")
      (fg-removed         "#ffbfbf")
      (fg-removed-intense "#ff9095")

      (bg-diff-context    "#1a1f30")

;;; Paren match

      (bg-paren-match        th-overlay1)
      (fg-paren-match        fg-main)
      (bg-paren-expression   th-maroon)
      (underline-paren-match unspecified)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor th-rosewater)

      (keybind magenta)
      (name magenta)
      (identifier yellow-faint)

      (err th-red)
      (warning th-yellow)
      (info th-green)

      (underline-err th-red)
      (underline-warning th-yellow)
      (underline-note th-sapphire)

      (bg-prominent-err bg-red-intense)
      (fg-prominent-err fg-main)
      (bg-prominent-warning bg-yellow-intense)
      (fg-prominent-warning fg-main)
      (bg-prominent-note bg-cyan-intense)
      (fg-prominent-note fg-main)

      (bg-active-argument bg-yellow-nuanced)
      (fg-active-argument yellow-cooler)
      (bg-active-value bg-cyan-nuanced)
      (fg-active-value cyan-cooler)

;;;; Code mappings

      (bracket fg-main)
      (builtin th-peach)
      (comment th-overlay1)
      (constant th-peach)
      (docstring th-overlay1)
      (docmarkup th-overlay1)
      (fnname th-blue)
      (keyword magenta)
      (preprocessor red)
      (string th-green)
      (type th-yellow)
      (variable text)
      (rx-construct th-green)
      (rx-backslash th-flamingo)

      (delimiter fg-main)
      (number fg-main)
      (operator fg-main)
      (punctuation fg-main)

;;;; Accent mappings

      (accent-0 magenta-cooler)
      (accent-1 th-peach)
      (accent-2 th-green)
      (accent-3 th-yellow)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 blue)
      (fg-completion-match-1 magenta)
      (fg-completion-match-2 cyan)
      (fg-completion-match-3 yellow)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common th-green)
      (date-deadline red)
      (date-deadline-subtle red-faint)
      (date-event fg-alt)
      (date-holiday red)
      (date-holiday-other blue)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-scheduled-subtle yellow-faint)
      (date-weekday cyan)
      (date-weekend red)

;;;; Line number mappings

      (fg-line-number-inactive fg-dim)
      (fg-line-number-active fg-main)
      (bg-line-number-inactive th-mantle)
      (bg-line-number-active th-mantle)

;;;; Link mappings

      (fg-link blue)
      (bg-link unspecified)
      (underline-link blue)

      (fg-link-symbolic cyan)
      (bg-link-symbolic unspecified)
      (underline-link-symbolic cyan)

      (fg-link-visited magenta)
      (bg-link-visited unspecified)
      (underline-link-visited magenta)

;;;; Mail mappings

      (mail-cite-0 blue)
      (mail-cite-1 yellow)
      (mail-cite-2 th-maroon)
      (mail-cite-3 red)
      (mail-part th-rosewater)
      (mail-recipient green)
      (mail-subject magenta)
      (mail-other blue)

;;;; Mark mappings

      (bg-mark-delete bg-red-subtle)
      (fg-mark-delete red-cooler)
      (bg-mark-select bg-red-subtle)
      (fg-mark-select red)
      (bg-mark-other bg-yellow-subtle)
      (fg-mark-other yellow)

;;;; Prompt mappings

      (fg-prompt th-mauve)
      (bg-prompt unspecified)

;;;; Prose mappings

      (bg-prose-block-delimiter bg-dim)
      (fg-prose-block-delimiter fg-dim)
      (bg-prose-block-contents bg-dim)

      (bg-prose-code unspecified)
      (fg-prose-code th-yellow)

      (bg-prose-macro unspecified)
      (fg-prose-macro th-teal)

      (bg-prose-verbatim unspecified)
      (fg-prose-verbatim th-magenta)

      (prose-done th-green)
      (prose-todo th-pink)

      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)

      (prose-table fg-alt)
      (prose-table-formula th-magenta)

      (prose-tag th-peach)

;;;; Rainbow mappings

      (rainbow-0 fg-main)
      (rainbow-1 th-mauve)
      (rainbow-2 th-maroon)
      (rainbow-3 th-green)
      (rainbow-4 th-peach)
      (rainbow-5 th-blue)
      (rainbow-6 th-red)
      (rainbow-7 th-pink)
      (rainbow-8 th-yellow)

;;;; Search mappings

      (bg-search-current bg-magenta-intense)
      (bg-search-lazy th-overlay0)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 blue)
      (bg-search-rx-group-1 magenta)
      (bg-search-rx-group-2 green)
      (bg-search-rx-group-3 red)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-red-intense)

;;;; Terminal mappings

      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "gray35")
      (fg-term-black-bright    "gray35")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-warmer)
      (fg-term-yellow-bright   yellow-warmer)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-warmer)
      (fg-term-blue-bright     blue-warmer)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "gray65")
      (fg-term-white           "gray65")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Heading mappings

      (fg-heading-0 th-mauve)
      (fg-heading-1 th-mauve)
      (fg-heading-2 th-peach)
      (fg-heading-3 th-blue)
      (fg-heading-4 th-red)
      (fg-heading-5 th-pink)
      (fg-heading-6 th-flamingo)
      (fg-heading-7 th-rosewater)
      (fg-heading-8 th-green)

      (bg-heading-0 unspecified)
      (bg-heading-1 unspecified)
      (bg-heading-2 unspecified)
      (bg-heading-3 unspecified)
      (bg-heading-4 unspecified)
      (bg-heading-5 unspecified)
      (bg-heading-6 unspecified)
      (bg-heading-7 unspecified)
      (bg-heading-8 unspecified)

      (overline-heading-0 unspecified)
      (overline-heading-1 unspecified)
      (overline-heading-2 unspecified)
      (overline-heading-3 unspecified)
      (overline-heading-4 unspecified)
      (overline-heading-5 unspecified)
      (overline-heading-6 unspecified)
      (overline-heading-7 unspecified)
      (overline-heading-8 unspecified))
    "The entire palette of the `modus-alucard' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-alucard-palette-user nil
    "Like the `modus-alucard-palette' for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings.  Those may then be used in combination with
palette overrides (also see `modus-themes-common-palette-overrides' and
`modus-alucard-palette-overrides')."
    :group 'modus-themes
    :package-version '(modus-themes . "4.5.0")
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

  (defcustom modus-alucard-palette-overrides nil
    "Overrides for `modus-alucard-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme modus-alucard
                      modus-alucard-palette
                      modus-alucard-palette-overrides)

  (provide-theme 'modus-alucard))

;;; modus-alucard-theme.el ends here
