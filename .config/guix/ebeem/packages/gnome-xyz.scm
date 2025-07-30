;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>

(define-module (ebeem packages gnome-xyz)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public colloid-catppuccin-purple-gtk-theme
  (package
    (name "colloid-gtk-theme")
    (version "2024.11.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vinceliuice/Colloid-gtk-theme")
                    (commit (string-replace-substring version "." "-"))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (sha256
               (base32
                "0nb525kq16svmfqfcblmis8rlsd0h175xnzcgck6sw6srsgw6hgg"))))
    (build-system copy-build-system)
    (inputs
     (list murrine sassc))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (let* ((dest (string-append #$output "/share/themes"))
                          (flags (list "--theme" "purple"
                                       "--color" "dark"
                                       "--tweaks" "catppuccin"
                                       "--dest" dest)))
                     (mkdir-p dest)
                     (apply invoke "bash" "install.sh" flags)))))))
    (home-page "https://www.pling.com/p/1296407/")
    (synopsis "Colloid gtk theme for Linux")
    (description "This package provides a flat colorful design gtk theme.")
    (license license:gpl3)))

(define-public colloid-catppuccin-purple-icon-theme
  (package
    (name "colloid-catppuccin-purple-icon-theme")
    (version "2025.07.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vinceliuice/Colloid-icon-theme")
                    (commit (string-replace-substring version "." "-"))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet '(substitute* "install.sh"
                          (("gtk-update-icon-cache") "true")))
              (sha256
               (base32
                "1gm76i62nmary08nivx86brccb5apl44236j3pn4s978ilql8c8b"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (let* ((dest (string-append #$output "/share/icons"))
                          (flags (list "--theme" "purple"
                                       "--scheme" "catppuccin"
                                       "--dest" dest)))
                     (mkdir-p dest)
                     (apply invoke "bash" "install.sh" flags)))))))
    (home-page "https://www.pling.com/p/1296407/")
    (synopsis "Colloid icon theme for Linux")
    (description "This package provides a flat colorful design icon theme.")
    (license license:gpl3)))
