(define-module (ebeem packages fonts)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module (gnu packages fonts)
  #:use-module ((guix licenses) #:prefix license:))

(define-public font-iosevka-ss14
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss14")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-ss14-" version ".zip"))
       (sha256
        (base32 "1liapgr528qd88y6brhskcniddxanqqmx2qww21rqfyv9wl110wj"))))))

(define-public font-iosevka-nerd
  (package
    (name "font-iosevka-nerd")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
             version "/Iosevka.zip"))
       (sha256
        (base32 "09zmfksn1qyhc0pql5cvr0nmd8i8swjlipjfhwgf4zs6mdgac3fv"))))
    (build-system font-build-system)
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Iosevka Nerd Font")
    (description "Iosevka font with nerd icons.")
    (license license:silofl1.1)))
