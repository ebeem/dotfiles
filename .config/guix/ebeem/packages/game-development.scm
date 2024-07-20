;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>

(define-module (ebeem packages game-development)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public godot-mono
  (package
    (name "godot-mono")
    (version "4.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godotengine/godot")
                    (commit (string-append version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wm0pla6f6gvk21gbm5kiihinn05dvvprk0242m6s8c78wy60wka"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               '(begin
                  ;; Keep only those bundled files we have not (yet) replaced
                  ;; with Guix versions. Note that some of these may be
                  ;; modified; see "thirdparty/README.md".
                  (with-directory-excursion "thirdparty"
                    (let* ((preserved-files
                            '("README.md"
                              "amd-fsr"
                              "amd-fsr2"
                              "assimp"
                              "astcenc"
                              "basis_universal"
                              ;; Godot needs ca-certificates.crt, but that is
                              ;; not available in build environment
                              "certs"
                              "clipper2"
                              "cvtt"
                              "linuxbsd_headers"
                              "etc2comp"
                              "etcpak"
                              "fonts"
                              "glad"
                              ;; TODO: Remove once Godot once again builds
                              ;; with our glslang package, or with a
                              ;; workaround.  Currently it looks for a Types.h
                              ;; which is no longer in the glslang output
                              ;; after the most recent update.
                              "glslang"
                              "jpeg-compressor"
                              "libktx"
                              "libsimplewebm"
                              "meshoptimizer"
                              "minimp3"
                              "miniupnpc"
                              "minizip"
                              "misc"
                              "msdfgen"
                              "nanosvg"
                              "noise"
                              "oidn"
                              "openxr"
                              "pvrtccompressor"
                              "recastnavigation"
                              "rvo2"
                              "spirv-reflect"
                              "squish"
                              "stb_rect_pack"
                              "thorvg"
                              "tinyexr"
                              "vhacd"
                              "volk"
                              "vulkan"
                              "xatlas")))
                      (for-each delete-file-recursively
                                (lset-difference string=?
                                                 (scandir ".")
                                                 (cons* "." ".." preserved-files)))))))))
    (build-system scons-build-system)
    (arguments
     (list
      #:scons-flags #~`("platform=linuxbsd" "target=editor" "production=yes"
                        ;; XXX: There may be advantages to enabling volk,
                        ;; requiring unbundling and patching to use our input.
                        "use_volk=no"
                        ;; Avoid using many of the bundled libs.
                        ;; Note: These options can be found in the SConstruct file.
                        "builtin_brotli=no"
                        "builtin_embree=no"
                        "builtin_enet=no"
                        "builtin_freetype=no"
                        ;; TODO: Uncomment this option when the todo for
                        ;; glslang in the snippet is resolved.
                        ;; "builtin_glslang=no"
                        "builtin_graphite=no"
                        "builtin_harfbuzz=no"
                        "builtin_icu4c=no"
                        "builtin_libogg=no"
                        "builtin_libpng=no"
                        "builtin_libtheora=no"
                        "builtin_libvorbis=no"
                        "builtin_libwebp=no"
                        "builtin_mbedtls=no"
                        "builtin_pcre2=no"
                        "builtin_pcre2_with_jit=no"
                        "builtin_wslay=no"
                        "builtin_zlib=no"
                        "builtin_zstd=no"
                        "module_mono_enabled=yes")
      #:tests? #f                      ; There are no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'scons-use-env
            (lambda _
              ;; Scons does not use the environment variables by default,
              ;; but this substitution makes it do so.
              (substitute* "SConstruct"
                (("env_base = Environment\\(tools=custom_tools\\)")
                 (string-append
                  "env_base = Environment(tools=custom_tools)\n"
                  "env_base = Environment(ENV=os.environ)")))))
          (add-after 'scons-use-env 'fix-dlopen-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((files '("drivers/alsa/asound-so_wrap.c"
                             "drivers/pulseaudio/pulse-so_wrap.c"
                             "platform/linuxbsd/dbus-so_wrap.c"
                             "platform/linuxbsd/fontconfig-so_wrap.c"
                             "platform/linuxbsd/libudev-so_wrap.c"
                             "platform/linuxbsd/speechd-so_wrap.c"
                             "platform/linuxbsd/x11/display_server_x11.cpp"
                             "platform/linuxbsd/x11/dynwrappers/xcursor-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xext-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xinerama-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xinput2-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xlib-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xrandr-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xrender-so_wrap.c"
                             "platform/linuxbsd/xkbcommon-so_wrap.c"
                             "thirdparty/volk/volk.c"
                             "thirdparty/volk/volk.c"))
                    (libs '("libasound.so.2"
                            "libpulse.so.0"
                            "libdbus-1.so.3"
                            "libfontconfig.so.1"
                            "libudev.so.1"
                            "libspeechd.so.2"
                            "libXrandr.so.2"
                            "libXcursor.so.1"
                            "libXext.so.6"
                            "libXinerama.so.1"
                            "libXi.so.6"
                            "libX11.so.6"
                            "libXrandr.so.2"
                            "libXrender.so.1"
                            "libxkbcommon.so.0"
                            "libvulkan.so.1"
                            "libvulkan.so")))
                (for-each (lambda (file lib)
                            (substitute* file
                              (((string-append "dlopen\\(\"" lib "\""))
                               (string-append "dlopen(\""
                                              (search-input-file
                                               inputs (string-append "lib/" lib))
                                              "\""))))
                          files libs))
              (substitute* "thirdparty/glad/gl.c"
                (("libGL.so") ; for both .so and .so.1
                 (string-append (search-input-file inputs "lib/libGL.so"))))
              (substitute* "thirdparty/glad/glx.c"
                (("libGL.so") ; for both .so and .so.1
                 (string-append (search-input-file inputs "lib/libGL.so"))))))
          (add-after 'fix-dlopen-paths 'unbundle-xkbcommon
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "platform/linuxbsd/xkbcommon-so_wrap.c"
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon.h")))
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon-compose.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon-compose.h")))
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon-keysyms.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon-keysyms.h"))))))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((zenity (search-input-file inputs "bin/zenity")))
                ;; Strip build info from filenames.
                (with-directory-excursion "bin"
                  (for-each
                   (lambda (file)
                     (let ((dest (car (string-split (basename file) #\.))))
                       (rename-file file dest)))
                   (find-files "." "godot.*\\.linuxbsd\\.editor.*"))
                  (install-file "godot" (string-append #$output "/bin")))
                ;; Tell the editor where to find zenity for OS.alert().
                ;; TODO: This could be changed in
                ;; platform/linuxbsd/os_linuxbsd.cpp directly, along with the
                ;; other alert programs.
                (wrap-program (string-append #$output "/bin/godot")
                  `("PATH" ":" prefix (,(string-append zenity "/bin")))))))
          (add-after 'install 'install-godot-desktop
            (lambda _
              (let ((applications (string-append #$output "/share/applications"))
                     (icons (string-append #$output "/share/icons/hicolor")))
                (mkdir-p applications)
                (copy-file "misc/dist/linux/org.godotengine.Godot.desktop"
                           (string-append applications "/godot.desktop"))
                (for-each (lambda (icon dest)
                            (mkdir-p (dirname dest))
                            (copy-file icon dest))
                          '("icon.png" "icon.svg")
                           `(,(string-append icons "/256x256/apps/godot.png")
                             ,(string-append icons "/scalable/apps/godot.svg")))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           brotli
           dbus
           embree
           enet
           eudev
           fontconfig
           freetype-with-brotli
           glew
           glslang
           glu
           libpng
           harfbuzz
           icu4c
           libtheora
           libvorbis
           libvpx
           libwebp
           libx11
           libxcursor
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           mbedtls-lts
           mesa
           openxr
           opusfile
           pcre2
           pulseaudio
           speech-dispatcher
           vulkan-loader
           wslay
           zenity
           zlib
           `(,zstd "lib")))
    (home-page "https://godotengine.org/")
    (synopsis "Advanced 2D and 3D game engine")
    (description
     "Godot is an advanced multi-platform game engine written in C++.  If
features design tools such as a visual editor, can import 3D models and
provide high-quality 3D rendering, it contains an animation editor, and can be
scripted in a Python-like language.")
    (license license:expat)))
