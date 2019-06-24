;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-sdl2.
;;;
;;; Guile-sdl2 is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; SDL display and window management functions.
;;
;;; Code:

(define-module (sdl2 video)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (window?
            make-window
            close-window!
            call-with-window
            window-title
            window-size
            window-position
            window-id
            id->window
            hide-window!
            show-window!
            maximize-window!
            minimize-window!
            raise-window!
            restore-window!
            set-window-border!
            set-window-title!
            set-window-position!
            set-window-size!
            set-window-fullscreen!

            make-gl-context
            gl-context?
            delete-gl-context!
            call-with-gl-context
            swap-gl-window
            set-gl-attribute!
            set-gl-swap-interval!))


;;;
;;; Windows
;;;

(define-wrapped-pointer-type <window>
  window?
  wrap-window unwrap-window
  (lambda (window port)
    (format port "#<window id: ~s title: ~s size: ~s position: ~s>"
            (window-id window)
            (window-title window)
            (window-size window)
            (window-position window))))

(define* (make-window #:key (title "Guile SDL2 Window")
                          (position '(#f #f)) (size '(640 480))
                          (maximize? #f) (minimize? #f)
                          (show? #t) (resizable? #f)
                          (opengl? #f) (border? #t)
                          (fullscreen? #f) (fullscreen-desktop? #f)
                          (grab-input? #f) (high-dpi? #f))
  "Create a new window named TITLE with dimensions SIZE located at
POSITION on the display.  POSITION and SIZE are two-element lists in
the form '(x y)', where each coordinate is measured in pixels.  In the
case of POSITION, a coordinate may use the special symbol 'center' to
indicate that the window should be centered on that axis, or #f to
indicate that it does not matter where the window is located on that
axis."
  (define coord->int
    (match-lambda
      (#f ffi:SDL_WINDOWPOS_UNDEFINED)
      ('center ffi:SDL_WINDOWPOS_CENTERED)
      (n n)))

  (define x (match-lambda ((x _) x)))
  (define y (match-lambda ((_ y) y)))

  (let* ((flags (logior (if fullscreen?
                            ffi:SDL_WINDOW_FULLSCREEN
                            0)
                        (if fullscreen-desktop?
                            ffi:SDL_WINDOW_FULLSCREEN_DESKTOP
                            0)
                        (if opengl?
                            ffi:SDL_WINDOW_OPENGL
                            0)
                        (if show?
                            0
                            ffi:SDL_WINDOW_HIDDEN)
                        (if border?
                            0
                            ffi:SDL_WINDOW_BORDERLESS)
                        (if resizable?
                            ffi:SDL_WINDOW_RESIZABLE
                            0)
                        (if minimize?
                            ffi:SDL_WINDOW_MINIMIZED
                            0)
                        (if maximize?
                            ffi:SDL_WINDOW_MAXIMIZED
                            0)
                        (if grab-input?
                            ffi:SDL_WINDOW_INPUT_GRABBED
                            0)
                        (if high-dpi?
                            ffi:SDL_WINDOW_ALLOW_HIGHDPI
                            0)))
         (ptr (ffi:sdl-create-window (string->pointer title)
                                     (coord->int (x position))
                                     (coord->int (y position))
                                     (x size)
                                     (y size)
                                     flags)))
    (if (null-pointer? ptr)
        (sdl-error "make-window" "failed to create window")
        (wrap-window ptr))))

(define (close-window! window)
  "Close WINDOW."
  (ffi:sdl-destroy-window (unwrap-window window)))

(define (call-with-window window proc)
  "Call PROC with WINDOW, an SDL window object, and close it when PROC
returns or otherwise exits."
  (dynamic-wind
    (const #t)
    (lambda () (proc window))
    (lambda ()
      (close-window! window))))

(define (window-title window)
  "Return the title for WINDOW."
  (pointer->string (ffi:sdl-get-window-title (unwrap-window window))))

(define (%get-coords window proc)
  (let ((bv (make-bytevector (* 2 (sizeof int)) 0)))
    (proc (unwrap-window window)
          (bytevector->pointer bv)
          (bytevector->pointer bv (sizeof int)))
    (bytevector->sint-list bv (native-endianness) (sizeof int))))

(define (window-size window)
  "Return the dimensions of WINDOW."
  (%get-coords window ffi:sdl-get-window-size))

(define (window-position window)
  "Return the position of WINDOW on the display."
  (%get-coords window ffi:sdl-get-window-position))

(define (window-id window)
  "Return the numeric ID of WINDOW."
  (ffi:sdl-get-window-id (unwrap-window window)))

(define (id->window id)
  "Return the window corresponding to ID, a positive integer, or #f if
there is no such window."
  (let ((ptr (ffi:sdl-get-window-from-id id)))
    (if (null-pointer? ptr)
        #f
        (wrap-window ptr))))

(define (hide-window! window)
  "Hide WINDOW."
  (ffi:sdl-hide-window (unwrap-window window)))

(define (show-window! window)
  "Show WINDOW and focus on it."
  (ffi:sdl-show-window (unwrap-window window)))

(define (maximize-window! window)
  "Make WINDOW as large as possible."
  (ffi:sdl-maximize-window (unwrap-window window)))

(define (minimize-window! window)
  "Shrink WINDOW to an iconic representation."
  (ffi:sdl-minimize-window (unwrap-window window)))

(define (raise-window! window)
  "Raise WINDOW above all other windows and set input focus."
  (ffi:sdl-raise-window (unwrap-window window)))

(define (restore-window! window)
  "Restore the size and position of a minimized or maximized WINDOW."
  (ffi:sdl-restore-window (unwrap-window window)))

(define (set-window-border! window border?)
  "When BORDER?, draw the usual border around WINDOW, otherwise remove
the border."
  (ffi:sdl-set-window-bordered (unwrap-window window)
                               (ffi:boolean->sdl-bool border?)))

(define (set-window-title! window title)
  "Set the title of WINDOW to the string TITLE."
  (ffi:sdl-set-window-title (unwrap-window window)
                            (string->pointer title)))

(define (set-window-position! window position)
  "Set the position of WINDOW to POSITION, a two-element list of (x,y)
coordinates measured in pixels."
  (match position
    ((x y)
     (ffi:sdl-set-window-position (unwrap-window window) x y))))

(define (set-window-size! window size)
  "Set the dimensions of WINDOW to SIZE, a two-element list
of (width,height) coordinates measured in pixels."
  (match size
    ((width height)
     (ffi:sdl-set-window-size (unwrap-window window) width height))))

(define* (set-window-fullscreen! window fullscreen? #:key desktop?)
  "Toggle fullscreen mode on/off for WINDOW.  If FULLSCREEN?,
fullscreen mode is activated, otherwise it is deactivated.  If
FULLSCREEN? and DESKTOP?, a special \"fake\" fullscreen mode is used
that takes the size of the desktop."
  (let ((flag (cond
               ((and fullscreen? desktop?)
                ffi:SDL_WINDOW_FULLSCREEN_DESKTOP)
               (fullscreen?
                ffi:SDL_WINDOW_FULLSCREEN)
               (else 0))))
    (unless (zero? (ffi:sdl-set-window-fullscreen (unwrap-window window) flag))
      (sdl-error "set-window-fullscreen!" "failed to change fullscreen mode"))))


;;;
;;; OpenGL
;;;

(define-wrapped-pointer-type <gl-context>
  gl-context?
  wrap-gl-context unwrap-gl-context
  (lambda (context port)
    (format port "#<gl-context ~x>"
            (pointer-address (unwrap-gl-context context)))))

(define (make-gl-context window)
  "Create an OpenGL context for WINDOW."
  (let ((ptr (ffi:sdl-gl-create-context (unwrap-window window))))
    (if (null-pointer? ptr)
        (sdl-error "make-gl-context" "failed to create OpenGL context")
        (wrap-gl-context ptr))))

(define (delete-gl-context! context)
  "Delete CONTEXT, an OpenGL context object."
  (ffi:sdl-gl-delete-context (unwrap-gl-context context)))

(define (call-with-gl-context window proc)
  "Call PROC with a new OpenGL context created for WINDOW, and close
the context when PROC returns or otherwise exits.."
  (let ((context (make-gl-context window)))
    (dynamic-wind
      (const #t)
      (lambda () (proc context))
      (lambda ()
        (delete-gl-context! context)))))

(define (swap-gl-window window)
  "Update WINDOW with OpenGL rendering."
  (ffi:sdl-gl-swap-window (unwrap-window window)))

(define (set-gl-attribute! attr value)
  "Set the OpenGL attribute represented by the symbol ATTR to VALUE.
Possible values for ATTR are:

- red-size
- green-size
- blue-size
- alpha-size
- buffer-size
- double-buffer
- depth-size
- stencil-size
- accum-red-size
- accum-green-size
- accum-blue-size
- stereo
- multisample-buffers
- multisample-samples
- retained-backing
- context-major-version
- contet-minor-version
- context-egl
- context-flags
- context-profile-mask
- share-with-current-context
- framebuffer-srgb-capable"
  (let ((attr-enum
         (match attr
           ('red-size ffi:SDL_GL_RED_SIZE)
           ('green-size ffi:SDL_GL_GREEN_SIZE)
           ('blue-size ffi:SDL_GL_BLUE_SIZE)
           ('alpha-size ffi:SDL_GL_ALPHA_SIZE)
           ('buffer-size ffi:SDL_GL_BUFFER_SIZE)
           ('double-buffer ffi:SDL_GL_DOUBLEBUFFER)
           ('depth-size ffi:SDL_GL_DEPTH_SIZE)
           ('stencil-size ffi:SDL_GL_STENCIL_SIZE)
           ('accum-red-size ffi:SDL_GL_ACCUM_RED_SIZE)
           ('accum-green-size ffi:SDL_GL_ACCUM_GREEN_SIZE)
           ('accum-blue-size ffi:SDL_GL_ACCUM_BLUE_SIZE)
           ('stereo ffi:SDL_GL_STEREO)
           ('multisample-buffers ffi:SDL_GL_MULTISAMPLEBUFFERS)
           ('multisample-samples ffi:SDL_GL_MULTISAMPLESAMPLES)
           ('retained-backing ffi:SDL_GL_RETAINED_BACKING)
           ('context-major-version ffi:SDL_GL_CONTEXT_MAJOR_VERSION)
           ('context-minor-version ffi:SDL_GL_CONTEXT_MINOR_VERSION)
           ('context-egl ffi:SDL_GL_CONTEXT_EGL)
           ('context-flags ffi:SDL_GL_CONTEXT_FLAGS)
           ('context-profile-mask ffi:SDL_GL_CONTEXT_PROFILE_MASK)
           ('share-with-current-context ffi:SDL_GL_SHARE_WITH_CURRENT_CONTEXT)
           ('framebuffer-srgb-capable ffi:SDL_GL_FRAMEBUFFER_SRGB_CAPABLE))))
    (unless (zero? (ffi:sdl-gl-set-attribute attr-enum value))
      (sdl-error "set-gl-attribute!" "failed to set OpenGL attribute"))))

(define (set-gl-swap-interval! interval)
  "Set the framebuffer swap interval for the current OpenGL context to
the type indicated by the symbol INTERVAL.  Possible values of INTERVAL are:

- immediate, for immediate updates
- vsync, for updates synchronized with the screen's vertical retrace
- late-swap-tear, for late swap tearing

Late swap tearing works the same as vsync, but if the vertical retrace
has been missed for a given frame, buffers are swapped immediately,
which might be less jarring for the user during occasional framerate
drops."
  (unless (zero? (ffi:sdl-gl-set-swap-interval
                  (match interval
                    ('immediate 0)
                    ('vsync 1)
                    ('late-swap-tear -1))))
    (sdl-error "set-gl-swap-interval!" "failed to set OpenGL swap interval")))
