;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
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
;; SDL event handling.
;;
;;; Code:

(define-module (sdl2 events)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (make-quit-event
            quit-event?
            quit-event-timestamp

            make-window-event
            window-event?
            window-shown-event?
            window-hidden-event?
            window-exposed-event?
            window-moved-event?
            window-resized-event?
            window-size-changed-event?
            window-minimized-event?
            window-maximized-event?
            window-restored-event?
            window-enter-event?
            window-leave-event?
            window-focus-gained-event?
            window-focus-lost-event?
            window-closed-event?
            window-event-timestamp
            window-event-window-id
            window-event-type
            window-event-vector

            make-keyboard-event
            keyboard-event?
            keyboard-down-event?
            keyboard-up-event?
            keyboard-event-timestamp
            keyboard-event-window-id
            keyboard-event-pressed?
            keyboard-event-repeat?
            keyboard-event-key
            keyboard-event-scancode
            keyboard-event-modifiers

            make-text-input-event
            text-input-event?
            text-input-event-timestamp
            text-input-event-window-id
            text-input-event-text

            make-mouse-button-event
            mouse-button-event?
            mouse-button-down-event?
            mouse-button-up-event?
            mouse-button-event-timestamp
            mouse-button-event-window-id
            mouse-button-event-which
            mouse-button-event-button
            mouse-button-event-pressed?
            mouse-button-event-clicks
            mouse-button-event-x
            mouse-button-event-y

            make-mouse-motion-event
            mouse-motion-event?
            mouse-motion-event-timestamp
            mouse-motion-event-window-id
            mouse-motion-event-which
            mouse-motion-event-buttons
            mouse-motion-event-x
            mouse-motion-event-y
            mouse-motion-event-x-rel
            mouse-motion-event-y-rel

            make-joystick-axis-event
            joystick-axis-event?
            joystick-axis-event-timestamp
            joystick-axis-event-which
            joystick-axis-event-axis
            joystick-axis-event-value

            make-joystick-button-event
            joystick-button-event?
            joystick-button-down-event?
            joystick-button-up-event?
            joystick-button-event-timestamp
            joystick-button-event-which
            joystick-button-event-button
            joystick-button-event-pressed?

            make-joystick-ball-event
            joystick-ball-event?
            joystick-ball-event-timestamp
            joystick-ball-event-which
            joystick-ball-event-ball
            joystick-ball-event-x-rel
            joystick-ball-event-y-rel

            make-joystick-hat-event
            joystick-hat-event?
            joystick-hat-event-timestamp
            joystick-hat-event-which
            joystick-hat-event-hat
            joystick-hat-event-value

            make-joystick-device-event
            joystick-device-event?
            joystick-device-event-timestamp
            joystick-device-event-which
            joystick-device-event-action

            make-controller-axis-event
            controller-axis-event?
            controller-axis-event-timestamp
            controller-axis-event-which
            controller-axis-event-axis
            controller-axis-event-value

            make-controller-button-event
            controller-button-event?
            controller-button-down-event?
            controller-button-up-event?
            controller-button-event-timestamp
            controller-button-event-which
            controller-button-event-button
            controller-button-event-pressed?

            make-controller-device-event
            controller-device-event?
            controller-added-event?
            controller-removed-event?
            controller-remapped-event?
            controller-device-event-timestamp
            controller-device-event-which
            controller-device-event-action

            poll-event))

(define (make-sdl-event)
  (make-u8vector 56 0)) ; sizeof(SDL_Event) is 56

(define (sdl-event-type event)
  (u32vector-ref event 0))


;;;
;;; Quit
;;;

(define-record-type <quit-event>
  (make-quit-event timestamp)
  quit-event?
  (timestamp quit-event-timestamp))

(define (parse-quit-event ptr)
  (match (parse-c-struct ptr (list uint32 uint32))
    ((_ timestamp)
     (make-quit-event timestamp))))


;;;
;;; Window
;;;

(define-record-type <window-event>
  (make-window-event timestamp window-id type vector)
  window-event?
  (timestamp window-event timestamp)
  (window-id window-event-window-id)
  (type window-event-type)
  (vector window-event-vector))

(define (window-shown-event? e)
  "Return #t if E is a window shown event."
  (and (window-event? e)
       (eq? 'shown (window-event-type e))))

(define (window-hidden-event? e)
  "Return #t if E is a window hidden event."
  (and (window-event? e)
       (eq? 'hidden (window-event-type e))))

(define (window-exposed-event? e)
  "Return #t if E is a window exposed event."
  (and (window-event? e)
       (eq? 'exposed (window-event-type e))))

(define (window-moved-event? e)
  "Return #t if E is a window moved event."
  (and (window-event? e)
       (eq? 'moved (window-event-type e))))

(define (window-resized-event? e)
  "Return #t if E is a window resized event."
  (and (window-event? e)
       (eq? 'resized (window-event-type e))))

(define (window-size-changed-event? e)
  "Return #t if E is a window size changed event."
  (and (window-event? e)
       (eq? 'size-changed (window-event-type e))))

(define (window-minimized-event? e)
  "Return #t if E is a window minimized event."
  (and (window-event? e)
       (eq? 'minimized (window-event-type e))))

(define (window-maximized-event? e)
  "Return #t if E is a window maximized event."
  (and (window-event? e)
       (eq? 'maximized (window-event-type e))))

(define (window-restored-event? e)
  "Return #t if E is a window restored event."
  (and (window-event? e)
       (eq? 'restored (window-event-type e))))

(define (window-enter-event? e)
  "Return #t if E is a window enter event."
  (and (window-event? e)
       (eq? 'enter (window-event-type e))))

(define (window-leave-event? e)
  "Return #t if E is a window leave event."
  (and (window-event? e)
       (eq? 'leave (window-event-type e))))

(define (window-focus-gained-event? e)
  "Return #t if E is a window focus gained event."
  (and (window-event? e)
       (eq? 'focus-gained (window-event-type e))))

(define (window-focus-lost-event? e)
  "Return #t if E is a window focus lost event."
  (and (window-event? e)
       (eq? 'focus-lost (window-event-type e))))

(define (window-closed-event? e)
  "Return #t if E is a window closed event."
  (and (window-event? e)
       (eq? 'close (window-event-type e))))

(define (parse-window-event ptr)
  (define (type-symbol n)
    (list-ref '(none
                shown
                hidden
                exposed
                moved
                resized
                size-changed
                minimized
                maximized
                restored
                enter
                leave
                focus-gained
                focus-lost
                close)
              (1- n)))

  (define types
    (list uint32  ; type
          uint32  ; timestamp
          uint32  ; windowID
          uint8   ; event
          uint8   ; padding1
          uint8   ; padding2
          uint8   ; padding3
          int32   ; data1
          int32)) ; data2

  (match (parse-c-struct ptr types)
    ((_ timestamp id event _ _ _ x y)
     (make-window-event timestamp id (type-symbol event) (list x y)))))


;;;
;;; Keyboard
;;;

(define-record-type <keyboard-event>
  (make-keyboard-event timestamp window-id pressed? repeat?
                       key scancode modifiers)
  keyboard-event?
  (timestamp keyboard-event-timestamp)
  (window-id keyboard-event-window-id)
  (pressed? keyboard-event-pressed?)
  (repeat? keyboard-event-repeat?)
  (key keyboard-event-key)
  (scancode keyboard-event-scancode)
  (modifiers keyboard-event-modifiers))

(define (keyboard-down-event? e)
  "Return #t if E is a key press event."
  (and (keyboard-event? e)
       (keyboard-event-pressed? e)))

(define (keyboard-up-event? e)
  "Return #t if E is a key release event."
  (and (keyboard-event? e)
       (not (keyboard-event-pressed? e))))

(define keycode-map
  (alist->hash-table
   `((,ffi:SDLK_UNKNOWN . unknown)
     (,ffi:SDLK_RETURN . return)
     (,ffi:SDLK_ESCAPE . escape)
     (,ffi:SDLK_BACKSPACE . backspace)
     (,ffi:SDLK_TAB . tab)
     (,ffi:SDLK_SPACE . space)
     (,ffi:SDLK_EXCLAIM . exclaim)
     (,ffi:SDLK_QUOTEDBL . double-quote)
     (,ffi:SDLK_HASH . hash)
     (,ffi:SDLK_PERCENT . percent)
     (,ffi:SDLK_DOLLAR . dollar)
     (,ffi:SDLK_AMPERSAND . ampersand)
     (,ffi:SDLK_QUOTE . quote)
     (,ffi:SDLK_LEFTPAREN . left-paren)
     (,ffi:SDLK_RIGHTPAREN . right-paren)
     (,ffi:SDLK_ASTERISK . asterisk)
     (,ffi:SDLK_PLUS . plus)
     (,ffi:SDLK_COMMA . comma)
     (,ffi:SDLK_MINUS . minus)
     (,ffi:SDLK_PERIOD . period)
     (,ffi:SDLK_SLASH . slash)
     (,ffi:SDLK_0 . 0)
     (,ffi:SDLK_1 . 1)
     (,ffi:SDLK_2 . 2)
     (,ffi:SDLK_3 . 3)
     (,ffi:SDLK_4 . 4)
     (,ffi:SDLK_5 . 5)
     (,ffi:SDLK_6 . 6)
     (,ffi:SDLK_7 . 7)
     (,ffi:SDLK_8 . 8)
     (,ffi:SDLK_9 . 9)
     (,ffi:SDLK_COLON . colon)
     (,ffi:SDLK_SEMICOLON . semicolon)
     (,ffi:SDLK_LESS . less)
     (,ffi:SDLK_EQUALS . equals)
     (,ffi:SDLK_GREATER . greater)
     (,ffi:SDLK_QUESTION . question)
     (,ffi:SDLK_AT . at)
     (,ffi:SDLK_LEFTBRACKET . left-bracket)
     (,ffi:SDLK_BACKSLASH . backslash)
     (,ffi:SDLK_RIGHTBRACKET . right-bracket)
     (,ffi:SDLK_CARET . caret)
     (,ffi:SDLK_UNDERSCORE . underscore)
     (,ffi:SDLK_BACKQUOTE . backquote)
     (,ffi:SDLK_a . a)
     (,ffi:SDLK_b . b)
     (,ffi:SDLK_c . c)
     (,ffi:SDLK_d . d)
     (,ffi:SDLK_e . e)
     (,ffi:SDLK_f . f)
     (,ffi:SDLK_g . g)
     (,ffi:SDLK_h . h)
     (,ffi:SDLK_i . i)
     (,ffi:SDLK_j . j)
     (,ffi:SDLK_k . k)
     (,ffi:SDLK_l . l)
     (,ffi:SDLK_m . m)
     (,ffi:SDLK_n . n)
     (,ffi:SDLK_o . o)
     (,ffi:SDLK_p . p)
     (,ffi:SDLK_q . q)
     (,ffi:SDLK_r . r)
     (,ffi:SDLK_s . s)
     (,ffi:SDLK_t . t)
     (,ffi:SDLK_u . u)
     (,ffi:SDLK_v . v)
     (,ffi:SDLK_w . w)
     (,ffi:SDLK_x . x)
     (,ffi:SDLK_y . y)
     (,ffi:SDLK_z . z)
     (,ffi:SDLK_CAPSLOCK . caps-lock)
     (,ffi:SDLK_F1 . f1)
     (,ffi:SDLK_F2 . f2)
     (,ffi:SDLK_F3 . f3)
     (,ffi:SDLK_F4 . f4)
     (,ffi:SDLK_F5 . f5)
     (,ffi:SDLK_F6 . f6)
     (,ffi:SDLK_F7 . f7)
     (,ffi:SDLK_F8 . f8)
     (,ffi:SDLK_F9 . f9)
     (,ffi:SDLK_F10 . f10)
     (,ffi:SDLK_F11 . f11)
     (,ffi:SDLK_F12 . f12)
     (,ffi:SDLK_PRINTSCREEN . print-screen)
     (,ffi:SDLK_SCROLLLOCK . scroll-lock)
     (,ffi:SDLK_PAUSE . pause)
     (,ffi:SDLK_INSERT . insert)
     (,ffi:SDLK_HOME . home)
     (,ffi:SDLK_PAGEUP . page-up)
     (,ffi:SDLK_DELETE . delete)
     (,ffi:SDLK_END . end)
     (,ffi:SDLK_PAGEDOWN . page-down)
     (,ffi:SDLK_RIGHT . right)
     (,ffi:SDLK_LEFT . left)
     (,ffi:SDLK_DOWN . down)
     (,ffi:SDLK_UP . up)
     (,ffi:SDLK_NUMLOCKCLEAR . num-lock-clear)
     (,ffi:SDLK_KP_DIVIDE . keypad-divide)
     (,ffi:SDLK_KP_MULTIPLY . keypad-multiply)
     (,ffi:SDLK_KP_MINUS . keypad-minus)
     (,ffi:SDLK_KP_PLUS . keypad-plus)
     (,ffi:SDLK_KP_ENTER . keypad-enter)
     (,ffi:SDLK_KP_1 . keypad-1)
     (,ffi:SDLK_KP_2 . keypad-2)
     (,ffi:SDLK_KP_3 . keypad-3)
     (,ffi:SDLK_KP_4 . keypad-4)
     (,ffi:SDLK_KP_5 . keypad-5)
     (,ffi:SDLK_KP_6 . keypad-6)
     (,ffi:SDLK_KP_7 . keypad-7)
     (,ffi:SDLK_KP_8 . keypad-8)
     (,ffi:SDLK_KP_9 . keypad-9)
     (,ffi:SDLK_KP_0 . keypad-0)
     (,ffi:SDLK_KP_PERIOD . keypad-period)
     (,ffi:SDLK_APPLICATION . application)
     (,ffi:SDLK_POWER . power)
     (,ffi:SDLK_KP_EQUALS . keypad-equals)
     (,ffi:SDLK_F13 . f13)
     (,ffi:SDLK_F14 . f14)
     (,ffi:SDLK_F15 . f15)
     (,ffi:SDLK_F16 . f16)
     (,ffi:SDLK_F17 . f17)
     (,ffi:SDLK_F18 . f18)
     (,ffi:SDLK_F19 . f19)
     (,ffi:SDLK_F20 . f20)
     (,ffi:SDLK_F21 . f21)
     (,ffi:SDLK_F22 . f22)
     (,ffi:SDLK_F23 . f23)
     (,ffi:SDLK_F24 . f24)
     (,ffi:SDLK_EXECUTE . execute)
     (,ffi:SDLK_HELP . help)
     (,ffi:SDLK_MENU . menu)
     (,ffi:SDLK_SELECT . select)
     (,ffi:SDLK_STOP . stop)
     (,ffi:SDLK_AGAIN . again)
     (,ffi:SDLK_UNDO . undo)
     (,ffi:SDLK_CUT . cut)
     (,ffi:SDLK_COPY . copy)
     (,ffi:SDLK_PASTE . paste)
     (,ffi:SDLK_FIND . find)
     (,ffi:SDLK_MUTE . mute)
     (,ffi:SDLK_VOLUMEUP . volume-up)
     (,ffi:SDLK_VOLUMEDOWN . volume-down)
     (,ffi:SDLK_KP_COMMA . keypad-comma)
     (,ffi:SDLK_KP_EQUALSAS400 . keypad-equalsas400)
     (,ffi:SDLK_ALTERASE . alt-erase)
     (,ffi:SDLK_SYSREQ . sysreq)
     (,ffi:SDLK_CANCEL . cancel)
     (,ffi:SDLK_CLEAR . clear)
     (,ffi:SDLK_PRIOR . prior)
     (,ffi:SDLK_RETURN2 . return-2)
     (,ffi:SDLK_SEPARATOR . separator)
     (,ffi:SDLK_OUT . out)
     (,ffi:SDLK_OPER . oper)
     (,ffi:SDLK_CLEARAGAIN . clear-again)
     (,ffi:SDLK_CRSEL . crsel)
     (,ffi:SDLK_EXSEL . exsel)
     (,ffi:SDLK_KP_00 . keypad-00)
     (,ffi:SDLK_KP_000 . keypad-000)
     (,ffi:SDLK_THOUSANDSSEPARATOR . thousands-separator)
     (,ffi:SDLK_DECIMALSEPARATOR . decimal-separator)
     (,ffi:SDLK_CURRENCYUNIT . currency-unit)
     (,ffi:SDLK_CURRENCYSUBUNIT . currency-subunit)
     (,ffi:SDLK_KP_LEFTPAREN . keypad-left-paren)
     (,ffi:SDLK_KP_RIGHTPAREN . keypad-right-paren)
     (,ffi:SDLK_KP_LEFTBRACE . keypad-left-brace)
     (,ffi:SDLK_KP_RIGHTBRACE . keypad-right-brace)
     (,ffi:SDLK_KP_TAB . keypad-tab)
     (,ffi:SDLK_KP_BACKSPACE . keypad-backspace)
     (,ffi:SDLK_KP_A . keypad-a)
     (,ffi:SDLK_KP_B . keypad-b)
     (,ffi:SDLK_KP_C . keypad-c)
     (,ffi:SDLK_KP_D . keypad-d)
     (,ffi:SDLK_KP_E . keypad-e)
     (,ffi:SDLK_KP_F . keypad-f)
     (,ffi:SDLK_KP_XOR . keypad-xor)
     (,ffi:SDLK_KP_POWER . keypad-power)
     (,ffi:SDLK_KP_PERCENT . keypad-percent)
     (,ffi:SDLK_KP_LESS . keypad-less)
     (,ffi:SDLK_KP_GREATER . keypad-greater)
     (,ffi:SDLK_KP_AMPERSAND . keypad-ampersand)
     (,ffi:SDLK_KP_DBLAMPERSAND . keypad-double-ampersand)
     (,ffi:SDLK_KP_VERTICALBAR . keypad-vertical-bar)
     (,ffi:SDLK_KP_DBLVERTICALBAR . keypad-double-vertical-bar)
     (,ffi:SDLK_KP_COLON . keypad-colon)
     (,ffi:SDLK_KP_HASH . keypad-hash)
     (,ffi:SDLK_KP_SPACE . keypad-space)
     (,ffi:SDLK_KP_AT . keypad-at)
     (,ffi:SDLK_KP_EXCLAM . keypad-exclam)
     (,ffi:SDLK_KP_MEMSTORE . keypad-mem-store)
     (,ffi:SDLK_KP_MEMRECALL . keypad-mem-recall)
     (,ffi:SDLK_KP_MEMCLEAR . keypad-mem-clear)
     (,ffi:SDLK_KP_MEMADD . keypad-mem-add)
     (,ffi:SDLK_KP_MEMSUBTRACT . keypad-mem-subtract)
     (,ffi:SDLK_KP_MEMMULTIPLY . keypad-mem-multiply)
     (,ffi:SDLK_KP_MEMDIVIDE . keypad-mem-divide)
     (,ffi:SDLK_KP_PLUSMINUS . keypad-plus-minus)
     (,ffi:SDLK_KP_CLEAR . keypad-clear)
     (,ffi:SDLK_KP_CLEARENTRY . keypad-clear-entry)
     (,ffi:SDLK_KP_BINARY . keypad-binary)
     (,ffi:SDLK_KP_OCTAL . keypad-octal)
     (,ffi:SDLK_KP_DECIMAL . keypad-decimal)
     (,ffi:SDLK_KP_HEXADECIMAL . keypad-hexadecimal)
     (,ffi:SDLK_LCTRL . left-control)
     (,ffi:SDLK_LSHIFT . left-shift)
     (,ffi:SDLK_LALT . left-alt)
     (,ffi:SDLK_LGUI . left-gui)
     (,ffi:SDLK_RCTRL . right-control)
     (,ffi:SDLK_RSHIFT . right-shift)
     (,ffi:SDLK_RALT . right-alt)
     (,ffi:SDLK_RGUI . right-gui)
     (,ffi:SDLK_MODE . mode)
     (,ffi:SDLK_AUDIONEXT . audio-next)
     (,ffi:SDLK_AUDIOPREV . audio-prev)
     (,ffi:SDLK_AUDIOSTOP . audio-stop)
     (,ffi:SDLK_AUDIOPLAY . audio-play)
     (,ffi:SDLK_AUDIOMUTE . audio-mute)
     (,ffi:SDLK_MEDIASELECT . media-select)
     (,ffi:SDLK_WWW . www)
     (,ffi:SDLK_MAIL . mail)
     (,ffi:SDLK_CALCULATOR . calculator)
     (,ffi:SDLK_COMPUTER . computer)
     (,ffi:SDLK_AC_SEARCH . ac-search)
     (,ffi:SDLK_AC_HOME . ac-home)
     (,ffi:SDLK_AC_BACK . ac-back)
     (,ffi:SDLK_AC_FORWARD . ac-forward)
     (,ffi:SDLK_AC_STOP . ac-stop)
     (,ffi:SDLK_AC_REFRESH . ac-refresh)
     (,ffi:SDLK_AC_BOOKMARKS . ac-bookmarks)
     (,ffi:SDLK_BRIGHTNESSDOWN . brightness-down)
     (,ffi:SDLK_BRIGHTNESSUP . brightness-up)
     (,ffi:SDLK_DISPLAYSWITCH . display-switch)
     (,ffi:SDLK_KBDILLUMTOGGLE . keyboard-illum-toggle)
     (,ffi:SDLK_KBDILLUMDOWN . keyboard-illum-down)
     (,ffi:SDLK_KBDILLUMUP . keyboard-illum-up)
     (,ffi:SDLK_EJECT . eject)
     (,ffi:SDLK_SLEEP . sleep))))

(define scancode-map
  (alist->hash-table
   `((,ffi:SDL_SCANCODE_UNKNOWN . unknown)
     (,ffi:SDL_SCANCODE_A . a)
     (,ffi:SDL_SCANCODE_B . b)
     (,ffi:SDL_SCANCODE_C . c)
     (,ffi:SDL_SCANCODE_D . d)
     (,ffi:SDL_SCANCODE_E . e)
     (,ffi:SDL_SCANCODE_F . f)
     (,ffi:SDL_SCANCODE_G . g)
     (,ffi:SDL_SCANCODE_H . h)
     (,ffi:SDL_SCANCODE_I . i)
     (,ffi:SDL_SCANCODE_J . j)
     (,ffi:SDL_SCANCODE_K . k)
     (,ffi:SDL_SCANCODE_L . l)
     (,ffi:SDL_SCANCODE_M . m)
     (,ffi:SDL_SCANCODE_N . n)
     (,ffi:SDL_SCANCODE_O . o)
     (,ffi:SDL_SCANCODE_P . p)
     (,ffi:SDL_SCANCODE_Q . q)
     (,ffi:SDL_SCANCODE_R . r)
     (,ffi:SDL_SCANCODE_S . s)
     (,ffi:SDL_SCANCODE_T . t)
     (,ffi:SDL_SCANCODE_U . u)
     (,ffi:SDL_SCANCODE_V . v)
     (,ffi:SDL_SCANCODE_W . w)
     (,ffi:SDL_SCANCODE_X . x)
     (,ffi:SDL_SCANCODE_Y . y)
     (,ffi:SDL_SCANCODE_Z . z)
     (,ffi:SDL_SCANCODE_1 . 1)
     (,ffi:SDL_SCANCODE_2 . 2)
     (,ffi:SDL_SCANCODE_3 . 3)
     (,ffi:SDL_SCANCODE_4 . 4)
     (,ffi:SDL_SCANCODE_5 . 5)
     (,ffi:SDL_SCANCODE_6 . 6)
     (,ffi:SDL_SCANCODE_7 . 7)
     (,ffi:SDL_SCANCODE_8 . 8)
     (,ffi:SDL_SCANCODE_9 . 9)
     (,ffi:SDL_SCANCODE_0 . 0)
     (,ffi:SDL_SCANCODE_RETURN . return)
     (,ffi:SDL_SCANCODE_ESCAPE . escape)
     (,ffi:SDL_SCANCODE_BACKSPACE . backspace)
     (,ffi:SDL_SCANCODE_TAB . tab)
     (,ffi:SDL_SCANCODE_SPACE . space)
     (,ffi:SDL_SCANCODE_MINUS . minus)
     (,ffi:SDL_SCANCODE_EQUALS . equals)
     (,ffi:SDL_SCANCODE_LEFTBRACKET . left-bracket)
     (,ffi:SDL_SCANCODE_RIGHTBRACKET . right-bracket)
     (,ffi:SDL_SCANCODE_BACKSLASH . backslash)
     (,ffi:SDL_SCANCODE_NONUSHASH . nonushash)
     (,ffi:SDL_SCANCODE_SEMICOLON . semicolon)
     (,ffi:SDL_SCANCODE_APOSTROPHE . apostrophe)
     (,ffi:SDL_SCANCODE_GRAVE . grave)
     (,ffi:SDL_SCANCODE_COMMA . comma)
     (,ffi:SDL_SCANCODE_PERIOD . period)
     (,ffi:SDL_SCANCODE_SLASH . slash)
     (,ffi:SDL_SCANCODE_CAPSLOCK . caps-lock)
     (,ffi:SDL_SCANCODE_F1 . f1)
     (,ffi:SDL_SCANCODE_F2 . f2)
     (,ffi:SDL_SCANCODE_F3 . f3)
     (,ffi:SDL_SCANCODE_F4 . f4)
     (,ffi:SDL_SCANCODE_F5 . f5)
     (,ffi:SDL_SCANCODE_F6 . f6)
     (,ffi:SDL_SCANCODE_F7 . f7)
     (,ffi:SDL_SCANCODE_F8 . f8)
     (,ffi:SDL_SCANCODE_F9 . f9)
     (,ffi:SDL_SCANCODE_F10 . f10)
     (,ffi:SDL_SCANCODE_F11 . f11)
     (,ffi:SDL_SCANCODE_F12 . f12)
     (,ffi:SDL_SCANCODE_PRINTSCREEN . print-screen)
     (,ffi:SDL_SCANCODE_SCROLLLOCK . scroll-lock)
     (,ffi:SDL_SCANCODE_PAUSE . pause)
     (,ffi:SDL_SCANCODE_INSERT . insert)
     (,ffi:SDL_SCANCODE_HOME . home)
     (,ffi:SDL_SCANCODE_PAGEUP . page-up)
     (,ffi:SDL_SCANCODE_DELETE . delete)
     (,ffi:SDL_SCANCODE_END . end)
     (,ffi:SDL_SCANCODE_PAGEDOWN . page-down)
     (,ffi:SDL_SCANCODE_RIGHT . right)
     (,ffi:SDL_SCANCODE_LEFT . left)
     (,ffi:SDL_SCANCODE_DOWN . down)
     (,ffi:SDL_SCANCODE_UP . up)
     (,ffi:SDL_SCANCODE_NUMLOCKCLEAR . num-lock-clear)
     (,ffi:SDL_SCANCODE_KP_DIVIDE . keypad-divide)
     (,ffi:SDL_SCANCODE_KP_MULTIPLY . keypad-multiply)
     (,ffi:SDL_SCANCODE_KP_MINUS . keypad-minus)
     (,ffi:SDL_SCANCODE_KP_PLUS . keypad-plus)
     (,ffi:SDL_SCANCODE_KP_ENTER . keypad-enter)
     (,ffi:SDL_SCANCODE_KP_1 . keypad-1)
     (,ffi:SDL_SCANCODE_KP_2 . keypad-2)
     (,ffi:SDL_SCANCODE_KP_3 . keypad-3)
     (,ffi:SDL_SCANCODE_KP_4 . keypad-4)
     (,ffi:SDL_SCANCODE_KP_5 . keypad-5)
     (,ffi:SDL_SCANCODE_KP_6 . keypad-6)
     (,ffi:SDL_SCANCODE_KP_7 . keypad-7)
     (,ffi:SDL_SCANCODE_KP_8 . keypad-8)
     (,ffi:SDL_SCANCODE_KP_9 . keypad-9)
     (,ffi:SDL_SCANCODE_KP_0 . keypad-0)
     (,ffi:SDL_SCANCODE_KP_PERIOD . keypad-period)
     (,ffi:SDL_SCANCODE_NONUSBACKSLASH . nonusbackslash)
     (,ffi:SDL_SCANCODE_APPLICATION . application)
     (,ffi:SDL_SCANCODE_POWER . power)
     (,ffi:SDL_SCANCODE_KP_EQUALS . keypad-equals)
     (,ffi:SDL_SCANCODE_F13 . f13)
     (,ffi:SDL_SCANCODE_F14 . f14)
     (,ffi:SDL_SCANCODE_F15 . f15)
     (,ffi:SDL_SCANCODE_F16 . f16)
     (,ffi:SDL_SCANCODE_F17 . f17)
     (,ffi:SDL_SCANCODE_F18 . f18)
     (,ffi:SDL_SCANCODE_F19 . f19)
     (,ffi:SDL_SCANCODE_F20 . f20)
     (,ffi:SDL_SCANCODE_F21 . f21)
     (,ffi:SDL_SCANCODE_F22 . f22)
     (,ffi:SDL_SCANCODE_F23 . f23)
     (,ffi:SDL_SCANCODE_F24 . f24)
     (,ffi:SDL_SCANCODE_EXECUTE . execute)
     (,ffi:SDL_SCANCODE_HELP . help)
     (,ffi:SDL_SCANCODE_MENU . menu)
     (,ffi:SDL_SCANCODE_SELECT . select)
     (,ffi:SDL_SCANCODE_STOP . stop)
     (,ffi:SDL_SCANCODE_AGAIN . again)
     (,ffi:SDL_SCANCODE_UNDO . undo)
     (,ffi:SDL_SCANCODE_CUT . cut)
     (,ffi:SDL_SCANCODE_COPY . copy)
     (,ffi:SDL_SCANCODE_PASTE . paste)
     (,ffi:SDL_SCANCODE_FIND . find)
     (,ffi:SDL_SCANCODE_MUTE . mute)
     (,ffi:SDL_SCANCODE_VOLUMEUP . volume-up)
     (,ffi:SDL_SCANCODE_VOLUMEDOWN . volume-down)
     (,ffi:SDL_SCANCODE_KP_COMMA . keypad-comma)
     (,ffi:SDL_SCANCODE_KP_EQUALSAS400 . keypad-equalsas400)
     (,ffi:SDL_SCANCODE_INTERNATIONAL1 . international-1)
     (,ffi:SDL_SCANCODE_INTERNATIONAL2 . international-2)
     (,ffi:SDL_SCANCODE_INTERNATIONAL3 . international-3)
     (,ffi:SDL_SCANCODE_INTERNATIONAL4 . international-4)
     (,ffi:SDL_SCANCODE_INTERNATIONAL5 . international-5)
     (,ffi:SDL_SCANCODE_INTERNATIONAL6 . international-6)
     (,ffi:SDL_SCANCODE_INTERNATIONAL7 . international-7)
     (,ffi:SDL_SCANCODE_INTERNATIONAL8 . international-8)
     (,ffi:SDL_SCANCODE_INTERNATIONAL9 . international-9)
     (,ffi:SDL_SCANCODE_LANG1 . lang-1)
     (,ffi:SDL_SCANCODE_LANG2 . lang-2)
     (,ffi:SDL_SCANCODE_LANG3 . lang-3)
     (,ffi:SDL_SCANCODE_LANG4 . lang-4)
     (,ffi:SDL_SCANCODE_LANG5 . lang-5)
     (,ffi:SDL_SCANCODE_LANG6 . lang-6)
     (,ffi:SDL_SCANCODE_LANG7 . lang-7)
     (,ffi:SDL_SCANCODE_LANG8 . lang-8)
     (,ffi:SDL_SCANCODE_LANG9 . lang-9)
     (,ffi:SDL_SCANCODE_ALTERASE . alt-erase)
     (,ffi:SDL_SCANCODE_SYSREQ . sysreq)
     (,ffi:SDL_SCANCODE_CANCEL . cancel)
     (,ffi:SDL_SCANCODE_CLEAR . clear)
     (,ffi:SDL_SCANCODE_PRIOR . prior)
     (,ffi:SDL_SCANCODE_RETURN2 . return-2)
     (,ffi:SDL_SCANCODE_SEPARATOR . separator)
     (,ffi:SDL_SCANCODE_OUT . out)
     (,ffi:SDL_SCANCODE_OPER . oper)
     (,ffi:SDL_SCANCODE_CLEARAGAIN . clear-again)
     (,ffi:SDL_SCANCODE_CRSEL . crsel)
     (,ffi:SDL_SCANCODE_EXSEL . exsel)
     (,ffi:SDL_SCANCODE_KP_00 . keypad-00)
     (,ffi:SDL_SCANCODE_KP_000 . keypad-000)
     (,ffi:SDL_SCANCODE_THOUSANDSSEPARATOR . thousands-separator)
     (,ffi:SDL_SCANCODE_DECIMALSEPARATOR . decimal-separator)
     (,ffi:SDL_SCANCODE_CURRENCYUNIT . currency-unit)
     (,ffi:SDL_SCANCODE_CURRENCYSUBUNIT . current-subunit)
     (,ffi:SDL_SCANCODE_KP_LEFTPAREN . keypad-left-paren)
     (,ffi:SDL_SCANCODE_KP_RIGHTPAREN . keypad-right-paren)
     (,ffi:SDL_SCANCODE_KP_LEFTBRACE . keypad-left-brace)
     (,ffi:SDL_SCANCODE_KP_RIGHTBRACE . keypad-right-brace)
     (,ffi:SDL_SCANCODE_KP_TAB . keypad-tab)
     (,ffi:SDL_SCANCODE_KP_BACKSPACE . keypad-backspace)
     (,ffi:SDL_SCANCODE_KP_A . keypad-a)
     (,ffi:SDL_SCANCODE_KP_B . keypad-b)
     (,ffi:SDL_SCANCODE_KP_C . keypad-c)
     (,ffi:SDL_SCANCODE_KP_D . keypad-d)
     (,ffi:SDL_SCANCODE_KP_E . keypad-e)
     (,ffi:SDL_SCANCODE_KP_F . keypad-f)
     (,ffi:SDL_SCANCODE_KP_XOR . keypad-xor)
     (,ffi:SDL_SCANCODE_KP_POWER . keypad-power)
     (,ffi:SDL_SCANCODE_KP_PERCENT . keypad-percent)
     (,ffi:SDL_SCANCODE_KP_LESS . keypad-less)
     (,ffi:SDL_SCANCODE_KP_GREATER . keypad-greater)
     (,ffi:SDL_SCANCODE_KP_AMPERSAND . keypad-ampersand)
     (,ffi:SDL_SCANCODE_KP_DBLAMPERSAND . keypad-double-ampersand)
     (,ffi:SDL_SCANCODE_KP_VERTICALBAR . keypad-vertical-bar)
     (,ffi:SDL_SCANCODE_KP_DBLVERTICALBAR . keypad-double-vertical-bar)
     (,ffi:SDL_SCANCODE_KP_COLON . keypad-colon)
     (,ffi:SDL_SCANCODE_KP_HASH . keypad-hash)
     (,ffi:SDL_SCANCODE_KP_SPACE . keypad-space)
     (,ffi:SDL_SCANCODE_KP_AT . keypad-at)
     (,ffi:SDL_SCANCODE_KP_EXCLAM . keypad-exclam)
     (,ffi:SDL_SCANCODE_KP_MEMSTORE . keypad-mem-store)
     (,ffi:SDL_SCANCODE_KP_MEMRECALL . keypad-mem-recall)
     (,ffi:SDL_SCANCODE_KP_MEMCLEAR . keypad-mem-clear)
     (,ffi:SDL_SCANCODE_KP_MEMADD . keypad-mem-add)
     (,ffi:SDL_SCANCODE_KP_MEMSUBTRACT . keypadd-mem-subtract)
     (,ffi:SDL_SCANCODE_KP_MEMMULTIPLY . keypad-mem-multiply)
     (,ffi:SDL_SCANCODE_KP_MEMDIVIDE . keypad-mem-divide)
     (,ffi:SDL_SCANCODE_KP_PLUSMINUS . keypad-plus-minus)
     (,ffi:SDL_SCANCODE_KP_CLEAR . keypad-clear)
     (,ffi:SDL_SCANCODE_KP_CLEARENTRY . keypad-clear-entry)
     (,ffi:SDL_SCANCODE_KP_BINARY . keypad-binary)
     (,ffi:SDL_SCANCODE_KP_OCTAL . keypad-octal)
     (,ffi:SDL_SCANCODE_KP_DECIMAL . keypad-decimal)
     (,ffi:SDL_SCANCODE_KP_HEXADECIMAL . keypad-hexadecimal)
     (,ffi:SDL_SCANCODE_LCTRL . left-control)
     (,ffi:SDL_SCANCODE_LSHIFT . left-shift)
     (,ffi:SDL_SCANCODE_LALT . left-alt)
     (,ffi:SDL_SCANCODE_LGUI . left-gui)
     (,ffi:SDL_SCANCODE_RCTRL . right-control)
     (,ffi:SDL_SCANCODE_RSHIFT . right-shift)
     (,ffi:SDL_SCANCODE_RALT . right-alt)
     (,ffi:SDL_SCANCODE_RGUI . right-cui)
     (,ffi:SDL_SCANCODE_MODE . mode)
     (,ffi:SDL_SCANCODE_AUDIONEXT . audio-next)
     (,ffi:SDL_SCANCODE_AUDIOPREV . audio-prev)
     (,ffi:SDL_SCANCODE_AUDIOSTOP . audio-stop)
     (,ffi:SDL_SCANCODE_AUDIOPLAY . audio-play)
     (,ffi:SDL_SCANCODE_AUDIOMUTE . audio-mute)
     (,ffi:SDL_SCANCODE_MEDIASELECT . media-select)
     (,ffi:SDL_SCANCODE_WWW . www)
     (,ffi:SDL_SCANCODE_MAIL . mail)
     (,ffi:SDL_SCANCODE_CALCULATOR . calculator)
     (,ffi:SDL_SCANCODE_COMPUTER . computer)
     (,ffi:SDL_SCANCODE_AC_SEARCH . ac-search)
     (,ffi:SDL_SCANCODE_AC_HOME . ac-home)
     (,ffi:SDL_SCANCODE_AC_BACK . ac-back)
     (,ffi:SDL_SCANCODE_AC_FORWARD . ac-forward)
     (,ffi:SDL_SCANCODE_AC_STOP . ac-stop)
     (,ffi:SDL_SCANCODE_AC_REFRESH . ac-refresh)
     (,ffi:SDL_SCANCODE_AC_BOOKMARKS . ac-bookmarks)
     (,ffi:SDL_SCANCODE_BRIGHTNESSDOWN . brightness-down)
     (,ffi:SDL_SCANCODE_BRIGHTNESSUP . brightness-up)
     (,ffi:SDL_SCANCODE_DISPLAYSWITCH . display-switch)
     (,ffi:SDL_SCANCODE_KBDILLUMTOGGLE . keyboard-illum-toggle)
     (,ffi:SDL_SCANCODE_KBDILLUMDOWN . keyboard-illum-down)
     (,ffi:SDL_SCANCODE_KBDILLUMUP . keyboard-illum-up)
     (,ffi:SDL_SCANCODE_EJECT . eject)
     (,ffi:SDL_SCANCODE_SLEEP . sleep)
     (,ffi:SDL_SCANCODE_APP1 . app-1)
     (,ffi:SDL_SCANCODE_APP2 . app-2))))

(define (parse-keyboard-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          uint32  ; windowID
          uint8   ; state
          uint8   ; repeat
          uint8   ; padding2
          uint8   ; padding3
          (list int       ; scancode
                int       ; sym
                uint16    ; mod
                uint32))) ; unused

  (define (mod->list mod)
    (fold (lambda (pair prev)
            (match pair
              ((sym . bit)
               (if (zero? (logand mod bit))
                   prev
                   (cons sym prev)))))
          '()
          `((left-shift    . ,ffi:KMOD_LSHIFT)
            (right-shift   . ,ffi:KMOD_RSHIFT)
            (left-control  . ,ffi:KMOD_LCTRL)
            (right-control . ,ffi:KMOD_RCTRL)
            (left-alt      . ,ffi:KMOD_LALT)
            (right-alt     . ,ffi:KMOD_RALT)
            (left-gui      . ,ffi:KMOD_LGUI)
            (right-gui     . ,ffi:KMOD_RGUI)
            (num-lock      . ,ffi:KMOD_NUM)
            (caps-lock     . ,ffi:KMOD_CAPS)
            (alt-gr        . ,ffi:KMOD_MODE))))

  (define (keycode->symbol keycode)
    (hash-ref keycode-map keycode))

  (define (scancode->symbol scancode)
    (hash-ref scancode-map scancode))

  (match (parse-c-struct ptr types)
    ((_ timestamp window-id state repeat _ _ (scancode sym mod _))
     (make-keyboard-event timestamp
                          window-id
                          (= state ffi:SDL_PRESSED)
                          (not (zero? repeat))
                          (keycode->symbol sym)
                          (scancode->symbol scancode)
                          (mod->list mod)))))


;;;
;;; Text Input
;;;

(define-record-type <text-input-event>
  (make-text-input-event timestamp window-id text)
  text-input-event?
  (timestamp text-input-event-timestamp)
  (window-id text-input-event-window-id)
  (text text-input-event-text))

(define (parse-text-input-event ptr)
  ;; Separate the first 3 struct fields which are all uint32s from the
  ;; 32-byte UTF-8 encoded character buffer at the end of the struct.
  (let* ((len (* (sizeof uint32) 3))
         (bv (pointer->bytevector ptr len))
         (timestamp (u32vector-ref bv 1))
         (window-id (u32vector-ref bv 2))
         (text (pointer->string (make-pointer (+ (pointer-address ptr) len)))))
    (make-text-input-event timestamp window-id text)))


;;;
;;; Mouse
;;;

(define-record-type <mouse-button-event>
  (make-mouse-button-event timestamp window-id which
                           button pressed? clicks x y)
  mouse-button-event?
  (timestamp mouse-button-event-timestamp)
  (window-id mouse-button-event-window-id)
  (which mouse-button-event-which)
  (button mouse-button-event-button)
  (pressed? mouse-button-event-pressed?)
  (clicks mouse-button-event-clicks)
  (x mouse-button-event-x)
  (y mouse-button-event-y))

(define (mouse-button-down-event? e)
  "Return #t if E is a mouse button down event."
  (and (mouse-button-event? e)
       (mouse-button-event-pressed? e)))

(define (mouse-button-up-event? e)
  "Return #t if E is a mouse button up event."
  (and (mouse-button-event? e)
       (not (mouse-button-event-pressed? e))))

(define (parse-mouse-button-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          uint32  ; windowID
          uint32  ; which
          uint8   ; button
          uint8   ; state
          uint8   ; clicks
          uint8   ; padding1
          int32   ; x
          int32)) ; y

  (define (button->symbol n)
    (list-ref '(left middle right x1 x2) (1- n)))

  (match (parse-c-struct ptr types)
    ((_ timestamp window-id which button state clicks _ x y)
     (make-mouse-button-event timestamp
                              window-id
                              which
                              (button->symbol button)
                              (= state ffi:SDL_PRESSED)
                              clicks
                              x y))))

(define-record-type <mouse-motion-event>
  (make-mouse-motion-event timestamp window-id which buttons x y x-rel y-rel)
  mouse-motion-event?
  (timestamp mouse-motion-event-timestamp)
  (window-id mouse-motion-event-window-id)
  (which mouse-motion-event-which)
  (buttons mouse-motion-event-buttons)
  (x mouse-motion-event-x)
  (y mouse-motion-event-y)
  (x-rel mouse-motion-event-x-rel)
  (y-rel mouse-motion-event-y-rel))

(define (parse-mouse-motion-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          uint32  ; windowID
          uint32  ; which
          uint32  ; state
          int32   ; x
          int32   ; y
          int32   ; xrel
          int32)) ; yrel

  (define (button-mask->list mask)
    (fold (lambda (pair prev)
            (match pair
              ((sym . bit)
               (if (zero? (logand mask bit))
                   prev
                   (cons sym prev)))))
          '()
          `((left   . ,ffi:SDL_BUTTON_LMASK)
            (middle . ,ffi:SDL_BUTTON_MMASK)
            (right  . ,ffi:SDL_BUTTON_RMASK)
            (x1     . ,ffi:SDL_BUTTON_X1MASK)
            (x2     . ,ffi:SDL_BUTTON_X2MASK))))

  (match (parse-c-struct ptr types)
    ((_ timestamp window-id which state x y xrel yrel)
     (make-mouse-motion-event timestamp
                              window-id
                              which
                              (button-mask->list state)
                              x y xrel yrel))))


;;;
;;; Joystick
;;;

(define-record-type <joystick-axis-event>
  (make-joystick-axis-event timestamp which axis value)
  joystick-axis-event?
  (timestamp joystick-axis-event-timestamp)
  (which joystick-axis-event-which)
  (axis joystick-axis-event-axis)
  (value joystick-axis-event-value))

(define (parse-joystick-axis-event ptr)
  (define types
    (list uint32   ; type
          uint32   ; timestamp
          int32    ; which
          uint8    ; axis
          uint8    ; padding1
          uint8    ; padding2
          uint8    ; padding3
          int16    ; value
          uint16)) ; padding4

  (match (parse-c-struct ptr types)
    ((_ timestamp which axis _ _ _ value _)
     (make-joystick-axis-event timestamp which axis value))))

(define-record-type <joystick-button-event>
  (make-joystick-button-event timestamp which button pressed?)
  joystick-button-event?
  (timestamp joystick-button-event-timestamp)
  (which joystick-button-which)
  (button joystick-button-event-button)
  (pressed? joystick-button-event-pressed?))

(define (joystick-button-down-event? e)
  "Return #t if E is a joystick button press event."
  (and (joystick-button-event? e)
       (joystick-button-event-pressed? e)))

(define (joystick-button-up-event? e)
  "Return #t if E is a joystick button release event."
  (and (joystick-button-event? e)
       (not (joystick-button-event-pressed? e))))

(define (parse-joystick-button-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32   ; which
          uint8   ; button
          uint8   ; state
          uint8   ; padding1
          uint8)) ; padding2

  (match (parse-c-struct ptr types)
    ((_ timestamp which button state _ _)
     (make-joystick-button-event timestamp which button
                                 (= state ffi:SDL_PRESSED)))))

(define-record-type <joystick-ball-event>
  (make-joystick-ball-event timestamp which ball x-rel y-rel)
  joystick-ball-event?
  (timestamp joystick-ball-event-timestamp)
  (which joystick-ball-event-which)
  (ball joystick-ball-event-ball)
  (x-rel joystick-ball-event-x-rel)
  (y-rel joystick-ball-event-y-rel))

(define (parse-joystick-ball-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32   ; which
          uint8   ; ball
          uint8   ; padding1
          uint8   ; padding2
          uint8   ; padding3
          int16   ; xrel
          int16)) ; yrel

  (match (parse-c-struct ptr types)
    ((_ timestamp which ball xrel yrel)
     (make-joystick-ball-event timestamp which ball xrel yrel))))

(define-record-type <joystick-hat-event>
  (make-joystick-hat-event timestamp which hat value)
  joystick-hat-event?
  (timestamp joystick-hat-event-timestamp)
  (which joystick-hat-event-which)
  (hat joystick-hat-event-hat)
  (value joystick-hat-event-value))

(define (parse-joystick-hat-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32   ; which
          uint8   ; hat
          uint8)) ; value

  (match (parse-c-struct ptr types)
    ((_ timestamp which hat value)
     ;; TODO: Parse 'value' and convert to symbol.
     (make-joystick-hat-event timestamp which hat value))))

(define-record-type <joystick-device-event>
  (make-joystick-device-event timestamp which action)
  joystick-device-event?
  (timestamp joystick-device-event-timestamp)
  (which joystick-device-event-which)
  (action joystick-device-event-action)) ; added or removed

(define (parse-joystick-device-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32)) ; which

  (match (parse-c-struct ptr types)
    ((type timestamp which)
     (make-joystick-device-event timestamp which
                                 (if (= type ffi:SDL_JOYDEVICEADDED)
                                     'added
                                     'removed)))))


;;;
;;; Game Controller
;;;

(define-record-type <controller-axis-event>
  (make-controller-axis-event timestamp which axis value)
  controller-axis-event?
  (timestamp controller-axis-event-timestamp)
  (which controller-axis-event-which)
  (axis controller-axis-event-axis)
  (value controller-axis-event-value))

(define (parse-controller-axis-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32   ; which
          uint8   ; axis
          uint8   ; padding1
          uint8   ; padding2
          uint8   ; padding3
          int16)) ; value

  (define (int->axis-symbol axis)
    (cond
      ((= axis ffi:SDL_CONTROLLER_AXIS_LEFTX) 'left-x)
      ((= axis ffi:SDL_CONTROLLER_AXIS_LEFTY) 'left-y)
      ((= axis ffi:SDL_CONTROLLER_AXIS_RIGHTX) 'right-x)
      ((= axis ffi:SDL_CONTROLLER_AXIS_RIGHTY) 'right-y)
      ((= axis ffi:SDL_CONTROLLER_AXIS_TRIGGERLEFT) 'trigger-left)
      ((= axis ffi:SDL_CONTROLLER_AXIS_TRIGGERRIGHT) 'trigger-right)))

  (match (parse-c-struct ptr types)
    ((_ timestamp which axis _ _ _ value)
     (make-controller-axis-event timestamp
                                 which
                                 (int->axis-symbol axis)
                                 value))))

(define-record-type <controller-button-event>
  (make-controller-button-event timestamp which button pressed?)
  controller-button-event?
  (timestamp controller-button-event-timestamp)
  (which controller-button-event-which)
  (button controller-button-event-button)
  (pressed? controller-button-event-pressed?))

(define (controller-button-down-event? event)
  (and (controller-button-event? event)
       (controller-button-event-pressed? event)))

(define (controller-button-up-event? event)
  (and (controller-button-event? event)
       (not (controller-button-event-pressed? event))))

(define (parse-controller-button-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32   ; which
          uint8   ; button
          uint8)) ; state

  (define (int->button-symbol button)
    (cond
      ((= button ffi:SDL_CONTROLLER_BUTTON_A) 'a)
      ((= button ffi:SDL_CONTROLLER_BUTTON_B) 'b)
      ((= button ffi:SDL_CONTROLLER_BUTTON_X) 'x)
      ((= button ffi:SDL_CONTROLLER_BUTTON_Y) 'y)
      ((= button ffi:SDL_CONTROLLER_BUTTON_BACK) 'back)
      ((= button ffi:SDL_CONTROLLER_BUTTON_GUIDE) 'guide)
      ((= button ffi:SDL_CONTROLLER_BUTTON_START) 'start)
      ((= button ffi:SDL_CONTROLLER_BUTTON_LEFTSTICK) 'left-stick)
      ((= button ffi:SDL_CONTROLLER_BUTTON_RIGHTSTICK) 'right-stick)
      ((= button ffi:SDL_CONTROLLER_BUTTON_LEFTSHOULDER) 'left-shoulder)
      ((= button ffi:SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) 'right-shoulder)
      ((= button ffi:SDL_CONTROLLER_BUTTON_DPAD_UP) 'dpad-up)
      ((= button ffi:SDL_CONTROLLER_BUTTON_DPAD_DOWN) 'dpad-down)
      ((= button ffi:SDL_CONTROLLER_BUTTON_DPAD_LEFT) 'dpad-left)
      ((= button ffi:SDL_CONTROLLER_BUTTON_DPAD_RIGHT) 'dpad-right)))

  (match (parse-c-struct ptr types)
    ((_ timestamp which button state)
     (make-controller-button-event timestamp
                                   which
                                   (int->button-symbol button)
                                   (= state ffi:SDL_PRESSED)))))

(define-record-type <controller-device-event>
  (make-controller-device-event timestamp which action)
  controller-device-event?
  (timestamp controller-device-event-timestamp)
  (which controller-device-event-which)
  (action controller-device-event-action))

(define (controller-added-event? event)
  "Return #t if EVENT is a game controller device event with the
'added' action."
  (and (controller-device-event? event)
       (eq? (controller-device-event-action event) 'added)))

(define (controller-removed-event? event)
  "Return #t if EVENT is a game controller device event with the
'removed' action."
  (and (controller-device-event? event)
       (eq? (controller-device-event-action event) 'removed)))

(define (controller-remapped-event? event)
  "Return #t if EVENT is a game controller device event with the
'remapped' action."
  (and (controller-device-event? event)
       (eq? (controller-device-event-action event) 'remapped)))

(define (parse-controller-device-event ptr)
  (define types
    (list uint32  ; type
          uint32  ; timestamp
          int32)) ; which

  (match (parse-c-struct ptr types)
    ((type timestamp which)
     (make-controller-device-event timestamp
                                   which
                                   (cond
                                    ((= type ffi:SDL_CONTROLLERDEVICEADDED)
                                     'added)
                                    ((= type ffi:SDL_CONTROLLERDEVICEREMOVED)
                                     'removed)
                                    ((= type ffi:SDL_CONTROLLERDEVICEREMAPPED)
                                     'remapped))))))


;;;
;;; Event management
;;;

(define poll-event
  ;; Multiple threads calling this procedure is not a concern because
  ;; most SDL functions only work from the main thread, so we can
  ;; reduce memory allocation by reusing the same bytevector each time
  ;; we poll for events.
  (let* ((e (make-sdl-event))
         (ptr (bytevector->pointer e)))
    (lambda ()
      (let ((result (ffi:sdl-poll-event ptr)))
        (and (= result 1)
             (let ((type (sdl-event-type e)))
               (cond
                ((= type ffi:SDL_QUIT)
                 (parse-quit-event ptr))
                ((= type ffi:SDL_WINDOWEVENT)
                 (parse-window-event ptr))
                ((or (= type ffi:SDL_KEYDOWN)
                     (= type ffi:SDL_KEYUP))
                 (parse-keyboard-event ptr))
                ((= type ffi:SDL_TEXTINPUT)
                 (parse-text-input-event ptr))
                ((or (= type ffi:SDL_MOUSEBUTTONDOWN)
                     (= type ffi:SDL_MOUSEBUTTONUP))
                 (parse-mouse-button-event ptr))
                ((= type ffi:SDL_MOUSEMOTION)
                 (parse-mouse-motion-event ptr))
                ((= type ffi:SDL_JOYAXISMOTION)
                 (parse-joystick-axis-event ptr))
                ((= type ffi:SDL_JOYBALLMOTION)
                 (parse-joystick-ball-event ptr))
                ((= type ffi:SDL_JOYHATMOTION)
                 (parse-joystick-hat-event ptr))
                ((or (= type ffi:SDL_JOYBUTTONDOWN)
                     (= type ffi:SDL_JOYBUTTONUP))
                 (parse-joystick-button-event ptr))
                ((or (= type ffi:SDL_JOYDEVICEADDED)
                     (= type ffi:SDL_JOYDEVICEREMOVED))
                 (parse-joystick-device-event ptr))
                ((= type ffi:SDL_CONTROLLERAXISMOTION)
                 (parse-controller-axis-event ptr))
                ((or (= type ffi:SDL_CONTROLLERBUTTONDOWN)
                     (= type ffi:SDL_CONTROLLERBUTTONUP))
                 (parse-controller-button-event ptr))
                ((or (= type ffi:SDL_CONTROLLERDEVICEADDED)
                     (= type ffi:SDL_CONTROLLERDEVICEREMOVED)
                     (= type ffi:SDL_CONTROLLERDEVICEREMAPPED))
                 (parse-controller-device-event ptr))
                (else 'fixme:unsupported-event))))))))
