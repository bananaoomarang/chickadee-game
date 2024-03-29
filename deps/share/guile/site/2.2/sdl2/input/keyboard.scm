;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2017 David Thompson <davet@gnu.org>
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
;; Keyboard input.
;;
;;; Code:

(define-module (sdl2 input keyboard)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (key-pressed?
            key-released?))

(define scancode-map
  (alist->hashq-table
   `((unknown . ,ffi:SDL_SCANCODE_UNKNOWN)
     (a . ,ffi:SDL_SCANCODE_A)
     (b . ,ffi:SDL_SCANCODE_B)
     (c . ,ffi:SDL_SCANCODE_C)
     (d . ,ffi:SDL_SCANCODE_D)
     (e . ,ffi:SDL_SCANCODE_E)
     (f . ,ffi:SDL_SCANCODE_F)
     (g . ,ffi:SDL_SCANCODE_G)
     (h . ,ffi:SDL_SCANCODE_H)
     (i . ,ffi:SDL_SCANCODE_I)
     (j . ,ffi:SDL_SCANCODE_J)
     (k . ,ffi:SDL_SCANCODE_K)
     (l . ,ffi:SDL_SCANCODE_L)
     (m . ,ffi:SDL_SCANCODE_M)
     (n . ,ffi:SDL_SCANCODE_N)
     (o . ,ffi:SDL_SCANCODE_O)
     (p . ,ffi:SDL_SCANCODE_P)
     (q . ,ffi:SDL_SCANCODE_Q)
     (r . ,ffi:SDL_SCANCODE_R)
     (s . ,ffi:SDL_SCANCODE_S)
     (t . ,ffi:SDL_SCANCODE_T)
     (u . ,ffi:SDL_SCANCODE_U)
     (v . ,ffi:SDL_SCANCODE_V)
     (w . ,ffi:SDL_SCANCODE_W)
     (x . ,ffi:SDL_SCANCODE_X)
     (y . ,ffi:SDL_SCANCODE_Y)
     (z . ,ffi:SDL_SCANCODE_Z)
     (1 . ,ffi:SDL_SCANCODE_1)
     (2 . ,ffi:SDL_SCANCODE_2)
     (3 . ,ffi:SDL_SCANCODE_3)
     (4 . ,ffi:SDL_SCANCODE_4)
     (5 . ,ffi:SDL_SCANCODE_5)
     (6 . ,ffi:SDL_SCANCODE_6)
     (7 . ,ffi:SDL_SCANCODE_7)
     (8 . ,ffi:SDL_SCANCODE_8)
     (9 . ,ffi:SDL_SCANCODE_9)
     (0 . ,ffi:SDL_SCANCODE_0)
     (return . ,ffi:SDL_SCANCODE_RETURN)
     (escape . ,ffi:SDL_SCANCODE_ESCAPE)
     (backspace . ,ffi:SDL_SCANCODE_BACKSPACE)
     (tab . ,ffi:SDL_SCANCODE_TAB)
     (space . ,ffi:SDL_SCANCODE_SPACE)
     (minus . ,ffi:SDL_SCANCODE_MINUS)
     (equals . ,ffi:SDL_SCANCODE_EQUALS)
     (left-bracket . ,ffi:SDL_SCANCODE_LEFTBRACKET)
     (right-bracket . ,ffi:SDL_SCANCODE_RIGHTBRACKET)
     (backslash . ,ffi:SDL_SCANCODE_BACKSLASH)
     (nonushash . ,ffi:SDL_SCANCODE_NONUSHASH)
     (semicolon . ,ffi:SDL_SCANCODE_SEMICOLON)
     (apostrophe . ,ffi:SDL_SCANCODE_APOSTROPHE)
     (grave . ,ffi:SDL_SCANCODE_GRAVE)
     (comma . ,ffi:SDL_SCANCODE_COMMA)
     (period . ,ffi:SDL_SCANCODE_PERIOD)
     (slash . ,ffi:SDL_SCANCODE_SLASH)
     (caps-lock . ,ffi:SDL_SCANCODE_CAPSLOCK)
     (f1 . ,ffi:SDL_SCANCODE_F1)
     (f2 . ,ffi:SDL_SCANCODE_F2)
     (f3 . ,ffi:SDL_SCANCODE_F3)
     (f4 . ,ffi:SDL_SCANCODE_F4)
     (f5 . ,ffi:SDL_SCANCODE_F5)
     (f6 . ,ffi:SDL_SCANCODE_F6)
     (f7 . ,ffi:SDL_SCANCODE_F7)
     (f8 . ,ffi:SDL_SCANCODE_F8)
     (f9 . ,ffi:SDL_SCANCODE_F9)
     (f10 . ,ffi:SDL_SCANCODE_F10)
     (f11 . ,ffi:SDL_SCANCODE_F11)
     (f12 . ,ffi:SDL_SCANCODE_F12)
     (print-screen . ,ffi:SDL_SCANCODE_PRINTSCREEN)
     (scroll-lock . ,ffi:SDL_SCANCODE_SCROLLLOCK)
     (pause . ,ffi:SDL_SCANCODE_PAUSE)
     (insert . ,ffi:SDL_SCANCODE_INSERT)
     (home . ,ffi:SDL_SCANCODE_HOME)
     (page-up . ,ffi:SDL_SCANCODE_PAGEUP)
     (delete . ,ffi:SDL_SCANCODE_DELETE)
     (end . ,ffi:SDL_SCANCODE_END)
     (page-down . ,ffi:SDL_SCANCODE_PAGEDOWN)
     (right . ,ffi:SDL_SCANCODE_RIGHT)
     (left . ,ffi:SDL_SCANCODE_LEFT)
     (down . ,ffi:SDL_SCANCODE_DOWN)
     (up . ,ffi:SDL_SCANCODE_UP)
     (num-lock-clear . ,ffi:SDL_SCANCODE_NUMLOCKCLEAR)
     (keypad-divide . ,ffi:SDL_SCANCODE_KP_DIVIDE)
     (keypad-multiply . ,ffi:SDL_SCANCODE_KP_MULTIPLY)
     (keypad-minus . ,ffi:SDL_SCANCODE_KP_MINUS)
     (keypad-plus . ,ffi:SDL_SCANCODE_KP_PLUS)
     (keypad-enter . ,ffi:SDL_SCANCODE_KP_ENTER)
     (keypad-1 . ,ffi:SDL_SCANCODE_KP_1)
     (keypad-2 . ,ffi:SDL_SCANCODE_KP_2)
     (keypad-3 . ,ffi:SDL_SCANCODE_KP_3)
     (keypad-4 . ,ffi:SDL_SCANCODE_KP_4)
     (keypad-5 . ,ffi:SDL_SCANCODE_KP_5)
     (keypad-6 . ,ffi:SDL_SCANCODE_KP_6)
     (keypad-7 . ,ffi:SDL_SCANCODE_KP_7)
     (keypad-8 . ,ffi:SDL_SCANCODE_KP_8)
     (keypad-9 . ,ffi:SDL_SCANCODE_KP_9)
     (keypad-0 . ,ffi:SDL_SCANCODE_KP_0)
     (keypad-period . ,ffi:SDL_SCANCODE_KP_PERIOD)
     (nonusbackslash . ,ffi:SDL_SCANCODE_NONUSBACKSLASH)
     (application . ,ffi:SDL_SCANCODE_APPLICATION)
     (power . ,ffi:SDL_SCANCODE_POWER)
     (keypad-equals . ,ffi:SDL_SCANCODE_KP_EQUALS)
     (f13 . ,ffi:SDL_SCANCODE_F13)
     (f14 . ,ffi:SDL_SCANCODE_F14)
     (f15 . ,ffi:SDL_SCANCODE_F15)
     (f16 . ,ffi:SDL_SCANCODE_F16)
     (f17 . ,ffi:SDL_SCANCODE_F17)
     (f18 . ,ffi:SDL_SCANCODE_F18)
     (f19 . ,ffi:SDL_SCANCODE_F19)
     (f20 . ,ffi:SDL_SCANCODE_F20)
     (f21 . ,ffi:SDL_SCANCODE_F21)
     (f22 . ,ffi:SDL_SCANCODE_F22)
     (f23 . ,ffi:SDL_SCANCODE_F23)
     (f24 . ,ffi:SDL_SCANCODE_F24)
     (execute . ,ffi:SDL_SCANCODE_EXECUTE)
     (help . ,ffi:SDL_SCANCODE_HELP)
     (menu . ,ffi:SDL_SCANCODE_MENU)
     (select . ,ffi:SDL_SCANCODE_SELECT)
     (stop . ,ffi:SDL_SCANCODE_STOP)
     (again . ,ffi:SDL_SCANCODE_AGAIN)
     (undo . ,ffi:SDL_SCANCODE_UNDO)
     (cut . ,ffi:SDL_SCANCODE_CUT)
     (copy . ,ffi:SDL_SCANCODE_COPY)
     (paste . ,ffi:SDL_SCANCODE_PASTE)
     (find . ,ffi:SDL_SCANCODE_FIND)
     (mute . ,ffi:SDL_SCANCODE_MUTE)
     (volume-up . ,ffi:SDL_SCANCODE_VOLUMEUP)
     (volume-down . ,ffi:SDL_SCANCODE_VOLUMEDOWN)
     (keypad-comma . ,ffi:SDL_SCANCODE_KP_COMMA)
     (keypad-equalsas400 . ,ffi:SDL_SCANCODE_KP_EQUALSAS400)
     (international-1 . ,ffi:SDL_SCANCODE_INTERNATIONAL1)
     (international-2 . ,ffi:SDL_SCANCODE_INTERNATIONAL2)
     (international-3 . ,ffi:SDL_SCANCODE_INTERNATIONAL3)
     (international-4 . ,ffi:SDL_SCANCODE_INTERNATIONAL4)
     (international-5 . ,ffi:SDL_SCANCODE_INTERNATIONAL5)
     (international-6 . ,ffi:SDL_SCANCODE_INTERNATIONAL6)
     (international-7 . ,ffi:SDL_SCANCODE_INTERNATIONAL7)
     (international-8 . ,ffi:SDL_SCANCODE_INTERNATIONAL8)
     (international-9 . ,ffi:SDL_SCANCODE_INTERNATIONAL9)
     (lang-1 . ,ffi:SDL_SCANCODE_LANG1)
     (lang-2 . ,ffi:SDL_SCANCODE_LANG2)
     (lang-3 . ,ffi:SDL_SCANCODE_LANG3)
     (lang-4 . ,ffi:SDL_SCANCODE_LANG4)
     (lang-5 . ,ffi:SDL_SCANCODE_LANG5)
     (lang-6 . ,ffi:SDL_SCANCODE_LANG6)
     (lang-7 . ,ffi:SDL_SCANCODE_LANG7)
     (lang-8 . ,ffi:SDL_SCANCODE_LANG8)
     (lang-9 . ,ffi:SDL_SCANCODE_LANG9)
     (alt-erase . ,ffi:SDL_SCANCODE_ALTERASE)
     (sysreq . ,ffi:SDL_SCANCODE_SYSREQ)
     (cancel . ,ffi:SDL_SCANCODE_CANCEL)
     (clear . ,ffi:SDL_SCANCODE_CLEAR)
     (prior . ,ffi:SDL_SCANCODE_PRIOR)
     (return-2 . ,ffi:SDL_SCANCODE_RETURN2)
     (separator . ,ffi:SDL_SCANCODE_SEPARATOR)
     (out . ,ffi:SDL_SCANCODE_OUT)
     (oper . ,ffi:SDL_SCANCODE_OPER)
     (clear-again . ,ffi:SDL_SCANCODE_CLEARAGAIN)
     (crsel . ,ffi:SDL_SCANCODE_CRSEL)
     (exsel . ,ffi:SDL_SCANCODE_EXSEL)
     (keypad-00 . ,ffi:SDL_SCANCODE_KP_00)
     (keypad-000 . ,ffi:SDL_SCANCODE_KP_000)
     (thousands-separator . ,ffi:SDL_SCANCODE_THOUSANDSSEPARATOR)
     (decimal-separator . ,ffi:SDL_SCANCODE_DECIMALSEPARATOR)
     (currency-unit . ,ffi:SDL_SCANCODE_CURRENCYUNIT)
     (current-subunit . ,ffi:SDL_SCANCODE_CURRENCYSUBUNIT)
     (keypad-left-paren . ,ffi:SDL_SCANCODE_KP_LEFTPAREN)
     (keypad-right-paren . ,ffi:SDL_SCANCODE_KP_RIGHTPAREN)
     (keypad-left-brace . ,ffi:SDL_SCANCODE_KP_LEFTBRACE)
     (keypad-right-brace . ,ffi:SDL_SCANCODE_KP_RIGHTBRACE)
     (keypad-tab . ,ffi:SDL_SCANCODE_KP_TAB)
     (keypad-backspace . ,ffi:SDL_SCANCODE_KP_BACKSPACE)
     (keypad-a . ,ffi:SDL_SCANCODE_KP_A)
     (keypad-b . ,ffi:SDL_SCANCODE_KP_B)
     (keypad-c . ,ffi:SDL_SCANCODE_KP_C)
     (keypad-d . ,ffi:SDL_SCANCODE_KP_D)
     (keypad-e . ,ffi:SDL_SCANCODE_KP_E)
     (keypad-f . ,ffi:SDL_SCANCODE_KP_F)
     (keypad-xor . ,ffi:SDL_SCANCODE_KP_XOR)
     (keypad-power . ,ffi:SDL_SCANCODE_KP_POWER)
     (keypad-percent . ,ffi:SDL_SCANCODE_KP_PERCENT)
     (keypad-less . ,ffi:SDL_SCANCODE_KP_LESS)
     (keypad-greater . ,ffi:SDL_SCANCODE_KP_GREATER)
     (keypad-ampersand . ,ffi:SDL_SCANCODE_KP_AMPERSAND)
     (keypad-double-ampersand . ,ffi:SDL_SCANCODE_KP_DBLAMPERSAND)
     (keypad-vertical-bar . ,ffi:SDL_SCANCODE_KP_VERTICALBAR)
     (keypad-double-vertical-bar . ,ffi:SDL_SCANCODE_KP_DBLVERTICALBAR)
     (keypad-colon . ,ffi:SDL_SCANCODE_KP_COLON)
     (keypad-hash . ,ffi:SDL_SCANCODE_KP_HASH)
     (keypad-space . ,ffi:SDL_SCANCODE_KP_SPACE)
     (keypad-at . ,ffi:SDL_SCANCODE_KP_AT)
     (keypad-exclam . ,ffi:SDL_SCANCODE_KP_EXCLAM)
     (keypad-mem-store . ,ffi:SDL_SCANCODE_KP_MEMSTORE)
     (keypad-mem-recall . ,ffi:SDL_SCANCODE_KP_MEMRECALL)
     (keypad-mem-clear . ,ffi:SDL_SCANCODE_KP_MEMCLEAR)
     (keypad-mem-add . ,ffi:SDL_SCANCODE_KP_MEMADD)
     (keypadd-mem-subtract . ,ffi:SDL_SCANCODE_KP_MEMSUBTRACT)
     (keypad-mem-multiply . ,ffi:SDL_SCANCODE_KP_MEMMULTIPLY)
     (keypad-mem-divide . ,ffi:SDL_SCANCODE_KP_MEMDIVIDE)
     (keypad-plus-minus . ,ffi:SDL_SCANCODE_KP_PLUSMINUS)
     (keypad-clear . ,ffi:SDL_SCANCODE_KP_CLEAR)
     (keypad-clear-entry . ,ffi:SDL_SCANCODE_KP_CLEARENTRY)
     (keypad-binary . ,ffi:SDL_SCANCODE_KP_BINARY)
     (keypad-octal . ,ffi:SDL_SCANCODE_KP_OCTAL)
     (keypad-decimal . ,ffi:SDL_SCANCODE_KP_DECIMAL)
     (keypad-hexadecimal . ,ffi:SDL_SCANCODE_KP_HEXADECIMAL)
     (left-control . ,ffi:SDL_SCANCODE_LCTRL)
     (left-shift . ,ffi:SDL_SCANCODE_LSHIFT)
     (left-alt . ,ffi:SDL_SCANCODE_LALT)
     (left-gui . ,ffi:SDL_SCANCODE_LGUI)
     (right-control . ,ffi:SDL_SCANCODE_RCTRL)
     (right-shift . ,ffi:SDL_SCANCODE_RSHIFT)
     (right-alt . ,ffi:SDL_SCANCODE_RALT)
     (right-cui . ,ffi:SDL_SCANCODE_RGUI)
     (mode . ,ffi:SDL_SCANCODE_MODE)
     (audio-next . ,ffi:SDL_SCANCODE_AUDIONEXT)
     (audio-prev . ,ffi:SDL_SCANCODE_AUDIOPREV)
     (audio-stop . ,ffi:SDL_SCANCODE_AUDIOSTOP)
     (audio-play . ,ffi:SDL_SCANCODE_AUDIOPLAY)
     (audio-mute . ,ffi:SDL_SCANCODE_AUDIOMUTE)
     (media-select . ,ffi:SDL_SCANCODE_MEDIASELECT)
     (www . ,ffi:SDL_SCANCODE_WWW)
     (mail . ,ffi:SDL_SCANCODE_MAIL)
     (calculator . ,ffi:SDL_SCANCODE_CALCULATOR)
     (computer . ,ffi:SDL_SCANCODE_COMPUTER)
     (ac-search . ,ffi:SDL_SCANCODE_AC_SEARCH)
     (ac-home . ,ffi:SDL_SCANCODE_AC_HOME)
     (ac-back . ,ffi:SDL_SCANCODE_AC_BACK)
     (ac-forward . ,ffi:SDL_SCANCODE_AC_FORWARD)
     (ac-stop . ,ffi:SDL_SCANCODE_AC_STOP)
     (ac-refresh . ,ffi:SDL_SCANCODE_AC_REFRESH)
     (ac-bookmarks . ,ffi:SDL_SCANCODE_AC_BOOKMARKS)
     (brightness-down . ,ffi:SDL_SCANCODE_BRIGHTNESSDOWN)
     (brightness-up . ,ffi:SDL_SCANCODE_BRIGHTNESSUP)
     (display-switch . ,ffi:SDL_SCANCODE_DISPLAYSWITCH)
     (keyboard-illum-toggle . ,ffi:SDL_SCANCODE_KBDILLUMTOGGLE)
     (keyboard-illum-down . ,ffi:SDL_SCANCODE_KBDILLUMDOWN)
     (keyboard-illum-up . ,ffi:SDL_SCANCODE_KBDILLUMUP)
     (eject . ,ffi:SDL_SCANCODE_EJECT)
     (sleep . ,ffi:SDL_SCANCODE_SLEEP)
     (app-1 . ,ffi:SDL_SCANCODE_APP1)
     (app-2 . ,ffi:SDL_SCANCODE_APP2))))

(define key-pressed?
  (let ((keystate (pointer->bytevector
                   (ffi:sdl-get-keyboard-state %null-pointer)
                   ffi:SDL_NUM_SCANCODES)))
    (lambda (key)
      "Return #t if KEY is currently being pressed."
      (= 1 (u8vector-ref keystate (hashq-ref scancode-map key))))))

(define (key-released? key)
  "Return #t if KEY is not currently being pressed."
  (not (key-pressed? key)))
