;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Eero Leno <eero@leno.fi>
;;; Copyright © 2019 Pierre-Antoine Rouby <contact@parouby.fr>
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
;; Low-level FFI bindings.
;;
;;; Code:

(define-module (sdl2 bindings)
  #:use-module (system foreign)
  #:use-module (sdl2 config))

(define sdl-func
  (let ((lib (dynamic-link %libsdl2)))
    (lambda (return-type function-name arg-types)
      "Return a procedure for the foreign function FUNCTION-NAME in
the SDL2 shared library.  That function must return a value of
RETURN-TYPE and accept arguments of ARG-TYPES."
      (pointer->procedure return-type
                          (dynamic-func function-name lib)
                          arg-types))))

(define-syntax-rule (define-foreign name return-type func-name arg-types)
  (define-public name
    (sdl-func return-type func-name arg-types)))


;;;
;;; Foreign Types
;;;

(define-public sdl-color
  (list uint8 uint8 uint8 uint8))

(define-public sdl-rect
  (list int int int int))

(define-public sdl-point
  (list int int))

(define sdl-bool int)

(define-public (boolean->sdl-bool b)
  "Convert the boolean B to an SDL_bool."
  (if b 1 0))


;;;
;;; Errors
;;;

(define-foreign sdl-get-error
  '* "SDL_GetError" '())


;;;
;;; Initialization
;;;

(define-public SDL_INIT_TIMER          #x00000001)
(define-public SDL_INIT_AUDIO          #x00000010)
(define-public SDL_INIT_VIDEO          #x00000020)
(define-public SDL_INIT_JOYSTICK       #x00000200)
(define-public SDL_INIT_HAPTIC         #x00001000)
(define-public SDL_INIT_GAMECONTROLLER #x00002000)
(define-public SDL_INIT_EVENTS         #x00004000)

(define-foreign sdl-init
  int "SDL_Init" (list uint32))

(define-foreign sdl-quit
  void "SDL_Quit" '())


;;;
;;; Version
;;;

(define-foreign sdl-get-version
  void "SDL_GetVersion" '(*))


;;;
;;; Video
;;;

(define-public SDL_WINDOW_FULLSCREEN         #x00000001)
(define-public SDL_WINDOW_OPENGL             #x00000002)
(define-public SDL_WINDOW_SHOWN              #x00000004)
(define-public SDL_WINDOW_HIDDEN             #x00000008)
(define-public SDL_WINDOW_BORDERLESS         #x00000010)
(define-public SDL_WINDOW_RESIZABLE          #x00000020)
(define-public SDL_WINDOW_MINIMIZED          #x00000040)
(define-public SDL_WINDOW_MAXIMIZED          #x00000080)
(define-public SDL_WINDOW_INPUT_GRABBED      #x00000100)
(define-public SDL_WINDOW_INPUT_FOCUS        #x00000200)
(define-public SDL_WINDOW_MOUSE_FOCUS        #x00000400)
(define-public SDL_WINDOW_FULLSCREEN_DESKTOP (logior SDL_WINDOW_FULLSCREEN
                                              #x00001000))
(define-public SDL_WINDOW_FOREIGN            #x00000800)
(define-public SDL_WINDOW_ALLOW_HIGHDPI      #x00002000)
(define-public SDL_WINDOW_MOUSE_CAPTURE      #x00004000)

(define-public SDL_WINDOWPOS_CENTERED 805240832)
(define-public SDL_WINDOWPOS_UNDEFINED 536805376)

(define-foreign sdl-create-window
  '* "SDL_CreateWindow" (list '* int int int int uint32))

(define-foreign sdl-destroy-window
  void "SDL_DestroyWindow" '(*))

(define-foreign sdl-get-window-title
  '* "SDL_GetWindowTitle" '(*))

(define-foreign sdl-get-window-size
  void "SDL_GetWindowSize" '(* * *))

(define-foreign sdl-get-window-position
  void "SDL_GetWindowPosition" '(* * *))

(define-foreign sdl-get-window-id
  uint32 "SDL_GetWindowID" '(*))

(define-foreign sdl-get-window-from-id
  '* "SDL_GetWindowFromID" (list uint32))

(define-foreign sdl-hide-window
  void "SDL_HideWindow" '(*))

(define-foreign sdl-show-window
  void "SDL_ShowWindow" '(*))

(define-foreign sdl-maximize-window
  void "SDL_MaximizeWindow" '(*))

(define-foreign sdl-minimize-window
  void "SDL_MinimizeWindow" '(*))

(define-foreign sdl-raise-window
  void "SDL_RaiseWindow" '(*))

(define-foreign sdl-restore-window
  void "SDL_RestoreWindow" '(*))

(define-foreign sdl-set-window-bordered
  void "SDL_SetWindowBordered" (list '* sdl-bool))

(define-foreign sdl-set-window-title
  void "SDL_SetWindowTitle" '(* *))

(define-foreign sdl-set-window-position
  void "SDL_SetWindowPosition" (list '* int int))

(define-foreign sdl-set-window-size
  void "SDL_SetWindowSize" (list '* int int))

(define-foreign sdl-set-window-fullscreen
  int "SDL_SetWindowFullscreen" (list '* uint32))

(define-foreign sdl-gl-create-context
  '* "SDL_GL_CreateContext" '(*))

(define-foreign sdl-gl-delete-context
  void "SDL_GL_DeleteContext" '(*))

(define-foreign sdl-gl-swap-window
  void "SDL_GL_SwapWindow" '(*))

(define-public SDL_GL_RED_SIZE 0)
(define-public SDL_GL_GREEN_SIZE 1)
(define-public SDL_GL_BLUE_SIZE 2)
(define-public SDL_GL_ALPHA_SIZE 3)
(define-public SDL_GL_BUFFER_SIZE 4)
(define-public SDL_GL_DOUBLEBUFFER 5)
(define-public SDL_GL_DEPTH_SIZE 6)
(define-public SDL_GL_STENCIL_SIZE 7)
(define-public SDL_GL_ACCUM_RED_SIZE 8)
(define-public SDL_GL_ACCUM_GREEN_SIZE 9)
(define-public SDL_GL_ACCUM_BLUE_SIZE 10)
(define-public SDL_GL_ACCUM_ALPHA_SIZE 11)
(define-public SDL_GL_STEREO 12)
(define-public SDL_GL_MULTISAMPLEBUFFERS 13)
(define-public SDL_GL_MULTISAMPLESAMPLES 14)
(define-public SDL_GL_ACCELERATED_VISUAL 15)
(define-public SDL_GL_RETAINED_BACKING 16)
(define-public SDL_GL_CONTEXT_MAJOR_VERSION 17)
(define-public SDL_GL_CONTEXT_MINOR_VERSION 18)
(define-public SDL_GL_CONTEXT_EGL 19)
(define-public SDL_GL_CONTEXT_FLAGS 20)
(define-public SDL_GL_CONTEXT_PROFILE_MASK 21)
(define-public SDL_GL_SHARE_WITH_CURRENT_CONTEXT 22)
(define-public SDL_GL_FRAMEBUFFER_SRGB_CAPABLE 23)

(define-foreign sdl-gl-set-attribute
  int "SDL_GL_SetAttribute" (list int int))

(define-foreign sdl-gl-set-swap-interval
  int "SDL_GL_SetSwapInterval" (list int))

(define-public SDL_RENDERER_SOFTWARE #x00000001)
(define-public SDL_RENDERER_ACCELERATED #x00000002)
(define-public SDL_RENDERER_PRESENTVSYNC #x00000004)
(define-public SDL_RENDERER_TARGETTEXTURE #x00000008)

(define-foreign sdl-create-renderer
  '* "SDL_CreateRenderer" (list '* int uint32))

(define-foreign sdl-destroy-renderer
  void "SDL_DestroyRenderer" '(*))

(define-foreign sdl-render-clear
  int "SDL_RenderClear" '(*))

(define-foreign sdl-render-present
  void "SDL_RenderPresent" '(*))

(define-foreign sdl-render-copy
  int "SDL_RenderCopy" '(* * * *))

(define-foreign sdl-render-copy-ex
  int "SDL_RenderCopyEx" (list '* '* '* '* double '* int))

(define-foreign sdl-create-texture-from-surface
  '* "SDL_CreateTextureFromSurface" '(* *))

(define-foreign sdl-destroy-texture
  void "SDL_DestroyTexture" '(*))

(define-foreign sdl-set-render-draw-color
  int "SDL_SetRenderDrawColor" (list '* uint8 uint8 uint8 uint8))

(define-foreign sdl-render-draw-line
  int "SDL_RenderDrawLine" (list '* int int int int))

(define-foreign sdl-render-draw-lines
  int "SDL_RenderDrawLines" (list '* '* int))

(define-foreign sdl-render-draw-point
  int "SDL_RenderDrawPoint" (list '* int int))

(define-foreign sdl-render-draw-points
  int "SDL_RenderDrawPoints" (list '* '* int))


;;;
;;; Events
;;;

(define-public SDL_QUIT #x100)
(define-public SDL_APP_TERMINATING #x101)
(define-public SDL_APP_LOWMEMORY #x102)
(define-public SDL_APP_WILLENTERBACKGROUND #x103)
(define-public SDL_APP_DIDENTERBACKGROUND #x104)
(define-public SDL_APP_WILLENTERFOREGROUND #x105)
(define-public SDL_APP_DIDENTERFOREGROUND #x106)
(define-public SDL_WINDOWEVENT #x200)
(define-public SDL_SYSWMEVENT #x201)
(define-public SDL_KEYDOWN #x300)
(define-public SDL_KEYUP #x301)
(define-public SDL_TEXTEDITING #x302)
(define-public SDL_TEXTINPUT #x303)
(define-public SDL_MOUSEMOTION #x400)
(define-public SDL_MOUSEBUTTONDOWN #x401)
(define-public SDL_MOUSEBUTTONUP #x402)
(define-public SDL_MOUSEWHEEL #x403)
(define-public SDL_JOYAXISMOTION #x600)
(define-public SDL_JOYBALLMOTION #x601)
(define-public SDL_JOYHATMOTION #x602)
(define-public SDL_JOYBUTTONDOWN #x603)
(define-public SDL_JOYBUTTONUP #x604)
(define-public SDL_JOYDEVICEADDED #x605)
(define-public SDL_JOYDEVICEREMOVED #x606)
(define-public SDL_CONTROLLERAXISMOTION #x650)
(define-public SDL_CONTROLLERBUTTONDOWN #x651)
(define-public SDL_CONTROLLERBUTTONUP #x652)
(define-public SDL_CONTROLLERDEVICEADDED #x653)
(define-public SDL_CONTROLLERDEVICEREMOVED #x654)
(define-public SDL_CONTROLLERDEVICEREMAPPED #x655)
(define-public SDL_FINGERDOWN #x700)
(define-public SDL_FINGERUP #x701)
(define-public SDL_FINGERMOTION #x702)
(define-public SDL_DOLLARGESTURE #x800)
(define-public SDL_DOLLARRECORD #x801)
(define-public SDL_MULTIGESTURE #x802)
(define-public SDL_CLIPBOARDUPDATE #x900)
(define-public SDL_DROPFILE #x1000)
(define-public SDL_RENDER_TARGETS_RESET #x2000)
(define-public SDL_USEREVENT #x8000)

(define-public SDL_RELEASED 0)
(define-public SDL_PRESSED  1)

(define-public SDL_WINDOWEVENT_NONE         0)
(define-public SDL_WINDOWEVENT_SHOWN        1)
(define-public SDL_WINDOWEVENT_HIDDEN       2)
(define-public SDL_WINDOWEVENT_EXPOSED      3)
(define-public SDL_WINDOWEVENT_MOVED        4)
(define-public SDL_WINDOWEVENT_RESIZED      5)
(define-public SDL_WINDOWEVENT_SIZE_CHANGED 6)
(define-public SDL_WINDOWEVENT_MINIMIZED    7)
(define-public SDL_WINDOWEVENT_MAXIMIZED    8)
(define-public SDL_WINDOWEVENT_RESTORED     9)
(define-public SDL_WINDOWEVENT_ENTER        10)
(define-public SDL_WINDOWEVENT_LEAVE        11)
(define-public SDL_WINDOWEVENT_FOCUS_GAINED 12)
(define-public SDL_WINDOWEVENT_FOCUS_LOST   13)
(define-public SDL_WINDOWEVENT_CLOSE        14)

(define-foreign sdl-poll-event
  int "SDL_PollEvent" '(*))


;;;
;;; Keycodes and scancodes
;;;

(define-public KMOD_NONE #x0000)
(define-public KMOD_LSHIFT #x0001)
(define-public KMOD_RSHIFT #x0002)
(define-public KMOD_LCTRL #x0040)
(define-public KMOD_RCTRL #x0080)
(define-public KMOD_LALT #x0100)
(define-public KMOD_RALT #x0200)
(define-public KMOD_LGUI #x0400)
(define-public KMOD_RGUI #x0800)
(define-public KMOD_NUM #x1000)
(define-public KMOD_CAPS #x2000)
(define-public KMOD_MODE #x4000)

(define-public SDLK_SCANCODE_MASK (ash 1 30))

(define-public (scancode->keycode scancode)
  (logior scancode SDLK_SCANCODE_MASK))

(define-public SDL_SCANCODE_UNKNOWN 0)
(define-public SDL_SCANCODE_A 4)
(define-public SDL_SCANCODE_B 5)
(define-public SDL_SCANCODE_C 6)
(define-public SDL_SCANCODE_D 7)
(define-public SDL_SCANCODE_E 8)
(define-public SDL_SCANCODE_F 9)
(define-public SDL_SCANCODE_G 10)
(define-public SDL_SCANCODE_H 11)
(define-public SDL_SCANCODE_I 12)
(define-public SDL_SCANCODE_J 13)
(define-public SDL_SCANCODE_K 14)
(define-public SDL_SCANCODE_L 15)
(define-public SDL_SCANCODE_M 16)
(define-public SDL_SCANCODE_N 17)
(define-public SDL_SCANCODE_O 18)
(define-public SDL_SCANCODE_P 19)
(define-public SDL_SCANCODE_Q 20)
(define-public SDL_SCANCODE_R 21)
(define-public SDL_SCANCODE_S 22)
(define-public SDL_SCANCODE_T 23)
(define-public SDL_SCANCODE_U 24)
(define-public SDL_SCANCODE_V 25)
(define-public SDL_SCANCODE_W 26)
(define-public SDL_SCANCODE_X 27)
(define-public SDL_SCANCODE_Y 28)
(define-public SDL_SCANCODE_Z 29)
(define-public SDL_SCANCODE_1 30)
(define-public SDL_SCANCODE_2 31)
(define-public SDL_SCANCODE_3 32)
(define-public SDL_SCANCODE_4 33)
(define-public SDL_SCANCODE_5 34)
(define-public SDL_SCANCODE_6 35)
(define-public SDL_SCANCODE_7 36)
(define-public SDL_SCANCODE_8 37)
(define-public SDL_SCANCODE_9 38)
(define-public SDL_SCANCODE_0 39)
(define-public SDL_SCANCODE_RETURN 40)
(define-public SDL_SCANCODE_ESCAPE 41)
(define-public SDL_SCANCODE_BACKSPACE 42)
(define-public SDL_SCANCODE_TAB 43)
(define-public SDL_SCANCODE_SPACE 44)
(define-public SDL_SCANCODE_MINUS 45)
(define-public SDL_SCANCODE_EQUALS 46)
(define-public SDL_SCANCODE_LEFTBRACKET 47)
(define-public SDL_SCANCODE_RIGHTBRACKET 48)
(define-public SDL_SCANCODE_BACKSLASH 49)
(define-public SDL_SCANCODE_NONUSHASH 50)
(define-public SDL_SCANCODE_SEMICOLON 51)
(define-public SDL_SCANCODE_APOSTROPHE 52)
(define-public SDL_SCANCODE_GRAVE 53)
(define-public SDL_SCANCODE_COMMA 54)
(define-public SDL_SCANCODE_PERIOD 55)
(define-public SDL_SCANCODE_SLASH 56)
(define-public SDL_SCANCODE_CAPSLOCK 57)
(define-public SDL_SCANCODE_F1 58)
(define-public SDL_SCANCODE_F2 59)
(define-public SDL_SCANCODE_F3 60)
(define-public SDL_SCANCODE_F4 61)
(define-public SDL_SCANCODE_F5 62)
(define-public SDL_SCANCODE_F6 63)
(define-public SDL_SCANCODE_F7 64)
(define-public SDL_SCANCODE_F8 65)
(define-public SDL_SCANCODE_F9 66)
(define-public SDL_SCANCODE_F10 67)
(define-public SDL_SCANCODE_F11 68)
(define-public SDL_SCANCODE_F12 69)
(define-public SDL_SCANCODE_PRINTSCREEN 70)
(define-public SDL_SCANCODE_SCROLLLOCK 71)
(define-public SDL_SCANCODE_PAUSE 72)
(define-public SDL_SCANCODE_INSERT 73)
(define-public SDL_SCANCODE_HOME 74)
(define-public SDL_SCANCODE_PAGEUP 75)
(define-public SDL_SCANCODE_DELETE 76)
(define-public SDL_SCANCODE_END 77)
(define-public SDL_SCANCODE_PAGEDOWN 78)
(define-public SDL_SCANCODE_RIGHT 79)
(define-public SDL_SCANCODE_LEFT 80)
(define-public SDL_SCANCODE_DOWN 81)
(define-public SDL_SCANCODE_UP 82)
(define-public SDL_SCANCODE_NUMLOCKCLEAR 83)
(define-public SDL_SCANCODE_KP_DIVIDE 84)
(define-public SDL_SCANCODE_KP_MULTIPLY 85)
(define-public SDL_SCANCODE_KP_MINUS 86)
(define-public SDL_SCANCODE_KP_PLUS 87)
(define-public SDL_SCANCODE_KP_ENTER 88)
(define-public SDL_SCANCODE_KP_1 89)
(define-public SDL_SCANCODE_KP_2 90)
(define-public SDL_SCANCODE_KP_3 91)
(define-public SDL_SCANCODE_KP_4 92)
(define-public SDL_SCANCODE_KP_5 93)
(define-public SDL_SCANCODE_KP_6 94)
(define-public SDL_SCANCODE_KP_7 95)
(define-public SDL_SCANCODE_KP_8 96)
(define-public SDL_SCANCODE_KP_9 97)
(define-public SDL_SCANCODE_KP_0 98)
(define-public SDL_SCANCODE_KP_PERIOD 99)
(define-public SDL_SCANCODE_NONUSBACKSLASH 100)
(define-public SDL_SCANCODE_APPLICATION 101)
(define-public SDL_SCANCODE_POWER 102)
(define-public SDL_SCANCODE_KP_EQUALS 103)
(define-public SDL_SCANCODE_F13 104)
(define-public SDL_SCANCODE_F14 105)
(define-public SDL_SCANCODE_F15 106)
(define-public SDL_SCANCODE_F16 107)
(define-public SDL_SCANCODE_F17 108)
(define-public SDL_SCANCODE_F18 109)
(define-public SDL_SCANCODE_F19 110)
(define-public SDL_SCANCODE_F20 111)
(define-public SDL_SCANCODE_F21 112)
(define-public SDL_SCANCODE_F22 113)
(define-public SDL_SCANCODE_F23 114)
(define-public SDL_SCANCODE_F24 115)
(define-public SDL_SCANCODE_EXECUTE 116)
(define-public SDL_SCANCODE_HELP 117)
(define-public SDL_SCANCODE_MENU 118)
(define-public SDL_SCANCODE_SELECT 119)
(define-public SDL_SCANCODE_STOP 120)
(define-public SDL_SCANCODE_AGAIN 121)
(define-public SDL_SCANCODE_UNDO 122)
(define-public SDL_SCANCODE_CUT 123)
(define-public SDL_SCANCODE_COPY 124)
(define-public SDL_SCANCODE_PASTE 125)
(define-public SDL_SCANCODE_FIND 126)
(define-public SDL_SCANCODE_MUTE 127)
(define-public SDL_SCANCODE_VOLUMEUP 128)
(define-public SDL_SCANCODE_VOLUMEDOWN 129)
(define-public SDL_SCANCODE_KP_COMMA 133)
(define-public SDL_SCANCODE_KP_EQUALSAS400 134)
(define-public SDL_SCANCODE_INTERNATIONAL1 135)
(define-public SDL_SCANCODE_INTERNATIONAL2 136)
(define-public SDL_SCANCODE_INTERNATIONAL3 137)
(define-public SDL_SCANCODE_INTERNATIONAL4 138)
(define-public SDL_SCANCODE_INTERNATIONAL5 139)
(define-public SDL_SCANCODE_INTERNATIONAL6 140)
(define-public SDL_SCANCODE_INTERNATIONAL7 141)
(define-public SDL_SCANCODE_INTERNATIONAL8 142)
(define-public SDL_SCANCODE_INTERNATIONAL9 143)
(define-public SDL_SCANCODE_LANG1 144)
(define-public SDL_SCANCODE_LANG2 145)
(define-public SDL_SCANCODE_LANG3 146)
(define-public SDL_SCANCODE_LANG4 147)
(define-public SDL_SCANCODE_LANG5 148)
(define-public SDL_SCANCODE_LANG6 149)
(define-public SDL_SCANCODE_LANG7 150)
(define-public SDL_SCANCODE_LANG8 151)
(define-public SDL_SCANCODE_LANG9 152)
(define-public SDL_SCANCODE_ALTERASE 153)
(define-public SDL_SCANCODE_SYSREQ 154)
(define-public SDL_SCANCODE_CANCEL 155)
(define-public SDL_SCANCODE_CLEAR 156)
(define-public SDL_SCANCODE_PRIOR 157)
(define-public SDL_SCANCODE_RETURN2 158)
(define-public SDL_SCANCODE_SEPARATOR 159)
(define-public SDL_SCANCODE_OUT 160)
(define-public SDL_SCANCODE_OPER 161)
(define-public SDL_SCANCODE_CLEARAGAIN 162)
(define-public SDL_SCANCODE_CRSEL 163)
(define-public SDL_SCANCODE_EXSEL 164)
(define-public SDL_SCANCODE_KP_00 176)
(define-public SDL_SCANCODE_KP_000 177)
(define-public SDL_SCANCODE_THOUSANDSSEPARATOR 178)
(define-public SDL_SCANCODE_DECIMALSEPARATOR 179)
(define-public SDL_SCANCODE_CURRENCYUNIT 180)
(define-public SDL_SCANCODE_CURRENCYSUBUNIT 181)
(define-public SDL_SCANCODE_KP_LEFTPAREN 182)
(define-public SDL_SCANCODE_KP_RIGHTPAREN 183)
(define-public SDL_SCANCODE_KP_LEFTBRACE 184)
(define-public SDL_SCANCODE_KP_RIGHTBRACE 185)
(define-public SDL_SCANCODE_KP_TAB 186)
(define-public SDL_SCANCODE_KP_BACKSPACE 187)
(define-public SDL_SCANCODE_KP_A 188)
(define-public SDL_SCANCODE_KP_B 189)
(define-public SDL_SCANCODE_KP_C 190)
(define-public SDL_SCANCODE_KP_D 191)
(define-public SDL_SCANCODE_KP_E 192)
(define-public SDL_SCANCODE_KP_F 193)
(define-public SDL_SCANCODE_KP_XOR 194)
(define-public SDL_SCANCODE_KP_POWER 195)
(define-public SDL_SCANCODE_KP_PERCENT 196)
(define-public SDL_SCANCODE_KP_LESS 197)
(define-public SDL_SCANCODE_KP_GREATER 198)
(define-public SDL_SCANCODE_KP_AMPERSAND 199)
(define-public SDL_SCANCODE_KP_DBLAMPERSAND 200)
(define-public SDL_SCANCODE_KP_VERTICALBAR 201)
(define-public SDL_SCANCODE_KP_DBLVERTICALBAR 202)
(define-public SDL_SCANCODE_KP_COLON 203)
(define-public SDL_SCANCODE_KP_HASH 204)
(define-public SDL_SCANCODE_KP_SPACE 205)
(define-public SDL_SCANCODE_KP_AT 206)
(define-public SDL_SCANCODE_KP_EXCLAM 207)
(define-public SDL_SCANCODE_KP_MEMSTORE 208)
(define-public SDL_SCANCODE_KP_MEMRECALL 209)
(define-public SDL_SCANCODE_KP_MEMCLEAR 210)
(define-public SDL_SCANCODE_KP_MEMADD 211)
(define-public SDL_SCANCODE_KP_MEMSUBTRACT 212)
(define-public SDL_SCANCODE_KP_MEMMULTIPLY 213)
(define-public SDL_SCANCODE_KP_MEMDIVIDE 214)
(define-public SDL_SCANCODE_KP_PLUSMINUS 215)
(define-public SDL_SCANCODE_KP_CLEAR 216)
(define-public SDL_SCANCODE_KP_CLEARENTRY 217)
(define-public SDL_SCANCODE_KP_BINARY 218)
(define-public SDL_SCANCODE_KP_OCTAL 219)
(define-public SDL_SCANCODE_KP_DECIMAL 220)
(define-public SDL_SCANCODE_KP_HEXADECIMAL 221)
(define-public SDL_SCANCODE_LCTRL 224)
(define-public SDL_SCANCODE_LSHIFT 225)
(define-public SDL_SCANCODE_LALT 226)
(define-public SDL_SCANCODE_LGUI 227)
(define-public SDL_SCANCODE_RCTRL 228)
(define-public SDL_SCANCODE_RSHIFT 229)
(define-public SDL_SCANCODE_RALT 230)
(define-public SDL_SCANCODE_RGUI 231)
(define-public SDL_SCANCODE_MODE 257)
(define-public SDL_SCANCODE_AUDIONEXT 258)
(define-public SDL_SCANCODE_AUDIOPREV 259)
(define-public SDL_SCANCODE_AUDIOSTOP 260)
(define-public SDL_SCANCODE_AUDIOPLAY 261)
(define-public SDL_SCANCODE_AUDIOMUTE 262)
(define-public SDL_SCANCODE_MEDIASELECT 263)
(define-public SDL_SCANCODE_WWW 264)
(define-public SDL_SCANCODE_MAIL 265)
(define-public SDL_SCANCODE_CALCULATOR 266)
(define-public SDL_SCANCODE_COMPUTER 267)
(define-public SDL_SCANCODE_AC_SEARCH 268)
(define-public SDL_SCANCODE_AC_HOME 269)
(define-public SDL_SCANCODE_AC_BACK 270)
(define-public SDL_SCANCODE_AC_FORWARD 271)
(define-public SDL_SCANCODE_AC_STOP 272)
(define-public SDL_SCANCODE_AC_REFRESH 273)
(define-public SDL_SCANCODE_AC_BOOKMARKS 274)
(define-public SDL_SCANCODE_BRIGHTNESSDOWN 275)
(define-public SDL_SCANCODE_BRIGHTNESSUP 276)
(define-public SDL_SCANCODE_DISPLAYSWITCH 277)
(define-public SDL_SCANCODE_KBDILLUMTOGGLE 278)
(define-public SDL_SCANCODE_KBDILLUMDOWN 279)
(define-public SDL_SCANCODE_KBDILLUMUP 280)
(define-public SDL_SCANCODE_EJECT 281)
(define-public SDL_SCANCODE_SLEEP 282)
(define-public SDL_SCANCODE_APP1 283)
(define-public SDL_SCANCODE_APP2 284)
(define-public SDL_NUM_SCANCODES 512)

(define-public SDLK_UNKNOWN 0)
(define-public SDLK_RETURN 13)
(define-public SDLK_ESCAPE 27)
(define-public SDLK_BACKSPACE 8)
(define-public SDLK_TAB 9)
(define-public SDLK_SPACE 32)
(define-public SDLK_EXCLAIM 33)
(define-public SDLK_QUOTEDBL 34)
(define-public SDLK_HASH 35)
(define-public SDLK_PERCENT 37)
(define-public SDLK_DOLLAR 36)
(define-public SDLK_AMPERSAND 38)
(define-public SDLK_QUOTE 39)
(define-public SDLK_LEFTPAREN 40)
(define-public SDLK_RIGHTPAREN 41)
(define-public SDLK_ASTERISK 42)
(define-public SDLK_PLUS 43)
(define-public SDLK_COMMA 44)
(define-public SDLK_MINUS 45)
(define-public SDLK_PERIOD 46)
(define-public SDLK_SLASH 47)
(define-public SDLK_0 48)
(define-public SDLK_1 49)
(define-public SDLK_2 50)
(define-public SDLK_3 51)
(define-public SDLK_4 52)
(define-public SDLK_5 53)
(define-public SDLK_6 54)
(define-public SDLK_7 55)
(define-public SDLK_8 56)
(define-public SDLK_9 57)
(define-public SDLK_COLON 58)
(define-public SDLK_SEMICOLON 59)
(define-public SDLK_LESS 60)
(define-public SDLK_EQUALS 61)
(define-public SDLK_GREATER 62)
(define-public SDLK_QUESTION 63)
(define-public SDLK_AT 64)
(define-public SDLK_LEFTBRACKET 91)
(define-public SDLK_BACKSLASH 92)
(define-public SDLK_RIGHTBRACKET 93)
(define-public SDLK_CARET 94)
(define-public SDLK_UNDERSCORE 95)
(define-public SDLK_BACKQUOTE 96)
(define-public SDLK_a 97)
(define-public SDLK_b 98)
(define-public SDLK_c 99)
(define-public SDLK_d 100)
(define-public SDLK_e 101)
(define-public SDLK_f 102)
(define-public SDLK_g 103)
(define-public SDLK_h 104)
(define-public SDLK_i 105)
(define-public SDLK_j 106)
(define-public SDLK_k 107)
(define-public SDLK_l 108)
(define-public SDLK_m 109)
(define-public SDLK_n 110)
(define-public SDLK_o 111)
(define-public SDLK_p 112)
(define-public SDLK_q 113)
(define-public SDLK_r 114)
(define-public SDLK_s 115)
(define-public SDLK_t 116)
(define-public SDLK_u 117)
(define-public SDLK_v 118)
(define-public SDLK_w 119)
(define-public SDLK_x 120)
(define-public SDLK_y 121)
(define-public SDLK_z 122)
(define-public SDLK_CAPSLOCK (scancode->keycode SDL_SCANCODE_CAPSLOCK))
(define-public SDLK_F1 (scancode->keycode SDL_SCANCODE_F1))
(define-public SDLK_F2 (scancode->keycode SDL_SCANCODE_F2))
(define-public SDLK_F3 (scancode->keycode SDL_SCANCODE_F3))
(define-public SDLK_F4 (scancode->keycode SDL_SCANCODE_F4))
(define-public SDLK_F5 (scancode->keycode SDL_SCANCODE_F5))
(define-public SDLK_F6 (scancode->keycode SDL_SCANCODE_F6))
(define-public SDLK_F7 (scancode->keycode SDL_SCANCODE_F7))
(define-public SDLK_F8 (scancode->keycode SDL_SCANCODE_F8))
(define-public SDLK_F9 (scancode->keycode SDL_SCANCODE_F9))
(define-public SDLK_F10 (scancode->keycode SDL_SCANCODE_F10))
(define-public SDLK_F11 (scancode->keycode SDL_SCANCODE_F11))
(define-public SDLK_F12 (scancode->keycode SDL_SCANCODE_F12))
(define-public SDLK_PRINTSCREEN (scancode->keycode SDL_SCANCODE_PRINTSCREEN))
(define-public SDLK_SCROLLLOCK (scancode->keycode SDL_SCANCODE_SCROLLLOCK))
(define-public SDLK_PAUSE (scancode->keycode SDL_SCANCODE_PAUSE))
(define-public SDLK_INSERT (scancode->keycode SDL_SCANCODE_INSERT))
(define-public SDLK_HOME (scancode->keycode SDL_SCANCODE_HOME))
(define-public SDLK_PAGEUP (scancode->keycode SDL_SCANCODE_PAGEUP))
(define-public SDLK_DELETE 127)
(define-public SDLK_END (scancode->keycode SDL_SCANCODE_END))
(define-public SDLK_PAGEDOWN (scancode->keycode SDL_SCANCODE_PAGEDOWN))
(define-public SDLK_RIGHT (scancode->keycode SDL_SCANCODE_RIGHT))
(define-public SDLK_LEFT (scancode->keycode SDL_SCANCODE_LEFT))
(define-public SDLK_DOWN (scancode->keycode SDL_SCANCODE_DOWN))
(define-public SDLK_UP (scancode->keycode SDL_SCANCODE_UP))
(define-public SDLK_NUMLOCKCLEAR (scancode->keycode SDL_SCANCODE_NUMLOCKCLEAR))
(define-public SDLK_KP_DIVIDE (scancode->keycode SDL_SCANCODE_KP_DIVIDE))
(define-public SDLK_KP_MULTIPLY (scancode->keycode SDL_SCANCODE_KP_MULTIPLY))
(define-public SDLK_KP_MINUS (scancode->keycode SDL_SCANCODE_KP_MINUS))
(define-public SDLK_KP_PLUS (scancode->keycode SDL_SCANCODE_KP_PLUS))
(define-public SDLK_KP_ENTER (scancode->keycode SDL_SCANCODE_KP_ENTER))
(define-public SDLK_KP_1 (scancode->keycode SDL_SCANCODE_KP_1))
(define-public SDLK_KP_2 (scancode->keycode SDL_SCANCODE_KP_2))
(define-public SDLK_KP_3 (scancode->keycode SDL_SCANCODE_KP_3))
(define-public SDLK_KP_4 (scancode->keycode SDL_SCANCODE_KP_4))
(define-public SDLK_KP_5 (scancode->keycode SDL_SCANCODE_KP_5))
(define-public SDLK_KP_6 (scancode->keycode SDL_SCANCODE_KP_6))
(define-public SDLK_KP_7 (scancode->keycode SDL_SCANCODE_KP_7))
(define-public SDLK_KP_8 (scancode->keycode SDL_SCANCODE_KP_8))
(define-public SDLK_KP_9 (scancode->keycode SDL_SCANCODE_KP_9))
(define-public SDLK_KP_0 (scancode->keycode SDL_SCANCODE_KP_0))
(define-public SDLK_KP_PERIOD (scancode->keycode SDL_SCANCODE_KP_PERIOD))
(define-public SDLK_APPLICATION (scancode->keycode SDL_SCANCODE_APPLICATION))
(define-public SDLK_POWER (scancode->keycode SDL_SCANCODE_POWER))
(define-public SDLK_KP_EQUALS (scancode->keycode SDL_SCANCODE_KP_EQUALS))
(define-public SDLK_F13 (scancode->keycode SDL_SCANCODE_F13))
(define-public SDLK_F14 (scancode->keycode SDL_SCANCODE_F14))
(define-public SDLK_F15 (scancode->keycode SDL_SCANCODE_F15))
(define-public SDLK_F16 (scancode->keycode SDL_SCANCODE_F16))
(define-public SDLK_F17 (scancode->keycode SDL_SCANCODE_F17))
(define-public SDLK_F18 (scancode->keycode SDL_SCANCODE_F18))
(define-public SDLK_F19 (scancode->keycode SDL_SCANCODE_F19))
(define-public SDLK_F20 (scancode->keycode SDL_SCANCODE_F20))
(define-public SDLK_F21 (scancode->keycode SDL_SCANCODE_F21))
(define-public SDLK_F22 (scancode->keycode SDL_SCANCODE_F22))
(define-public SDLK_F23 (scancode->keycode SDL_SCANCODE_F23))
(define-public SDLK_F24 (scancode->keycode SDL_SCANCODE_F24))
(define-public SDLK_EXECUTE (scancode->keycode SDL_SCANCODE_EXECUTE))
(define-public SDLK_HELP (scancode->keycode SDL_SCANCODE_HELP))
(define-public SDLK_MENU (scancode->keycode SDL_SCANCODE_MENU))
(define-public SDLK_SELECT (scancode->keycode SDL_SCANCODE_SELECT))
(define-public SDLK_STOP (scancode->keycode SDL_SCANCODE_STOP))
(define-public SDLK_AGAIN (scancode->keycode SDL_SCANCODE_AGAIN))
(define-public SDLK_UNDO (scancode->keycode SDL_SCANCODE_UNDO))
(define-public SDLK_CUT (scancode->keycode SDL_SCANCODE_CUT))
(define-public SDLK_COPY (scancode->keycode SDL_SCANCODE_COPY))
(define-public SDLK_PASTE (scancode->keycode SDL_SCANCODE_PASTE))
(define-public SDLK_FIND (scancode->keycode SDL_SCANCODE_FIND))
(define-public SDLK_MUTE (scancode->keycode SDL_SCANCODE_MUTE))
(define-public SDLK_VOLUMEUP (scancode->keycode SDL_SCANCODE_VOLUMEUP))
(define-public SDLK_VOLUMEDOWN (scancode->keycode SDL_SCANCODE_VOLUMEDOWN))
(define-public SDLK_KP_COMMA (scancode->keycode SDL_SCANCODE_KP_COMMA))
(define-public SDLK_KP_EQUALSAS400
  (scancode->keycode SDL_SCANCODE_KP_EQUALSAS400))
(define-public SDLK_ALTERASE (scancode->keycode SDL_SCANCODE_ALTERASE))
(define-public SDLK_SYSREQ (scancode->keycode SDL_SCANCODE_SYSREQ))
(define-public SDLK_CANCEL (scancode->keycode SDL_SCANCODE_CANCEL))
(define-public SDLK_CLEAR (scancode->keycode SDL_SCANCODE_CLEAR))
(define-public SDLK_PRIOR (scancode->keycode SDL_SCANCODE_PRIOR))
(define-public SDLK_RETURN2 (scancode->keycode SDL_SCANCODE_RETURN2))
(define-public SDLK_SEPARATOR (scancode->keycode SDL_SCANCODE_SEPARATOR))
(define-public SDLK_OUT (scancode->keycode SDL_SCANCODE_OUT))
(define-public SDLK_OPER (scancode->keycode SDL_SCANCODE_OPER))
(define-public SDLK_CLEARAGAIN (scancode->keycode SDL_SCANCODE_CLEARAGAIN))
(define-public SDLK_CRSEL (scancode->keycode SDL_SCANCODE_CRSEL))
(define-public SDLK_EXSEL (scancode->keycode SDL_SCANCODE_EXSEL))
(define-public SDLK_KP_00 (scancode->keycode SDL_SCANCODE_KP_00))
(define-public SDLK_KP_000 (scancode->keycode SDL_SCANCODE_KP_000))
(define-public SDLK_THOUSANDSSEPARATOR
  (scancode->keycode SDL_SCANCODE_THOUSANDSSEPARATOR))
(define-public SDLK_DECIMALSEPARATOR
  (scancode->keycode SDL_SCANCODE_DECIMALSEPARATOR))
(define-public SDLK_CURRENCYUNIT (scancode->keycode SDL_SCANCODE_CURRENCYUNIT))
(define-public SDLK_CURRENCYSUBUNIT
  (scancode->keycode SDL_SCANCODE_CURRENCYSUBUNIT))
(define-public SDLK_KP_LEFTPAREN (scancode->keycode SDL_SCANCODE_KP_LEFTPAREN))
(define-public SDLK_KP_RIGHTPAREN
  (scancode->keycode SDL_SCANCODE_KP_RIGHTPAREN))
(define-public SDLK_KP_LEFTBRACE (scancode->keycode SDL_SCANCODE_KP_LEFTBRACE))
(define-public SDLK_KP_RIGHTBRACE (scancode->keycode SDL_SCANCODE_KP_RIGHTBRACE))
(define-public SDLK_KP_TAB (scancode->keycode SDL_SCANCODE_KP_TAB))
(define-public SDLK_KP_BACKSPACE (scancode->keycode SDL_SCANCODE_KP_BACKSPACE))
(define-public SDLK_KP_A (scancode->keycode SDL_SCANCODE_KP_A))
(define-public SDLK_KP_B (scancode->keycode SDL_SCANCODE_KP_B))
(define-public SDLK_KP_C (scancode->keycode SDL_SCANCODE_KP_C))
(define-public SDLK_KP_D (scancode->keycode SDL_SCANCODE_KP_D))
(define-public SDLK_KP_E (scancode->keycode SDL_SCANCODE_KP_E))
(define-public SDLK_KP_F (scancode->keycode SDL_SCANCODE_KP_F))
(define-public SDLK_KP_XOR (scancode->keycode SDL_SCANCODE_KP_XOR))
(define-public SDLK_KP_POWER (scancode->keycode SDL_SCANCODE_KP_POWER))
(define-public SDLK_KP_PERCENT (scancode->keycode SDL_SCANCODE_KP_PERCENT))
(define-public SDLK_KP_LESS (scancode->keycode SDL_SCANCODE_KP_LESS))
(define-public SDLK_KP_GREATER (scancode->keycode SDL_SCANCODE_KP_GREATER))
(define-public SDLK_KP_AMPERSAND (scancode->keycode SDL_SCANCODE_KP_AMPERSAND))
(define-public SDLK_KP_DBLAMPERSAND
  (scancode->keycode SDL_SCANCODE_KP_DBLAMPERSAND))
(define-public SDLK_KP_VERTICALBAR
  (scancode->keycode SDL_SCANCODE_KP_VERTICALBAR))
(define-public SDLK_KP_DBLVERTICALBAR
  (scancode->keycode SDL_SCANCODE_KP_DBLVERTICALBAR))
(define-public SDLK_KP_COLON (scancode->keycode SDL_SCANCODE_KP_COLON))
(define-public SDLK_KP_HASH (scancode->keycode SDL_SCANCODE_KP_HASH))
(define-public SDLK_KP_SPACE (scancode->keycode SDL_SCANCODE_KP_SPACE))
(define-public SDLK_KP_AT (scancode->keycode SDL_SCANCODE_KP_AT))
(define-public SDLK_KP_EXCLAM (scancode->keycode SDL_SCANCODE_KP_EXCLAM))
(define-public SDLK_KP_MEMSTORE (scancode->keycode SDL_SCANCODE_KP_MEMSTORE))
(define-public SDLK_KP_MEMRECALL (scancode->keycode SDL_SCANCODE_KP_MEMRECALL))
(define-public SDLK_KP_MEMCLEAR (scancode->keycode SDL_SCANCODE_KP_MEMCLEAR))
(define-public SDLK_KP_MEMADD (scancode->keycode SDL_SCANCODE_KP_MEMADD))
(define-public SDLK_KP_MEMSUBTRACT
  (scancode->keycode SDL_SCANCODE_KP_MEMSUBTRACT))
(define-public SDLK_KP_MEMMULTIPLY
  (scancode->keycode SDL_SCANCODE_KP_MEMMULTIPLY))
(define-public SDLK_KP_MEMDIVIDE (scancode->keycode SDL_SCANCODE_KP_MEMDIVIDE))
(define-public SDLK_KP_PLUSMINUS (scancode->keycode SDL_SCANCODE_KP_PLUSMINUS))
(define-public SDLK_KP_CLEAR (scancode->keycode SDL_SCANCODE_KP_CLEAR))
(define-public SDLK_KP_CLEARENTRY (scancode->keycode SDL_SCANCODE_KP_CLEARENTRY))
(define-public SDLK_KP_BINARY (scancode->keycode SDL_SCANCODE_KP_BINARY))
(define-public SDLK_KP_OCTAL (scancode->keycode SDL_SCANCODE_KP_OCTAL))
(define-public SDLK_KP_DECIMAL (scancode->keycode SDL_SCANCODE_KP_DECIMAL))
(define-public SDLK_KP_HEXADECIMAL
  (scancode->keycode SDL_SCANCODE_KP_HEXADECIMAL))
(define-public SDLK_LCTRL (scancode->keycode SDL_SCANCODE_LCTRL))
(define-public SDLK_LSHIFT (scancode->keycode SDL_SCANCODE_LSHIFT))
(define-public SDLK_LALT (scancode->keycode SDL_SCANCODE_LALT))
(define-public SDLK_LGUI (scancode->keycode SDL_SCANCODE_LGUI))
(define-public SDLK_RCTRL (scancode->keycode SDL_SCANCODE_RCTRL))
(define-public SDLK_RSHIFT (scancode->keycode SDL_SCANCODE_RSHIFT))
(define-public SDLK_RALT (scancode->keycode SDL_SCANCODE_RALT))
(define-public SDLK_RGUI (scancode->keycode SDL_SCANCODE_RGUI))
(define-public SDLK_MODE (scancode->keycode SDL_SCANCODE_MODE))
(define-public SDLK_AUDIONEXT (scancode->keycode SDL_SCANCODE_AUDIONEXT))
(define-public SDLK_AUDIOPREV (scancode->keycode SDL_SCANCODE_AUDIOPREV))
(define-public SDLK_AUDIOSTOP (scancode->keycode SDL_SCANCODE_AUDIOSTOP))
(define-public SDLK_AUDIOPLAY (scancode->keycode SDL_SCANCODE_AUDIOPLAY))
(define-public SDLK_AUDIOMUTE (scancode->keycode SDL_SCANCODE_AUDIOMUTE))
(define-public SDLK_MEDIASELECT (scancode->keycode SDL_SCANCODE_MEDIASELECT))
(define-public SDLK_WWW (scancode->keycode SDL_SCANCODE_WWW))
(define-public SDLK_MAIL (scancode->keycode SDL_SCANCODE_MAIL))
(define-public SDLK_CALCULATOR (scancode->keycode SDL_SCANCODE_CALCULATOR))
(define-public SDLK_COMPUTER (scancode->keycode SDL_SCANCODE_COMPUTER))
(define-public SDLK_AC_SEARCH (scancode->keycode SDL_SCANCODE_AC_SEARCH))
(define-public SDLK_AC_HOME (scancode->keycode SDL_SCANCODE_AC_HOME))
(define-public SDLK_AC_BACK (scancode->keycode SDL_SCANCODE_AC_BACK))
(define-public SDLK_AC_FORWARD (scancode->keycode SDL_SCANCODE_AC_FORWARD))
(define-public SDLK_AC_STOP (scancode->keycode SDL_SCANCODE_AC_STOP))
(define-public SDLK_AC_REFRESH (scancode->keycode SDL_SCANCODE_AC_REFRESH))
(define-public SDLK_AC_BOOKMARKS (scancode->keycode SDL_SCANCODE_AC_BOOKMARKS))
(define-public SDLK_BRIGHTNESSDOWN
  (scancode->keycode SDL_SCANCODE_BRIGHTNESSDOWN))
(define-public SDLK_BRIGHTNESSUP (scancode->keycode SDL_SCANCODE_BRIGHTNESSUP))
(define-public SDLK_DISPLAYSWITCH (scancode->keycode SDL_SCANCODE_DISPLAYSWITCH))
(define-public SDLK_KBDILLUMTOGGLE
  (scancode->keycode SDL_SCANCODE_KBDILLUMTOGGLE))
(define-public SDLK_KBDILLUMDOWN (scancode->keycode SDL_SCANCODE_KBDILLUMDOWN))
(define-public SDLK_KBDILLUMUP (scancode->keycode SDL_SCANCODE_KBDILLUMUP))
(define-public SDLK_EJECT (scancode->keycode SDL_SCANCODE_EJECT))
(define-public SDLK_SLEEP (scancode->keycode SDL_SCANCODE_SLEEP))

(define-foreign sdl-get-keyboard-state
  '* "SDL_GetKeyboardState" '(*))


;;;
;;; Text Input
;;;

(define-foreign sdl-start-text-input
  void "SDL_StartTextInput" '())

(define-foreign sdl-stop-text-input
  void "SDL_StopTextInput" '())

(define-foreign sdl-is-text-input-active
  sdl-bool "SDL_IsTextInputActive" '())


;;;
;;; Mouse
;;;

(define-public SDL_BUTTON_LEFT   1)
(define-public SDL_BUTTON_MIDDLE 2)
(define-public SDL_BUTTON_RIGHT  3)
(define-public SDL_BUTTON_X1     4)
(define-public SDL_BUTTON_X2     5)

(define (button-mask n)
  (ash 1 (1- n)))

(define-public SDL_BUTTON_LMASK  (button-mask SDL_BUTTON_LEFT))
(define-public SDL_BUTTON_MMASK  (button-mask SDL_BUTTON_MIDDLE))
(define-public SDL_BUTTON_RMASK  (button-mask SDL_BUTTON_RIGHT))
(define-public SDL_BUTTON_X1MASK (button-mask SDL_BUTTON_X1))
(define-public SDL_BUTTON_X2MASK (button-mask SDL_BUTTON_X2))

(define-foreign sdl-get-mouse-state
  uint32 "SDL_GetMouseState" '(* *))


;;;
;;; Timer
;;;

(define-foreign sdl-get-ticks
  uint32 "SDL_GetTicks" '())


;;;
;;; File I/O
;;;

(define-foreign sdl-rw-from-file
  '* "SDL_RWFromFile" (list '* '*))


;;;
;;; Pixels
;;;

(define-public SDL_PIXELFORMAT_UNKNOWN     0)
(define-public SDL_PIXELFORMAT_INDEX1LSB   286261504)
(define-public SDL_PIXELFORMAT_INDEX1MSB   287310080)
(define-public SDL_PIXELFORMAT_INDEX4LSB   303039488)
(define-public SDL_PIXELFORMAT_INDEX4MSB   304088064)
(define-public SDL_PIXELFORMAT_INDEX8      318769153)
(define-public SDL_PIXELFORMAT_RGB332      336660481)
(define-public SDL_PIXELFORMAT_RGB444      353504258)
(define-public SDL_PIXELFORMAT_RGB555      353570562)
(define-public SDL_PIXELFORMAT_BGR555      357764866)
(define-public SDL_PIXELFORMAT_ARGB4444    355602434)
(define-public SDL_PIXELFORMAT_RGBA4444    356651010)
(define-public SDL_PIXELFORMAT_ABGR4444    359796738)
(define-public SDL_PIXELFORMAT_BGRA4444    360845314)
(define-public SDL_PIXELFORMAT_ARGB1555    355667970)
(define-public SDL_PIXELFORMAT_RGBA5551    356782082)
(define-public SDL_PIXELFORMAT_ABGR1555    359862274)
(define-public SDL_PIXELFORMAT_BGRA5551    360976386)
(define-public SDL_PIXELFORMAT_RGB565      353701890)
(define-public SDL_PIXELFORMAT_BGR565      357896194)
(define-public SDL_PIXELFORMAT_RGB24       386930691)
(define-public SDL_PIXELFORMAT_BGR24       390076419)
(define-public SDL_PIXELFORMAT_RGB888      370546692)
(define-public SDL_PIXELFORMAT_RGBX8888    371595268)
(define-public SDL_PIXELFORMAT_BGR888      374740996)
(define-public SDL_PIXELFORMAT_BGRX8888    375789572)
(define-public SDL_PIXELFORMAT_ARGB8888    372645892)
(define-public SDL_PIXELFORMAT_RGBA8888    373694468)
(define-public SDL_PIXELFORMAT_ABGR8888    376840196)
(define-public SDL_PIXELFORMAT_BGRA8888    377888772)
(define-public SDL_PIXELFORMAT_ARGB2101010 372711428)
(define-public SDL_PIXELFORMAT_YV12        842094169)
(define-public SDL_PIXELFORMAT_IYUV        1448433993)
(define-public SDL_PIXELFORMAT_YUY2        844715353)
(define-public SDL_PIXELFORMAT_UYVY        1498831189)
(define-public SDL_PIXELFORMAT_YVYU        1431918169)


;;;
;;; Surface
;;;

(define-foreign sdl-create-rgb-surface
  '* "SDL_CreateRGBSurface"
  (list uint32 int int int uint32 uint32 uint32 uint32))

(define-foreign sdl-create-rgb-surface-from
  '* "SDL_CreateRGBSurfaceFrom"
  (list '* int int int int uint32 uint32 uint32 uint32))

(define-foreign sdl-free-surface
  void "SDL_FreeSurface" '(*))

(define-foreign sdl-load-bmp-rw
  '* "SDL_LoadBMP_RW" (list '* int))

(define-foreign sdl-convert-surface-format
  '* "SDL_ConvertSurfaceFormat" (list '* uint32 uint32))

(define-foreign sdl-blit-surface
  int "SDL_UpperBlit" '(* * * *))

(define-foreign sdl-blit-scaled
  int "SDL_UpperBlitScaled" '(* * * *))

(define-foreign sdl-fill-rect
  int "SDL_FillRect" (list '* '* uint32))


;;;
;;; Audio
;;;

(define-public AUDIO_U8     #x0008)
(define-public AUDIO_S8     #x8008)
(define-public AUDIO_U16LSB #x0010)
(define-public AUDIO_S16LSB #x8010)
(define-public AUDIO_U16MSB #x1010)
(define-public AUDIO_S16MSB #x9010)
(define-public AUDIO_U16    AUDIO_U16LSB)
(define-public AUDIO_S16    AUDIO_S16LSB)
(define-public AUDIO_S32LSB #x8020)
(define-public AUDIO_S32MSB #x9020)
(define-public AUDIO_S32    AUDIO_S32LSB)
(define-public AUDIO_F32LSB #x8120)
(define-public AUDIO_F32MSB #x9120)
(define-public AUDIO_F32    AUDIO_F32LSB)


;;;
;;; Joystick
;;;

(define-public SDL_JOYSTICK_POWER_UNKNOWN -1)
(define-public SDL_JOYSTICK_POWER_EMPTY 0)
(define-public SDL_JOYSTICK_POWER_LOW 1)
(define-public SDL_JOYSTICK_POWER_MEDIUM 2)
(define-public SDL_JOYSTICK_POWER_FULL 3)
(define-public SDL_JOYSTICK_POWER_WIRED 4)
(define-public SDL_JOYSTICK_POWER_MAX 5)

(define-foreign sdl-joystick-open
  '* "SDL_JoystickOpen" (list int))

(define-foreign sdl-joystick-close
  void "SDL_JoystickClose" '(*))

(define-foreign sdl-joystick-current-power-level
  int "SDL_JoystickCurrentPowerLevel" '(*))

(define-foreign sdl-joystick-event-state
  int "SDL_JoystickEventState" (list int))

(define-foreign sdl-joystick-from-instance-id
  '* "SDL_JoystickFromInstanceID" (list int32))

(define-foreign sdl-joystick-get-attached
  sdl-bool "SDL_JoystickGetAttached" '(*))

(define-foreign sdl-joystick-get-axis
  int16 "SDL_JoystickGetAxis" (list '* int))

(define-foreign sdl-joystick-get-ball
  int "SDL_JoystickGetBall" (list '* int '* '*))

(define-foreign sdl-joystick-get-button
  uint8 "SDL_JoystickGetButton" (list '* int))

(define-foreign sdl-joystick-get-device-guid
  '* "SDL_JoystickGetDeviceGUID" (list int))

(define-foreign sdl-joystick-get-guid
  '* "SDL_JoystickGetGUID" (list int))

(define-foreign sdl-joystick-get-guid-from-string
  '* "SDL_JoystickGetGUIDFromString" '(*))

(define-foreign sdl-joystick-get-guid-string
  void "SDL_JoystickGetGUIDString" (list '* '* int))

(define-foreign sdl-joystick-get-hat
  uint8 "SDL_JoystickGetHat" (list '* int))

(define-foreign sdl-joystick-instance-id
  int32 "SDL_JoystickInstanceID" '(*))

(define-foreign sdl-joystick-name
  '* "SDL_JoystickName" '(*))

(define-foreign sdl-joystick-name-for-index
  '* "SDL_JoystickNameForIndex" (list int))

(define-foreign sdl-joystick-num-axes
  int "SDL_JoystickNumAxes" '(*))

(define-foreign sdl-joystick-num-balls
  int "SDL_JoystickNumBalls" '(*))

(define-foreign sdl-joystick-num-buttons
  int "SDL_JoystickNumButtons" '(*))

(define-foreign sdl-joystick-num-hats
  int "SDL_JoystickNumHats" '(*))

(define-foreign sdl-num-joysticks
  int "SDL_NumJoysticks" '())

(define-foreign sdl-joystick-update
  void "SDL_JoystickUpdate" '())


;;;
;;; Game Controllers
;;;

(define-public SDL_CONTROLLER_AXIS_INVALID -1)
(define-public SDL_CONTROLLER_AXIS_LEFTX 0)
(define-public SDL_CONTROLLER_AXIS_LEFTY 1)
(define-public SDL_CONTROLLER_AXIS_RIGHTX 2)
(define-public SDL_CONTROLLER_AXIS_RIGHTY 3)
(define-public SDL_CONTROLLER_AXIS_TRIGGERLEFT 4)
(define-public SDL_CONTROLLER_AXIS_TRIGGERRIGHT 5)
(define-public SDL_CONTROLLER_AXIS_MAX 6)

(define-public SDL_CONTROLLER_BUTTON_INVALID -1)
(define-public SDL_CONTROLLER_BUTTON_A 0)
(define-public SDL_CONTROLLER_BUTTON_B 1)
(define-public SDL_CONTROLLER_BUTTON_X 2)
(define-public SDL_CONTROLLER_BUTTON_Y 3)
(define-public SDL_CONTROLLER_BUTTON_BACK 4)
(define-public SDL_CONTROLLER_BUTTON_GUIDE 5)
(define-public SDL_CONTROLLER_BUTTON_START 6)
(define-public SDL_CONTROLLER_BUTTON_LEFTSTICK 7)
(define-public SDL_CONTROLLER_BUTTON_RIGHTSTICK 8)
(define-public SDL_CONTROLLER_BUTTON_LEFTSHOULDER 9)
(define-public SDL_CONTROLLER_BUTTON_RIGHTSHOULDER 10)
(define-public SDL_CONTROLLER_BUTTON_DPAD_UP 11)
(define-public SDL_CONTROLLER_BUTTON_DPAD_DOWN 12)
(define-public SDL_CONTROLLER_BUTTON_DPAD_LEFT 13)
(define-public SDL_CONTROLLER_BUTTON_DPAD_RIGHT 14)
(define-public SDL_CONTROLLER_BUTTON_MAX 15)

(define-foreign sdl-game-controller-add-mappings-from-rw
  int "SDL_GameControllerAddMappingsFromRW" (list '* int))

(define-foreign sdl-game-controller-add-mapping
  int "SDL_GameControllerAddMapping" '(*))

(define-foreign sdl-game-controller-open
  '* "SDL_GameControllerOpen" (list int))

(define-foreign sdl-game-controller-close
  void "SDL_GameControllerClose" '(*))

(define-foreign sdl-game-controller-event-state
  int "SDL_GameControllerEventState" (list int))

(define-foreign sdl-game-controller-from-instance-id
  '* "SDL_GameControllerFromInstanceID" (list int32))

(define-foreign sdl-game-controller-get-attached
  sdl-bool "SDL_GameControllerGetAttached" '(*))

(define-foreign sdl-game-controller-get-axis
  int16 "SDL_GameControllerGetAxis" (list '* int))

(define-foreign sdl-game-controller-get-axis-from-string
  int "SDL_GameControllerGetAxisFromString" '(*))

(define-foreign sdl-game-controller-get-string-for-axis
  '* "SDL_GameControllerGetStringForAxis" (list int))

(define-foreign sdl-game-controller-get-button
  uint8 "SDL_GameControllerGetButton" (list '* int))

(define-foreign sdl-game-controller-get-button-from-string
  int "SDL_GameControllerGetButtonFromString" '(*))

(define-foreign sdl-game-controller-get-string-for-button
  '* "SDL_GameControllerGetStringForButton" (list int))

(define-foreign sdl-game-controller-get-joystick
  '* "SDL_GameControllerGetJoystick" '(*))

(define-foreign sdl-game-controller-mapping
  '* "SDL_GameControllerMapping" '(*))

(define-foreign sdl-game-controller-mapping-for-guid
  '* "SDL_GameControllerMappingForGUID" '(*))

(define-foreign sdl-game-controller-name
  '* "SDL_GameControllerName" '(*))

(define-foreign sdl-game-controller-name-for-index
  '* "SDL_GameControllerNameForIndex" (list int))

(define-foreign sdl-game-controller-update
  void "SDL_GameControllerUpdate" '())

(define-foreign sdl-is-game-controller
  sdl-bool "SDL_IsGameController" (list int))


;;;
;;; Clipboard
;;;

(define-foreign sdl-get-clipboard-text
  '* "SDL_GetClipboardText" '())

(define-foreign sdl-set-clipboard-text
  int "SDL_SetClipboardText" '(*))

(define-foreign sdl-has-clipboard-text
  int "SDL_HasClipboardText" '())
