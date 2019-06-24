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
;; Low-level FFI bindings for SDL2_mixer.
;;
;;; Code:

(define-module (sdl2 bindings mixer)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (sdl2 config)
  #:use-module (sdl2 bindings))

(define sdl-mixer-func
  (let ((lib (dynamic-link %libsdl2-mixer)))
    (lambda (return-type function-name arg-types)
      "Return a procedure for the foreign function FUNCTION-NAME in
the SDL2_mixer shared library.  That function must return a value of
RETURN-TYPE and accept arguments of ARG-TYPES."
      (pointer->procedure return-type
                          (dynamic-func function-name lib)
                          arg-types))))

(define-syntax-rule (define-foreign name return-type func-name arg-types)
  (define-public name
    (sdl-mixer-func return-type func-name arg-types)))

(define-public MIX_INIT_FLAC       #x00000001)
(define-public MIX_INIT_MOD        #x00000002)
(define-public MIX_INIT_MODPLUG    #x00000004)
(define-public MIX_INIT_MP3        #x00000008)
(define-public MIX_INIT_OGG        #x00000010)
(define-public MIX_INIT_FLUIDSYNTH #x00000020)

(define-public MIX_CHANNELS 8)
(define-public MIX_DEFAULT_FREQUENCY 22050)
(define-public MIX_MAX_VOLUME 128)
(define-public MIX_DEFAULT_FORMAT
  (if (eq? (native-endianness) 'little)
      AUDIO_S16LSB
      AUDIO_S16MSB))

(define-foreign mix-init
  int "Mix_Init" (list int))

(define-foreign mix-quit
  void "Mix_Quit" '())

(define-foreign mix-open-audio
  int "Mix_OpenAudio" (list int uint16 int int))

(define-foreign mix-close-audio
  void "Mix_CloseAudio" '())

(define-foreign mix-load-wav-rw
  '* "Mix_LoadWAV_RW" (list '* int))

(define-foreign mix-free-chunk
  void "Mix_FreeChunk" '(*))

(define-foreign mix-volume-chunk
  int "Mix_VolumeChunk" (list '* int))

(define-foreign mix-volume
  int "Mix_Volume" (list int int))

(define-foreign mix-play-channel-timed
  int "Mix_PlayChannelTimed" (list int '* int int))

(define-foreign mix-pause
  void "Mix_Pause" (list int))

(define-foreign mix-resume
  void "Mix_Resume" (list int))

(define-foreign mix-halt-channel
  void "Mix_HaltChannel" (list int))

(define-foreign mix-playing
  int "Mix_Playing" (list int))

(define-foreign mix-paused
  int "Mix_Paused" (list int))

(define-foreign mix-load-mus
  '* "Mix_LoadMUS" '(*))

(define-foreign mix-free-music
  void "Mix_FreeMusic" '(*))

(define-foreign mix-play-music
  int "Mix_PlayMusic" (list '* int))

(define-foreign mix-volume-music
  int "Mix_VolumeMusic" (list int))

(define-foreign mix-pause-music
  void "Mix_PauseMusic" '())

(define-foreign mix-resume-music
  void "Mix_ResumeMusic" '())

(define-foreign mix-rewind-music
  void "Mix_RewindMusic" '())

(define-foreign mix-halt-music
  void "Mix_HaltMusic" '())

(define-foreign mix-playing-music
  int "Mix_PlayingMusic" '())

(define-foreign mix-paused-music
  int "Mix_PausedMusic" '())
