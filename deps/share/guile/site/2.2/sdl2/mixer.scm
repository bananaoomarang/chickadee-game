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
;; Font rendering.
;;
;;; Code:

(define-module (sdl2 mixer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module ((sdl2 bindings mixer) #:prefix ffi:)
  #:export (%default-frequency
            %default-format
            %default-chunk-size

            mixer-init
            mixer-quit
            open-audio
            close-audio

            chunk?
            load-chunk
            delete-chunk!
            set-chunk-volume!
            play-chunk!

            set-channel-volume!
            pause-channel!
            resume-channel!
            stop-channel!
            channel-playing?
            playing-channels-count
            channel-paused?
            paused-channels-count

            music?
            load-music
            delete-music!
            play-music!
            set-music-volume!
            music-volume
            pause-music!
            resume-music!
            rewind-music!
            stop-music!
            music-playing?
            music-paused?))

(define (audio-format->symbol n)
  (cond
   ((= n ffi:AUDIO_U8)     'u8)
   ((= n ffi:AUDIO_S8)     's8)
   ((= n ffi:AUDIO_U16LSB) 'u16lsb)
   ((= n ffi:AUDIO_S16LSB) 's16lsb)
   ((= n ffi:AUDIO_U16MSB) 'u16msb)
   ((= n ffi:AUDIO_S16MSB) 's16msb)
   ((= n ffi:AUDIO_U16)    'u16)
   ((= n ffi:AUDIO_S16)    's16)
   ((= n ffi:AUDIO_S32LSB) 's32lsb)
   ((= n ffi:AUDIO_S32MSB) 's32msb)
   ((= n ffi:AUDIO_S32)    's32)
   ((= n ffi:AUDIO_F32LSB) 'f32lsb)
   ((= n ffi:AUDIO_F32MSB) 'f32msb)
   ((= n ffi:AUDIO_F32)    'f32)))

(define symbol->audio-format
  (match-lambda
    ('u8     ffi:AUDIO_U8)
    ('s8     ffi:AUDIO_S8)
    ('u16lsb ffi:AUDIO_U16LSB)
    ('s16lsb ffi:AUDIO_S16LSB)
    ('u16msb ffi:AUDIO_U16MSB)
    ('s16msb ffi:AUDIO_S16MSB)
    ('u16    ffi:AUDIO_U16)
    ('s16    ffi:AUDIO_S16)
    ('s32lsb ffi:AUDIO_S32LSB)
    ('s32msb ffi:AUDIO_S32MSB)
    ('s32    ffi:AUDIO_S32)
    ('f32lsb ffi:AUDIO_F32LSB)
    ('f32msb ffi:AUDIO_F32MSB)
    ('f32    ffi:AUDIO_F32)))

(define %default-frequency ffi:MIX_DEFAULT_FREQUENCY)
(define %default-format (audio-format->symbol ffi:MIX_DEFAULT_FORMAT))
(define %default-chunk-size 4096)

(define* (mixer-init #:optional
                     (formats '(flac mod modplug mp3 ogg fluidsynth)))
  "Initialize mixer library with support for FORMATS, a list of
symbols representing audio file formats.  Possible formats are:

- flac
- mod
- modplug
- mp3
- ogg
- fluidsynth"
  (define symbol->init-flag
    (match-lambda
      ('flac       ffi:MIX_INIT_FLAC)
      ('mod        ffi:MIX_INIT_MOD)
      ('modplug    ffi:MIX_INIT_MODPLUG)
      ('mp3        ffi:MIX_INIT_MP3)
      ('ogg        ffi:MIX_INIT_OGG)
      ('fluidsynth ffi:MIX_INIT_FLUIDSYNTH)))

  (let* ((mask (apply logior (map symbol->init-flag formats)))
         (result (ffi:mix-init mask)))
    (unless (= mask result)
      (sdl-error "mixer-init" "failed to initialize mixer library"))))

(define (mixer-quit)
  "Shutdown mixer library."
  (ffi:mix-quit))

(define* (open-audio #:key (frequency %default-frequency)
                     (format %default-format)
                     (stereo? #t)
                     (chunk-size %default-chunk-size))
  "Initialize the mixer API.  FREQUENCY specificies the sample rate in
hertz.  When STEREO? is #t, two output channels are used, otherwise
mono output is used instead.  CHUNK-SIZE specifies the number of bytes
used per output sample.  FORMAT is a symbol that specifies the output
sample format.  Possible values are:

- u8
- s8
- u16lsb
- s16lsb
- u16msb
- s16msb
- u16
- s16
- s32lsb
- s32msb
- s32
- f32lsb
- f32msb
- f32"
  (unless (zero? (ffi:mix-open-audio frequency
                                     (symbol->audio-format format)
                                     (if stereo? 2 1)
                                     chunk-size))
    (sdl-error "open-audio" "failed to open audio")))

(define (close-audio)
  "Shut down the mixer API."
  (ffi:mix-close-audio))


;;;
;;; Chunks
;;;

(define-wrapped-pointer-type <chunk>
  chunk?
  wrap-chunk unwrap-chunk
  (lambda (chunk port)
    (format port "#<chunk ~x>"
            (pointer-address (unwrap-chunk chunk)))))

(define (load-chunk file)
  "Load the audio data in FILE and return an audio chunk."
  (let ((ptr (ffi:mix-load-wav-rw (ffi:sdl-rw-from-file (string->pointer file)
                                                        (string->pointer "rb"))
                                  1)))
    (if (null-pointer? ptr)
        (sdl-error "load-audio" "failed to load audio")
        (wrap-chunk ptr))))

(define (delete-chunk! chunk)
  "Free the memory used for CHUNK."
  (ffi:mix-free-chunk (unwrap-chunk chunk)))

(define (set-chunk-volume! chunk volume)
  "Set the loudness of CHUNK to VOLUME, an integer in the range [0,
128].  Return the previous chunk volume setting."
  (ffi:mix-volume-chunk (unwrap-chunk chunk) volume))

(define* (play-chunk! chunk #:key (loops 0) channel)
  "Play CHUNK on CHANNEL, an integer channel identifier or #f to use
the first unreserved audio channel.  CHUNK will play LOOPS + 1 times.
Return the channel identifier that CHUNK is played on."
  (let ((ret (ffi:mix-play-channel-timed (if channel channel -1)
                                         (unwrap-chunk chunk)
                                         loops -1)))
    (if (= -1 ret)
        (sdl-error "play-chunk!" "failed to play audio")
        ret)))


;;;
;;; Channels
;;;

(define (set-channel-volume! channel volume)
  "Set the loudness of CHANNEL, an integer channel identifier or #f
for all channels, to VOLUME, an integer in the range 0-128.  Return
the previous volume of CHANNEL, or the average of all channels if
CHANNEL is #f."
  (ffi:mix-volume (if channel channel -1) volume))

(define (pause-channel! channel)
  "Pause playback on CHANNEL, an integer channel identifier, or #f to
pause all channels."
  (ffi:mix-pause (if channel channel -1)))

(define (resume-channel! channel)
  "Resume playback on CHANNEL, an integer channel identifier, or #f to
pause all channels."
  (ffi:mix-resume (if channel channel -1)))

(define (stop-channel! channel)
  "Halt playback on CHANNEL, an integer channel identifier, or #f to
pause all channels."
  (ffi:mix-halt-channel (if channel channel -1)))

(define (channel-playing? channel)
  "Return #t if CHANNEL is playing."
  (= 1 (ffi:mix-playing channel)))

(define (playing-channels-count)
  "Return the number of channels currently playing."
  (ffi:mix-playing -1))

(define (channel-paused? channel)
  "Return #t if CHANNEL is paused."
  (= 1 (ffi:mix-paused channel)))

(define (paused-channels-count)
  "Return the number of channels that are paused."
  (ffi:mix-paused -1))


;;;
;;; Music
;;;

(define-wrapped-pointer-type <music>
  music?
  wrap-music unwrap-music
  (lambda (music port)
    (format port "#<music ~x>"
            (pointer-address (unwrap-music music)))))

(define (load-music file)
  "Load music from FILE."
  (let ((ptr (ffi:mix-load-mus (string->pointer file))))
    (if (null-pointer? ptr)
        (sdl-error "load-music" "failed to load music")
        (wrap-music ptr))))

(define (delete-music! music)
  "Delete the memory used for MUSIC."
  (ffi:mix-free-music (unwrap-music music)))

(define* (play-music! music #:optional (loops 1))
  "Play MUSIC, repeated LOOPS times.  LOOPS may be #f, in which case
the music loops indefinitely."
  (unless (zero? (ffi:mix-play-music (unwrap-music music) (if loops loops -1)))
    (sdl-error "play-music!" "failed to play music")))

(define (set-music-volume! volume)
  "Set music loudness to VOLUME, an integer in the range 0-128.
Return the previous volume."
  (if (>= volume 0)
      (ffi:mix-volume-music volume)
      (throw 'sdl-error "set-music-volume!"
             "volume must be a non-negative integer")))

(define (music-volume)
  "Return the music volume."
  (ffi:mix-volume-music -1))

(define (pause-music!)
  "Pause the music."
  (ffi:mix-pause-music))

(define (resume-music!)
  "Resume music playback."
  (ffi:mix-resume-music))

(define (rewind-music!)
  "Start music playback from the beginning.  Rewinding is only
supported for MOD, OGG, MP3, and native MIDI music."
  (ffi:mix-rewind-music))

(define (stop-music!)
  "Halt music playback."
  (ffi:mix-halt-music))

(define (music-playing?)
  "Return #t if music is currently playing."
  (= 1 (ffi:mix-playing-music)))

(define (music-paused?)
  "Return #t if music is currently paused."
  (= 1 (ffi:mix-paused-music)))
