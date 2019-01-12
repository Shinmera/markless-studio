#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)

(defparameter *key-table*
  (alexandria:alist-hash-table
   '((#x01000000 . :escape)
     (#x01000001 . :tab)
     (#x01000002 . :backtab)
     (#x01000003 . :backspace)
     (#x01000004 . :return)
     (#x01000005 . :enter)
     (#x01000006 . :insert)
     (#x01000007 . :delete)
     (#x01000008 . :pause)
     (#x01000009 . :print)
     (#x0100000a . :sysreq)
     (#x0100000b . :clear)
     (#x01000010 . :home)
     (#x01000011 . :end)
     (#x01000012 . :left)
     (#x01000013 . :up)
     (#x01000014 . :right)
     (#x01000015 . :down)
     (#x01000016 . :page-up)
     (#x01000017 . :page-down)
     (#x01000020 . :shift)
     (#x01000021 . :control)
     (#x01000022 . :meta)
     (#x01000023 . :alt)
     (#x01001103 . :altgr)
     (#x01000024 . :caps-lock)
     (#x01000025 . :num-lock)
     (#x01000026 . :scroll-lock)
     (#x01000030 . :f1)
     (#x01000031 . :f2)
     (#x01000032 . :f3)
     (#x01000033 . :f4)
     (#x01000034 . :f5)
     (#x01000035 . :f6)
     (#x01000036 . :f7)
     (#x01000037 . :f8)
     (#x01000038 . :f9)
     (#x01000039 . :f10)
     (#x0100003a . :f11)
     (#x0100003b . :f12)
     (#x0100003c . :f13)
     (#x0100003d . :f14)
     (#x0100003e . :f15)
     (#x0100003f . :f16)
     (#x01000040 . :f17)
     (#x01000041 . :f18)
     (#x01000042 . :f19)
     (#x01000043 . :f20)
     (#x01000044 . :f21)
     (#x01000045 . :f22)
     (#x01000046 . :f23)
     (#x01000047 . :f24)
     (#x01000048 . :f25)
     (#x01000049 . :f26)
     (#x0100004a . :f27)
     (#x0100004b . :f28)
     (#x0100004c . :f29)
     (#x0100004d . :f30)
     (#x0100004e . :f31)
     (#x0100004f . :f32)
     (#x01000050 . :f33)
     (#x01000051 . :f34)
     (#x01000052 . :f35)
     (#x01000053 . :super-l)
     (#x01000054 . :super-r)
     (#x01000055 . :menu)
     (#x01000056 . :hyper-l)
     (#x01000057 . :hyper-r)
     (#x01000058 . :help)
     (#x01000059 . :direction-l)
     (#x01000060 . :direction-r)
     (#x00000020 . #\ )
     (#x00000021 . #\!)
     (#x00000022 . #\")
     (#x00000023 . :numbersign)
     (#x00000024 . #\$)
     (#x00000025 . #\%)
     (#x00000026 . #\&)
     (#x00000027 . #\')
     (#x00000028 . #\()
     (#x00000029 . #\))
     (#x0000002a . #\*)
     (#x0000002b . #\+)
     (#x0000002c . #\,)
     (#x0000002d . #\-)
     (#x0000002e . #\.)
     (#x0000002f . #\/)
     (#x00000030 . #\0)
     (#x00000031 . #\1)
     (#x00000032 . #\2)
     (#x00000033 . #\3)
     (#x00000034 . #\4)
     (#x00000035 . #\5)
     (#x00000036 . #\6)
     (#x00000037 . #\7)
     (#x00000038 . #\8)
     (#x00000039 . #\9)
     (#x0000003a . #\:)
     (#x0000003b . #\;)
     (#x0000003c . #\<)
     (#x0000003d . #\=)
     (#x0000003e . #\>)
     (#x0000003f . #\?)
     (#x00000040 . #\@)
     (#x00000041 . #\a)
     (#x00000042 . #\b)
     (#x00000043 . #\c)
     (#x00000044 . #\d)
     (#x00000045 . #\e)
     (#x00000046 . #\f)
     (#x00000047 . #\g)
     (#x00000048 . #\h)
     (#x00000049 . #\i)
     (#x0000004a . #\j)
     (#x0000004b . #\k)
     (#x0000004c . #\l)
     (#x0000004d . #\m)
     (#x0000004e . #\n)
     (#x0000004f . #\o)
     (#x00000050 . #\p)
     (#x00000051 . #\q)
     (#x00000052 . #\r)
     (#x00000053 . #\s)
     (#x00000054 . #\t)
     (#x00000055 . #\u)
     (#x00000056 . #\v)
     (#x00000057 . #\w)
     (#x00000058 . #\x)
     (#x00000059 . #\y)
     (#x0000005a . #\z)
     (#x0000005b . #\[)
     (#x0000005c . #\\)
     (#x0000005d . #\])
     (#x0000005e . :ascii-circum)
     (#x0000005f . #\_)
     (#x00000060 . #\`)
     (#x0000007b . #\{)
     (#x0000007c . #\|)
     (#x0000007d . #\})
     (#x0000007e . #\~)
     (#x000000a0 . :nobreakspace)
     (#x000000a1 . #\¡)
     (#x000000a2 . :cent)
     (#x000000a3 . #\£)
     (#x000000a4 . :currency)
     (#x000000a5 . #\¥)
     (#x000000a6 . :broken-bar)
     (#x000000a7 . #\§)
     (#x000000a8 . :diaeresis)
     (#x000000a9 . :copyright)
     (#x000000aa . :ordfeminine)
     (#x000000ab . :guillemot-left)
     (#x000000ac . :notsign)
     (#x000000ad . :hyphen)
     (#x000000ae . #\®)
     (#x000000af . :macron)
     (#x000000b0 . :degree)
     (#x000000b1 . :plus-minus)
     (#x000000b2 . :two-superior)
     (#x000000b3 . :three-superior)
     (#x000000b4 . :acute)
     (#x000000b5 . :mu)
     (#x000000b6 . :paragraph)
     (#x000000b7 . :period-centered)
     (#x000000b8 . #\ç)
     (#x000000b9 . :one-superior)
     (#x000000ba . :masculine)
     (#x000000bb . :guillemot-right)
     (#x000000bc . :one-quarter)
     (#x000000bd . :one-half)
     (#x000000be . :three-quarters)
     (#x000000bf . #\¿)
     (#x000000c0 . :a-grave)
     (#x000000c1 . :a-acute)
     (#x000000c2 . :a-circumflex)
     (#x000000c3 . :a-tilde)
     (#x000000c4 . :a-diaeresis)
     (#x000000c5 . :a-ring)
     (#x000000c6 . :ae)
     (#x000000c7 . :c-cedilla)
     (#x000000c8 . :e-grave)
     (#x000000c9 . :e-acute)
     (#x000000ca . :e-circumflex)
     (#x000000cb . :e-diaeresis)
     (#x000000cc . :i-grave)
     (#x000000cd . :i-acute)
     (#x000000ce . :i-circumflex)
     (#x000000cf . :i-diaeresis)
     (#x000000d0 . :eth)
     (#x000000d1 . :n-tilde)
     (#x000000d2 . :o-grave)
     (#x000000d3 . :o-acute)
     (#x000000d4 . :o-circumflex)
     (#x000000d5 . :o-tilde)
     (#x000000d6 . :o-diaeresis)
     (#x000000d7 . :multiply)
     (#x000000d8 . :o-oblique)
     (#x000000d9 . :u-grave)
     (#x000000da . :u-acute)
     (#x000000db . :u-circumflex)
     (#x000000dc . :u-diaeresis)
     (#x000000dd . :y-acute)
     (#x000000de . :thorn)
     (#x000000df . :s-sharp)
     (#x000000f7 . :division)
     (#x000000ff . :y-diaeresis)
     (#x01001120 . :multi-key)
     (#x01001137 . :codeinput)
     (#x0100113c . :single-candidate)
     (#x0100113d . :multiple-candidate)
     (#x0100113e . :previous-candidate)
     (#x0100117e . :mode-switch)
     (#x01001121 . :kanji)
     (#x01001122 . :muhenkan)
     (#x01001123 . :henkan)
     (#x01001124 . :romaji)
     (#x01001125 . :hiragana)
     (#x01001126 . :katakana)
     (#x01001127 . :hiragana-katakana)
     (#x01001128 . :zenkaku)
     (#x01001129 . :hankaku)
     (#x0100112a . :zenkaku-hankaku)
     (#x0100112b . :touroku)
     (#x0100112c . :massyo)
     (#x0100112d . :kana-lock)
     (#x0100112e . :kana-shift)
     (#x0100112f . :eisu-shift)
     (#x01001130 . :eisu-toggle)
     (#x01001131 . :hangul)
     (#x01001132 . :hangul-start)
     (#x01001133 . :hangul-end)
     (#x01001134 . :hangul-hanja)
     (#x01001135 . :hangul-jamo)
     (#x01001136 . :hangul-romaja)
     (#x01001138 . :hangul-jeonja)
     (#x01001139 . :hangul-banja)
     (#x0100113a . :hangul-prehanja)
     (#x0100113b . :hangul-posthanja)
     (#x0100113f . :hangul-special)
     (#x01001250 . :dead-grave)
     (#x01001251 . :dead-acute)
     (#x01001252 . :dead-circumflex)
     (#x01001253 . :dead-tilde)
     (#x01001254 . :dead-macron)
     (#x01001255 . :dead-breve)
     (#x01001256 . :dead-abovedot)
     (#x01001257 . :dead-diaeresis)
     (#x01001258 . :dead-abovering)
     (#x01001259 . :dead-doubleacute)
     (#x0100125a . :dead-caron)
     (#x0100125b . :dead-cedilla)
     (#x0100125c . :dead-ogonek)
     (#x0100125d . :dead-iota)
     (#x0100125e . :dead-voiced-sound)
     (#x0100125f . :dead-semivoiced-sound)
     (#x01001260 . :dead-belowdot)
     (#x01001261 . :dead-hook)
     (#x01001262 . :dead-horn))
   :test 'eql))

(defun qt-key->key (key modifiers)
  (let ((key (gethash key *key-table* :?)))
    (if (and (characterp key) (< 0 (logand (q+:qt.shift-modifier) modifiers)))
        (char-upcase key)
        key)))
