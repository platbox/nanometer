#!/usr/bin/env python3
# coding=utf-8
TABLE = [
    (u"а", b"a"),
    (u"б", b"b"),
    (u"в", b"v"),
    (u"г", b"g"),
    (u"д", b"d"),
    (u"е", b"e"),
    (u"ё", b"e"),
    (u"ж", b"zh"),
    (u"з", b"z"),
    (u"и", b"i"),
    (u"й", b"i"),
    (u"к", b"k"),
    (u"л", b"l"),
    (u"м", b"m"),
    (u"н", b"n"),
    (u"о", b"o"),
    (u"п", b"p"),
    (u"р", b"r"),
    (u"с", b"s"),
    (u"т", b"t"),
    (u"у", b"u"),
    (u"ф", b"f"),
    (u"х", b"kh"),
    (u"ц", b"ts"),
    (u"ч", b"ch"),
    (u"ш", b"sh"),
    (u"щ", b"shch"),
    (u"ъ", b"ie"),
    (u"ы", b"y"),
    (u"ь", b""),
    (u"э", b"e"),
    (u"ю", b"iu"),
    (u"я", b"ia"),
]

print("-spec romanize_letter(binary()) -> binary().")

for (russian_letter, ascii_bytes) in TABLE:
    print("romanize_letter(<<\"{}\"/utf8>>) -> <<\"{}\">>;".format(russian_letter, ascii_bytes.decode("ascii")))

print()

for (russian_letter, ascii_bytes) in TABLE:
    print("romanize_letter(<<\"{}\"/utf8>>) -> <<\"{}\">>;".format(russian_letter.upper(), ascii_bytes.upper().decode("ascii")))

print()
print("romanize_letter(_) -> <<\"_\">>.")
