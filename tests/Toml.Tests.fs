module Tests.Toml

open System
open System.Globalization
open Xunit
open FsUnit.Xunit
open Flxble.Toml
open Flxble.Toml.LensLike

[<Literal>]
let example = """
# This is a TOML document. Boom.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
organization = "GitHub"
bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"
  country = "中国" # This should be parsed as UTF-8

[clients]
data = [ ["gamma", "delta"], [1, 2] ] # just an update to make sure parsers support it

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]

# Products

  [[products]]
  name = "Hammer"
  sku = 738594937

  [[products]]
  name = "Nail"
  sku = 284758393
  color = "gray"
"""

[<Literal>]
let fruit = """
[[fruit.blah]]
  name = "apple"

  [fruit.blah.physical]
    color = "red"
    shape = "round"

[[fruit.blah]]
  name = "banana"

  [fruit.blah.physical]
    color = "yellow"
    shape = "bent"
"""

[<Literal>]
let hardExample = """
# Test file for TOML
# Only this one tries to emulate a TOML file written by a user of the kind of parser writers probably hate
# This part you'll really hate

[the]
test_string = "You'll hate me after this - #"          # " Annoying, isn't it?

    [the.hard]
    test_array = [ "] ", " # "]      # ] There you go, parse this!
    test_array2 = [ "Test #11 ]proved that", "Experiment #9 was a success" ]
    # You didn't think it'd as easy as chucking out the last #, did you?
    another_test_string = " Same thing, but with a string #"
    harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
    # Things will get harder

        [the.hard."bit#"]
        "what?" = "You don't think some user won't do that?"
        multi_line_array = [
            "]",
            # ] Oh yes I did
            ]
"""

[<Literal>]
let broken = """
# Each of the following keygroups/key value pairs should produce an error. Uncomment to them to test
[error]   if you didn't catch this, your parser is broken
string = "Anything other than tabs, spaces and newline after a keygroup or key value pair has ended should produce an error unless it is a comment"   like this
array = [
         "This might most likely happen in multiline arrays",
         Like here,
         "or here,
         and here"
         ]     End of array comment, forgot the #
number = 3.14  pi <--again forgot the #         
"""

[<Literal>]
let hardExampleUnicode = """
# Tèƨƭ ƒïℓè ƒôř TÓM£

# Óñℓ¥ ƭλïƨ ôñè ƭřïèƨ ƭô è₥úℓáƭè á TÓM£ ƒïℓè ωřïƭƭèñ β¥ á úƨèř ôƒ ƭλè ƙïñδ ôƒ ƥářƨèř ωřïƭèřƨ ƥřôβáβℓ¥ λáƭè
# Tλïƨ ƥářƭ ¥ôú'ℓℓ řèáℓℓ¥ λáƭè

[the]
test_string = "Ýôú'ℓℓ λáƭè ₥è áƒƭèř ƭλïƨ - #"          # " Âññô¥ïñϱ, ïƨñ'ƭ ïƭ?

    [the.hard]
    test_array = [ "] ", " # "]      # ] Tλèřè ¥ôú ϱô, ƥářƨè ƭλïƨ!
    test_array2 = [ "Tèƨƭ #11 ]ƥřôƲèδ ƭλáƭ", "Éжƥèřï₥èñƭ #9 ωáƨ á ƨúççèƨƨ" ]
    # Ýôú δïδñ'ƭ ƭλïñƙ ïƭ'δ áƨ èáƨ¥ áƨ çλúçƙïñϱ ôúƭ ƭλè ℓáƨƭ #, δïδ ¥ôú?
    another_test_string = "§á₥è ƭλïñϱ, βúƭ ωïƭλ á ƨƭřïñϱ #"
    harder_test_string = " Âñδ ωλèñ \"'ƨ ářè ïñ ƭλè ƨƭřïñϱ, áℓôñϱ ωïƭλ # \""   # "áñδ çô₥₥èñƭƨ ářè ƭλèřè ƭôô"
    # Tλïñϱƨ ωïℓℓ ϱèƭ λářδèř

        [the.hard."βïƭ#"]
        "ωλáƭ?" = "Ýôú δôñ'ƭ ƭλïñƙ ƨô₥è úƨèř ωôñ'ƭ δô ƭλáƭ?"
        multi_line_array = [
            "]",
            # ] Óλ ¥èƨ Ì δïδ
            ]
"""