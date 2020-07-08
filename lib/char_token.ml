type t = int

let of_char ch = Char.code ch

let space = 0x20

let cr = (* \r *) 0x0D

let lf = (* \n *) 0x0A

let htab = 0x09

let null = 0x00

let lparen = 0x28

let rparen = 0x29

let eof = -1

let less_than = 0x3C

let greater_than = 0x3E

let at = 0x40

let comma = 0x2C

let colon = 0x3A

let semicolon = 0x3B

let forward_slash = (* / *) 0x2F

let back_slash = (* \ *) 0x5C

let double_quote = 0x22

let single_quote = 0x27

let lbracket = 0x5B

let rbracket = 0x5D

let question = 0x3F

let equal = 0x3D

let bang = 0x21

let hash = 0x23

let dollar = 0x24

let ampersand = 0x26

let minus = 0x2D

let caret = 0x5E

let underscore = 0x5F

let dot = 0x2E

let plus = 0x2B

let is_alpha ch = (ch >= 0x41 && ch <= 0x5A) || (ch >= 0x61 && ch <= 0x7A)

let is_digit ch = ch >= 0x30 && ch <= 0x39

let is_vchar ch = ch >= 0x21 && ch <= 0x7E

let is_whitespace ch = ch == space || ch == htab

let is_control ch = (ch >= 0x00 && ch <= 0x1F) || ch == 0x7F

let is_ascii ch = ch >= 0x00 && ch <= 0x7F

let is_ctext ch =
  (ch >= 0x21 && ch <= 0x27)
  || (ch >= 0x2A && ch <= 0x5B)
  || (ch >= 0x5D && ch <= 0x7E)

let is_qtext ch =
  ch == bang || (ch >= 0x23 && ch <= 0x5b) || (ch >= 0x5D && ch <= 0x7E)

let pp fmt ch =
  if is_vchar ch then Format.fprintf fmt "%c" (Char.unsafe_chr ch)
  else if ch == eof then Format.fprintf fmt "EOF"
  else Format.fprintf fmt "0x%03x" ch
