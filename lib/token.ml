type t =
  | Semi
  | Eof
  | Crlf
  | Boundary_value of string
  | Dash_bouudary_value of string
  | Close_boudary_value of string
