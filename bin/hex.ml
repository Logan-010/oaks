let hex_digit value =
  if value < 10 then Char.chr (Char.code '0' + value)
  else Char.chr (Char.code 'a' + value - 10)

let hex_of_bytes bytes =
  String.init
    (Bytes.length bytes * 2)
    (fun index ->
      let byte = Char.code (Bytes.get bytes (index / 2)) in
      if index mod 2 = 0 then hex_digit (byte lsr 4)
      else hex_digit (byte land 0xf))

let byte_of_hex_digit = function
  | '0' .. '9' as digit -> Char.code digit - Char.code '0'
  | 'a' .. 'f' as digit -> Char.code digit - Char.code 'a' + 10
  | 'A' .. 'F' as digit -> Char.code digit - Char.code 'A' + 10
  | _ -> invalid_arg "Invalid hexadecimal secret"

let bytes_of_hex value =
  if String.length value mod 2 <> 0 then
    invalid_arg "Invalid hexadecimal secret"
  else
    Bytes.init
      (String.length value / 2)
      (fun index ->
        let high = byte_of_hex_digit value.[index * 2] in
        let low = byte_of_hex_digit value.[(index * 2) + 1] in
        Char.chr ((high lsl 4) lor low))
