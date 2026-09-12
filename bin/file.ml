let bytes_of_int x =
  let b = Bytes.create 4 in
  Bytes.set b 0 (Char.chr (Int32.to_int (Int32.logand x 0xffl)));
  Bytes.set b 1
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical x 8) 0xffl)));
  Bytes.set b 2
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical x 16) 0xffl)));
  Bytes.set b 3
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical x 24) 0xffl)));
  b

let int_of_bytes b =
  if Bytes.length b < 4 then invalid_arg "int32_of_le32: need 4 bytes";
  let byte i = Int32.of_int (Char.code (Bytes.get b i)) in
  Int32.logor (byte 0)
    (Int32.logor
       (Int32.shift_left (byte 1) 8)
       (Int32.logor
          (Int32.shift_left (byte 2) 16)
          (Int32.shift_left (byte 3) 24)))

let send_framed sock msg =
  Bytes.length msg |> Int32.of_int |> bytes_of_int |> Oaks.send sock;
  Oaks.send sock msg

let recv_framed sock =
  Oaks.recv sock 4 |> int_of_bytes |> Int32.to_int |> Oaks.recv sock

let send_crypt key sock ctr msg =
  let nonce = Mirage_crypto_rng_unix.getrandom 12 in
  let adata = Int32.of_int ctr |> bytes_of_int |> Bytes.to_string in
  let encrypted =
    Bytes.of_string
    @@ Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce ~adata
    @@ Bytes.to_string msg
  in

  Oaks.send sock @@ Bytes.of_string nonce;
  send_framed sock encrypted;

  ()

let recv_crypt key sock ctr =
  let nonce = Oaks.recv sock 12 |> Bytes.to_string in
  let adata = Int32.of_int ctr |> bytes_of_int |> Bytes.to_string in
  match
    recv_framed sock |> Bytes.to_string
    |> Mirage_crypto.Chacha20.authenticate_decrypt ~key ~nonce ~adata
  with
  | Some msg -> Bytes.of_string msg
  | None -> invalid_arg "Wrong key."

let rec mkdir path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else begin
    mkdir (Filename.dirname path);
    Unix.mkdir path 0o755
  end

type file_meta = { name : string; size : int64 }

let buffer_size = 2 * 1024 * 1024

let send file sock key =
  let input = In_channel.open_bin file in

  let size = In_channel.length input in
  let key = Mirage_crypto.Chacha20.of_secret @@ Bytes.to_string key in
  let name = Filename.basename file in

  send_crypt key sock 0 @@ Marshal.to_bytes { name; size } [];

  let buf = Bytes.create buffer_size in

  let rec loop ctr rem =
    match rem with
    | 0L -> ()
    | n ->
        let read = In_channel.input input buf 0 buffer_size in

        send_crypt key sock ctr @@ Bytes.sub buf 0 read;

        loop (ctr + 1) @@ Int64.sub n @@ Int64.of_int read
  in

  loop 1 size

let recv path sock key =
  let key = Mirage_crypto.Chacha20.of_secret @@ Bytes.to_string key in

  prerr_string "Getting metadata.\n";

  let meta = recv_crypt key sock 0 |> fun b -> Marshal.from_bytes b 0 in

  Printf.fprintf stderr "Downloading \"%s\"\n" meta.name;
  flush stderr;

  let output =
    (match path with
      | Some base ->
          mkdir base;
          Filename.concat base meta.name
      | None -> meta.name)
    |> open_out_gen [ Open_creat; Open_wronly; Open_trunc; Open_binary ] 0o644
  in

  let rec loop ctr rem =
    match rem with
    | 0L -> ()
    | n ->
        let data = recv_crypt key sock ctr in
        let read = Bytes.length data in

        Out_channel.output_bytes output data;

        loop (ctr + 1) @@ Int64.sub n @@ Int64.of_int read
  in

  loop 1 meta.size
