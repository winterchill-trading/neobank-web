open! Core

module Ecc = struct
  type t =
    | L
    | M
    | Q
    | H
  [@@deriving sexp, compare, equal]

  let index = function
    | L -> 0
    | M -> 1
    | Q -> 2
    | H -> 3
  ;;

  (* Format information encodes L as 01, M as 00, Q as 11 and H as 10. *)
  let format_bits = function
    | L -> 1
    | M -> 0
    | Q -> 3
    | H -> 2
  ;;
end

(* Tables from ISO/IEC 18004 table 9, indexed by [Ecc.index] then version (index 0 is
   unused). *)
let ecc_codewords_per_block =
  [| [| -1
      ; 7
      ; 10
      ; 15
      ; 20
      ; 26
      ; 18
      ; 20
      ; 24
      ; 30
      ; 18
      ; 20
      ; 24
      ; 26
      ; 30
      ; 22
      ; 24
      ; 28
      ; 30
      ; 28
      ; 28
      ; 28
      ; 28
      ; 30
      ; 30
      ; 26
      ; 28
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
     |]
   ; [| -1
      ; 10
      ; 16
      ; 26
      ; 18
      ; 24
      ; 16
      ; 18
      ; 22
      ; 22
      ; 26
      ; 30
      ; 22
      ; 22
      ; 24
      ; 24
      ; 28
      ; 28
      ; 26
      ; 26
      ; 26
      ; 26
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
      ; 28
     |]
   ; [| -1
      ; 13
      ; 22
      ; 18
      ; 26
      ; 18
      ; 24
      ; 18
      ; 22
      ; 20
      ; 24
      ; 28
      ; 26
      ; 24
      ; 20
      ; 30
      ; 24
      ; 28
      ; 28
      ; 26
      ; 30
      ; 28
      ; 30
      ; 30
      ; 30
      ; 30
      ; 28
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
     |]
   ; [| -1
      ; 17
      ; 28
      ; 22
      ; 16
      ; 22
      ; 28
      ; 26
      ; 26
      ; 24
      ; 28
      ; 24
      ; 28
      ; 22
      ; 24
      ; 24
      ; 30
      ; 28
      ; 28
      ; 26
      ; 28
      ; 30
      ; 24
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
      ; 30
     |]
  |]
;;

let num_ecc_blocks =
  [| [| -1
      ; 1
      ; 1
      ; 1
      ; 1
      ; 1
      ; 2
      ; 2
      ; 2
      ; 2
      ; 4
      ; 4
      ; 4
      ; 4
      ; 4
      ; 6
      ; 6
      ; 6
      ; 6
      ; 7
      ; 8
      ; 8
      ; 9
      ; 9
      ; 10
      ; 12
      ; 12
      ; 12
      ; 13
      ; 14
      ; 15
      ; 16
      ; 17
      ; 18
      ; 19
      ; 19
      ; 20
      ; 21
      ; 22
      ; 24
      ; 25
     |]
   ; [| -1
      ; 1
      ; 1
      ; 1
      ; 2
      ; 2
      ; 4
      ; 4
      ; 4
      ; 5
      ; 5
      ; 5
      ; 8
      ; 9
      ; 9
      ; 10
      ; 10
      ; 11
      ; 13
      ; 14
      ; 16
      ; 17
      ; 17
      ; 18
      ; 20
      ; 21
      ; 23
      ; 25
      ; 26
      ; 28
      ; 29
      ; 31
      ; 33
      ; 35
      ; 37
      ; 38
      ; 40
      ; 43
      ; 45
      ; 47
      ; 49
     |]
   ; [| -1
      ; 1
      ; 1
      ; 2
      ; 2
      ; 4
      ; 4
      ; 6
      ; 6
      ; 8
      ; 8
      ; 8
      ; 10
      ; 12
      ; 16
      ; 12
      ; 17
      ; 16
      ; 18
      ; 21
      ; 20
      ; 23
      ; 23
      ; 25
      ; 27
      ; 29
      ; 34
      ; 34
      ; 35
      ; 38
      ; 40
      ; 43
      ; 45
      ; 48
      ; 51
      ; 53
      ; 56
      ; 59
      ; 62
      ; 65
      ; 68
     |]
   ; [| -1
      ; 1
      ; 1
      ; 2
      ; 4
      ; 4
      ; 4
      ; 5
      ; 6
      ; 8
      ; 8
      ; 11
      ; 11
      ; 16
      ; 16
      ; 18
      ; 16
      ; 19
      ; 21
      ; 25
      ; 25
      ; 25
      ; 34
      ; 30
      ; 32
      ; 35
      ; 37
      ; 40
      ; 42
      ; 45
      ; 48
      ; 51
      ; 54
      ; 57
      ; 60
      ; 63
      ; 66
      ; 70
      ; 74
      ; 77
      ; 81
     |]
  |]
;;

type t =
  { version : int
  ; mask : int
  ; size : int
  ; modules : bool array array (* [modules.(y).(x)] *)
  }

let version t = t.version
let mask t = t.mask
let size t = t.size
let get t ~x ~y = t.modules.(y).(x)

let num_raw_data_modules ver =
  let result = (((16 * ver) + 128) * ver) + 64 in
  let result =
    if ver >= 2
    then (
      let num_align = (ver / 7) + 2 in
      result - ((((25 * num_align) - 10) * num_align) - 55))
    else result
  in
  if ver >= 7 then result - 36 else result
;;

let num_data_codewords ver ecc =
  let e = Ecc.index ecc in
  (num_raw_data_modules ver / 8)
  - (ecc_codewords_per_block.(e).(ver) * num_ecc_blocks.(e).(ver))
;;

(* Byte mode: 4 mode bits, then the length in 8 bits up to version 9, 16 after. *)
let count_bits ver = if ver <= 9 then 8 else 16

(* {2 Reed-Solomon over GF(256) with the polynomial 0x11D} *)

let gf_mul x y =
  let z = ref 0 in
  for i = 7 downto 0 do
    z := (!z lsl 1) lxor ((!z lsr 7) * 0x11D);
    z := !z lxor ((y lsr i) land 1 * x)
  done;
  !z
;;

let rs_divisor degree =
  let result = Array.create ~len:degree 0 in
  result.(degree - 1) <- 1;
  let root = ref 1 in
  for _ = 0 to degree - 1 do
    for j = 0 to degree - 1 do
      result.(j) <- gf_mul result.(j) !root;
      if j + 1 < degree then result.(j) <- result.(j) lxor result.(j + 1)
    done;
    root := gf_mul !root 0x02
  done;
  result
;;

let rs_remainder data divisor =
  let degree = Array.length divisor in
  let result = Array.create ~len:degree 0 in
  Array.iter data ~f:(fun b ->
    let factor = b lxor result.(0) in
    Array.blit ~src:result ~src_pos:1 ~dst:result ~dst_pos:0 ~len:(degree - 1);
    result.(degree - 1) <- 0;
    Array.iteri divisor ~f:(fun i coef ->
      result.(i) <- result.(i) lxor gf_mul coef factor));
  result
;;

(* Splits the data codewords into blocks, appends each block's error correction and
   interleaves them. *)
let add_ecc_and_interleave data ver ecc =
  let e = Ecc.index ecc in
  let num_blocks = num_ecc_blocks.(e).(ver) in
  let block_ecc_len = ecc_codewords_per_block.(e).(ver) in
  let raw_codewords = num_raw_data_modules ver / 8 in
  let num_short_blocks = num_blocks - (raw_codewords % num_blocks) in
  let short_block_len = raw_codewords / num_blocks in
  let divisor = rs_divisor block_ecc_len in
  let k = ref 0 in
  let blocks =
    Array.init num_blocks ~f:(fun i ->
      let data_len =
        short_block_len - block_ecc_len + if i < num_short_blocks then 0 else 1
      in
      let dat = Array.sub data ~pos:!k ~len:data_len in
      k := !k + data_len;
      let ecc = rs_remainder dat divisor in
      (* Short blocks get a placeholder byte so every block has the same length; it is
         skipped when interleaving. *)
      if i < num_short_blocks
      then Array.concat [ dat; [| 0 |]; ecc ]
      else Array.append dat ecc)
  in
  let result = Queue.create () in
  for i = 0 to short_block_len do
    Array.iteri blocks ~f:(fun j block ->
      if i <> short_block_len - block_ecc_len || j >= num_short_blocks
      then Queue.enqueue result block.(i))
  done;
  Queue.to_array result
;;

(* {2 The symbol} *)

type grid =
  { size : int
  ; modules : bool array array
  ; is_function : bool array array
  }

let set_function g ~x ~y dark =
  g.modules.(y).(x) <- dark;
  g.is_function.(y).(x) <- true
;;

let bit v i = (v lsr i) land 1 <> 0

let alignment_positions ver =
  if ver = 1
  then [||]
  else (
    let num_align = (ver / 7) + 2 in
    let size = (ver * 4) + 17 in
    let step = ((ver * 8) + (num_align * 3) + 5) / ((num_align * 4) - 4) * 2 in
    let result = Array.create ~len:num_align 6 in
    let pos = ref (size - 7) in
    for i = num_align - 1 downto 1 do
      result.(i) <- !pos;
      pos := !pos - step
    done;
    result)
;;

let draw_format_bits g ecc mask =
  let data = (Ecc.format_bits ecc lsl 3) lor mask in
  let rem = ref data in
  for _ = 0 to 9 do
    rem := (!rem lsl 1) lxor ((!rem lsr 9) * 0x537)
  done;
  let bits = (data lsl 10) lor !rem lxor 0x5412 in
  let size = g.size in
  for i = 0 to 5 do
    set_function g ~x:8 ~y:i (bit bits i)
  done;
  set_function g ~x:8 ~y:7 (bit bits 6);
  set_function g ~x:8 ~y:8 (bit bits 7);
  set_function g ~x:7 ~y:8 (bit bits 8);
  for i = 9 to 14 do
    set_function g ~x:(14 - i) ~y:8 (bit bits i)
  done;
  for i = 0 to 7 do
    set_function g ~x:(size - 1 - i) ~y:8 (bit bits i)
  done;
  for i = 8 to 14 do
    set_function g ~x:8 ~y:(size - 15 + i) (bit bits i)
  done;
  (* The module that is always dark. *)
  set_function g ~x:8 ~y:(size - 8) true
;;

let draw_version g ver =
  if ver >= 7
  then (
    let rem = ref ver in
    for _ = 0 to 11 do
      rem := (!rem lsl 1) lxor ((!rem lsr 11) * 0x1F25)
    done;
    let bits = (ver lsl 12) lor !rem in
    for i = 0 to 17 do
      let a = g.size - 11 + (i % 3) in
      let b = i / 3 in
      set_function g ~x:a ~y:b (bit bits i);
      set_function g ~x:b ~y:a (bit bits i)
    done)
;;

let draw_function_patterns g ver ecc =
  let size = g.size in
  for i = 0 to size - 1 do
    set_function g ~x:6 ~y:i (i % 2 = 0);
    set_function g ~x:i ~y:6 (i % 2 = 0)
  done;
  let finder cx cy =
    for dy = -4 to 4 do
      for dx = -4 to 4 do
        let dist = Int.max (abs dx) (abs dy) in
        let x = cx + dx
        and y = cy + dy in
        if x >= 0 && x < size && y >= 0 && y < size
        then set_function g ~x ~y (dist <> 2 && dist <> 4)
      done
    done
  in
  finder 3 3;
  finder (size - 4) 3;
  finder 3 (size - 4);
  let positions = alignment_positions ver in
  let n = Array.length positions in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let corner = (i = 0 && j = 0) || (i = 0 && j = n - 1) || (i = n - 1 && j = 0) in
      if not corner
      then
        for dy = -2 to 2 do
          for dx = -2 to 2 do
            set_function
              g
              ~x:(positions.(i) + dx)
              ~y:(positions.(j) + dy)
              (Int.max (abs dx) (abs dy) <> 1)
          done
        done
    done
  done;
  (* Placeholder format bits mark their modules as function modules. *)
  draw_format_bits g ecc 0;
  draw_version g ver
;;

let draw_codewords g codewords =
  let size = g.size in
  let total_bits = Array.length codewords * 8 in
  let i = ref 0 in
  let right = ref (size - 1) in
  while !right >= 1 do
    if !right = 6 then right := 5;
    for vert = 0 to size - 1 do
      for j = 0 to 1 do
        let x = !right - j in
        let upward = (!right + 1) land 2 = 0 in
        let y = if upward then size - 1 - vert else vert in
        if (not g.is_function.(y).(x)) && !i < total_bits
        then (
          g.modules.(y).(x) <- bit codewords.(!i lsr 3) (7 - (!i land 7));
          incr i)
      done
    done;
    right := !right - 2
  done
;;

let apply_mask g mask =
  for y = 0 to g.size - 1 do
    for x = 0 to g.size - 1 do
      let invert =
        match mask with
        | 0 -> (x + y) % 2 = 0
        | 1 -> y % 2 = 0
        | 2 -> x % 3 = 0
        | 3 -> (x + y) % 3 = 0
        | 4 -> ((x / 3) + (y / 2)) % 2 = 0
        | 5 -> (x * y % 2) + (x * y % 3) = 0
        | 6 -> ((x * y % 2) + (x * y % 3)) % 2 = 0
        | _ -> (((x + y) % 2) + (x * y % 3)) % 2 = 0
      in
      if invert && not g.is_function.(y).(x)
      then g.modules.(y).(x) <- not g.modules.(y).(x)
    done
  done
;;

(* {2 Penalty score (ISO/IEC 18004 section 7.8.3)} *)

let penalty_n1 = 3
let penalty_n2 = 3
let penalty_n3 = 40
let penalty_n4 = 10

let penalty g =
  let size = g.size in
  let result = ref 0 in
  let add_history history run =
    (* The light border before the first run counts towards it. *)
    let run = if history.(0) = 0 then run + size else run in
    Array.blit ~src:history ~src_pos:0 ~dst:history ~dst_pos:1 ~len:6;
    history.(0) <- run
  in
  let count_patterns h =
    let n = h.(1) in
    let core = n > 0 && h.(2) = n && h.(3) = n * 3 && h.(4) = n && h.(5) = n in
    (if core && h.(0) >= n * 4 && h.(6) >= n then 1 else 0)
    + if core && h.(6) >= n * 4 && h.(0) >= n then 1 else 0
  in
  let line get =
    let run_color = ref false in
    let run = ref 0 in
    let history = Array.create ~len:7 0 in
    for i = 0 to size - 1 do
      if Bool.equal (get i) !run_color
      then (
        incr run;
        if !run = 5 then result := !result + penalty_n1 else if !run > 5 then incr result)
      else (
        add_history history !run;
        if not !run_color then result := !result + (count_patterns history * penalty_n3);
        run_color := get i;
        run := 1)
    done;
    (* Terminate with the light border after the last module. *)
    if !run_color
    then (
      add_history history !run;
      run := 0);
    add_history history (!run + size);
    result := !result + (count_patterns history * penalty_n3)
  in
  for y = 0 to size - 1 do
    line (fun x -> g.modules.(y).(x))
  done;
  for x = 0 to size - 1 do
    line (fun y -> g.modules.(y).(x))
  done;
  for y = 0 to size - 2 do
    for x = 0 to size - 2 do
      let c = g.modules.(y).(x) in
      if Bool.equal c g.modules.(y).(x + 1)
         && Bool.equal c g.modules.(y + 1).(x)
         && Bool.equal c g.modules.(y + 1).(x + 1)
      then result := !result + penalty_n2
    done
  done;
  let dark = Array.sum (module Int) g.modules ~f:(fun row -> Array.count row ~f:Fn.id) in
  let total = size * size in
  let k = ((abs ((dark * 20) - (total * 10)) + total - 1) / total) - 1 in
  !result + (k * penalty_n4)
;;

(* {2 Encoding} *)

let encode ?(ecc = Ecc.M) ?mask text =
  let len = String.length text in
  let fits ver = 4 + count_bits ver + (8 * len) <= num_data_codewords ver ecc * 8 in
  match List.find (List.range 1 41) ~f:fits with
  | None ->
    Or_error.error_s [%message "too long for a QR code" (len : int) ~ecc:(ecc : Ecc.t)]
  | Some _ when Option.exists mask ~f:(fun m -> m < 0 || m > 7) ->
    Or_error.error_s [%message "mask must be 0 to 7" (mask : int option)]
  | Some ver ->
    let capacity_bits = num_data_codewords ver ecc * 8 in
    let bits = Queue.create () in
    let append v n =
      for i = n - 1 downto 0 do
        Queue.enqueue bits (bit v i)
      done
    in
    append 0b0100 4;
    append len (count_bits ver);
    String.iter text ~f:(fun c -> append (Char.to_int c) 8);
    append 0 (Int.min 4 (capacity_bits - Queue.length bits));
    append 0 ((8 - (Queue.length bits % 8)) % 8);
    let pad = ref 0xEC in
    while Queue.length bits < capacity_bits do
      append !pad 8;
      pad := !pad lxor (0xEC lxor 0x11)
    done;
    let bits = Queue.to_array bits in
    let data =
      Array.init
        (Array.length bits / 8)
        ~f:(fun i ->
          let v = ref 0 in
          for j = 0 to 7 do
            v := (!v lsl 1) lor if bits.((i * 8) + j) then 1 else 0
          done;
          !v)
    in
    let size = (ver * 4) + 17 in
    let g =
      { size
      ; modules = Array.make_matrix ~dimx:size ~dimy:size false
      ; is_function = Array.make_matrix ~dimx:size ~dimy:size false
      }
    in
    draw_function_patterns g ver ecc;
    draw_codewords g (add_ecc_and_interleave data ver ecc);
    let mask =
      match mask with
      | Some m -> m
      | None ->
        let best = ref (0, Int.max_value) in
        for m = 0 to 7 do
          apply_mask g m;
          draw_format_bits g ecc m;
          let p = penalty g in
          if p < snd !best then best := m, p;
          (* Masking twice restores the modules. *)
          apply_mask g m
        done;
        fst !best
    in
    apply_mask g mask;
    draw_format_bits g ecc mask;
    Ok { version = ver; mask; size; modules = g.modules }
;;

let svg_path (t : t) =
  let buf = Buffer.create 4096 in
  for y = 0 to t.size - 1 do
    let x = ref 0 in
    while !x < t.size do
      if t.modules.(y).(!x)
      then (
        let start = !x in
        while !x < t.size && t.modules.(y).(!x) do
          incr x
        done;
        Buffer.add_string
          buf
          (sprintf "M%d %dh%dv1h-%dz" start y (!x - start) (!x - start)))
      else incr x
    done
  done;
  Buffer.contents buf
;;

let to_text (t : t) =
  Array.map t.modules ~f:(fun row ->
    String.of_array (Array.map row ~f:(fun d -> if d then '#' else '.')))
  |> Array.to_list
  |> String.concat ~sep:"\n"
;;
