(* Copyright © 2018 Sam Riyad. All rights reserved.
   Copyright © 2020 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: MIT *)

module Bigstring = Core_kernel.Bigstring

type error = [ `VarintEOF ]

module type S = sig
  type t

  val encode : t -> Bigstring.t
  val decode : Bigstring.t -> (t * Bigstring.t, error) result
  val decode_int : Bigstring.t -> (int * Bigstring.t, error) result
  val write : Bigstring.t -> t -> int
end

module Make (I : Stdint.Int) = struct
  type t = I.t

  let of_int, to_int = I.of_int, I.to_int

  let ( lor ), ( land ), ( lsl ), ( asr ) =
    I.(logor, logand, shift_left, shift_right)
  ;;

  let e = of_int 0x80
  let shft = of_int 0x7F

  let write buf t =
    let n = ref t in
    let rec aux off =
      let cont () =
        let b = !n lor e |> to_int |> Char.unsafe_chr in
        let _ = Bigstring.set buf off b in
        let _ = n := !n asr 7 in
        aux (off + 1)
      in
      if !n >= e
      then cont ()
      else (
        let b = to_int !n |> Char.unsafe_chr in
        let _ = Bigstring.set buf off b in
        off + 1)
    in
    aux 0
  ;;

  let encode input =
    let b = Bigstring.create 10 in
    Bigstring.memset ~pos:0 ~len:10 b '\000';
    let w = write b input in
    Bigstring.sub_shared ~len:w b
  ;;

  let decode buf =
    let v = ref I.zero in
    let y = ref 0 in
    let rec aux off =
      let b = Bigstring.get buf off |> Char.code |> of_int in
      let cont () =
        let _ = v := !v lor ((b land shft) lsl !y) in
        let _ = y := !y + 7 in
        aux (Int.add off 1)
      in
      if b land e <> I.zero
      then cont ()
      else (
        let _ = v := !v lor ((b land shft) lsl !y) in
        let rest = Bigstring.sub_shared ~pos:(Int.add 1 off) buf in
        !v, rest)
    in
    try Ok (aux 0) with
    | _ -> Error `VarintEOF
  ;;

  let decode_int buf =
    match decode buf with
    | Ok (vint, rest) -> Ok (to_int vint, rest)
    | Error err -> Error err
  ;;
end

module VarInt32 = Make (Stdint.Int32)
module VarInt64 = Make (Stdint.Int64)
module VarUint64 = Make (Stdint.Uint64)
