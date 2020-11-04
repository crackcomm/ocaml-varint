(* Copyright © 2018 Sam Riyad. All rights reserved.
   Copyright © 2020 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: MIT *)

open Core_kernel

type error = [ `VarintEOF ]

module type S = sig
  type t

  (** [encode t] Encodes varint to buffer. *)
  val encode : t -> Bigstring.t

  (** [decode buf] Decodes varint from buffer and returns with rest. *)
  val decode : Bigstring.t -> (t * Bigstring.t, error) result

  (** [decode_int buf] Helper for decoding varint. Converts result to int. *)
  val decode_int : Bigstring.t -> (int * Bigstring.t, error) result

  (** [write buf t] Writes varint to buffer. Be careful with this function be
      sure that the buffer has enough space The max sizes for 64 bit is 10, 5
      for 32 bit, and 3 for 16 bit so be sure you have that extra space
      allocated just in case. *)
  val write : Bigstring.t -> t -> int
end

module Make : functor (I : Stdint.Int) -> S with type t = I.t
module VarInt32 : S with type t = Stdint.Int32.t
module VarInt64 : S with type t = Stdint.Int64.t
module VarUint64 : S with type t = Stdint.Uint64.t
