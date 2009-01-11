(*
 * Urray: a module to encapsulate array operations to overcome 
 * OCaml array length restriction on 32-bit platforms 
 *
 * Copyright (C) 2007
 * Leonid Chistov, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** Module to encapsulate array operations to overcome 
    OCaml array length restriction on 32-bit platforms. 
 *)

(** Main type. *)
type 'a t

(** Gets urray length. *)
val length : 'a t -> int

(** Gets urray element. *)
val get : 'a t -> int -> 'a

(** Sets urray element. *)
val set : 'a t -> int -> 'a -> unit

(** Empty urray. *)
val empty : unit -> 'a t

(** [Urray.make n x] returns a fresh urray of length [n], initialized with [x]. All the 
    elements of this new urray are initially physically equal to [x] (in the sense of the 
    [==] predicate). Consequently, if [x] is mutable, it is shared among all elements of 
    the urray, and modifying [x] through one of the urray entries will modify all other 
    entries at the same time.

    Raises [Invalid_argument "Urray.init"] if [n] < 0.  
 *)
val make : int -> 'a -> 'a t

(** [Urray.make_matrix dimx dimy e] returns a two-dimensional urray (an urray of urrays) 
    with first dimension [dimx] and second dimension [dimy]. All the elements of this new 
    matrix are initially physically equal to [e]. The element [(x, y)] of a matrix [m] is 
    accessed with the notation [m.(x).(y)].
 *)
val make_matrix : int -> int -> 'a -> 'a t t

(** [Urray.init n f] returns a fresh urray of length [n], with element number [i] initialized 
    to the result of [f i]. In other terms, [Urray.init n f] tabulates the results of [f] 
    applied to the integers [0] to [n-1]. 

    Raises [Invalid_argument "Urray.init"] if [n] < 0.  
 *)
val init : int -> (int -> 'a) -> 'a t

(** [Urray.of_array x] makes urray from array. *)
val of_array : 'a array -> 'a t

(** [iter f a] applies function [f] to each element of urray [a]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Same as [!Urray.iter] but the function is applied to the index of the element as first 
    argument, and the element itself as second argument.
 *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** [Urray.map f a] applies function [f] to all the elements of [a], and builds an urray with 
    the results returned by [f].
  *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Same as [!Urray.map], but the function is applied to the index of the element as first argument, 
    and the element itself as second argument.
 *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** Converts list to urray. *)
val of_list : 'a list -> 'a t

(** Converts urray to list. *)
val to_list : 'a t -> 'a list

(** [Urray.fold_left f x a] computes [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)], 
    where [n] is the length of the urray [a]. 
 *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** [Urray.fold_right f a x] computes [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))], 
    where [n] is the length of the urray [a].
 *)
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a

(** Returns a copy of urray. *)
val copy : 'a t -> 'a t

(** Appends two urrays. *)
val append : 'a t -> 'a t -> 'a t

(** [Urray.fill a ofs len x] modifies the urray [a] in place, storing [x] in 
    elements number [ofs] to [ofs + len - 1].
 
    Raises [Invalid_argument "Urray.fill"] if [ofs] and [len] do not designate 
    a valid suburray of [a].
 *)
val fill : 'a t -> int -> int -> 'a -> unit

(** [sub a start len] returns a fresh urray of length [len],
    containing the elements of urray [a] numbered from [start] to [start + len - 1].
 *)
val sub : 'a t -> int -> int -> 'a t 

(** [Urray.blit v1 o1 v2 o2 len] copies [len] elements from urray [v1], starting at 
    element number [o1], to urray [v2], starting at element number [o2]. It works correctly 
    even if [v1] and [v2] are the same [urray], and the source and destination chunks overlap.

    Raises [Invalid_argument "Urray.blit"] if [o1] and [len] do not designate a valid suburray 
    of [v1], or if [o2] and [len] do not designate a valid suburray of [v2].
 *)
val blit : 'a t -> int -> 'a t -> int -> int -> unit

(** [concat l] concatenates the list of urrays [l] into one urray. *)
val concat : 'a t list -> 'a t

(** Sorta an urray in increasing order according to a comparison function. 
    See {!Array.sort} for details.
 *)
val sort : ('a -> 'a -> int) -> 'a t -> unit

(** Sorts an urray in increasing order according to a comparison function, preserving
    original order of equal elements. See {!Array.stable_sort} for details.
 *)
val stable_sort : ('a -> 'a -> int) -> 'a t -> unit

(** [fast_sort] is equal to [sort] in current implementation. *)
val fast_sort : ('a -> 'a -> int) -> 'a t -> unit

