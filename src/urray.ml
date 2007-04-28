(*
 * Urray: a module to incapsulate array operations to overcome 
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

type 'a t = 'a array array

let bs      = 2097152
let shift   = 21
let index i = i lsr shift, i land (bs-1)

let length a =
  let i = Array.length a in
  if i = 0 then 0 else (i-1) lsl shift + (Array.length a.(i-1))

let get a i = if i < bs then a.(0).(i) else a.(i lsr shift).(i land (bs-1))

let set a i x =
  if i < bs then a.(0).(i) <- x else a.(i lsr shift).(i land (bs-1)) <- x

let make size x =
  if size < 0 
  then raise (Invalid_argument "Urray.make")
  else
    let i, j = index size in
    let i, j = if j > 0 then i+1, j else i, bs in
    Array.init i (fun l -> Array.make (if l = i-1 then j else bs) x) 

let init size f =
  if size < 0 
  then raise (Invalid_argument "Urray.init")
  else
    let i, j = index size in
    let i, j = if j > 0 then i+1, j else i, bs in
    Array.init i (fun l -> Array.init (if l = i-1 then j else bs) (fun k -> f (l * bs + k))) 

let make_matrix sx sy init =
  let res = make sx (make 0 init) in
  for x = 0 to sx-1 do
    set res x (make sy init)
  done;
  res

let iteri f x = Array.iteri (fun i a -> Array.iteri (fun j b -> f (i lsl shift + j) b) a) x
let iter  f x = Array.iter  (fun   a -> Array.iter  (fun   b -> f b) a) x

let fold_left f x a = Array.fold_left (fun y b -> Array.fold_left f y b) x a
let fold_right f a x = Array.fold_right (fun b y -> Array.fold_right f b y) a x
  
let of_list = function
  | [] -> [||]
  | (hd :: tl) as l ->
      let a = make (List.length l) hd in
      ignore (List.fold_left (fun i x -> set a i x; i+1) 0 l);
      a

let to_list = function
  | [||] -> []
  | a ->
      let mi, mj = index (length a - 1) in
      let rec tolist i j res = 
	if j = 0 then	  
	  if i = 0 then a.(0).(0)::res
          else tolist (i-1) (bs-1) (a.(i).(j)::res)
	else tolist i (j-1) (a.(i).(j)::res)
      in  
      tolist mi mj []
	
let copy a =
  let l = length a in
  if l = 0 
  then [||] 
  else 
   let mi, mj = index (l-1) in
   Array.init (mi+1) (fun i -> let aa=a.(i) in Array.init( if i<mi then bs else (mj+1))(fun j->aa.(j))) 

let append a1 a2 = 
  let l1, l2 = length a1, length a2 in
  if l1 = 0 
  then a2 
  else
    if l2 = 0 
    then a1 
    else
      let i, j = index (l1 + l2 - 1) in
      let i1, j1 = index (l1 - 1) in
      Array.init 
	(i+1)
	(fun l -> 
          if l < i1 
	  then let aa = a1.(l) in Array.init bs (fun k -> aa.(k)) 
          else 
	    if l = i1 
	    then
              let size = if l < i then bs else j + 1 in
              let aa1, aa2 = a1.(l), a2.(0) in
              Array.init size (fun k -> if k <= j1 then aa1.(k) else aa2.(k - j1 - 1))
            else
              let size = if l < i then bs else j + 1 in
              let a21 = a2.(l - i1 - 1) in                                  
              let pl = bs - j1 - 1 in
              if size - 1 > j1 
	      then
		let a22 = a2.(l - i1) in                                                                     
   		Array.init size (fun k -> if k > j1 then a22.(k - j1 - 1) else a21.(pl + k))
              else Array.init size (fun k -> a21.(pl + k))
	)
       
let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Urray.fill"
  else
    let ofsi, ofsj = index ofs in
    let leni, lenj = index (len + ofs) in
    if ofsi = leni 
    then      
      let aa = a.(ofsi) in
      for j = ofsj to lenj-1 do aa.(j) <- v done      
    else begin
       let aa = a.(ofsi) in
       for j = ofsj to bs - 1 do aa.(j) <- v done;      
       for i = ofsi + 1 to leni - 1 do
         let aa = a.(i) in
         for j = 0 to bs - 1 do aa.(j) <- v done
       done;  
       if lenj != 0 then
         let aa = a.(leni) in
         for j = 0 to lenj - 1 do aa.(j) <- v done;
    end
 
let sub a ofs len =
  if ofs < 0 || len < 0 || ofs > length a - len 
  then invalid_arg "Urray.sub"
  else 
    if len = 0 then [||]
    else
      let c = get a ofs in
      let r = make len c in 
      let ofsi, ofsj = index ofs in
      let ofsir, ofsjr = ref ofsi, ref ofsj in
      let leni, lenj = index (len+ofs) in
      let i, j = ref 0, ref 0 in
      while !ofsir < leni || !ofsjr < lenj 
      do	
        r.(!i).(!j)<-a.(!ofsir).(!ofsjr);
        incr j; 
	incr ofsjr;
        if !j = bs then (j := 0; incr i);
        if !ofsjr = bs then (ofsjr := 0; incr ofsir)
      done; 
      r

let blit a1 ofs1 a2 ofs2 len = 
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Urray.blit" 
  else 
    if ofs1 < ofs2 then
      let i1, j1 = index (ofs1 + len - 1) in
      let i2, j2 = index (ofs2 + len - 1) in
      let i1r, j1r, i2r, j2r = ref i1, ref j1, ref i2, ref j2 in
      let mini, minj = index ofs1 in 
      while !i1r >= mini && !j1r >= minj 
      do
        a2.(!i2r).(!j2r) <- a1.(!i1r).(!j1r);
        j1r := !j1r - 1;
	j2r := !j2r - 1;
        if !j1r = 0 then (j1r := bs - 1; i1r := !i1r - 1);
        if !j2r = 0 then (j2r := bs - 1; i2r := !i2r - 1);
      done
    else
      let i1, j1 = index ofs1 in
      let i2, j2 = index ofs2 in
      let i1r, j1r, i2r, j2r = ref i1, ref j1, ref i2, ref j2 in
      let maxi, maxj = index (len + ofs1) in 
      while !i1r <= maxi && !j1r <= maxj 
      do
        a2.(!i2r).(!j2r) <- a1.(!i1r).(!j1r);
        j1r := !j1r + 1;
	j2r := !j2r + 1;
        if !j1r = bs then (j1r := 0; i1r := !i1r + 1);
        if !j2r = bs then (j2r := 0; i2r := !i2r + 1);
      done
                
let concat al =
  let concatAux init al =
    let rec size accu = function
      | [] -> accu
      | h::t -> size (accu + length h) t
    in
    let res = make (size 0 al) init in
    let rec fill pos = function
      | [] -> ()
      | h::t ->
          blit res pos h 0 (length h);
          fill (pos + length h) t
    in
    fill 0 al;
    res
  in
  let rec find_init = function
    | [] -> [||]
    | a :: rem -> if length a > 0 then concatAux (get a 0) al else find_init rem
  in 
  find_init al

exception Bottom of int

let sort cmp a =
  let maxson l i =
    let i31 = i + i + i + 1 in
    let x = ref i31 in
    if i31 + 2 < l 
    then (
      if cmp (get a i31) (get a (i31 + 1)) < 0 then x := i31 + 1;
      if cmp (get a !x) (get a (i31 + 2)) < 0 then x := i31 + 2;
      !x
    )
    else
      if i31 + 1 < l && cmp (get a i31) (get a (i31 + 1)) < 0
      then i31 + 1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (get a j) e > 0 
    then (
      set a i (get a j);
      trickledown l j e;
    ) 
    else set a i e    
  in
  let rec trickle l i e = try trickledown l i e with Bottom i -> set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    set a i (get a j);
    bubbledown l j
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (get a father) e < 0 
    then (
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e
    )
    else set a i e  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 
  do
    let e = get a i in
    set a i (get a 0);
    trickleup (bubble i 0) e
  done;
  if l > 1 then 
    let e = (get a 1) in (set a 1 (get a 0); set a 0 e)

let stable_sort cmp a = 
  if length a <= bs then Array.stable_sort cmp a.(0) 
  else
    begin
      let inc i j = 
	incr j;
	if !j = bs then (incr i; j := 0);
      in
      let dec i j =
	decr(j);
	if !j = -1 then (decr i; j := bs - 1)
      in  
      let len = Array.length a in
      let size = length a in  
      let tmp = make (size -((len + 1)/2) lsl shift) a.(0).(0) in
      let merge_up ab ae tb te db =
	let tmp_max_ind = te lsl shift + Array.length tmp.(te) - 1 in
	let tmp_max_i, tmp_max_j = index tmp_max_ind in
	let di, dj = index (db lsl shift) in
	let ti, tj = index (tb lsl shift) in
	let dir = ref di and djr = ref dj and tir = ref ti and tjr = ref tj in
	for i = ab to ae 
	do
	  let ai = a.(i) in
	  for j = 0 to Array.length a.(i) - 1 
	  do   
	    while 
	      (!tir < tmp_max_i) || ((!tir = tmp_max_i) && (!tjr <= tmp_max_j)) &&
	      (cmp ai.(j) tmp.(!tir).(!tjr) > 0) 
	    do
	      tmp.(!dir).(!djr) <- tmp.(!tir).(!tjr); 
	      inc tir tjr; 
	      inc dir djr
	    done; 
	    tmp.(!dir).(!djr) <- ai.(j);inc dir djr
	  done;
	done
      in
      let merge_down ab ae tb te de =
	let a_min_ind = (ab lsl shift) in
	let a_min_i, _ = index a_min_ind in
	let ai, aj = index (ae lsl shift + Array.length a.(ae) - 1) in 
	let di, dj = index (de lsl shift + Array.length a.(de) - 1) in
	let air = ref ai and ajr = ref aj and dir = ref di and djr = ref dj in
	for i = te downto tb 
	do
	  let tmpi = tmp.(i) in
	  for j = Array.length tmp.(i) - 1 downto 0 
	  do
	    if !air >= 0 
	    then
	      while (!air >= a_min_i) && (cmp a.(!air).(!ajr) tmpi.(j) > 0 )
	      do       
		a.(!dir).(!djr) <- a.(!air).(!ajr); 
		dec air ajr; 
		dec dir djr
	      done;
	    a.(!dir).(!djr) <- tmpi.(j); 
	    dec dir djr
	  done
	done
      in
      let rec merge_all spos epos dp wh =
	if spos = epos then ( 
	  if wh = `Up then tmp.(dp) <- Array.copy a.(spos)
	 )
	else 
	  begin
	    let divp = spos + (epos - spos) / 2 in
	    match wh with
	    | `Down ->
		merge_all spos divp spos `Down;
		merge_all (divp+1) epos 0 `Up;      
		merge_down spos divp 0 (epos-divp-1) epos
	    | `Up ->
		merge_all spos divp spos `Down;
		merge_all (divp+1) epos (dp+divp-spos+1) `Up;
		merge_up spos divp (dp+divp-spos+1) (dp+epos-spos) dp 
	  end
      in
      for i = 0 to len - 1 do Array.stable_sort cmp a.(i) done;
      merge_all 0 (len - 1) 0 `Down
    end

let fast_sort = sort

let map f a = init (length a) (fun i -> f (get a i))
let mapi f a = init (length a) (fun i -> f i (get a i))
