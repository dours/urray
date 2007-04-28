let len = Sys.max_array_length
   
let app = fun el l -> el::l


let _ = 
  Printf.printf "append time test\n"; 
  let a1 = Urray.make (len/2) 0 in
  let a2 = Urray.make (len/2) 1 in
  let a3 = Array.make (len/2) 0 in
  let a4 = Array.make (len/2) 1 in
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);
  let t1 = Sys.time() in
   ignore(Urray.append a1 a2);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.append a3 a4);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);

  
  
  
