type memory = int array;;

let memory_compare x y =
  let len = Array.length x in
  if Array.length y != len then -1
  else begin
    let rec go i =
      if i >= len then 0
      else
        match compare x.(i) y.(i) with
        | -1 -> -1
        | 1  -> 1
        | _  -> go (i + 1) in
    go 0
  end

module MemoryMap = Map.Make(struct
  type t = memory
  let compare = memory_compare
end)

let memory_maxi memory =
  let rec go max maxi i =
    if i >= Array.length memory then
      maxi
    else
      let x = memory.(i) in
      if x > max then go x i (i + 1) else go max maxi (i + 1) in
  go memory.(0) 0 1;;

let redistribute memory =
  let rec go x i =
    if x <= 0 then ()
    else if i >= Array.length memory then go x 0
    else begin
      memory.(i) <- memory.(i) + 1; go (x - 1) (i + 1)
    end in
  let i0 = memory_maxi memory in
  let x0 = memory.(i0) in
  memory.(i0) <- 0;
  go x0 (i0 + 1);;

let solve memory =
  let rec go map0 steps0 =
    match MemoryMap.find_opt memory map0 with
    | Some steps1 -> (steps0, steps0 - steps1)
    | None -> begin
      let map1 = MemoryMap.add (Array.copy memory) steps0 map0 in
      redistribute memory;
      go map1 (steps0 + 1)
    end in
  go MemoryMap.empty 0;;

let words = String.split_on_char '\t' (input_line stdin) in
let mem0 = Array.of_list (List.map int_of_string words) in
let (steps, cycles) = solve mem0 in
print_string "Steps: ";
print_int steps;
print_string "\n";
print_string "Cycles: ";
print_int cycles;
print_string "\n";
