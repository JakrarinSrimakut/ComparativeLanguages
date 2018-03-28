(* Jakrarin Srimakut
 * jsrimaku@ucsc.edu *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let aget = Array.get
let aset = Array.set
let amake = Array.make
let symbol_table = amake 256 (false, Bigint.zero)

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let print_number number = 
  let number' = string_of_bigint number in
  let length  = (String.length number') in
  let rec print_number' index =
    let num_left = length - index in
      if (num_left >= 69) then
        (printf "%s\\\n%!" (String.sub number' index 69);
         print_number' (index + 69))
      else printf "%s\n%!" (String.sub number' index num_left)
  in print_number' 0

let print_stackempty () = eprintf "dc: stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
  try match oper with
    | 'l' -> (
      let entry = aget symbol_table reg
      in match entry with
        | false, _ -> if (char_of_int reg) = '\n'
          then
            printf "dc: register 0%o is empty\n%!" reg
          else
            printf "dc: register '%c' (0%o) is empty\n%!"
            (char_of_int reg) reg
        | true, value -> push value thestack
    )
    | 's' ->
      aset symbol_table reg (true, (pop thestack))
    | _   ->
      printf "dc: '%c' (0%o) unimplemented\n%!" (char_of_int (ord oper)) (ord oper)
  with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
  try let right = pop thestack
    in  try let left = pop thestack
        in  push (oper left right) thestack
      with Stack.Empty -> (print_stackempty ();
                 push right thestack)
  with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
  try match oper with
    | '+'  -> executebinop thestack Bigint.add
    | '-'  -> executebinop thestack Bigint.sub
    | '*'  -> executebinop thestack Bigint.mul
    | '/'  -> executebinop thestack Bigint.div
    | '%'  -> executebinop thestack Bigint.rem
    | '^'  -> executebinop thestack Bigint.pow
    | 'c'  -> Stack.clear thestack
    | 'd'  -> push (Stack.top thestack) thestack
    | 'f'  -> Stack.iter print_number thestack
    | 'l'  -> failwith "operator l scanned with no register"
    | 'p'  -> print_number (Stack.top thestack)
    | 'q'  -> raise End_of_file
    | 's'  -> failwith "operator s scanned with no register"
    | '\n' -> ()
    | ' '  -> ()
    | _  -> printf "dc: '%c' (0%o) unimplemented\n%!" (char_of_int (ord oper)) (ord oper)
  with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
  let scanbuf = Lexing.from_channel inputchannel in
  let rec toploop () = 
    try  let nexttoken = Scanner.scanner scanbuf
       in  (match nexttoken with
         | Number number     -> push number thestack
         | Regoper (oper, reg) -> executereg thestack oper reg
         | Operator oper     -> execute thestack oper
         );
       toploop ()
    with End_of_file -> exit 1;
  in  toploop ()

let readfiles () =
  let thestack : bigint Stack.t = Stack.create ()
  in  ((if Array.length Sys.argv > 1 
     then try  let thefile = open_in Sys.argv.(1)
           in  toploop thestack thefile
        with Sys_error message -> (
           printf "%s: %s\n%!" Sys.argv.(0) message;
           exit 1));
    toploop thestack stdin)

let interact () =
  let thestack : bigint Stack.t = Stack.create ()
  in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

