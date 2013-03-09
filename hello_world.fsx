(* F# code kata *)

(* typical hello world  *)
let hello_world () =
    printfn "hello world!"

(* fizzbuzz implementations *)
let fizzbuzz_1 () =
    for n in {1..100} do
        if n % 15 = 0 then
            printfn "fizzbuzz"
        elif n % 3 = 0 then
            printfn "fizz"
        elif n % 5 = 0 then
            printfn "buzz"
        else
            printfn "%d" n

(* fizzbuzz using pattern matching *)
let fizzbuzz_2 () =
    let (|IsDivisor|_|) divisor n =
        if n % divisor = 0 then Some(n) else None
    
    for n in {1..100} do
        match n with
        | IsDivisor 15 _ -> printfn "fizzbuzz"
        | IsDivisor 3  _ -> printfn "fizz"
        | IsDivisor 5  _ -> printfn "buzz"
        | _ ->              printfn "%d" n


(* factorial *)
let fact n =
    {1I..n} |> Seq.reduce(fun acc i -> i * acc)

(* factorial recursive, with tail recursion *)
let fact_rec n =
    let rec fact acc n =
        if n = 1I
        then acc
        else fact (acc*n) (n-1I)
    fact 1I n

(* fibonacci standard *)
let rec fib_dummy n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib_dummy(n-1) + fib_dummy(n-2)

(* fibonacci with memoization *)
let fib_mem =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<bigint,bigint>()
    
    let rec fib n =
        cache.GetOrAdd(n, fun n -> if n = 0I then 0I
                                   elif n = 1I then 1I
                                   else fib(n-1I) + fib(n-2I))
    fib

(* fibbonacci sequence, the functional way, with tail recursion *)
let fib_functional =
    let rec fib m1 m2 n =
        if   m1 = 0I && m2 = 0I && n = 0I then 0I
        elif m1 = 0I && m2 = 0I           then fib 0I 1I (n - 1I)
        elif n = 0I                       then m2
        else fib m2 (m1 + m2) (n - 1I)
    fib 0I 0I

