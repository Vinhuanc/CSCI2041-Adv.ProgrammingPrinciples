# Code definitions:

```ocaml
let ( >>= ) o f =
    match o with
    | None -> None
    | Some y -> f y

let return x = Some x
```

# Example Proof:

```
Prove:
	x >>= return = x, for (x: 'a option)

By cases on (x: 'a option):

Case x = None:

	x >>= return
= { case }
	None >>= return
= { bind def }
	match None with
	| None -> None
	| Some a -> return a
= { apply match }
	None
= { case }
	x

Case x = Some y:

	x >>= return
= { case }
	Some y >>= return
= { bind def }
	match Some y with
	| None -> None
	| Some a -> return a
= { apply match }
	return a
= { return def }
	Some a
= { case }
	x

We have proven the statement for all possible values of x, so the statement is true, namely x >>= return = x for any (x: 'a option)
```

# Problem 1

```
Prove:
	(x >>= g) >>= h = x >>= (fun y -> g y >>= h), for (x: 'a option)

(x >>= g) >>= h
= { case }
  None >>= return
= { bind def }
  match None with
  | None -> None
  | Some a -> h a

x >>= (fun y -> g y >>= h)
= {case}
  None >>= return
= { bind def }
  match None with
  | None -> None
  | Some a -> (fun y -> g y >>= h ) a
= {apply match}
  return g a >>= h
  Some a
= {case}
a
```

# Problem 2

```
Prove:
	return x >>= f = f x, for (x: 'a option)
return x >>= f
Case x = None:
x >>= return
={case}
None >>= return
={bind def}
match None with
|None -> None
|Some a -> f x
={apply match}
None
={case}
f x

Case x = Some x:
x >>= return
={ case}
Some a >>= f a
match Some y with
|None -> None
| Some x -> return f x
={apply match}
return x
={return def}
Some x
={case}
f x
```

# Problem 3

```
Prove:
	x >>= (fun x' -> y >>= (fun y' -> plus x' y')) =
		match y with
		| None -> None
		| Some y' -> (
			match x with
			| None -> None
			| Some x' -> plus x' y')

Case x and y = None:
x >>= (fun x' -> y >>= (fun y' -> plus x' y')) return
={ case }
None >>= return
= {bind def }
match (x,y) with
|(None, None) -> None
|(None, _) | (_, None) -> None
|Some (a, b)->  return
(fun a' -> None >>= (fun b' -> plus a' b'))

Inductive: x is None, y is Some y
x >>= (fun x' -> y >>= (fun y' -> plus x' y'))
= None >>= (fun x' -> Some y' >>= (fun y' -> plus x' y'))
= { bind def }
match Some y with
={ apply match}
match y with
| Some y' -> (
  match x with
  | None -> None
  | Some x' -> return plus x' y')

Inductive: x is Some x, y is None
x >>= (fun x' -> y >>= (fun y' -> plus x' y')) = Some x' >>= (fun x' -> None >>= (fun y' -> plus x' y'))
= { bind def }
match Some y with
={ apply match}
match y with
| None -> None
| Some y' -> (
  match x with
  | None -> None
  | Some x' -> plus x' y')

Inductive: x is Some x, y is Some y
x >>= (fun x' -> y >>= (fun y' -> plus x' y')) = Some x' >>= (fun x' -> Some y' >>= (fun y' -> plus x' y'))
= { bind def }
match Some y with
={ apply match}
match y with
match y with
| Some y' -> (
  match x with
  | None -> None
  | Some x' -> plus x' y')

```
