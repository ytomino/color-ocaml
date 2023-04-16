let hsv_of_bool ~(red: bool) ~(green: bool) ~(blue: bool) = (
	let hue, saturation, value =
		match red, green, blue with
		| true, false, false -> 0., 1., 1.
		| true, true, false -> Float.pi /. 3., 1., 1.
		| false, true, false -> 2. *. Float.pi /. 3., 1., 1.
		| false, true, true -> Float.pi, 1., 1.
		| false, false, true -> 4. *. Float.pi /. 3., 1., 1.
		| true, false, true -> 5. *. Float.pi /. 3., 1., 1.
		| false, false, false -> 0., 0., 0.
		| true, true, true -> 0., 0., 1.
	in
	Color.HSV.make ~hue ~saturation ~value
);;

(* logic: flat *)

let flat ~(red: bool) ~(green: bool) ~(blue: bool) (black: float)
	(white: float) =
(
	let unit_hsv = hsv_of_bool ~red ~green ~blue in
	let {Color.HSV.hue; saturation; value} = unit_hsv in
	let value = (white -. black) *. value +. black in
	let saturation =
		if value = 0. then 0. else
		(1. -. black /. value) *. saturation
	in
	Color.rgb_of_hsv {Color.HSV.hue; saturation; value}
);;

(* logic: luminance proportion / third / upper *)

let luminance_num = [| 3.; 6.; 1. |];;  (* 10 times *)
let luminance_den = 10.;;

let luminance_of_rgb (x: Color.rgb_t) = (
	luminance_num.(0) *. x.Color.RGB.red
		+. luminance_num.(1) *. x.Color.RGB.green
		+. luminance_num.(2) *. x.Color.RGB.blue
);; (* 10 times *)

let luminance_of_array (a: float array) = (
	assert (Array.length a = 3);
	luminance_num.(0) *. a.(0)
		+. luminance_num.(1) *. a.(1)
		+. luminance_num.(2) *. a.(2)
);; (* 10 times *)

let fix_over (a: float array) = (
	let fix_over_1 i a = (
		assert (a.(i) >= 1.);
		let over = luminance_num.(i) *. (a.(i) -. 1.) in (* 10 times *)
		let b = over /. (luminance_den -. luminance_num.(i)) in
		a.(i) <- 1.;
		let j1 = (i + 1) mod 3 in a.(j1) <- a.(j1) +. b;
		let j2 = (i + 2) mod 3 in a.(j2) <- a.(j2) +. b
	) in
	let fix_over_2 j a = (
		let i1 = (j + 1) mod 3 in
		let i2 = (j + 2) mod 3 in
		assert (a.(i1) >= 1. && a.(i2) >= 1.);
		assert (abs_float (a.(i1) -. a.(i2)) < 0.0001);
		let over =
			luminance_num.(i1) *. (a.(i1) -. 1.) +. luminance_num.(i2) *. (a.(i2) -. 1.)
			(* 10 times *)
		in
		a.(i1) <- 1.;
		a.(i2) <- 1.;
		a.(j) <- Float.min 1. (a.(j) +. over /. luminance_num.(j))
	) in
	let max = Float.max a.(0) (Float.max a.(1) a.(2)) in
	if max > 1. then (
		if a.(1) = max && a.(2) = max then fix_over_2 0 a else
		if a.(0) = max && a.(2) = max then fix_over_2 1 a else
		if a.(0) = max && a.(1) = max then fix_over_2 2 a else
		if a.(0) = max then fix_over_1 0 a else
		if a.(1) = max then fix_over_1 1 a else
		if a.(2) = max then fix_over_1 2 a
	);
	assert (a.(0) <= 1. && a.(1) <= 1. && a.(2) <= 1.)
);;

let make_rgb_with_luminance (unit_hsv: Color.hsv_t) (luminance: float) = (
	let target_num = luminance_den *. luminance in (* 10 times *)
	let a =
		let open Color.HSV in
		if unit_hsv.value = 0. then [| luminance; luminance; luminance |] else
		let unit_rgb = Color.rgb_of_hsv unit_hsv in
		let c = target_num /. luminance_of_rgb unit_rgb in
		let open Color.RGB in
		[| c *. unit_rgb.red; c *. unit_rgb.green; c *. unit_rgb.blue |]
	in
	fix_over a;
	assert (abs_float (luminance_of_array a -. target_num) < 0.001);
	Color.RGB.make ~red:a.(0) ~green:a.(1) ~blue:a.(2)
);;

(* luminance proportion:
   The changing of luminance from black to white is in direct proportion. *)
let luminance_proportion ~(red: bool) ~(green: bool) ~(blue: bool)
	(black: float) (white: float) =
(
	let seventh_num =
		(if red then 2. else 0.)
			+. (if green then 4. else 0.)
			+. (if blue then 1. else 0.)
		(* 7 times *)
	in
	let target = (white -. black) *. seventh_num /. 7. +. black in
	let unit_hsv = hsv_of_bool ~red ~green ~blue in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance third:
   Red, green, and blue have 1/3 luminance of between white and black.
   Yellow, magenta, and cyan have 2/3 luminance of between white and black. *)
let luminance_third ~(red: bool) ~(green: bool) ~(blue: bool) (black: float)
	(white: float) =
(
	let third_num =
		let elt_count = Bool.to_int red + Bool.to_int green + Bool.to_int blue in
		float_of_int elt_count
		(* 3 times *)
	in
	let target = (white -. black) *. third_num /. 3. +. black in
	let unit_hsv = hsv_of_bool ~red ~green ~blue in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance third mg:
   This is similar to luminance third, except luminances of green and magenta
   are swapped.
   Red, blue, and magenta have 1/3 luminance of between white and black.
   Green, Yellow, and cyan have 2/3 luminance of between white and black. *)
let luminance_third_mg ~(red: bool) ~(green: bool) ~(blue: bool) (black: float)
	(white: float) =
(
	let third_num =
		let elt_count = Bool.to_int red + Bool.to_int green + Bool.to_int blue in
		float_of_int (
			if elt_count = 1 && green then 2 else
			if elt_count = 2 && red && blue then 1 else
			elt_count
		) (* 3 times *)
	in
	let target = (white -. black) *. third_num /. 3. +. black in
	let unit_hsv = hsv_of_bool ~red ~green ~blue in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance upper:
   Red, blue have same luminance as green.
   Magenta and cyan have same luminance as yellow. *)
let luminance_upper ~(red: bool) ~(green: bool) ~(blue: bool) (black: float)
	(white: float) =
(
	let luminance_num =
		let elt_count = Bool.to_int red + Bool.to_int green + Bool.to_int blue in
		(if elt_count >= 2 then luminance_num.(0) else 0.)
			+. (if elt_count >= 1 then luminance_num.(1) else 0.)
			+. (if elt_count >= 3 then luminance_num.(2) else 0.)
		(* 10 times *)
	in
	let target = (white -. black) *. luminance_num /. luminance_den +. black in
	let unit_hsv = hsv_of_bool ~red ~green ~blue in
	make_rgb_with_luminance unit_hsv target
);;

(* name *)

let name ~(red: bool) ~(green: bool) ~(blue: bool) = (
	match red, green, blue with
	| false, false, false -> "black"
	| true, false, false -> "red"
	| false, true, false -> "green"
	| true, true, false -> "yellow"
	| false, false, true -> "blue"
	| true, false, true -> "magenta"
	| false, true, true -> "cyan"
	| true, true, true -> "white"
);;

(* main *)

let min_int (x: int) (y: int) = (
	if x < y then x else
	y
);; (* Int.min is added since OCaml 13 *)

module SRGB24 = Color.SRGB_Int (struct let max_int = 255;; end);;

let grayscale_rgb_of_srgb24 (value: int) = (
	let c =
		Color.rgb_of_srgb (
			SRGB24.to_srgb (SRGB24.make ~red:value ~green:value ~blue:value)
		)
	in
	let open Color.RGB in
	c.green
);;

let srgb24_of_rgb (rgb: Color.rgb_t) = (
	SRGB24.of_srgb (Color.srgb_of_rgb rgb)
);;

let logic = ref flat;;

let faint_min, faint_max = ref 0, ref 0;;
let normal_min, normal_max = ref 0, ref 0xBF;;
let bright_min, bright_max = ref 0x66, ref 0xE5;;

let decimal = ref false;;
let order = ref `lrgb;;

exception Unknown_option of string;;
exception Missing_value of string;;
exception Bad_value of string * string;;
let usage () = (
	let command = Sys.argv.(0) in
	let range = "[BLACK:]WHITE" in
	Printf.printf "usage: %s\n" command;
	print_newline ();
	print_endline "logic:";
	print_endline "  -F --flat                  use flat logic (default)";
	print_endline "  -P --luminance-proportion  use luminance proportion logic";
	print_endline "  -T --luminance-third       use luminance third logic";
	print_endline "     --luminance-third-mg    green and magenta are swapped";
	print_endline "  -U --luminance-upper       use luminance upper logic";
	print_endline "range:";
	Printf.printf "     --faint  %s  set faint black and white (none)\n" range;
	Printf.printf "     --normal %s  set normal black and white (%02X:%02X)\n"
		range !normal_min !normal_max;
	Printf.printf "     --bright %s  set bright black and white (%02X:%02X)\n"
		range !bright_min !bright_max;
	print_endline "output:";
	print_endline "  -d --decimal      print as decimal";
	print_endline "  -x --hexadecimal  print as hexadecimal (default)";
	print_endline "     --lrgb         order light, red, green, blue (default)";
	print_endline "     --rgbl         order red, green, blue, light";
	print_newline ();
	Printf.printf "example: %s --luminance-proportion --normal 00:BF\n" command
) in
let not_hexadecimal_value s = (
	Bad_value (s, "not hexadecimal value")
) in
let out_of_range s = (
	Bad_value (s, "out of range")
) in
let parse_range ~min arg = (
	try
		let v1, s = Scanf.sscanf arg "%x%s" (fun x y -> x, y) in
		if String.length s = 0 then (min_int min v1), v1 else
		let v2, s = Scanf.sscanf s ":%x%s" (fun x y -> x, y) in
		if String.length s > 0 then raise (not_hexadecimal_value arg) else
		if v1 <= v2 then v1, v2 else
		v2, v1
	with Scanf.Scan_failure _ -> raise (not_hexadecimal_value arg)
) in
try
	let rec parse_argv i = (
		if i >= Array.length Sys.argv then () else
		let next =
			match Sys.argv.(i) with
			| "-F" | "--flat" ->
				logic := flat;
				i + 1
			| "-P" | "--luminance-proportion" ->
				logic := luminance_proportion;
				i + 1
			| "-T" | "--luminance-third" ->
				logic := luminance_third;
				i + 1
			| "--luminance-third-mg" ->
				logic := luminance_third_mg;
				i + 1
			| "-U" | "--luminance-upper" ->
				logic := luminance_upper;
				i + 1
			| "--faint" as option ->
				if i + 1 >= Array.length Sys.argv then raise (Missing_value option) else
				let min, max = parse_range ~min:!faint_min Sys.argv.(i + 1) in
				if min < 0 || max > 0xFF then raise (out_of_range Sys.argv.(i + 1)) else (
					faint_min := min;
					faint_max := max;
					i + 2
				)
			| "--normal" as option ->
				if i + 1 >= Array.length Sys.argv then raise (Missing_value option) else
				let min, max = parse_range ~min:!normal_min Sys.argv.(i + 1) in
				if min < 0 || max > 0xFF then raise (out_of_range Sys.argv.(i + 1)) else (
					normal_min := min;
					normal_max := max;
					i + 2
				)
			| "--bright" as option ->
				if i + 1 >= Array.length Sys.argv then raise (Missing_value option) else
				let min, max = parse_range ~min:!bright_min Sys.argv.(i + 1) in
				if min < 0 || max > 0xFF then raise (out_of_range Sys.argv.(i + 1)) else (
					bright_min := min;
					bright_max := max;
					i + 2
				)
			| "-d" | "--decimal" ->
				decimal := true;
				i + 1
			| "-x" | "--hexadecimal" ->
				decimal := false;
				i + 1
			| "--lrgb" ->
				order := `lrgb;
				i + 1
			| "--rgbl" ->
				order := `rgbl;
				i + 1
			| "--help" ->
				usage ();
				exit 1
			| _ as unknown -> raise (Unknown_option unknown)
		in
		parse_argv next
	) in
	parse_argv 1
with
| Unknown_option option ->
	Printf.eprintf "%s: unknown option: %s\n" Sys.argv.(0) option;
	exit 1
| Missing_value option ->
	Printf.eprintf "%s: missing value for %s\n" Sys.argv.(0) option;
	exit 1
| Bad_value (value, message) ->
	Printf.eprintf "%s: %s: %s\n" Sys.argv.(0) message value;
	exit 1;;

let process prefix black white ~red ~green ~blue = (
	let c = !logic ~red ~green ~blue black white in
	let s = srgb24_of_rgb c in
	let name = prefix ^ name ~red ~green ~blue in
	let open SRGB24 in
	if !decimal then (
		Printf.printf "%3d,%3d,%3d" s.red s.green s.blue
	) else (
		Printf.printf "#%02X%02X%02X" s.red s.green s.blue
	);
	Printf.printf " %s\n" name
) in
let process_rgb f = (
	for blue = 0 to 1 do
		let blue = blue > 0 in
		for green = 0 to 1 do
			let green = green > 0 in
			for red = 0 to 1 do
				let red = red > 0 in
				f ~red ~green ~blue
			done
		done
	done
) in
let process_l f = (
	if !faint_max > 0 then (
		let faint_black = grayscale_rgb_of_srgb24 !faint_min in
		let faint_white = grayscale_rgb_of_srgb24 !faint_max in
		f "faint " faint_black faint_white
	);
	let normal_black = grayscale_rgb_of_srgb24 !normal_min in
	let normal_white = grayscale_rgb_of_srgb24 !normal_max in
	f "" normal_black normal_white;
	if !bright_max > 0 then (
		let bright_black = grayscale_rgb_of_srgb24 !bright_min in
		let bright_white = grayscale_rgb_of_srgb24 !bright_max in
		f "bright" bright_black bright_white
	)
) in
match !order with
| `lrgb ->
	process_l (fun prefix black white ->
		process_rgb (process prefix black white)
	)
| `rgbl ->
	process_rgb (fun ~red ~green ~blue ->
		process_l (process ~red ~green ~blue)
	);;
