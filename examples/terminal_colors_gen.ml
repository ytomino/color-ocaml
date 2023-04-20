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

let flat (filter: Color.hsv_t -> Color.hsv_t) ~(red: bool) ~(green: bool)
	~(blue: bool) (black: float) (white: float) =
(
	let unit_hsv = filter (hsv_of_bool ~red ~green ~blue) in
	let {Color.HSV.hue; saturation; value} = unit_hsv in
	let value = (white -. black) *. value +. black in
	let saturation =
		if value = 0. then 0. else
		(1. -. black /. value) *. saturation
	in
	Color.rgb_of_hsv {Color.HSV.hue; saturation; value}
);;

(* logic: luminance proportion / third / upper *)

module Luminance = struct
	let red = 3.;;
	let green = 6.;;
	let blue = 1.;;
	let den = 10.;;
end;; (* 10 times *)

module HSY = Color.HSY (Luminance);;

let make_rgb_with_luminance (unit_hsv: Color.hsv_t) (luminance: float) = (
	let unit_rgb = Color.rgb_of_hsv unit_hsv in
	let unit_hsy = HSY.of_rgb unit_rgb in
	let hsy = {unit_hsy with HSY.intensity = luminance} in
	HSY.to_rgb hsy
);;

(* luminance proportion:
   The changing of luminance from black to white is in direct proportion. *)
let luminance_proportion (filter: Color.hsv_t -> Color.hsv_t) ~(red: bool)
	~(green: bool) ~(blue: bool) (black: float) (white: float) =
(
	let seventh_num =
		(if red then 2. else 0.)
			+. (if green then 4. else 0.)
			+. (if blue then 1. else 0.)
		(* 7 times *)
	in
	let target = (white -. black) *. seventh_num /. 7. +. black in
	let unit_hsv = filter (hsv_of_bool ~red ~green ~blue) in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance third:
   Red, green, and blue have 1/3 luminance of between white and black.
   Yellow, magenta, and cyan have 2/3 luminance of between white and black. *)
let luminance_third (filter: Color.hsv_t -> Color.hsv_t) ~(red: bool)
	~(green: bool) ~(blue: bool) (black: float) (white: float) =
(
	let third_num =
		let elt_count = Bool.to_int red + Bool.to_int green + Bool.to_int blue in
		float_of_int elt_count
		(* 3 times *)
	in
	let target = (white -. black) *. third_num /. 3. +. black in
	let unit_hsv = filter (hsv_of_bool ~red ~green ~blue) in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance third mg:
   This is similar to luminance third, except luminances of green and magenta
   are swapped.
   Red, blue, and magenta have 1/3 luminance of between white and black.
   Green, Yellow, and cyan have 2/3 luminance of between white and black. *)
let luminance_third_mg (filter: Color.hsv_t -> Color.hsv_t) ~(red: bool)
	~(green: bool) ~(blue: bool) (black: float) (white: float) =
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
	let unit_hsv = filter (hsv_of_bool ~red ~green ~blue) in
	make_rgb_with_luminance unit_hsv target
);;

(* luminance upper:
   Red, blue have same luminance as green.
   Magenta and cyan have same luminance as yellow. *)
let luminance_upper (filter: Color.hsv_t -> Color.hsv_t) ~(red: bool)
	~(green: bool) ~(blue: bool) (black: float) (white: float) =
(
	let luminance_num =
		let elt_count = Bool.to_int red + Bool.to_int green + Bool.to_int blue in
		(if elt_count >= 2 then Luminance.red else 0.)
			+. (if elt_count >= 1 then Luminance.green else 0.)
			+. (if elt_count >= 3 then Luminance.blue else 0.)
		(* 10 times *)
	in
	let target = (white -. black) *. luminance_num /. Luminance.den +. black in
	let unit_hsv = filter (hsv_of_bool ~red ~green ~blue) in
	make_rgb_with_luminance unit_hsv target
);;

(* filter: add hue *)

let double_pi = 2. *. Float.pi;;

let rec round_double_pi (x: float) = (
	if x > double_pi then round_double_pi (x -. double_pi) else
	if x < 0. then round_double_pi (x +. double_pi) else
	x
);;

let add_hue (rad: float) (unit_hsv: Color.hsv_t) = (
	let {Color.HSV.hue; saturation; value} = unit_hsv in
	let hue = round_double_pi (hue +. rad) in
	Color.HSV.make ~hue ~saturation ~value
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

let grayscale_rgb_of_srgb24 (value: int) = (
	let c =
		Color.rgb_of_srgb (
			Color.SRGB24.to_srgb (Color.SRGB24.make ~red:value ~green:value ~blue:value)
		)
	in
	let open Color.RGB in
	c.green
);;

let srgb24_of_rgb (rgb: Color.rgb_t) = (
	Color.SRGB24.of_srgb (Color.srgb_of_rgb rgb)
);;

let logic = ref flat;;
let filter = ref (fun x -> x);;

let faint_min, faint_max = ref 0, ref 0;;
let normal_min, normal_max = ref 0, ref 0xBF;;
let bright_min, bright_max = ref 0x66, ref 0xE5;;

let decimal = ref false;;
let order = ref `lrgb;;

let print_sample = ref false;;

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
	print_endline "filter:";
	print_endline "     --add-hue RAD  rotate hue by specified angle";
	print_endline "range:";
	Printf.printf "     --faint  %s  set faint black and white (none)\n" range;
	Printf.printf "     --normal %s  set normal black and white (%02X:%02X)\n"
		range !normal_min !normal_max;
	Printf.printf "     --bright %s  set bright black and white (%02X:%02X)\n"
		range !bright_min !bright_max;
	print_endline "output:";
	print_endline "  -d --decimal       print as decimal";
	print_endline "  -x --hexadecimal   print as hexadecimal (default)";
	print_endline "     --lrgb          order light, red, green, blue (default)";
	print_endline "     --rgbl          order red, green, blue, light";
	print_endline "     --print-sample  print true color escape sequence samples";
	print_newline ();
	Printf.printf "example: %s --luminance-proportion --normal 00:BF\n" command
) in
let not_hexadecimal_value s = (
	Bad_value (s, "not hexadecimal value")
) in
let out_of_range s = (
	Bad_value (s, "out of range")
) in
let parse_float arg = (
	match float_of_string_opt arg with
	| Some rad ->	rad
	| None -> raise (Bad_value (arg, "not float value"))
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
			| "--add-hue" as option ->
				if i + 1 >= Array.length Sys.argv then raise (Missing_value option) else
				let rad = parse_float Sys.argv.(i + 1) in
				if Float.is_infinite rad || abs_float rad > double_pi then
					raise (out_of_range Sys.argv.(i + 1))
				else (
					filter := add_hue rad;
					i + 2
				)
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
			| "--print-sample" ->
				print_sample := true;
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
	let c = !logic !filter ~red ~green ~blue black white in
	let s = srgb24_of_rgb c in
	let name = prefix ^ name ~red ~green ~blue in
	let open Color.SRGB24 in
	if !decimal then (
		Printf.printf "%3d,%3d,%3d" s.red s.green s.blue
	) else (
		Printf.printf "#%02X%02X%02X" s.red s.green s.blue
	);
	if !print_sample then (
		for i = 3 to 4 do
			(* 13 = String.length "brightmagenta" *)
			Printf.printf " \x1b[%d8;2;%d;%d;%dm%-13s\x1b[0m" i s.red s.green s.blue name
		done
	) else (
		Printf.printf " %s" name
	);
	print_newline ()
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
