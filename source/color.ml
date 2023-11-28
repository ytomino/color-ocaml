(* RGB *)

type rgb_t = {red: float; green: float; blue: float};;

let is_valid_rgb {red; green; blue} =
	red >= 0.0 && red <= 1.0
		&& blue >= 0.0 && blue <= 1.0
		&& green >= 0.0 && green <= 1.0;;

module RGB = struct
	type t = rgb_t = {red: float; green: float; blue: float};;
	
	let is_valid = is_valid_rgb;;
	
	let make ~red ~green ~blue =
		let result = {red; green; blue} in
		if is_valid_rgb result then result else
		invalid_arg "RGB.make";;
	
	let black = {red = 0.0; green = 0.0; blue = 0.0};;
	
	let white = {red = 1.0; green = 1.0; blue = 1.0};;
	
	let distance left right =
		(left.red -. right.red) ** 2.0
			+. (left.green -. right.green) ** 2.0
			+. (left.blue -. right.blue) ** 2.0;;
end;;

(* sRGB *)

type srgb_t = {red: float; green: float; blue: float};;

let is_valid_srgb ({red; green; blue}: srgb_t) =
	red >= 0.0 && red <= 1.0
		&& blue >= 0.0 && blue <= 1.0
		&& green >= 0.0 && green <= 1.0;;

let srgb_of_rgb ({RGB.red; green; blue} as x) =
	if is_valid_rgb x then (
		let convert value =
			(* the threshold is: 12.92 * x = 1.055 * x ** (1.0 / 2.4) - 0.055 *)
			if value <= 0.003130668443 then 12.92 *. value else
			if value < 1.0 then 1.055 *. value ** (1.0 /. 2.4) -. 0.055 else
			1.0
		in
		({red = convert red; green = convert green; blue = convert blue}: srgb_t)
	) else
	invalid_arg "srgb_of_rgb";;

let rgb_of_srgb ({red; green; blue} as x: srgb_t) =
	if is_valid_srgb x then (
		let convert value =
			if value <= 0.04044823628 then value /. 12.92 else
			if value < 1.0 then ((value +. 0.055) /. 1.055) ** 2.4 else
			1.0
		in
		{RGB.red = convert red; green = convert green; blue = convert blue}
	) else
	invalid_arg "rgb_of_srgb";;

module SRGB = struct
	type t = srgb_t = {red: float; green: float; blue: float};;
	
	let is_valid = is_valid_srgb;;
	
	let make ~red ~green ~blue =
		let result = ({red; green; blue}: t) in
		if is_valid_srgb result then result else
		invalid_arg "SRGB.make";;
	
	let black = ({red = 0.0; green = 0.0; blue = 0.0}: t);;
	
	let white = ({red = 1.0; green = 1.0; blue = 1.0}: t);;
	
	let of_rgb = srgb_of_rgb;;
	
	let to_rgb = rgb_of_srgb;;
end;;

module type SRGB_IntS = sig
	type t = {red: int; green: int; blue: int}
	
	val is_valid: t -> bool
	val make: red:int -> green:int -> blue:int -> t
	val black: t
	val white: t
	val of_srgb: srgb_t -> t
	val to_srgb: t -> srgb_t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end;;

module SRGB_Int (Param: sig val max_int: int end) = struct
	type t = {red: int; green: int; blue: int};;
	
	let is_valid ({red; green; blue}: t) =
		red >= 0 && red <= Param.max_int
			&& green >= 0 && green <= Param.max_int
			&& blue >= 0 && blue <= Param.max_int;;
	
	let make ~red ~green ~blue =
		let result = ({red; green; blue}: t) in
		if is_valid result then result else
		invalid_arg "SRGB_Int.make";;
	
	let black = ({red = 0; green = 0; blue = 0} : t);;
	
	let white =
		({red = Param.max_int; green = Param.max_int; blue = Param.max_int}: t);;
	
	let of_srgb ({SRGB.red; green; blue} as x) =
		if is_valid_srgb x then (
			let convert value =
				int_of_float (Float.round (float_of_int Param.max_int *. value))
			in
			({red = convert red; green = convert green; blue = convert blue}: t)
		) else
		invalid_arg "SRGB_Int.of_srgb";;
	
	let to_srgb ({red; green; blue} as x: t) =
		if is_valid x then (
			let convert value =
				float_of_int value /. float_of_int Param.max_int
			in
			{SRGB.red = convert red; green = convert green; blue = convert blue}
		) else
		invalid_arg "SRGB_Int.to_srgb";;
	
	let of_rgb (x: rgb_t) =
		of_srgb (srgb_of_rgb x);;
	
	let to_rgb (x: t) =
		rgb_of_srgb (to_srgb x);;
end;;

module SRGB24 = Color__SRGB24;;

(* HSV *)

let min_max_3 red green blue =
	if red <= green then (
		if green <= blue then red, blue else
		(* red, blue < green *)
		if red <= blue then red, green else
		blue, green
	) else
	(* green < red *)
	if red <= blue then green, blue else
	(* green, blue < red *)
	if green <= blue then green, red else
	blue, red;;

let to_hue_6 {RGB.red; green; blue} d max =
	if d = 0.0 then 0.0 else
	let result =
		if green = max then 2.0 *. d +. (blue -. red) else
		if blue = max then 4.0 *. d +. (red -. green) else
		let result = green -. blue in
		if result >= 0.0 then result else
		6.0 *. d +. result
	in
	result /. d;;

let to_hue x d max =
	2.0 *. Float.pi /. 6.0 *. to_hue_6 x d max;;

type hsv_t = {hue: float; saturation: float; value: float};;

let is_valid_hsv {hue; saturation; value} =
	hue >= 0.0 && hue < 2. *. Float.pi
		&& saturation >= 0.0 && saturation <= 1.0
		&& value >= 0.0 && value <= 1.0;;

let hsv_of_rgb ({RGB.red; green; blue} as x: rgb_t) =
	if is_valid_rgb x then (
		let min, max = min_max_3 red green blue in
		let value = max in
		if min < max then (
			let d = max -. min in
			let hue = to_hue x d max in
			let saturation = d /. max in
			{hue; saturation; value}
		) else
		{hue = 0.0; saturation = 0.0; value}
	) else
	invalid_arg "hsv_of_rgb";;

let rgb_of_hsv ({hue; saturation; value} as x) =
	if is_valid_hsv x then (
		let h = 6.0 /. (2.0 *. Float.pi) *. hue in
		let d, _ = modf h in
		let min = value *. (1.0 -. saturation) in
		let max = value in
		let m_a = value *. (1.0 -. saturation *. d) in
		let m_b = value *. (1.0 -. saturation *. (1.0 -. d)) in
		if h < 1.0 then {RGB.red = max; green = m_b; blue = min} else
		if h < 2.0 then {RGB.red = m_a; green = max; blue = min} else
		if h < 3.0 then {RGB.red = min; green = max; blue = m_b} else
		if h < 4.0 then {RGB.red = min; green = m_a; blue = max} else
		if h < 5.0 then {RGB.red = m_b; green = min; blue = max} else
		{RGB.red = max; green = min; blue = m_a}
	) else
	invalid_arg "rgb_of_hsv";;

module HSV = struct
	type t = hsv_t = {hue: float; saturation: float; value: float};;
	
	let is_valid = is_valid_hsv;;
	
	let make ~hue ~saturation ~value =
		let result = {hue; saturation; value} in
		if is_valid_hsv result then result else
		invalid_arg "HSV.make";;
	
	let of_rgb = hsv_of_rgb;;
	
	let to_rgb = rgb_of_hsv;;
end;;

(* HSL *)

type hsl_t = {hue: float; saturation: float; lightness: float};;

let is_valid_hsl {hue; saturation; lightness} =
	hue >= 0.0 && hue < 2. *. Float.pi
		&& saturation >= 0.0 && saturation <= 1.0
		&& lightness >= 0.0 && lightness <= 1.0;;

let hsl_of_rgb ({RGB.red; green; blue} as x: rgb_t) =
	if is_valid_rgb x then (
		let min, max = min_max_3 red green blue in
		let lightness = (max +. min) /. 2.0 in
		if min < max then (
			let d = max -. min in
			let hue = to_hue x d max in
			let saturation = d /. (1.0 -. abs_float (2.0 *. lightness -. 1.0)) in
			{hue; saturation; lightness}
		) else
		{hue = 0.0; saturation = 0.0; lightness = lightness}
	) else
	invalid_arg "hsl_of_rgb";;

let rgb_of_hsl ({hue; saturation; lightness} as x) =
	if is_valid_hsl x then (
		let h = 3.0 /. (2.0 *. Float.pi) *. hue in
		let d, _ = modf h in
		let c = (1.0 -. abs_float (2.0 *. lightness -. 1.0)) *. saturation in
		let min = lightness -. c /. 2.0 in
		let max = min +. c in
		let mid = min +. (1.0 -. abs_float (2.0 *. d -. 1.0)) *. c in
		if h < 0.5 then {RGB.red = max; green = mid; blue = min} else
		if h < 1.0 then {RGB.red = mid; green = max; blue = min} else
		if h < 1.5 then {RGB.red = min; green = max; blue = mid} else
		if h < 2.0 then {RGB.red = min; green = mid; blue = max} else
		if h < 2.5 then {RGB.red = mid; green = min; blue = max} else
		{RGB.red = max; green = min; blue = mid}
	) else
	invalid_arg "rgb_of_hsl";;

module HSL = struct
	type t = hsl_t = {hue: float; saturation: float; lightness: float};;
	
	let is_valid = is_valid_hsl;;
	
	let make ~hue ~saturation ~lightness =
		let result = {hue; saturation; lightness} in
		if is_valid_hsl result then result else
		invalid_arg "HSL.make";;
	
	let of_rgb = hsl_of_rgb;;
	
	let to_rgb = rgb_of_hsl;;
end;;

(* HSY *)

module type HSYS = sig
	type t = {hue: float; saturation: float; intensity: float}
	
	val is_valid: t -> bool
	val make: hue:float -> saturation:float -> intensity:float -> t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end;;

module HSY
	(Param: sig
		val red: float
		val green: float
		val blue: float
	end): HSYS =
struct
	type t = {hue: float; saturation: float; intensity: float};;
	
	let param_den = Param.red +. Param.green +. Param.blue;;
	
	let luma ~red ~green ~blue =
		(Param.red *. red +. Param.green *. green +. Param.blue *. blue) /. param_den;;
	
	let max_sat hue_6 =
		(
			if hue_6 < 1.0 then Param.red +. Param.green *. hue_6 else
			if hue_6 < 2.0 then Param.green +. Param.red *. (1.0 -. (hue_6 -. 1.0)) else
			if hue_6 < 3.0 then Param.green +. Param.blue *. (hue_6 -. 2.0) else
			if hue_6 < 4.0 then Param.blue +. Param.green *. (1.0 -. (hue_6 -. 3.0)) else
			if hue_6 < 5.0 then Param.blue +. Param.red *. (hue_6 -. 4.0) else
			Param.red +. Param.blue *. (1.0 -. (hue_6 -. 5.0))
		) /. param_den;;
	
	let is_valid {hue; saturation; intensity} =
		hue >= 0.0 && hue < 2. *. Float.pi
			&& saturation >= 0.0 && saturation <= 1.0
			&& intensity >= 0.0 && intensity <= 1.0;;
	
	let make ~hue ~saturation ~intensity =
		let result = {hue; saturation; intensity} in
		if is_valid result then result else
		invalid_arg "HSY.make";;
	
	let of_rgb ({RGB.red; green; blue} as x: rgb_t) =
		if is_valid_rgb x then (
			let intensity = luma ~red ~green ~blue in
			let min, max = min_max_3 red green blue in
			if min < max then (
				let d = max -. min in
				let hue_6 = to_hue_6 x d max in
				let hue = 2.0 *. Float.pi *. hue_6 /. 6.0 in
				let saturation =
					let max_sat = max_sat hue_6 in
					if intensity <= max_sat then d *. max_sat /. intensity else
					d *. (1.0 -. max_sat) /. (1.0 -. intensity)
				in
				let saturation =
					if saturation <= 1.0 then saturation else (
						assert (saturation -. 1.0 < 0x1.0p-12);
						1.0
					) (* fix computation error *)
				in
				{hue; saturation; intensity}
			) else
			{hue = 0.0; saturation = 0.0; intensity}
		) else
		invalid_arg "HSY.of_rgb";;
	
	let to_rgb ({hue; saturation; intensity} as x: t) =
		if is_valid x then (
			let hue_3 = 3.0 /. (2.0 *. Float.pi) *. hue in
			let d, _ = modf hue_3 in
			let hue_6 = 2.0 *. hue_3 in
			let max =
				let max_sat = max_sat hue_6 in
				if intensity <= max_sat then saturation *. intensity /. max_sat else
				saturation *. (1.0 -. intensity) /. (1.0 -. max_sat)
			in
			let mid = (1.0 -. abs_float (2.0 *. d -. 1.0)) *. max in
			let min = 0.0 in
			let red, green, blue =
				if hue_6 < 1.0 then max, mid, min else
				if hue_6 < 2.0 then mid, max, min else
				if hue_6 < 3.0 then min, max, mid else
				if hue_6 < 4.0 then min, mid, max else
				if hue_6 < 5.0 then mid, min, max else
				max, min, mid
			in
			let m = intensity -. luma ~red ~green ~blue in
			let m =
				if m >= 0.0 then m else (
					assert (m > -. 0x1.0p-12);
					0.0
				) (* ignore computation error *)
			in
			let red = red +. m in
			let green = green +. m in
			let blue = blue +. m in
			{RGB.red; green; blue}
		) else
		invalid_arg "HSY.to_rgb";;
end;;

module HSI = Color__HSI;;
