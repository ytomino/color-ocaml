let diff = 0x0001p-16;;

for ired = 0 to 4 do
	for igreen = 0 to 4 do
		for iblue = 0 to 4 do
			let rgb = Color.RGB.make ~red:(float_of_int ired /. 4.0)
				~green:(float_of_int igreen /. 4.0) ~blue:(float_of_int iblue /. 4.0)
			in
			(* sRGB *)
			let srgb = Color.srgb_of_rgb rgb in
			let via_srgb = Color.rgb_of_srgb srgb in
			assert (Color.RGB.distance via_srgb rgb < diff);
			(* HSV *)
			let hsv = Color.hsv_of_rgb rgb in
			let via_hsv = Color.rgb_of_hsv hsv in
			assert (Color.RGB.distance via_hsv rgb < diff);
			(* HSL *)
			let hsl = Color.hsl_of_rgb rgb in
			let via_hsl = Color.rgb_of_hsl hsl in
			assert (Color.RGB.distance via_hsl rgb < diff);
			(* HSI *)
			let hsi = Color.HSI.of_rgb rgb in
			let via_hsi = Color.HSI.to_rgb hsi in
			assert (Color.RGB.distance via_hsi rgb < diff)
		done
	done
done;;

prerr_endline "ok";;
