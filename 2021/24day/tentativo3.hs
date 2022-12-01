

block :: (Int, Int, Int, Int) -> Int -> Int -> Int -> (Int, Int, Int, Int)
block (_, _, _, z) num1 num2 w = (0, 0, 0, (z `div` num3) + (25 * b + 1) + (num2 * b))
	where num3 = if num1 < 0 then 26 else 1
	      b = if (((z `div` num3) `mod` 26) + num1) == w then 0 else 1

find = 
	where v1 k    = block (0, 0, 0, 0) 11 7 1 k
              v2 k s  = block s 14 8 k
	      v3 k s  = block s 10 16 k
	      v4 k s  = block s 14 8 k
	      v5 k s  = block s -8 3 k
	      v6 k s  = block s 14 12 k
	      v7 k s  = block s -11 1 k
	      v8 k s  = block s 10 8 k
	      v9 k s  = block s -6 8 k
	      v10 k s = block s -9 14 k
	      v11 k s = block s 12 4 k
	      v12 k s = block s -5 14 k
	      v13 k s = block s -4 15 k
	      v14 k s = block s -9 6 k

-- parte_prima(num1, num2, num3) {
-- 	inp w
-- 	mul x 0
-- 	add x z      (w, z, y, z)
-- 	mod x 26     (w, z mod 26, y, z)
-- 	div z num3   (w, (z / num3) mod 26, y, z)
-- 	add x num1   (w, (z mod 26) + num1, y, z)
-- 	eql x w      (w, eql ((z mod 26) + num1) w, y, z)
-- 	eql x 0      (w, !(((z mod 26) + num1) == w), y, z)
-- 	mul y 0      (w, !(((z mod 26) + num1) == w), 0, z) 
-- 	add y 25     (w, !(((z mod 26) + num1) == w), 25, z)
-- 	mul y x      (w, !(((z mod 26) + num1) == w), 25 * !(((z mod 26) + num1) == w), z)
-- 	add y 1      (w, !(((z mod 26) + num1) == w), (25 * !(((z mod 26) + num1) == w)) + 1, z)
-- 	mul z y      (w, !(((z mod 26) + num1) == w), (25 * !(((z mod 26) + num1) == w)) + 1, z + (25 * !(((z mod 26) + num1) == w) + 1))
-- 	mul y 0      (w, !(((z mod 26) + num1) == w), 0, z + (25 * !(((z mod 26) + num1) == w) + 1))
-- 	add y num2   (w, !(((z mod 26) + num1) == w), num2, z + (25 * !(((z mod 26) + num1) == w) + 1))
-- 	mul y x      (w, !(((z mod 26) + num1) == w), num2 * !(((z mod 26) + num1) == w), z + (25 * !(((z mod 26) + num1) == w) + 1))
-- 	add z y      (w, !(((z mod 26) + num1) == w), num2 * !(((z mod 26) + num1) == w), z + (25 * !(((z mod 26) + num1) == w) + 1) + (num2 * !(((z mod 26) + num1) == w)))
-- }
