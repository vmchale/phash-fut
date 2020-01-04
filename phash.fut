module perceptual_hash (M: float) = {

  import "lib/github.com/diku-dk/sorts/radix_sort"

  let to_u64 (x: [64]bool) : u64 =
    let bool_to_u64 (b: bool) : u64=
      if b then 1 else 0

    let is : [64]u64 =
      u64.iota 64
    in

    u64.sum (map2 (\b i -> bool_to_u64 b * (2 ** i)) x is)

  let median (x: []M.t) : M.t =
    let sorted = radix_sort_float M.num_bits M.get_bit x
    let n = length x
    in

    if n % 2 == 0
      then (sorted[n/2 - 1] M.+ sorted[n/2]) M./ (M.from_fraction 2 1)
      else sorted[n/2]

  let above_med [n] (x: [n]M.t) : [n]bool =
    let med = median x
    in map (M.> med) x

  let matmul [n][m][p] (x: [n][m]M.t) (y: [m][p]M.t) : [n][p]M.t =
    map (\x_i ->
          map (\y_j -> M.sum (map2 (M.*) x_i y_j))
              (transpose y))
        x

  let crop [m][n] (i: i32) (j: i32) (x: [m][n]M.t) : [i][j]M.t =
    take i (map (\x_i -> take j x_i) x)

  -- TODO convolve/reflect at edges
  -- TODO resizing image?

  let conj_dct (x: [32][32]M.t) : [32][32]M.t =
    let dct32 : *[32][32]M.t =
      let n = M.from_fraction 32 1
      in

      tabulate_2d 32 32
        (\i j ->
          M.sqrt((M.from_fraction 2 1) M./ n) M.* M.cos((M.from_fraction (2*i*j+1) 1) M.* M.pi M./ ((M.from_fraction 2 1) M./ n)))
    in

    matmul dct32 (matmul x (transpose dct32))
}

module phash_32 = perceptual_hash f32
module phash_64 = perceptual_hash f64

entry crop_f64 = phash_64.crop
entry median_f64 = phash_64.median

-- Test crop dimensions
-- ==
-- entry: crop_f64
-- input { 1 2 [[0.0, 1.0, 3.0], [2.0, 2.0, 3.0], [2.0, 1.0, 3.0]] }
-- output { [[0.0, 1.0]] }

-- Test median function
-- ==
-- entry: median_f64
-- input { [2.0, 1.0, 3.0] }
-- output { 2.0 }
-- input { [2.0, 4.0, 3.0, 3.5, 2.0, 4.0] }
-- output { 3.25 }
