module perceptual_hash (M: float) = {

  import "lib/github.com/diku-dk/sorts/radix_sort"

  local let to_u64 (x: [64]bool) : u64 =
    let bool_to_u64 (b: bool) : u64=
      if b then 1 else 0

    let is : [64]u64 =
      u64.iota 64
    in

    u64.sum (map2 (\b i -> bool_to_u64 b * (2 ** i)) x is)

  local let sort : []M.t -> []M.t =
    radix_sort_float M.num_bits M.get_bit

  let median (x: []M.t) : M.t =
    let sorted = sort x
    let n = length x
    in

    if n % 2 == 0
      then (sorted[n/2 - 1] M.+ sorted[n/2]) M./ (M.from_fraction 2 1)
      else sorted[n/2]

  local let above_med [n] (x: [n]M.t) : [n]bool =
    let med = median x
    in map (M.> med) x

  local let matmul [n][m][p] (x: [n][m]M.t) (y: [m][p]M.t) : [n][p]M.t =
    map (\x_i ->
          map (\y_j -> M.sum (map2 (M.*) x_i y_j))
              (transpose y))
        x

  let crop [m][n] (i: i32) (j: i32) (x: [m][n]M.t) : [i][j]M.t =
    map (\x_i -> x_i[:j]) (x[:i])

  -- | This is an unsatisfying way to resize an image. Basically we throw away a bunch of
  -- points so it's the right size.
  let shrink (m: i32) (n: i32) (x: [][]M.t) : [m][n]M.t =
    let rows = length x
    let cols = length (head x)
    in

    tabulate_2d m n
      (\i j -> unsafe (x[i * (rows / m)])[j * (cols / n)])

  let conj_dct (x: [32][32]M.t) : [32][32]M.t =
    let dct32 : *[32][32]M.t =
      let n = M.from_fraction 32 1
      in

      tabulate_2d 32 32
        (\i j ->
          M.sqrt((M.from_fraction 2 1) M./ n) M.* M.cos((M.from_fraction (2*i*j+1) 1) M.* M.pi M./ ((M.from_fraction 2 1) M./ n)))
    in

    matmul dct32 (matmul x (transpose dct32))

  let mean_filter [m][n] (x: [m][n]M.t) : [m][n]M.t =
    let ker_rows = 7
    let ker_cols = 7
    let x_rows = length x
    let x_cols = length (head x)

    let extended_row = ker_rows / 2
    let extended_col = ker_cols / 2

    -- extend it at the edges
    let extended =
      tabulate_2d (x_rows + ker_rows - 1) (x_cols + ker_cols - 1)
        (\i j ->
          let i' =
            if i <= extended_row then
              0
            else
              if i + extended_row >= x_rows
                then x_rows - 1
                else i - extended_row
          let j' =
            if j <= extended_col then
              0
            else
              if j + extended_col >= x_cols
                then x_cols - 1
                else j - extended_col
          in unsafe (x[i'])[j'])

    let extract (x : [7][7]M.t) : M.t =
      M.sum (tabulate 7 (\i -> (x[i])[i]))

    let window (row_start: i32) (col_start: i32) (row_end: i32) (col_end: i32) (x: [][]M.t) : [][]M.t =
      map (\x_i -> x_i[col_start:col_end]) (x[row_start:row_end])

    let mean (x: [][]M.t) : M.t =
      let rows = length x
      let cols = length (head x)

      in M.sum (map M.sum x) M./ (M.from_fraction (rows * cols) 1)

    let stenciled =
      tabulate_2d x_rows x_cols
        (\i j ->
          let surroundings = window i j (i + ker_rows) (j + ker_cols) extended
          in
          mean surroundings)
    in

    stenciled

  let img_hash : [][]M.t -> u64 =
    to_u64 <-< above_med <-< flatten <-< crop 8 8 <-< conj_dct <-< shrink 32 32 <-< mean_filter

}

module phash_32 = perceptual_hash f32
module phash_64 = perceptual_hash f64

entry crop_f64 = phash_64.crop
entry median_f64 = phash_64.median
entry shrink_f64 = phash_64.shrink

entry mean_filter_f32 = phash_32.mean_filter
entry shrink_f32 = phash_32.shrink
entry dct_f32 = phash_32.conj_dct

entry img_hash_f64 = phash_64.img_hash
entry img_hash_f32 = phash_32.img_hash

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

-- Shrink function example
-- ==
-- entry: shrink_f64
-- input { 2 2 [[1.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 0.0], [1.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 0.0]] }
-- output { [[1.0, 1.0], [1.0, 1.0]] }
