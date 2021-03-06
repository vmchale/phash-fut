module perceptual_hash (M: float) = {

  local import "lib/github.com/diku-dk/statistics/statistics"

  module statistics = mk_statistics M

  local let to_u64 (x: [64]bool) : u64 =
    let bool_to_u64 (b: bool) : u64=
      if b then 1 else 0

    let is : [64]u64 =
      u64.iota 64
    in

    u64.sum (map2 (\b i -> bool_to_u64 b * (2 ** i)) x is)

  local let median (x: []M.t) : M.t =
    statistics.median x

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

  local let conj_dct (x: [32][32]M.t) : [32][32]M.t =
    let dct32 : *[32][32]M.t =
      let n = M.from_fraction 32 1
      in

      tabulate_2d 32 32
        (\i j ->
          M.sqrt((M.from_fraction 2 1) M./ n) M.* M.cos((M.from_fraction (2*i*j+1) 1) M.* M.pi M./ ((M.from_fraction 2 1) M.* n)))
    in

    matmul dct32 (matmul x (transpose dct32))

  local let mean_filter [m][n] (x: [m][n]M.t) : [m][n]M.t =
    let ker_n = 7
    let x_rows = length x
    let x_cols = length (head x)

    let extended_n = ker_n / 2

    -- extend it at the edges
    let extended =
      tabulate_2d (x_rows + ker_n - 1) (x_cols + ker_n - 1)
        (\i j ->
          let i' =
            if i <= extended_n then
              0
            else
              if i + extended_n >= x_rows
                then x_rows - 1
                else i - extended_n
          let j' =
            if j <= extended_n then
              0
            else
              if j + extended_n >= x_cols
                then x_cols - 1
                else j - extended_n
          in unsafe (x[i'])[j'])

    let window (row_start: i32) (col_start: i32) (row_end: i32) (col_end: i32) (x: [][]M.t) : [][]M.t =
      map (\x_i -> x_i[col_start:col_end]) (x[row_start:row_end])

    let mean (x: [][]M.t) : M.t =
      let rows = length x
      let cols = length (head x)

      in M.sum (map M.sum x) M./ (M.from_fraction (rows * cols) 1)

    in

    tabulate_2d x_rows x_cols
      (\i j ->
        let surroundings = window i j (i + ker_n) (j + ker_n) extended
        in
        mean surroundings)

  let img_hash : [][]M.t -> u64 =
    to_u64 <-< above_med <-< flatten <-< crop 8 8 <-< conj_dct <-< shrink 32 32 <-< mean_filter

}

module phash_32 = perceptual_hash f32
module phash_64 = perceptual_hash f64

entry crop_f64 = phash_64.crop
entry shrink_f64 = phash_64.shrink

entry img_hash_f64 = phash_64.img_hash
entry img_hash_f32 = phash_32.img_hash

-- Test crop dimensions
-- ==
-- entry: crop_f64
-- input { 1 2 [[0.0, 1.0, 3.0], [2.0, 2.0, 3.0], [2.0, 1.0, 3.0]] }
-- output { [[0.0, 1.0]] }

-- Shrink function example
-- ==
-- entry: shrink_f64
-- input { 2 2 [[1.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 0.0], [1.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 0.0]] }
-- output { [[1.0, 1.0], [1.0, 1.0]] }

-- Bench img_hash
-- ==
-- entry: img_hash_f64
-- compiled random input { [400][400]f64 }
-- auto output
