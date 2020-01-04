import phash

import imageio
import numpy as np

mod = phash.phash()

img = np.array(imageio.imread('data/frog.jpeg', pilmode='F'))

print(img)
print(mod.mean_filter_f32(img))
