import phash

import imageio
import numpy as np

mod = phash.phash()

img0 = np.array(imageio.imread('data/frog.jpeg', pilmode='F'))
img1 = np.array(imageio.imread('data/frog.png', pilmode='F'))
img2 = np.array(imageio.imread('data/cat.png', pilmode='F'))

print(mod.img_hash_f32(img0))
print(mod.img_hash_f32(img1))
print(mod.img_hash_f32(img2))
