import timeit

setup = """
import phash
import imageio
import numpy as np

mod = phash.phash()
"""

read_image = """
img0 = np.array(imageio.imread('data/frog.jpeg', pilmode='F'))
mod.img_hash_f32(img0)
"""

print('data/frog.jpeg', timeit.timeit(read_image, setup=setup, number=100) * 10, "ms")

setup_imagehash = """
from PIL import Image
import imagehash
"""

hash_bench = """
imagehash.phash(Image.open('data/frog.jpeg'))
"""

print('data/frog.jpeg', timeit.timeit(hash_bench, setup=setup_imagehash, number=100) * 10, "ms")
