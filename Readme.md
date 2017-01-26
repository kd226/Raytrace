# Raytrace
Rendering engine written in Haskell using pathtracing algoritm. Rays are terminated
when their importance (maximal percent of influence on output image) goes below 0.001.
Currently only Lambertian BRDF supports Monte Carlo integration.

## Scene
Right now Raytrace lacks any sophisticated scene manipulation from a file, so it just renders predefined scene in 1366x768 resolution. Hovewer, changing resolution or any other setting, as well as modifing scene is intuitive and easily doable by just changing appropriate lines in Main.hs.

## Output
Rendered images are saved in .ppm format file which is easily
## Build instruction
``` bash
  stack build
```

## Run instruction
``` bash
  stack exec -- Raytrace-exe
```

## Example images
All example images are rendered in resolution 500 by 500 pixels and depth of rays of 16 with Intel Core i7 on all 4 threads.

![Image of 1 sample](500px-16dip-1s.png)     |![Image of 2 samples](500px-16dip-2s.png)
:-------------------------------------------:|:----------------------------------------:
1 sample image - 32 seconds.                 |2 samples image - 1 minute and 3 seconds.

![Image of 4 samples](500px-16dip-4s.png)    |![Image of 8 samples](500px-16dip-8s.png)
:-------------------------------------------:|:----------------------------------------:
4 samples image - 2 minutes and 7 seconds.   |8 samples image - 4 minutes and 20 seconds.

To show full power of the renderer this a is scene in 2732x1536 with 8 samples and render time of about 3 hours.
![Image of 8 samples](2732px-16dip-8s.png)


## TODO
- Add diffusive reflection
