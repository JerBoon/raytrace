# raytrace
A ray tracing package using R

NB: The **raytrace** package utilises my **vecspace** and (optionally) **vecshapes** packages to define the basic geometry of the world. The **vecshapes** is a collection of the more interesting objects, and currently includes the geometry for the dodecahedron and teapotahedron.
All are available on this github.

This package is still very much a work in progress at this stage, but I've implemented at least *some* elements of ray tracing so far. Lighting and shadowing are still a bit rudimentary at the moment, but it seems to all basically work.

![teapotetc](https://user-images.githubusercontent.com/23141865/38453044-32e00fe0-3a47-11e8-94a1-2b2ed474796e.png)

The teapot is an interesting one. Writing the function to create the basic teapot was a 20 minute job, courtesy of the geometry at http://www.holmes3d.net/graphics/teapot/ and it validated the usefulness of the world-building functionality in the **vecshapes** package. But it certainly throws up the need for some better and more efficient indexing within the object hierarchy. The image above took almost an hour to render at 500x500 pixels!



