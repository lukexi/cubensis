# cubensis
Cube rendering playground

Code live with:
`halive app/Main.hs`

Installation:
```
Install Steam and SteamVR
git clone git@github.com:lukexi/linear-extra
git clone git@github.com:lukexi/lens-extra
git clone git@github.com:lukexi/glfw-pal
git clone git@github.com:lukexi/vr-pal
git clone git@github.com:lukexi/openvr-hs
git clone git@github.com:lukexi/gl-pal
git clone git@github.com:lukexi/halive
# Works around a segfault related to GLFW on Windows
git clone -b win-halive-fix http://github.com/lukexi/bindings-GLFW

(cd halive && stack install)

git clone git@github.com:lukexi/cubensis
cd cubensis
stack build
halive app/Main.hs
```
