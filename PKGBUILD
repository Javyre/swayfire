# This is an example PKGBUILD file. Use this as a start to creating your own,
# and remove these comments. For more information, see 'man PKGBUILD'.
# NOTE: Please fill out the license field for your package! If it is unknown,
# then please put 'unknown'.

# Maintainer: Waseem Alkurdi <waseem19992009@outlook.com>
pkgname=swayfire-git
pkgver=r32.f364d51
pkgrel=1
epoch=
pkgdesc="Sway/I3 inspired tiling window manager for Wayfire."
arch=(x86_64)
url=""
license=('GPL')
groups=()
depends=(meson)
makedepends=(git)
checkdepends=()
optdepends=()
provides=("${pkgname%-git}")
conflicts=("${pkgname%-git}")
replaces=()
backup=()
options=()
install=
changelog=
source=("swayfire-git::git+https://github.com/Javyre/swayfire#branch=master")
noextract=()
md5sums=()
validpgpkeys=()

pkgver() {
  cd "$srcdir/${pkgname%-VCS}"
  ( set -o pipefail
    git describe --long 2>/dev/null | sed 's/\([^-]*-g\)/r\1/;s/-/./g' ||
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
  )
}

build() {
	cd "$srcdir/${pkgname%-VCS}"
	meson setup --prefix /usr build
	meson compile -C build
}

package() {
	cd "$srcdir/${pkgname%-VCS}"
	echo $pkgdir
	DESTDIR="$pkgdir/" meson install -C build
}
md5sums=('SKIP')
