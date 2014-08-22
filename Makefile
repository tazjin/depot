VERSION=$(shell bash -c "grep -P -o -e '\d\.\d$$' TazBlog.cabal | head -n1")
ARCH_PKG=arch/tazblog-$(VERSION)-1-x86_64.pkg.tar.xz
export ARCH_PKG

all: archpkg docker

archpkg: $(ARCH_PKG)

$(ARCH_PKG):
	cd arch && makepkg

docker: archpkg
	cat Dockerfile.raw | envsubst > Dockerfile; \
	docker build -t tazjin/tazblog .

clean:
	rm -rf dist arch/*.pkg.tar.xz arch/pkg arch/src arch/*. Dockerfile
