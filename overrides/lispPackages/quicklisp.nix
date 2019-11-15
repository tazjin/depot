{ lib, lispPackages }:

let inherit (lispPackages) buildLispPackage qlOverrides fetchurl;
in lispPackages // lib.fix(self: {
  "s-xml" = buildLispPackage
    ((f: x: (x // (f x)))
       (qlOverrides."s-xml" or (x: {}))
       (import ./quicklisp-to-nix-output/s-xml.nix {
         inherit fetchurl;
       }));

  "s-sysdeps" = buildLispPackage
    ((f: x: (x // (f x)))
       (qlOverrides."s-sysdeps" or (x: {}))
       (import ./quicklisp-to-nix-output/s-sysdeps.nix {
         inherit fetchurl;
       }));

  "cl-prevalence" = buildLispPackage
    ((f: x: (x // (f x)))
       (qlOverrides."cl-prevalence" or (x: {}))
       (import ./quicklisp-to-nix-output/cl-prevalence.nix {
         inherit fetchurl;
         inherit (self) s-sysdeps s-xml;
       }));
})
