let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
    });
  });

  myHaskellPkgsOverlay = (hSelf: hSuper: {
    tmcr-newlogic-editor = hSelf.callCabal2nix "tmcr-newlogic-editor" ./. {};
    tmcr-newlogic-lib = hSelf.callCabal2nix "tmcr-newlogic-lib" ../tmcr-newlogic-lib {};
    miso = hSelf.callCabal2nix "miso" (builtins.fetchGit { url = "https://github.com/dmjio/miso"; ref = "refs/tags/1.8.3";}) {};
  });
  
  myDevTools = with myNixPkgs; [
    cabal-install 
    haskellPackages.ghcid
    haskell-language-server
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.tmcr-newlogic-editor.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
