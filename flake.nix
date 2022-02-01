{
	description = "A Rust web server including a NixOS module";

	# Nixpkgs / NixOS version to use.
	inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

	outputs = { self, nixpkgs }:
	{
		# The default package for 'nix build'. This makes sense if the
		defaultPackage."x86_64-linux" = 
			with import nixpkgs{ system = "x86_64-linux"; };
			let
				deps = import ./rebar-deps.nix { inherit (pkgs) fetchHex fetchFromGitHub; };
			in
			pkgs.stdenv.mkDerivation{
				name = "runddb";
				src = self;

				buildPhase = ''
				mkdir -p _checkouts
				${toString (pkgs.lib.mapAttrsToList (k: v: ''
				cp -R --no-preserve=mode ${v} _checkouts/${k}
				'') deps)}
				HOME=. rebar3 tar
				'';

				installPhase = ''
				mkdir -p $out
				tar -xzvf _build/default/rel/*/*.tar.gz  -C $out/
				'';


				buildInputs = [ openssl rebar3 gnutar ];
			};

		# A NixOS overlay.
		overlay = 
			let 
				runddb = self.defaultPackage."x86_64-linux";
			in
			final: prev: {

			#config = {
				systemd.services.runddb = {
					description = "Online REST database for cattle";

					wantedBy="multi-user.target";

					serviceConfig = {
						Type="notify";
						User="runddb";

						ExecStart="${runddb}/bin/runddb daemon";
						WatchdogSec="10s";
						Restart="on-failure";
					};

			#	};

			};
		};
	};
}
