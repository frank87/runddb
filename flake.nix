{
	description = "A Rust web server including a NixOS module";

	# Nixpkgs / NixOS version to use.
	inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

	outputs = { self, nixpkgs }:
	let
		system="x86_64-linux";
	in
	{
		# The default package for 'nix build'. This makes sense if the
		packages.${system}.default =
			with import nixpkgs{ inherit system; };
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

		# A NixOS module.
		nixosModules.default = ( { pkgs, self }: {

				config = {
					systemd.services.runddb = {
						description = "Online REST database for cattle";

						wantedBy="multi-user.target";

						serviceConfig = {
							Type="notify";
							User="runddb";

							ExecStart="${self}/bin/runddb daemon";
							WatchdogSec="10s";
							Restart="on-failure";
						};

					};

				};
			} );
	};
}
